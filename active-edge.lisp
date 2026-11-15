;;; Active-screen edge line
;;;
;;; With more than one output, the current output shows which screen
;;; is active: its bar takes the active background (see
;;; bar-background-color), and a current output without a bar shows a
;;; line along one of its edges instead. The line is a WM-owned
;;; river_shell_surface_v1, one per output, transparent to input and
;;; placed above every other node, so content never covers it.
;;;
;;; Protocol discipline (river-window-management-v1):
;;;   get_shell_surface, get_node  anytime; the surface must be fresh
;;;   set_position, place_top      render sequence
;;;   sync_next_commit             render sequence only; the surface
;;;                                must commit before render_finish
;;;   destroy                      anytime
;;; All of it runs from active-edge-render-pass, inside
;;; on-render-sequence.
;;;
;;; Settings, under :screens:
;;;   :active-edge        :top (the default), :left, :bottom or
;;;                       :right; any other value turns the line off
;;;   :active-edge-width  line width in px, clamped to 1-5 (default 3)
;;;   :active-edge-color  "#rrggbb", or T (the default) for the
;;;                       (:theme :focused-border) colour

(in-package #:rhizome)

(defvar *active-edges* (make-hash-table :test #'eq)
  "OUTPUT -> line state plist:
(:surface :shell :node :ptr :fd :size :pool :buffer :w :h
 :drawn :mapped :position).")

(defparameter *active-edge-sides* '(:top :left :bottom :right)
  "The edges the line can follow.")

;;; --- Settings and geometry ---

(defun active-edge-style ()
  "The line as (VALUES EDGE WIDTH R G B), or NIL when it is off.
WIDTH is (:screens :active-edge-width) clamped to 1-5; a value that
is not an integer gives 3."
  (let ((edge (config-getf '(:screens) :active-edge :default :top))
        (width (config-getf '(:screens) :active-edge-width :default 3))
        (color (config-getf '(:screens) :active-edge-color :default t)))
    (when (member edge *active-edge-sides*)
      (multiple-value-bind (r g b)
          (if (stringp color)
              (parse-hex-color color)
              (theme-border-rgb t))
        (values edge
                (if (integerp width) (max 1 (min 5 width)) 3)
                r g b)))))

(defun active-edge-geometry (edge width x y w h)
  "The line's rectangle as (VALUES X Y W H): WIDTH px thick along EDGE
of the output rectangle X Y W H."
  (ecase edge
    (:top (values x y w width))
    (:bottom (values x (+ y (- h width)) w width))
    (:left (values x y width h))
    (:right (values (+ x (- w width)) y width h))))

(defun active-edge-wanted-p (output current output-count bar-p)
  "True when OUTPUT shows the line: OUTPUT is CURRENT, OUTPUT-COUNT is
more than one, and BAR-P, a bar on OUTPUT, is false."
  (and output
       (eq output current)
       (> output-count 1)
       (not bar-p)))

(defun active-edge-output ()
  "The current output, as the bar snapshot marks it."
  (first (all-outputs)))

(defun active-edge-target (output current)
  "The line's rectangle on OUTPUT as (VALUES X Y W H) in global
coordinates, or NIL when OUTPUT shows no line. CURRENT is the current
output."
  (multiple-value-bind (edge width) (active-edge-style)
    (when (and edge
               (plusp (output-width output))
               (plusp (output-height output))
               (active-edge-wanted-p output current (length (all-outputs))
                                     (bar-output-selected-p output)))
      (active-edge-geometry edge width
                            (output-x output) (output-y output)
                            (output-width output) (output-height output)))))

;;; --- Pixels ---

(defun active-edge-free (state)
  "Release STATE's shm buffer, pool, mapping, and memfd."
  (when (getf state :buffer)
    (wl:wl-buffer.destroy (getf state :buffer)))
  (when (getf state :pool)
    (wl:wl-shm-pool.destroy (getf state :pool)))
  (when (getf state :ptr)
    (cffi:foreign-funcall "munmap" :pointer (getf state :ptr)
                          :unsigned-long (getf state :size) :int))
  (when (getf state :fd)
    (cffi:foreign-funcall "close" :int (getf state :fd) :int))
  nil)

(defun active-edge-draw (state w h r g b)
  "Fill STATE's W x H shm buffer with opaque R G B, reallocating it
when W or H changes. Returns the updated STATE, or NIL when no buffer
could be allocated."
  (unless (and (eql (getf state :w) w) (eql (getf state :h) h))
    ;; decoration-shm reuses a buffer of equal byte size, which keeps
    ;; the old stride for a rectangle of the same area and new shape
    (active-edge-free state)
    (setf (getf state :ptr) nil (getf state :buffer) nil
          (getf state :pool) nil (getf state :fd) nil
          (getf state :size) nil (getf state :w) nil (getf state :h) nil))
  (let ((state (decoration-shm state w h)))
    (when state
      (setf (getf state :w) w (getf state :h) h)
      (let ((ptr (getf state :ptr))
            (pixel (make-argb r g b)))
        (dotimes (i (* w h))
          (setf (cffi:mem-aref ptr :unsigned-int i) pixel))
        state))))

;;; --- Surface lifecycle ---

(defun active-edge-create ()
  "A fresh line state: a shell surface with its render-list node,
transparent to input. NIL when the globals are missing."
  (when (and *compositor* *shm* *wm-proxy*)
    (let ((surface (wl:wl-compositor.create-surface *compositor*))
          (region (wl:wl-compositor.create-region *compositor*)))
      (wl:wl-surface.set-input-region surface region)
      (wl:wl-region.destroy region)
      (let* ((shell (rwm:river-window-manager-v1.get-shell-surface
                     *wm-proxy* surface))
             (node (rwm:river-shell-surface-v1.get-node shell)))
        (list :surface surface :shell shell :node node
              :drawn nil :mapped nil :position nil)))))

(defun active-edge-destroy (state)
  "Destroy STATE's node, shell surface, surface, and buffer."
  (handler-case
      (progn
        (when (getf state :node)
          (rwm:river-node-v1.destroy (getf state :node)))
        (when (getf state :shell)
          (rwm:river-shell-surface-v1.destroy (getf state :shell)))
        (when (getf state :surface)
          (wl:wl-surface.destroy (getf state :surface)))
        (active-edge-free state))
    (error (e)
      (format t "~&; Rhizome: active edge cleanup error: ~A~%" e)))
  nil)

(defun active-edge-commit (state buffer w h)
  "Attach BUFFER (NIL unmaps) and commit, synced to this render
sequence."
  (rwm:river-shell-surface-v1.sync-next-commit (getf state :shell))
  (wl:wl-surface.attach (getf state :surface) buffer 0 0)
  (when buffer
    (wl:wl-surface.damage (getf state :surface) 0 0 w h))
  (wl:wl-surface.commit (getf state :surface)))

(defun active-edge-show (state x y w h r g b)
  "Map STATE as a W x H line at global X Y, above every other node.
Redraws only when the size or colour changed. Returns the updated
STATE."
  (let ((key (list w h r g b)))
    (unless (and (getf state :mapped) (equal key (getf state :drawn)))
      (let ((drawn (active-edge-draw state w h r g b)))
        (when drawn
          (setf state drawn
                (getf state :drawn) key
                (getf state :mapped) t)
          (active-edge-commit state (getf state :buffer) w h))))
    (when (getf state :mapped)
      (unless (equal (getf state :position) (list x y))
        (rwm:river-node-v1.set-position (getf state :node) x y)
        (setf (getf state :position) (list x y)))
      (rwm:river-node-v1.place-top (getf state :node)))
    state))

(defun active-edge-hide (state)
  "Unmap STATE if mapped. Returns the updated STATE."
  (when (getf state :mapped)
    (active-edge-commit state nil 0 0)
    (setf (getf state :mapped) nil
          (getf state :drawn) nil))
  state)

;;; --- Render pass ---

(defun active-edge-render-pass ()
  "Keep each output's line current: mapped on the output that shows
it, unmapped elsewhere, destroyed for outputs that are gone. Render
sequence only. Returns NIL."
  (handler-case
      (let ((outputs (all-outputs))
            (current (active-edge-output)))
        (loop for output being the hash-keys of *active-edges*
                using (hash-value state)
              unless (member output outputs :test #'eq)
                do (active-edge-destroy state)
                   (remhash output *active-edges*))
        (multiple-value-bind (edge width r g b) (active-edge-style)
          (declare (ignore edge width))
          (dolist (output outputs)
            (multiple-value-bind (x y w h) (active-edge-target output current)
              (let ((state (or (gethash output *active-edges*)
                               (when x (active-edge-create)))))
                (when state
                  (setf (gethash output *active-edges*)
                        (if x
                            (active-edge-show state x y w h r g b)
                            (active-edge-hide state)))))))))
    (error (e)
      (format t "~&; Rhizome: active edge error: ~A~%" e)))
  nil)
