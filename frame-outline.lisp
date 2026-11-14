;;; Focused-frame outline
;;;
;;; A ring around the frame commands act on, on the output they act
;;; on, so an empty focused frame and the focused head are visible.
;;; River decorates windows only (set_borders, decoration surfaces),
;;; so the ring is a WM-owned river_shell_surface_v1, one per output,
;;; sized to the frame and transparent inside.
;;;
;;; Stacking: the ring's node sits at the bottom of the render list,
;;; below every window. It shows in the gaps around a tiled window
;;; and across an empty frame, and never covers window content or a
;;; float. With the default gap (6) and focused border (2), a ring of
;;; up to 4 px stays clear of the window border.
;;;
;;; Protocol discipline (river-window-management-v1):
;;;   get_shell_surface, get_node  anytime; the surface must be fresh
;;;   set_position, place_bottom   manage or render sequence
;;;   sync_next_commit             render sequence only; the surface
;;;                                must commit before render_finish
;;;   destroy                      anytime
;;; All of it runs from frame-outline-render-pass, inside
;;; on-render-sequence. Hiding attaches a null buffer; the shm buffer
;;; is redrawn only when the size, colour, or width changes.
;;;
;;; Settings, under :theme:
;;;   :frame-outline        "#rrggbb", T (the :focused-border colour,
;;;                         the default), or NIL (off)
;;;   :frame-outline-width  ring width in px (default 2; 0 is off)

(in-package #:rhizome)

(defvar *frame-outlines* (make-hash-table :test #'eq)
  "OUTPUT -> outline state plist:
(:surface :shell :node :ptr :fd :size :pool :buffer :w :h
 :drawn :mapped :position).")

;;; --- Settings ---

(defun frame-outline-style ()
  "The ring colour and width as (VALUES R G B WIDTH), or NIL when the
outline is off."
  (let ((colour (config-getf '(:theme) :frame-outline :default t))
        (width (config-getf '(:theme) :frame-outline-width :default 2)))
    (when (and colour (integerp width) (plusp width))
      (multiple-value-bind (r g b)
          (if (stringp colour)
              (parse-hex-color colour)
              (theme-border-rgb t))
        (values r g b width)))))

;;; --- Target ---

(defun frame-outline-output ()
  "The output frame commands act on."
  (first (all-outputs)))

(defun frame-outline-hidden-p (output workspace)
  "True when OUTPUT/WORKSPACE shows no frames to outline: a scroll
ribbon, a floating workspace, an expose in progress, or a visible
fullscreen window."
  (or (scroll-workspace-p output workspace)
      (floating-workspace-p output workspace)
      (frames-preserve-empty-p)
      (some (lambda (win)
              (and (window-fullscreen-p win)
                   (not (window-minimized-p win))))
            (windows-on-workspace output workspace))))

(defun frame-outline-target ()
  "Where the outline goes: (VALUES OUTPUT FRAME X Y W H), the frame in
global coordinates, or NIL when no outline shows."
  (let ((output (frame-outline-output)))
    (when (and output (frame-outline-style))
      (let ((workspace (current-workspace output)))
        (unless (frame-outline-hidden-p output workspace)
          (let ((frame (current-frame-for output workspace)))
            (when (and (plusp (frame-width frame))
                       (plusp (frame-height frame)))
              (values output frame
                      (+ (output-x output) (frame-x frame))
                      (+ (output-y output) (frame-y frame)
                         (bar-reserved-height))
                      (frame-width frame)
                      (frame-height frame)))))))))

;;; --- Pixels ---

(defun frame-outline-draw (state w h width r g b)
  "Draw a W x H ring WIDTH px wide in R G B, clear inside, into
STATE's shm buffer, reallocated when W or H changes. Returns the
updated STATE, or NIL when no buffer could be allocated."
  (unless (and (eql (getf state :w) w) (eql (getf state :h) h))
    ;; decoration-shm reuses a buffer of equal byte size, which would
    ;; keep the old stride for a frame of the same area and new shape
    (frame-outline-free state)
    (setf (getf state :ptr) nil (getf state :buffer) nil
          (getf state :pool) nil (getf state :fd) nil
          (getf state :size) nil (getf state :w) nil (getf state :h) nil))
  (let ((state (decoration-shm state w h)))
    (when state
      (setf (getf state :w) w (getf state :h) h)
      (let ((ptr (getf state :ptr))
            (band (max 1 (min width (floor w 2) (floor h 2))))
            (pixel (make-argb r g b)))
        (cffi:foreign-funcall "memset" :pointer ptr :int 0
                              :unsigned-long (* w h 4) :pointer)
        (loop for y below h
              do (if (or (< y band) (>= y (- h band)))
                     (loop for x below w
                           do (put-pixel-argb ptr w h x y pixel))
                     (loop for x below band
                           do (put-pixel-argb ptr w h x y pixel)
                              (put-pixel-argb ptr w h (- w 1 x) y pixel))))
        state))))

(defun frame-outline-free (state)
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

;;; --- Surface lifecycle ---

(defun frame-outline-create ()
  "A fresh outline state: a shell surface with its render-list node,
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

(defun frame-outline-destroy (state)
  "Destroy STATE's node, shell surface, surface, and buffer."
  (handler-case
      (progn
        (when (getf state :node)
          (rwm:river-node-v1.destroy (getf state :node)))
        (when (getf state :shell)
          (rwm:river-shell-surface-v1.destroy (getf state :shell)))
        (when (getf state :surface)
          (wl:wl-surface.destroy (getf state :surface)))
        (frame-outline-free state))
    (error (e)
      (format t "~&; Rhizome: frame outline cleanup error: ~A~%" e)))
  nil)

(defun frame-outline-commit (state buffer w h)
  "Attach BUFFER (NIL unmaps) and commit, synced to this render
sequence."
  (rwm:river-shell-surface-v1.sync-next-commit (getf state :shell))
  (wl:wl-surface.attach (getf state :surface) buffer 0 0)
  (when buffer
    (wl:wl-surface.damage (getf state :surface) 0 0 w h))
  (wl:wl-surface.commit (getf state :surface)))

(defun frame-outline-show (state x y w h r g b width)
  "Map STATE as a W x H ring at global X Y. Redraws only when the
size, colour, or width changed. Returns the updated STATE."
  (let ((key (list w h r g b width)))
    (unless (and (getf state :mapped) (equal key (getf state :drawn)))
      (let ((drawn (frame-outline-draw state w h width r g b)))
        (when drawn
          (setf state drawn
                (getf state :drawn) key
                (getf state :mapped) t)
          (frame-outline-commit state (getf state :buffer) w h))))
    (when (getf state :mapped)
      (unless (equal (getf state :position) (list x y))
        (rwm:river-node-v1.set-position (getf state :node) x y)
        (setf (getf state :position) (list x y)))
      (rwm:river-node-v1.place-bottom (getf state :node)))
    state))

(defun frame-outline-hide (state)
  "Unmap STATE if mapped. Returns the updated STATE."
  (when (getf state :mapped)
    (frame-outline-commit state nil 0 0)
    (setf (getf state :mapped) nil
          (getf state :drawn) nil))
  state)

;;; --- Render pass ---

(defun frame-outline-render-pass ()
  "Keep each output's outline current: mapped around the target frame
on the target output, unmapped elsewhere, destroyed for outputs that
are gone. Render sequence only. Returns NIL."
  (handler-case
      (let ((outputs (all-outputs)))
        (loop for output being the hash-keys of *frame-outlines*
                using (hash-value state)
              unless (member output outputs :test #'eq)
                do (frame-outline-destroy state)
                   (remhash output *frame-outlines*))
        (multiple-value-bind (target frame x y w h) (frame-outline-target)
          (declare (ignore frame))
          (multiple-value-bind (r g b width) (frame-outline-style)
            (dolist (output outputs)
              (let ((state (or (gethash output *frame-outlines*)
                               (when (eq output target)
                                 (frame-outline-create)))))
                (when state
                  (setf (gethash output *frame-outlines*)
                        (if (eq output target)
                            (frame-outline-show state x y w h r g b width)
                            (frame-outline-hide state)))))))))
    (error (e)
      (format t "~&; Rhizome: frame outline error: ~A~%" e)))
  nil)
