;;; fselect: pick a frame by its label (StumpWM's fselect, C-t f)
;;;
;;; C-t f puts a label on every frame of every visible output that
;;; holds a window, numbered across outputs from left to right and
;;; through each frame tree in layout order, with the characters of
;;; *frame-number-map*. Typing a label focuses that frame and its
;;; window, on whichever output it sits. C-g, Escape, any other key,
;;; and the prefix timeout cancel without a change. Empty frames carry
;;; a label only under (:frames (:focus-empty t)); picking one clears
;;; keyboard focus, so the next new window opens there.
;;;
;;; Keys arrive through the prefix capture surface, as in expose and
;;; iresize. Each label is a small river_shell_surface_v1 placed at
;;; the top of the render list, transparent to input, created and
;;; destroyed only in fselect-labels-render-pass (render sequence),
;;; with the same protocol discipline as frame-outline.lisp. The pass
;;; derives the labels from live state, so every exit path, and the
;;; removal of an output, leaves no label behind after the next
;;; render sequence.
;;;
;;; Colours: the fill is (:theme :frame-outline) when it is a colour,
;;; else the focused border colour; the text is the bar background on
;;; a light fill and the bar foreground on a dark one.

(in-package #:rhizome)

(defparameter *frame-number-map* "0123456789abcdefghijklmnopqrstuvwxyz"
  "Label characters, in frame order (StumpWM's *frame-number-map*).
Frames past the end of the string carry no label.")

(defparameter *fselect-label-inset* 8
  "Pixels between a frame's top-left corner and its label.")

(defparameter *fselect-label-padding* 6
  "Pixels between a label's edges and its text.")

(defvar *fselect-labels* (make-hash-table :test #'eq)
  "FRAME -> label state plist (:surface :shell :node :ptr :fd :size
:pool :buffer :w :h :drawn :mapped :position).")

;;; --- Which frames ---

(defun fselect-frame-window (frame)
  "The window picking FRAME focuses (frame-focus-window)."
  (frame-focus-window frame))

(defun fselect-targets ()
  "The labelled frames as a list of (CHAR OUTPUT WORKSPACE FRAME):
outputs left to right, each current workspace's leaves in layout
order, skipping outputs that show no frames, and frames without an
unminimized window unless (:frames (:focus-empty t)). At most
(length *frame-number-map*) entries."
  (let ((chars (coerce *frame-number-map* 'list))
        (empty-ok (frames-focus-empty-p))
        (targets '()))
    (dolist (output (all-outputs-sorted))
      (let ((workspace (current-workspace output)))
        (unless (frame-outline-hidden-p output workspace)
          (dolist (frame (frame-leaves (frame-tree-for output workspace)))
            (when (and chars
                       (plusp (frame-width frame))
                       (plusp (frame-height frame))
                       (or empty-ok (fselect-frame-window frame)))
              (push (list (pop chars) output workspace frame) targets))))))
    (nreverse targets)))

(defun fselect-key-target (key)
  "The target of (fselect-targets) whose label KEY types, or NIL.
Modifiers are ignored, so a Control still held from C-t matches."
  (let* ((keysym (key-keysym key))
         (char (and (< 32 keysym 127) (code-char keysym))))
    (and char
         (find char (fselect-targets) :key #'first))))

(defun fselect-cancel-key-p (key)
  "True when KEY is C-g or Escape."
  (let ((keysym (key-keysym key)))
    (or (= keysym 65307)
        (and (= keysym 103)
             (member :control (key-modifiers key))
             t))))

;;; --- Picking ---

(defun fselect-pick (target)
  "Make TARGET's output current and focus TARGET's frame there
(focus-frame): its window takes keyboard focus on every seat, or, for
an empty frame, keyboard focus clears and the output stays current for
the next new window."
  (destructuring-bind (char output workspace frame) target
    (declare (ignore char))
    (setf *selected-output* output)
    (focus-frame output workspace frame)))

(defun enter-fselect ()
  "C-t f: label the frames and wait for one key (StumpWM's fselect).
Does nothing but report when no frame can be picked."
  (unless *fselect-active*
    (if (null (fselect-targets))
        (message "no frame to select")
        (progn
          (enter-prefix-mode)
          (when *prefix-mode-active*
            (setf *fselect-active* t
                  *prefix-expiry* (+ (get-universal-time) 10))
            (show-message "fselect: type a frame label; C-g cancels" 10)
            (request-manage-dirty))))))

(defun fselect-handle-key (key)
  "Handle KEY while fselect waits: a label picks its frame; C-g and
any other key cancel. Every path ends the capture. Returns T."
  (let ((target (and (not (fselect-cancel-key-p key))
                     (fselect-key-target key))))
    (exit-prefix-mode)
    (when target
      (fselect-pick target)))
  t)

;;; --- Label pixels ---

(defun fselect-label-colours ()
  "Fill and text colours as (VALUES FR FG FB TR TG TB)."
  (let ((colour (config-getf '(:theme) :frame-outline :default t)))
    (multiple-value-bind (fr fg fb)
        (if (stringp colour)
            (parse-hex-color colour)
            (theme-border-rgb t))
      (multiple-value-bind (tr tg tb)
          (parse-hex-color
           (if (> (+ (* 299 fr) (* 587 fg) (* 114 fb)) 140000)
               (config-getf '(:bar) :background :default "#1a1b26")
               (config-getf '(:bar) :foreground :default "#c0caf5")))
        (values fr fg fb tr tg tb)))))

(defun fselect-label-size (text)
  "Label width and height in px for TEXT as (VALUES W H)."
  (values (+ (font-text-width text) (* 2 *fselect-label-padding*))
          (+ (font-line-height) (* 2 *fselect-label-padding*))))

(defun fselect-label-position (output frame)
  "Global X and Y of FRAME's label on OUTPUT as (VALUES X Y)."
  (values (+ (output-x output) (frame-x frame) *fselect-label-inset*)
          (+ (output-y output) (frame-y frame) (bar-reserved-height output)
             *fselect-label-inset*)))

(defun fselect-label-draw (state text w h fr fg fb tr tg tb)
  "Fill a W x H box in FR FG FB and write TEXT on it in TR TG TB, into
STATE's shm buffer, reallocated when W or H changes. Returns the
updated STATE, or NIL when no buffer could be allocated."
  (unless (and (eql (getf state :w) w) (eql (getf state :h) h))
    ;; decoration-shm keeps a buffer of equal byte size, whose stride
    ;; would be wrong for a box of the same area and a new shape
    (frame-outline-free state)
    (setf (getf state :ptr) nil (getf state :buffer) nil
          (getf state :pool) nil (getf state :fd) nil
          (getf state :size) nil (getf state :w) nil (getf state :h) nil))
  (let ((state (decoration-shm state w h)))
    (when state
      (setf (getf state :w) w (getf state :h) h)
      (let ((ptr (getf state :ptr))
            (pixel (make-argb fr fg fb)))
        (dotimes (y h)
          (dotimes (x w)
            (put-pixel-argb ptr w h x y pixel)))
        (font-draw-text ptr w h text *fselect-label-padding*
                        (max 0 (floor (- h (font-line-height)) 2))
                        tr tg tb))
      state)))

;;; --- Label surfaces ---

(defun fselect-label-show (state text x y)
  "Map STATE as the label TEXT at global X Y, on top of the render
list. Redraws only when the text, size, or colours changed. Returns
the updated STATE."
  (font-ensure-initialized)
  (multiple-value-bind (w h) (fselect-label-size text)
    (multiple-value-bind (fr fg fb tr tg tb) (fselect-label-colours)
      (let ((key (list text w h fr fg fb tr tg tb)))
        (unless (and (getf state :mapped) (equal key (getf state :drawn)))
          (let ((drawn (fselect-label-draw state text w h
                                           fr fg fb tr tg tb)))
            (when drawn
              (setf state drawn
                    (getf state :drawn) key
                    (getf state :mapped) t)
              (frame-outline-commit state (getf state :buffer) w h))))
        (when (getf state :mapped)
          (unless (equal (getf state :position) (list x y))
            (rwm:river-node-v1.set-position (getf state :node) x y)
            (setf (getf state :position) (list x y)))
          (rwm:river-node-v1.place-top (getf state :node)))
        state))))

(defun fselect-labels-render-pass ()
  "Keep the label surfaces in step with fselect: one per target while
it waits, none otherwise. Render sequence only. Returns NIL."
  (handler-case
      (let ((targets (and *fselect-active* (fselect-targets))))
        (loop for frame being the hash-keys of *fselect-labels*
                using (hash-value state)
              unless (find frame targets :key #'fourth :test #'eq)
                do (frame-outline-destroy state)
                   (remhash frame *fselect-labels*))
        (dolist (target targets)
          (destructuring-bind (char output workspace frame) target
            (declare (ignore workspace))
            (let ((state (or (gethash frame *fselect-labels*)
                             (frame-outline-create))))
              (when state
                (multiple-value-bind (x y) (fselect-label-position output frame)
                  (setf (gethash frame *fselect-labels*)
                        (fselect-label-show state (string char) x y))))))))
    (error (e)
      (format t "~&; Rhizome: fselect label error: ~A~%" e)))
  nil)

(define-command :fselect (output args)
  "Label the frames and focus the one whose label is typed next."
  (declare (ignore output args))
  (enter-fselect))
