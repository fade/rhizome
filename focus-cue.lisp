;;; Focus cue
;;;
;;; An opt-in flash that shows where keyboard focus went: when focus
;;; moves to another frame or output, a ring around the newly focused
;;; frame appears fully opaque and fades out. A workspace switch, a
;;; split, and a merge show no cue, and a workspace switch ends a cue
;;; in flight. The cue changes opacity and shape, never colour alone,
;;; and it is independent of the frame outline's settings.
;;;
;;; Settings, under :animations:
;;;   :focus-cue (:enabled nil :duration-ms 180 :width 3)
;;; (:animations :enabled) NIL and reduced motion (reduced-motion-p)
;;; turn it off too. Colour: the :frame-outline theme element.
;;;
;;; The ring is one WM-owned river_shell_surface_v1 drawn with
;;; frame-outline.lisp's buffer code and placed above every node. It
;;; is input-transparent. Requests run from focus-cue-render-pass,
;;; inside on-render-sequence, under the same protocol discipline as
;;; the frame outline. While a cue fades, *focus-cue* keeps
;;; animations-running-p true, so the main loop requests a frame each
;;; interval; otherwise the pass draws nothing.

(in-package #:rhizome)

(defvar *focus-cue-surface* nil
  "The cue's outline state plist (see *frame-outlines*), or NIL before
the first cue.")

(defvar *focus-cue-last* nil
  "(OUTPUT WORKSPACE FRAME) holding keyboard focus at the last render
pass, or NIL when not tracked.")

(defun focus-cue-settings ()
  "The cue as (VALUES DURATION-MS WIDTH), or NIL when it is off:
(:animations :focus-cue :enabled) not true, (:animations :enabled) NIL,
reduced motion in effect, or a duration or width that is not a
positive integer."
  (let ((ms (config-getf '(:animations :focus-cue) :duration-ms
                         :default 180))
        (width (config-getf '(:animations :focus-cue) :width :default 3)))
    (when (and (config-getf '(:animations :focus-cue) :enabled)
               (animations-enabled-p)
               (not (reduced-motion-p))
               (integerp ms) (plusp ms)
               (integerp width) (plusp width))
      (values ms width))))

(defun focus-cue-alpha (cue now)
  "The ring's opacity for CUE at NOW as an alpha byte (0-255): 255,
fully opaque, at the start, falling linearly to 0, transparent, at the
end of its duration."
  (let ((elapsed (- now (getf cue :start)))
        (duration (getf cue :duration)))
    (if (>= elapsed duration)
        0
        (max 0 (min 255 (round (* 255 (- 1 (/ elapsed duration)))))))))

(defun focus-cue-focus ()
  "Where keyboard focus is: (VALUES (OUTPUT WORKSPACE FRAME) X Y W H), the frame
in global coordinates, with X NIL when the frame shows no outline (see
frame-outline-hidden-p) or has no area. NIL when there is no output."
  (let ((output (current-output)))
    (when output
      (let* ((workspace (current-workspace output))
             (frame (current-frame-for output workspace))
             (key (list output workspace frame)))
        (if (or (frame-outline-hidden-p output workspace)
                (not (plusp (frame-width frame)))
                (not (plusp (frame-height frame))))
            (values key nil)
            (values key
                    (+ (output-x output) (frame-x frame))
                    (+ (output-y output) (frame-y frame)
                       (bar-reserved-height output))
                    (frame-width frame)
                    (frame-height frame)))))))

(defun focus-cue-workspace-switch-p (last key)
  "True when focus at KEY, an (OUTPUT WORKSPACE FRAME), is on the same
output as LAST but on another workspace."
  (and (eq (first last) (first key))
       (not (eql (second last) (second key)))))

(defun focus-cue-moved-p (last key)
  "True when focus at KEY, an (OUTPUT WORKSPACE FRAME), has moved from
LAST to another frame or output: another output, whatever its
workspace, or on the same workspace a frame whose rectangle neither
contains nor lies inside LAST's. A workspace switch, a split, and a
merge are not moves."
  (flet ((inside-p (a b)
           (and (<= (frame-x b) (frame-x a))
                (<= (frame-y b) (frame-y a))
                (<= (+ (frame-x a) (frame-width a))
                    (+ (frame-x b) (frame-width b)))
                (<= (+ (frame-y a) (frame-height a))
                    (+ (frame-y b) (frame-height b))))))
    (destructuring-bind (old-output old-workspace old) last
      (destructuring-bind (output workspace new) key
        (cond ((not (eq old-output output)) t)
              ((not (eql old-workspace workspace)) nil)
              (t (not (or (eq old new)
                          (inside-p old new)
                          (inside-p new old)))))))))

(defun focus-cue-hide ()
  "Unmap the cue and end it. Return NIL."
  (setf *focus-cue* nil)
  (when *focus-cue-surface*
    (setf *focus-cue-surface* (frame-outline-hide *focus-cue-surface*)))
  nil)

(defun focus-cue-show (x y w h width alpha)
  "Map the cue as a W x H ring WIDTH px wide at global X Y, at ALPHA,
above every node. Return NIL."
  (let ((state (or *focus-cue-surface* (frame-outline-create))))
    (when state
      (multiple-value-bind (r g b) (theme-rgb :frame-outline)
        (let ((drawn (frame-outline-draw state w h width r g b alpha)))
          (when drawn
            (setf state drawn
                  (getf state :mapped) t
                  (getf state :drawn) (list w h r g b width alpha))
            (frame-outline-commit state (getf state :buffer) w h)
            (unless (equal (getf state :position) (list x y))
              (rwm:river-node-v1.set-position (getf state :node) x y)
              (setf (getf state :position) (list x y)))
            (rwm:river-node-v1.place-top (getf state :node)))))
      (setf *focus-cue-surface* state)))
  nil)

(defun focus-cue-render-pass ()
  "Start the cue when keyboard focus has moved to another frame or
output since the last pass, and draw the cue in flight at its current
opacity, unmapping it once fully transparent. A workspace switch ends
any cue in flight and starts none; a split or a merge starts none.
With the cue off, unmap any cue and stop tracking focus. Render
sequence only. Returns NIL."
  (handler-case
      (multiple-value-bind (ms width) (focus-cue-settings)
        (if (null ms)
            (progn
              (setf *focus-cue-last* nil)
              (when (or *focus-cue*
                        (getf *focus-cue-surface* :mapped))
                (focus-cue-hide)))
            (multiple-value-bind (key x y w h) (focus-cue-focus)
              (let ((now (funcall *animation-clock*))
                    (last *focus-cue-last*))
                (when (and key last)
                  (cond ((focus-cue-workspace-switch-p last key)
                         (when (or *focus-cue*
                                   (getf *focus-cue-surface* :mapped))
                           (focus-cue-hide)))
                        ((focus-cue-moved-p last key)
                         (setf *focus-cue*
                               (list :start now
                                     :duration (* ms (/ internal-time-units-per-second
                                                        1000)))))))
                (setf *focus-cue-last* key)
                (when *focus-cue*
                  (let ((alpha (focus-cue-alpha *focus-cue* now)))
                    (if (or (zerop alpha) (null x))
                        (focus-cue-hide)
                        (focus-cue-show x y w h width alpha))))))))
    (error (e)
      (setf *focus-cue* nil)
      (format t "~&; Rhizome: focus cue error: ~A~%" e)))
  nil)
