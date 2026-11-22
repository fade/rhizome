;;; The animation engine
;;;
;;; River renders; the WM only proposes geometry. So rhizome
;;; animates by keeping a decaying per-window OFFSET: the render
;;; passes compute final positions exactly as before and then ask
;;; ANIMATION-POSITION to interpolate toward them from a recorded
;;; start. A pump in the WM's main loop (animation-drain) requests
;;; fresh manage/render sequences at ~60fps while anything moves.
;;;
;;; Two start modes:
;;;   :absolute — :from-x/:from-y (defaults: the window's current
;;;     rendered position). Interpolate from there to whatever the
;;;     render pass computes. Expose glides, float/maximize slides.
;;;   :relative — :dx/:dy offset at t=0 decaying to 0. The final
;;;     position is unknown at trigger time. Workspace slide-ins.
;;;
;;; Config: (:animations (:enabled t :duration-ms 200
;;;                       :workspace-duration-ms 160
;;;                       :easing :ease-out-cubic
;;;                       :workspace (:enabled t)
;;;                       :expose (:enabled t)
;;;                       :float (:enabled t)
;;;                       :reduced-motion :follow))
;;; Each motion kind (*animation-kinds*) has its own :enabled and an
;;; optional :duration-ms; see animation-kind-enabled-p and
;;; animation-kind-duration. Reduced motion (reduced-motion-p) turns
;;; every kind off.
;;; Easing: :linear, :ease-out-cubic (default), :ease-in-out.

(in-package #:rhizome)

(defparameter *animations* (make-hash-table)
  "Window → plist (:mode :from-x :from-y :dx :dy :start :duration
:ease). DEFVAR: reloading must not freeze mid-flight windows.")

(defparameter *animation-started-count* 0
  "Animations started (test introspection).")

(defvar *animation-clock* #'get-internal-real-time
  "Function of no arguments returning the current time in internal time
units. Supplies glide start times and interpolation; frame pacing reads
real time directly. Tests bind it to freeze or step time.")

(defun animations-enabled-p ()
  (config-getf '(:animations) :enabled :default t))

(defun animation-duration ()
  "Duration in universal-time subseconds — internal-time units per
millisecond, so animations work off INTERNAL-REAL-TIME."
  (max 0 (config-getf '(:animations) :duration-ms :default 200)))

(defun workspace-slide-duration ()
  "Milliseconds a workspace switch slides for: (:animations
:workspace-duration-ms), 160 when unset. Independent of :duration-ms."
  (max 0 (config-getf '(:animations) :workspace-duration-ms :default 160)))

(defun animation-easing ()
  (config-getf '(:animations) :easing :default :ease-out-cubic))

(defparameter *animation-kinds* '(:workspace :expose :float)
  "Motion kinds with their own switch and duration under :animations:
:workspace, the slide on a workspace switch; :expose, windows gliding
into and out of the expose grid; :float, floating windows gliding on
float, move, unfloat, and maximize.")

(defvar *desktop-reduced-motion* nil
  "True when the desktop asks for reduced motion: the settings portal's
org.freedesktop.appearance reduced-motion key is 1. Set by
reduced-motion.lisp; NIL when the key is absent or unread.")

(defun reduced-motion-p ()
  "True when reduced motion is in effect. (:animations :reduced-motion)
T always reduces, NIL never does, and :follow (the default, and any
other value) follows *desktop-reduced-motion*."
  (let ((setting (config-getf '(:animations) :reduced-motion
                              :default :follow)))
    (case setting
      ((t) t)
      ((nil) nil)
      (otherwise (and *desktop-reduced-motion* t)))))

(defun animation-kind-setting (kind)
  "KIND's group under :animations as (VALUES ENABLED DURATION-MS).
ENABLED is NIL when the group is NIL or sets :enabled NIL, else T.
DURATION-MS is the group's :duration-ms, or NIL when it is unset or
not an integer."
  (multiple-value-bind (group foundp) (config-get (list :animations kind))
    (cond ((not foundp) (values t nil))
          ((null group) (values nil nil))
          ((not (config-plist-p group)) (values t nil))
          (t (let ((ms (getf group :duration-ms)))
               (values (and (getf group :enabled t) t)
                       (and (integerp ms) ms)))))))

(defun animation-kind-enabled-p (kind)
  "True when motion of KIND runs: (:animations :enabled) is on, reduced
motion is not in effect (reduced-motion-p), and KIND's own :enabled is
not NIL. KIND NIL asks about the global switches alone."
  (and (animations-enabled-p)
       (not (reduced-motion-p))
       (or (null kind) (nth-value 0 (animation-kind-setting kind)))
       t))

(defun animation-kind-duration (kind)
  "Milliseconds a glide of KIND lasts, never negative: KIND's own
:duration-ms, else workspace-slide-duration for :workspace and
animation-duration for every other kind."
  (let ((own (nth-value 1 (animation-kind-setting kind))))
    (cond (own (max 0 own))
          ((eq kind :workspace) (workspace-slide-duration))
          (t (animation-duration)))))

(defun ease (name t*)
  "Easing functions over normalized T* in [0,1] → [0,1]. T* is
coerced to a float first: SBCL's internal-real-time arithmetic is
EXACT RATIONAL, and rational easings leak fractional pixel
positions into river's int32 request args — a mid-request type
error that corrupts the wire and gets the client terminated."
  (let ((t* (coerce (min 1.0 (max 0.0 t*)) 'single-float)))
    (ecase name
      (:linear t*)
      ((:ease-out-cubic)
       ;; fast out, gentle landing
       (- 1 (expt (- 1 t*) 3)))
      (:ease-in-out
       (if (< t* 0.5)
           (* 4 t* t* t*)
           (- 1 (/ (expt (- 2 (* 2 t*)) 3) 2)))))))

(defun animation-active-p (win)
  "Whether WIN has an animation in flight."
  (and (gethash win *animations*) t))

(defun animations-running-p ()
  (plusp (hash-table-count *animations*)))

(defun animation-start (win &key kind (from-x nil) (from-y nil)
                                 (dx 0) (dy 0)
                                 (duration (if kind
                                               (animation-kind-duration kind)
                                               (animation-duration)))
                                 (easing (animation-easing)))
  "Start animating WIN as motion of KIND (see *animation-kinds*), or of
no kind when KIND is NIL. Absolute mode (from-x/from-y given or
defaulted to the current rendered position) glides from there to
whatever the next render pass computes; relative mode (dx/dy)
decays an offset to zero. DURATION defaults to KIND's resolved
duration. No-op when KIND is switched off (animation-kind-enabled-p)
or the duration is zero: instant placement."
  (when (and win (animation-kind-enabled-p kind) (plusp duration))
    (incf *animation-started-count*)
    (setf (gethash win *animations*)
          (list :mode (cond ((or from-x from-y) :absolute)
                            ;; NB: 0 offsets are truthy in CL —
                            ;; a zero-dx start is absolute-from-now
                            ((or (plusp (abs dx)) (plusp (abs dy)))
                             :relative)
                            (t :absolute))
                :kind kind
                :from-x (or from-x (window-x win))
                :from-y (or from-y (window-y win))
                :dx dx
                :dy dy
                :start (funcall *animation-clock*)
                :duration (* duration
                             (/ internal-time-units-per-second 1000))
                :ease easing))))

(defun animation-position (win x y)
  "The position WIN should render at this frame, given the layout's
final (X Y). Interpolates while an animation is in flight and
RETURNS the target and clears the animation once finished, so a
missed frame never freezes a window mid-air."
  (let ((anim (gethash win *animations*)))
    (if (null anim)
        (values x y)
        (let* ((elapsed (- (funcall *animation-clock*)
                            (getf anim :start)))
               (raw (/ elapsed (getf anim :duration))))
          (if (>= raw 1)
              (progn
                (remhash win *animations*)
                (values x y))
              (let* ((k (ease (getf anim :ease) raw))
                     (fx (getf anim :from-x))
                     (fy (getf anim :from-y)))
                (ecase (getf anim :mode)
                  (:absolute
                   ;; Whole pixels: river's set_position takes int32
                   (values (round (+ fx (* (- x fx) k)))
                           (round (+ fy (* (- y fy) k)))))
                  (:relative
                   (values (round (+ x (* (getf anim :dx) (- 1 k))))
                           (round (+ y (* (getf anim :dy) (- 1 k)))))))))))))

(defun animation-land (win x y)
  "End WIN's animation where the glide has reached at this moment.
Return that position for layout target (X Y), and remove the
animation, so the next render places WIN at whatever target the caller
sets from that position. Without an animation, return X and Y."
  (multiple-value-prog1 (animation-position win x y)
    (remhash win *animations*)))

(defun animation-drain ()
  "Main-loop pump: while anything animates, request fresh
manage/render sequences at frame cadence. Returns T when
animating (the main loop skips its idle sleep and frame-caps
instead). Coalesced: while a manage sequence is already pending
(*pending-manage*), no duplicate manage_dirty goes on the wire —
one request per cycle is the contract."
  (when (and *wm-proxy* (animations-running-p))
    (unless *pending-manage*
      (request-manage-dirty))
    t))

(defparameter *animation-frame-interval* 1/60
  "Seconds between the starts of successive animation frames.")

(defun frame-wait-seconds (previous-start now
                           &optional (interval *animation-frame-interval*))
  "Seconds left until the next frame deadline, PREVIOUS-START plus
INTERVAL seconds, as a float; 0.0 when the deadline has passed or
PREVIOUS-START is NIL. PREVIOUS-START and NOW are internal real times."
  (if (null previous-start)
      0.0
      (float (max 0 (- interval
                       (/ (- now previous-start)
                          internal-time-units-per-second)))
             1.0)))

(defun animation-frame-wait (previous-start now manage-pending)
  "Seconds the main loop waits on the display before starting the next
animation frame, or NIL when the frame is due now. PREVIOUS-START is
the internal real time the last frame started, or NIL; NOW is the
current internal real time. Before the deadline, wait out the rest of
the interval. After it, with MANAGE-PENDING true, the previous frame's
manage request is still out: wait up to one interval for river's reply
instead of polling."
  (let ((wait (frame-wait-seconds previous-start now)))
    (cond ((plusp wait) wait)
          (manage-pending (float *animation-frame-interval* 1.0))
          (t nil))))

;;; --- Clip boxes: glides never overdraw the bar ---

(defvar *clipped-windows* (make-hash-table :test #'eq)
  "Windows with a clip box currently set on river — so the clip
can be cleared (0 0 0 0) once the animation lands. Cleared on
window close via the :window-closed hook.")

(defun animation-apply (win x y)
  "Render-pass entry: the position WIN should draw at this frame
for layout target (X Y) — animation-position's math, plus clip
box management (set_clip_box is render-sequence-legal; this runs
only from render paths). While ANY animation glides the window,
it is clipped to its output's usable area (below the bar), so
intermediate positions never overdraw the bar. When the
animation lands, the clip clears."
  (multiple-value-bind (ax ay) (animation-position win x y)
    (let ((anim (gethash win *animations*)))
      (cond
        ;; In-flight animation: clip to the usable area
        ((and anim (window-output win))
         (let* ((out (window-output win))
                (uy (bar-reserved-height out))
                (uw (or (output-width out) 1920))
                (uh (max 1 (- (or (output-height out) 1080) uy)))
                (w (or (window-width win) 100))
                (h (or (window-height win) 80))
                ;; usable rect (screen coords): (0, uy, uw, uh)
                ;; intersect with the window at (ax, ay), in
                ;; window-LOCAL coordinates
                (cx (max 0 (- ax)))
                (cy (max 0 (- uy ay)))
                (cw (max 1 (min (- w cx) (- (+ uw 0) ax) w)))
                (ch (max 1 (min (- h cy) (- (+ uy uh) ay) h))))
           ;; (a 0 dimension would DISABLE clipping — never send 0)
           (set-window-clip-box win cx cy cw ch)
           (setf (gethash win *clipped-windows*) t)))
        ;; No animation, but a clip was set: clear it
        ((gethash win *clipped-windows*)
         (remhash win *clipped-windows*)
         (set-window-clip-box win 0 0 0 0))))
    (values ax ay)))

(defun clear-window-clips-on-close (win)
  "Forget WIN's clip-tracking entry when the window goes away."
  (remhash win *clipped-windows*))

(add-hook :window-closed #'clear-window-clips-on-close)
