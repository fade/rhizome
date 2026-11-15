;;; Tests for the bar's static layers (bar.lisp): the active-screen
;;; outline (2 px on the current output's bar, 1 px elsewhere), its
;;; entry in the non-hue cue table, the optional glow and the surface
;;; padding that keeps it unclipped, and the cache that keeps a module
;;; redraw away from the distance field. Each property has a control
;;; that fails without the change. River requests are stubbed; pixels
;;; are drawn into private buffers.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: the configure calls below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(defvar *bo-pass* 0)
(defvar *bo-fail* 0)

(defun bo-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *bo-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *bo-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro bo-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *bo-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

(defun bo-config (&optional overrides)
  "Set *config* to the defaults with OVERRIDES merged over them, the bar
disabled so no bar process starts."
  (setf *config* (merge-plist (default-config)
                              (merge-plist '(:bar (:enabled nil))
                                           overrides))))

(defun bo-alpha (ptr width x y)
  "The alpha byte of pixel (X Y) in a WIDTH-wide buffer."
  (ldb (byte 8 24) (cffi:mem-aref ptr :uint32 (+ x (* y width)))))

(defun bo-rgb (ptr width x y)
  "The premultiplied (R G B) of pixel (X Y) in a WIDTH-wide buffer."
  (let ((p (cffi:mem-aref ptr :uint32 (+ x (* y width)))))
    (list (ldb (byte 8 16) p) (ldb (byte 8 8) p) (ldb (byte 8 0) p))))

(defun bo-paint (width height pad active-p)
  "Paint the static layers of a WIDTH x HEIGHT surface padded by PAD
into a fresh buffer; return it. The caller frees it."
  (let ((ptr (cffi:foreign-alloc :uint32 :count (* width height)
                                         :initial-element 0)))
    (bar-paint-static ptr (bar-static-spec width height pad active-p))
    ptr))

(defun bo-outline-rows (ptr width x pad active-p)
  "How many rows down from the body's top edge, in column X, carry the
outline colour rather than the background, for a bar painted with
ACTIVE-P."
  (let ((spec (bar-static-spec 1 1 0 active-p)))
    (destructuring-bind (orr og ob) (getf spec :outline)
      (destructuring-bind (br bg bb) (getf spec :background)
        (flet ((nearer-outline-p (rgb)
                 (destructuring-bind (r g b) rgb
                   (< (+ (abs (- r orr)) (abs (- g og)) (abs (- b ob)))
                      (+ (abs (- r br)) (abs (- g bg)) (abs (- b bb)))))))
          (loop for y from pad
                while (nearer-outline-p (bo-rgb ptr width x y))
                count t))))))

(defun bo-stub-surface ()
  "Stub the wl_surface requests a render sends."
  (dolist (name '(wl:wl-surface.attach wl:wl-surface.damage
                  wl:wl-surface.commit))
    (setf (symbol-function name)
          (lambda (&rest args) (declare (ignore args)) nil))))

(defvar *bo-active* nil
  "What the stubbed bar-ostate-active-p returns.")

(defun bo-render-state (width height pad)
  "An ostate for a WIDTH x HEIGHT bar surface padded by PAD, with a
zeroed buffer."
  (let ((state (make-hash-table)))
    (setf (gethash :shm-ptr state)
          (cffi:foreign-alloc :uint32 :count (* width height)
                                      :initial-element 0)
          (gethash :width state) width
          (gethash :height state) height
          (gethash :pad state) pad)
    state))

(defun bo-free-state (state)
  (bar-free-static state)
  (cffi:foreign-free (gethash :shm-ptr state)))

;; Without a compositor: fixed snapshot, stubbed surface, and an
;; active state the tests set.
(setf (symbol-function 'bar-snapshot)
      (lambda () '(:workspace 1 :workspace-count 3 :occupied (1 2))))
(setf (symbol-function 'bar-ostate-active-p) (lambda () *bo-active*))
(bo-stub-surface)
(font-ensure-initialized)

;;; --- Outline width ---

(bo-case "outline width"
  (bo-config '(:bar (:height 34 :corner-radius 10)))
  (let ((active (bo-paint 400 34 0 t))
        (inactive (bo-paint 400 34 0 nil)))
    (unwind-protect
         (progn
           (bo-ok "the active bar's outline is 2 px on its top edge"
                  (bo-outline-rows active 400 200 0 t) 2)
           (bo-ok "an inactive bar's outline is 1 px on its top edge"
                  (bo-outline-rows inactive 400 200 0 nil) 1)
           (bo-ok "the active outline takes :bar-outline-active"
                  (getf (bar-static-spec 1 1 0 t) :outline)
                  (multiple-value-list (theme-rgb :bar-outline-active)))
           (bo-ok "the inactive outline keeps :bar-border"
                  (getf (bar-static-spec 1 1 0 nil) :outline)
                  (multiple-value-list (theme-rgb :bar-border))))
      (cffi:foreign-free active)
      (cffi:foreign-free inactive))))

(bo-case "outline colour default"
  (bo-config)
  (bo-ok ":bar-outline-active follows :screen-active-edge"
         (theme-color :bar-outline-active) (theme-color :screen-active-edge))
  (bo-config '(:theme (:elements (:bar-outline-active "#ffcc00"))))
  (bo-ok "a theme sets :bar-outline-active"
         (theme-color :bar-outline-active) "#ffcc00")
  (bo-config))

;;; --- The hue-only gate ---

(bo-case "hue gate"
  (bo-ok "the cue table gives the active bar 2 px and the inactive 1 px"
         (list (state-cue :bar-active :width) (state-cue :bar-inactive :width))
         '(2 1))
  (bo-ok "no hue-only bar pair"
         (find :bar-active (hue-only-state-pairs) :key #'first)
         nil)
  (bo-ok "the cue table stays consistent" (state-cue-conflicts) nil)
  (bo-ok "control: the pair with equal widths is reported as hue-only"
         (find :bar-active
               (hue-only-state-pairs
                :table (substitute '(:bar-active
                                     :against :bar-inactive
                                     :elements (:bar-outline-active :bar-border)
                                     :cues ((:width 1 1)))
                                   :bar-active *state-cues* :key #'first))
               :key #'first)
         '(:bar-active :bar-inactive)))

;;; --- Glow ---

(bo-case "glow setting"
  (bo-config)
  (bo-ok "the glow is off by default, so no padding" (bar-glow-radius) 0)
  (bo-config '(:bar (:active-glow t)))
  (bo-ok "on, it pads by 8 px" (bar-glow-radius) 8)
  (bo-config '(:bar (:active-glow t :active-glow-radius 30)))
  (bo-ok "the radius is clamped to 12" (bar-glow-radius) 12)
  (bo-config '(:bar (:active-glow t :active-glow-radius 1)))
  (bo-ok "and to 4" (bar-glow-radius) 4)
  (bo-config))

(bo-case "glow off"
  (bo-config '(:bar (:height 34 :corner-radius 10)))
  ;; An inactive bar on a padded surface: the pad stays empty.
  (let ((ptr (bo-paint 120 50 8 nil)))
    (unwind-protect
         (bo-ok "an unlit bar on a padded surface draws nothing outside its body"
                (loop for y below 50
                      sum (loop for x below 120
                                count (and (or (< x 8) (>= x 112)
                                               (< y 8) (>= y 42))
                                           (plusp (bo-alpha ptr 120 x y)))))
                0)
      (cffi:foreign-free ptr))))

(bo-case "glow on"
  (bo-config '(:bar (:height 34 :corner-radius 10)))
  (let* ((pad 9) (w (+ 200 (* 2 pad))) (h (+ 34 (* 2 pad)))
         (ptr (bo-paint w h pad t)))
    (unwind-protect
         (let* ((x (floor w 2))
                ;; the column above the body's top edge, outward
                (ramp (loop for y from (1- pad) downto 0
                            collect (bo-alpha ptr w x y)))
                (steps (loop for (a b) on ramp while b collect (- a b))))
           (bo-ok "the glow lights the pad" (> (first ramp) 30) t)
           (bo-ok "the glow falls off monotonically"
                  (every (lambda (s) (>= s 0)) steps) t)
           (bo-ok (format nil "no step between adjacent pixels exceeds 26 (~S)"
                          ramp)
                  (every (lambda (s) (<= s 26)) steps) t)
           (bo-ok "nothing is clipped at the padded edge: the outer ring is empty"
                  (+ (loop for i below w
                           sum (+ (bo-alpha ptr w i 0) (bo-alpha ptr w i (1- h))))
                     (loop for j below h
                           sum (+ (bo-alpha ptr w 0 j) (bo-alpha ptr w (1- w) j))))
                  0)
           (bo-ok "the glow reaches round the corner"
                  (plusp (bo-alpha ptr w (- pad 2) (- pad 2))) t))
      (cffi:foreign-free ptr))))

;;; --- Surface padding keeps the body in place ---

(bo-case "padding geometry"
  (bo-config '(:bar (:height 34 :margin 8 :active-glow t)))
  (bo-ok "the surface is padded one px past the glow" (bar-glow-pad) 9)
  (bo-ok "the glow setting reopens the surface" (third (bar-geometry)) 9)
  (multiple-value-bind (side surface-height zone)
      (bar-surface-geometry 34 8 9)
    (bo-ok "the surface grows by the pad on every side" surface-height 52)
    (bo-ok "the margins shrink by as much" side -1)
    (bo-ok "the body keeps its place: side margin + pad = margin"
           (+ side 9) 8)
    (bo-ok "the reserved area is unchanged: top margin + zone = 3 margins + height"
           (+ side zone) (+ (* 3 8) 34)))
  (multiple-value-bind (side surface-height zone)
      (bar-surface-geometry 34 8 0)
    (bo-ok "unpadded, the requests are the ones the bar always sent"
           (list side surface-height zone) (list 8 34 (+ 16 34))))
  (let ((ostate (make-hash-table)))
    (setf (gethash :pad ostate) 9)
    (bo-ok "a pointer at surface x 20 is body x 11" (bar-body-x ostate 20) 11))
  (bo-config))

;;; --- The static cache ---

(bo-case "static cache"
  (bo-config '(:bar (:height 34 :corner-radius 10
                     :modules (:left (:workspaces) :center () :right ()))))
  (setf *bo-active* t)
  (let ((state (bo-render-state 600 34 0)))
    (unwind-protect
         (progn
           (%bar-render state)
           (let ((before *rounded-rect-distance-calls*))
             (%bar-render state)
             (bo-ok "a redraw with the cache leaves the distance field alone"
                    (- *rounded-rect-distance-calls* before) 0))
           (setf *bo-active* nil)
           (let ((before *rounded-rect-distance-calls*))
             (%bar-render state)
             (bo-ok "an active-screen change repaints the static layers"
                    (> (- *rounded-rect-distance-calls* before) 0) t))
           (let ((*bar-static-cache-enabled* nil)
                 (before *rounded-rect-distance-calls*))
             (%bar-render state)
             (bo-ok "control: without the cache a redraw computes the field"
                    (>= (- *rounded-rect-distance-calls* before) (* 600 34))
                    t)))
      (bo-free-state state)))
  (setf *bo-active* nil))

(bo-case "padded render"
  (bo-config '(:bar (:height 34 :corner-radius 10
                     :modules (:left (:workspaces) :center () :right ()))))
  (setf *bo-active* t)
  (let ((state (bo-render-state 616 50 8)))
    (unwind-protect
         (progn
           (%bar-render state)
           (bo-ok "the body's top-left corner pixel sits at the pad"
                  (list (bo-alpha (gethash :shm-ptr state) 616 8 25)
                        (bo-alpha (gethash :shm-ptr state) 616 30 8))
                  (list (bar-surface-alpha) (bar-surface-alpha)))
           (bo-ok "hitboxes are in body coordinates"
                  (every (lambda (hb) (< (second hb) 600))
                         (gethash :hitboxes state))
                  t))
      (bo-free-state state)))
  (setf *bo-active* nil))

(format t "~&=== ~D passed, ~D failed ===~%" *bo-pass* *bo-fail*)
(uiop:quit (if (zerop *bo-fail*) 0 1))
