;;; Tests for a floating window's shadow (decorations.lisp): sized from
;;; the size the client committed, around the border and titlebar,
;;; transparent under the window, neutral, soft, and shifted down.
;;; Control: a shadow sized from a requested size larger than the
;;; committed one shows a flat block beside the window. River requests
;;; are stubbed; the memfd buffer path runs for real.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defvar *sh-pass* 0)
(defvar *sh-fail* 0)

(defun sh-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *sh-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *sh-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro sh-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *sh-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

(defvar *sh-offsets* nil
  "Every set_offset the stubs saw, newest first.")

(defun sh-stub ()
  "Stub every river and wl request a decoration sends."
  (clrhash *decorations*)
  (setf *sh-offsets* nil)
  (flet ((stub (name value)
           (setf (symbol-function name)
                 (lambda (&rest args) (declare (ignore args)) value))))
    (stub 'wl:wl-compositor.create-surface :fake-surface)
    (stub 'rwm:river-window-v1.get-decoration-below :fake-deco)
    (stub 'rwm:river-decoration-v1.sync-next-commit nil)
    (stub 'wl:wl-surface.attach nil)
    (stub 'wl:wl-surface.damage nil)
    (stub 'wl:wl-surface.commit nil)
    (stub 'wl:wl-surface.destroy nil)
    (stub 'wl:wl-buffer.destroy nil)
    (stub 'wl:wl-shm-pool.destroy nil)
    (stub 'wl:wl-shm.create-pool :fake-pool)
    (stub 'wl:wl-shm-pool.create-buffer :fake-buffer))
  (setf (symbol-function 'rwm:river-decoration-v1.set-offset)
        (lambda (proxy x y)
          (declare (ignore proxy))
          (push (list x y) *sh-offsets*)))
  (setf *compositor* :fake-compositor
        *shm* :fake-shm
        *windows* (make-hash-table)))

(defun sh-float (proxy requested-w requested-h committed-w committed-h)
  "A floating window registered under PROXY, requested at REQUESTED-W
x REQUESTED-H, its client committed at COMMITTED-W x COMMITTED-H (0
for none yet)."
  (let ((win (make-instance 'rhizome-window :proxy proxy)))
    (setf (gethash proxy *windows*) win
          (window-floating-p win) t
          (window-float-width win) requested-w
          (window-float-height win) requested-h
          (window-width win) committed-w
          (window-height win) committed-h)
    win))

(defun sh-alpha (state x y)
  "The alpha byte of pixel (X Y) of the decoration buffer in STATE."
  (ldb (byte 8 24) (cffi:mem-aref (getf state :ptr) :uint32
                                  (+ x (* y (getf state :w))))))

(defun sh-pixel (state x y)
  (cffi:mem-aref (getf state :ptr) :uint32 (+ x (* y (getf state :w)))))

(defun sh-hole (state)
  "The cut-out rect of STATE's last draw, (X Y W H) in buffer pixels."
  (third (getf state :geometry)))

(sh-stub)
(configure '(:decorations (:enabled t :radius 16 :alpha 0.3
                           :focused-alpha 0.4)))

;;; --- Size from the committed size ---

(sh-case "committed size"
  (sh-stub)
  (let* ((win (sh-float :sh-a 800 600 780 590))
         (bw (border-width nil))
         (th (if (window-titlebar-p win) (titlebar-height) 0)))
    (decorations-render-pass)
    (let ((state (gethash win *decorations*)))
      (sh-ok "the shadow surrounds the committed size, border and titlebar"
             (list (getf state :w) (getf state :h))
             (list (+ 780 (* 2 bw) 32) (+ 590 (* 2 bw) th 32)))
      (sh-ok "the cut-out is the window with its border and titlebar"
             (cddr (sh-hole state))
             (list (+ 780 (* 2 bw)) (+ 590 (* 2 bw) th)))
      (sh-ok "offset: band out on the left, 4 px down"
             (first *sh-offsets*)
             (list (- (+ bw 16)) (+ (- (+ bw th)) 4 -16))))
    (setf (window-width win) 0 (window-height win) 0)
    (sh-ok "with nothing committed yet, the requested size stands in"
           (multiple-value-list (decoration-window-size win))
           '(800 600))))

;;; --- The cut-out, the colour, and the falloff ---

(sh-case "interior and falloff"
  (sh-stub)
  (let ((win (sh-float :sh-b 800 600 800 600)))
    (decorations-render-pass)
    (let* ((state (gethash win *decorations*))
           (hole (sh-hole state)))
      (destructuring-bind (hx hy hw hh) hole
        (sh-ok "transparent everywhere under the window"
               (loop for y from hy below (+ hy hh)
                     sum (loop for x from hx below (+ hx hw)
                               count (plusp (sh-alpha state x y))))
               0)
        (let* ((mid (+ hy (floor hh 2)))
               (right (loop for x from (+ hx hw) below (getf state :w)
                            collect (sh-alpha state x mid))))
          (sh-ok (format nil "the right band falls off monotonically ~S" right)
                 (every #'>= right (rest right)) t)
          (sh-ok "it starts near half the peak at the window edge"
                 (<= 25 (first right) 45) t)
          (sh-ok "and fades to nothing at the buffer edge"
                 (car (last right)) 0))
        (let ((below (sh-alpha state (+ hx (floor hw 2)) (+ hy hh 4)))
              (above (sh-alpha state (+ hx (floor hw 2)) (- hy 5))))
          (sh-ok (format nil "shifted down: darker 4 px below than 5 px above (~D, ~D)"
                         below above)
                 (> below above) t))
        (let* ((p (sh-pixel state (+ hx hw 1) (+ hy (floor hh 2))))
               (a (ldb (byte 8 24) p)))
          (multiple-value-bind (r g b) (theme-rgb :decoration-shadow)
            (sh-ok "neutral: the shadow colour, untinted"
                   (list (ldb (byte 8 16) p) (ldb (byte 8 8) p) (ldb (byte 8 0) p))
                   (list (round (* r a) 255) (round (* g a) 255)
                         (round (* b a) 255)))))))))

;;; --- Control: a shadow sized from the requested size ---

(sh-case "requested size control"
  (sh-stub)
  ;; The client committed 20 px narrower than requested. Drawn around
  ;; the requested size with the committed window cut out, the strip
  ;; between them is a flat block near the peak; drawn around the
  ;; committed size, it ramps down from half the peak at the edge.
  (let* ((committed-w 760) (requested-w 780) (h 400) (band 16)
         (peak (* 255 0.3))
         (width (+ requested-w (* 2 band))) (height (+ h (* 2 band)))
         (ptr (cffi:foreign-alloc :uint32 :count (* width height))))
    (unwind-protect
         (flet ((strip-max ()
                  (loop for x from (+ band committed-w) below (+ band committed-w 4)
                        maximize (ldb (byte 8 24)
                                      (cffi:mem-aref ptr :uint32
                                                     (+ x (* (+ band 200) width)))))))
           (decoration-paint ptr width height band band band committed-w h
                             peak 0 0 0)
           (sh-ok "control: sized from the requested size, a flat block beside the window"
                  (> (strip-max) (* 0.85 peak)) t)
           (decoration-paint ptr (+ committed-w (* 2 band)) height band
                             band band committed-w h peak 0 0 0)
           (let ((x0 (+ band committed-w))
                 (w2 (+ committed-w (* 2 band))))
             (sh-ok "sized from the committed size, the band ramps from the edge"
                    (< (loop for x from x0 below (+ x0 4)
                             maximize (ldb (byte 8 24)
                                           (cffi:mem-aref ptr :uint32
                                                          (+ x (* (+ band 200) w2)))))
                       (* 0.6 peak))
                    t)))
      (cffi:foreign-free ptr))))

;;; --- Redraws ---

(sh-case "redraws"
  (sh-stub)
  (let ((win (sh-float :sh-c 400 300 400 300))
        (draws 0)
        (real (symbol-function 'decoration-draw)))
    (setf (symbol-function 'decoration-draw)
          (lambda (&rest args) (incf draws) (apply real args)))
    (unwind-protect
         (progn
           (decorations-render-pass)
           (decorations-render-pass)
           (sh-ok "an unchanged float is drawn once" draws 1)
           (setf (window-width win) 390)
           (decorations-render-pass)
           (sh-ok "a new committed size redraws it" draws 2)
           (setf (window-focused-p win) t)
           (decorations-render-pass)
           (sh-ok "a focus change redraws it" draws 3))
      (setf (symbol-function 'decoration-draw) real))))

(format t "~&=== ~D passed, ~D failed ===~%" *sh-pass* *sh-fail*)
(uiop:quit (if (zerop *sh-fail*) 0 1))
