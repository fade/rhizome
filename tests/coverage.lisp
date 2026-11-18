;;; Tests for the antialiased rounded-rect coverage (surface.lisp):
;;; the signed distance field behind every rounded shape, edge
;;; coverage, even stroke weight on arcs and straights, the coverage
;;; mask cache, and clipping at the buffer edge. Each property has a
;;; control: the binary inside/outside test the field replaced, kept
;;; here as REFERENCE-BINARY-TEST, fails it.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defvar *cv-pass* 0)
(defvar *cv-fail* 0)

(defun cv-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *cv-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *cv-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro cv-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *cv-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

(defun reference-binary-test (x y w h radius)
  "The binary rounded-rect test the coverage field replaced: (VALUES
INSIDE-P BORDER-P) for pixel (X Y), with no partial coverage."
  (let* ((rr (max 0 radius))
         (cx (cond ((< x rr) rr) ((> x (- w rr 1)) (- w rr 1)) (t x)))
         (cy (cond ((< y rr) rr) ((> y (- h rr 1)) (- h rr 1)) (t y)))
         (edge (or (= x 0) (= x (1- w)) (= y 0) (= y (1- h))))
         (in-corner (or (and (< x rr) (< y rr))
                        (and (< x rr) (> y (- h rr 1)))
                        (and (> x (- w rr 1)) (< y rr))
                        (and (> x (- w rr 1)) (> y (- h rr 1)))))
         (d (sqrt (+ (expt (- cx x) 2) (expt (- cy y) 2)))))
    (cond ((and in-corner (> d rr)) (values nil nil))
          ((or edge (and in-corner (>= d (- rr 1.5)))) (values t t))
          (t (values t nil)))))

(defun ray-stroke-weight (stroke-at radius)
  "The stroke weight along the 45-degree ray out of the top-left
corner centre of a shape with corner RADIUS: the integral of
STROKE-AT (a function of pixel X Y returning 0.0 to 1.0) over the
pixels the ray crosses."
  (let ((sum 0d0) (step 0.001d0) (c (/ (sqrt 2d0) 2)))
    (loop for s from (- radius 4d0) to (+ radius 2d0) by step
          for px = (- radius (* s c))
          for py = (- radius (* s c))
          when (and (>= px 0) (>= py 0))
            do (incf sum (* step (funcall stroke-at (floor px) (floor py)))))
    sum))

(defun row-stroke-weight (stroke-at y)
  "The stroke weight across the left straight edge on row Y: the sum
of STROKE-AT over the first six pixels."
  (loop for x below 6 sum (funcall stroke-at x y)))

(defun cv-call-with-buffer (width height thunk)
  "Call THUNK with a zeroed WIDTH x HEIGHT ARGB buffer; free it after."
  (let ((ptr (cffi:foreign-alloc :uint32 :count (* width height)
                                         :initial-element 0)))
    (unwind-protect (funcall thunk ptr)
      (cffi:foreign-free ptr))))

(defun cv-alpha-at (ptr width x y)
  "The alpha byte of pixel (X Y) in a WIDTH-wide buffer."
  (ldb (byte 8 24) (cffi:mem-aref ptr :uint32 (+ x (* y width)))))

;;; --- Edge coverage ---

(cv-case "edge coverage"
  (cv-call-with-buffer 60 34
    (lambda (ptr)
      (paint-rounded-surface ptr 60 34 40 40 40 :alpha 255 :radius 10
                             :border-r 200 :border-g 200 :border-b 200)
      (cv-ok "arc pixels take intermediate alpha"
             (> (loop for y below 34
                      sum (loop for x below 60
                                count (< 0 (cv-alpha-at ptr 60 x y) 255)))
                8)
             t)
      (cv-ok "the corner pixel outside the arc is transparent"
             (cv-alpha-at ptr 60 0 0) 0)
      (cv-ok "a straight top edge pixel is fully covered"
             (cv-alpha-at ptr 60 30 0) 255)
      (cv-ok "a straight left edge pixel is fully covered"
             (cv-alpha-at ptr 60 0 17) 255)
      (cv-ok "premultiplied: no channel exceeds its alpha"
             (loop for i below (* 60 34)
                   for p = (cffi:mem-aref ptr :uint32 i)
                   count (or (> (ldb (byte 8 16) p) (ldb (byte 8 24) p))
                             (> (ldb (byte 8 8) p) (ldb (byte 8 24) p))
                             (> (ldb (byte 8 0) p) (ldb (byte 8 24) p))))
             0))))

(cv-case "edge coverage control"
  (let ((seen nil))
    (dotimes (y 34)
      (dotimes (x 60)
        (pushnew (if (reference-binary-test x y 60 34 10) 255 0) seen)))
    (cv-ok "control: the binary test gives only full or no coverage"
           (sort seen #'<) '(0 255))))

;;; --- Stroke weight: arc against straight edge ---

(cv-case "stroke weight"
  (dolist (width '(1 2))
    (let* ((stroke-at (lambda (x y)
                        (nth-value 1 (rounded-rect-coverage
                                      x y 60 34 10 :stroke width))))
           (arc (ray-stroke-weight stroke-at 10))
           (straight (row-stroke-weight stroke-at 17)))
      (cv-ok (format nil "a ~D px stroke is ~D px on the straight edge"
                     width width)
             (round straight) width)
      (cv-ok (format nil "a ~D px stroke at the arc's 45-degree point is within 0.25 px of the straight edge (~,3F against ~,3F)"
                     width arc straight)
             (<= (abs (- arc straight)) 0.25) t))))

(cv-case "stroke weight control"
  (let* ((stroke-at (lambda (x y)
                      (if (nth-value 1 (reference-binary-test x y 60 34 10))
                          1.0 0.0)))
         (arc (ray-stroke-weight stroke-at 10))
         (straight (row-stroke-weight stroke-at 17)))
    (cv-ok (format nil "control: the binary border at 45 degrees differs from its straight edge by more than 0.25 px (~,3F against ~,3F)"
                   arc straight)
           (> (abs (- arc straight)) 0.25) t)))

;;; --- The coverage mask cache ---

(cv-case "mask cache"
  (clrhash *coverage-masks*)
  (rounded-rect-masks 41 23 7 1)
  (let ((before *rounded-rect-distance-calls*))
    (rounded-rect-masks 41 23 7 1)
    (cv-ok "a cached shape leaves the distance field alone"
           (- *rounded-rect-distance-calls* before) 0))
  (let ((before *rounded-rect-distance-calls*))
    (rounded-rect-masks 41 24 7 1)
    (cv-ok "a new shape computes its field once per pixel"
           (- *rounded-rect-distance-calls* before) (* 41 24))))

;;; --- Clipping at the buffer edge ---

(cv-case "clipping"
  (cv-call-with-buffer 20 10
    (lambda (ptr)
      (blend-rounded-rect ptr 20 10 -3 0 10 10 255 255 255
                          :alpha 255 :radius 3)
      (cv-ok "a veil that starts left of the buffer still draws its visible part"
             (cv-alpha-at ptr 20 2 5) 255)
      (cv-ok "and nothing past its right edge"
             (cv-alpha-at ptr 20 8 5) 0)))
  (cv-call-with-buffer 20 10
    (lambda (ptr)
      (fill-rounded-rect ptr 20 10 2 2 12 6 255 0 0 :alpha 255 :radius 3)
      (cv-ok "a filled pill covers its middle"
             (cffi:mem-aref ptr :uint32 (+ 8 (* 5 20))) #xffff0000)
      (cv-ok "a filled pill's arc pixel is partly covered"
             (< 0 (cv-alpha-at ptr 20 3 2) 255) t))))

(format t "~&=== ~D passed, ~D failed ===~%" *cv-pass* *cv-fail*)
(uiop:quit (if (zerop *cv-fail*) 0 1))
