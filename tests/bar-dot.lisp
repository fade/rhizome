;;; Tests for the occupied-workspace dot (bar.lisp): drawn under each
;;; digit as the occupied workspace numbers draw it, bold, it stays
;;; clear of every glyph pixel of "1" to "9" and inside the line box.
;;; Control: the dot as it was placed before, one row below the
;;; baseline, touches a digit's foot. Pixels are drawn into private
;;; buffers.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: the configure calls below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(defvar *bd-pass* 0)
(defvar *bd-fail* 0)

(defun bd-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *bd-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *bd-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro bd-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *bd-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

(defparameter *bd-width* 40)
(defparameter *bd-height* 30)
(defparameter *bd-x* 10)
(defparameter *bd-y* 4)

(defun bd-lit (label dot-p)
  "The set of lit pixels, as a list of (X Y), after drawing LABEL with
the occupied workspace's flags into a clear buffer, with the dot when
DOT-P."
  (let ((ptr (cffi:foreign-alloc :uint32 :count (* *bd-width* *bd-height*)
                                         :initial-element 0))
        (flags (state-cue-flags :workspace-occupied)))
    (unwind-protect
         (let ((*bar-hitboxes* nil))
           (bar-draw-module ptr *bd-width* *bd-height* *bd-x* *bd-y*
                            (list (list* label nil
                                         (if dot-p
                                             flags
                                             (let ((f (copy-list flags)))
                                               (remf f :dot)
                                               f))))
                            255 255 255 nil)
           (loop for y below *bd-height*
                 nconc (loop for x below *bd-width*
                             unless (zerop (cffi:mem-aref
                                            ptr :uint32 (+ x (* y *bd-width*))))
                               collect (list x y))))
      (cffi:foreign-free ptr))))

(defun bd-touches-p (dot glyph)
  "True when fewer than two clear pixels separate DOT from GLYPH: some
pixel of DOT lies within two pixels (Chebyshev distance) of a pixel of
GLYPH. A one-pixel gap reads as touching at bar size."
  (some (lambda (d)
          (some (lambda (g)
                  (and (<= (abs (- (first d) (first g))) 2)
                       (<= (abs (- (second d) (second g))) 2)))
                glyph))
        dot))

(defun bd-old-dot (label)
  "The pixels of the dot as it was placed before: 2x2, centred under
LABEL, one row below the baseline."
  (let* ((tw (bar-text-width label :bold t))
         (x0 (+ *bd-x* (floor tw 2) -1))
         (y0 (min (+ *bd-y* (font-ascent) 1)
                  (+ *bd-y* (font-line-height) -2))))
    (loop for dy below 2
          nconc (loop for dx below 2 collect (list (+ x0 dx) (+ y0 dy))))))

(font-ensure-initialized)

(bd-case "dot clear of glyphs"
  (let ((touching nil) (outside nil) (missing nil))
    (loop for digit from 1 to 9
          for label = (princ-to-string digit)
          for glyph = (bd-lit label nil)
          for dot = (set-difference (bd-lit label t) glyph :test #'equal)
          do (when (null dot) (push label missing))
             (when (bd-touches-p dot glyph) (push label touching))
             (when (some (lambda (p)
                           (>= (second p) (+ *bd-y* (font-line-height))))
                         dot)
               (push label outside)))
    (bd-ok "every digit draws a dot" missing nil)
    (bd-ok "two clear pixels separate the dot from every glyph pixel of 1 to 9" touching nil)
    (bd-ok "every dot stays inside the line box" outside nil)))

(bd-case "dot shape"
  (let* ((glyph (bd-lit "4" nil))
         (dot (set-difference (bd-lit "4" t) glyph :test #'equal)))
    (bd-ok (format nil "the dot is a 3 px disc (~D pixels)" (length dot))
           (<= 5 (length dot) 9) t)))

(bd-case "control"
  (bd-ok "control: the dot one row below the baseline comes within a pixel of a digit"
         (loop for digit from 1 to 9
               for label = (princ-to-string digit)
               thereis (bd-touches-p (bd-old-dot label) (bd-lit label nil)))
         t))

(format t "~&=== ~D passed, ~D failed ===~%" *bd-pass* *bd-fail*)
(uiop:quit (if (zerop *bd-fail*) 0 1))
