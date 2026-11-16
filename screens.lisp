;;; Screen numbers for Rhizome
;;;
;;; Every output (screen) has a number, counted from the shared index
;;; base (see index-base): the primary screen is the first, and the
;;; others follow on a left-handed spiral out from it. Screens nearer
;;; the primary, centre to centre, come first; at equal distance the
;;; screen met first turning counter-clockwise from the top comes
;;; first, so in a row L2 L1 M R1 R2 with M primary the order is M,
;;; L1, R1, L2, R2.
;;;
;;;   (rhizome:configure
;;;     '(:screens (:primary "DVI-D-1"
;;;                 :order ("DVI-D-1" "HDMI-A-1"))))
;;;
;;; :primary names the primary screen by connector; without it, or
;;; when it names no connected screen, the first output river lists is
;;; primary. :order, when set, numbers the connected screens it names
;;; in its own order, and any other screens follow on the spiral from
;;; the first of them. Connectors in :order that are not connected are
;;; skipped.

(in-package #:rhizome)

(defun outputs-in-listing-order ()
  "Every output, in the order river listed them this session."
  (sort (copy-list (all-outputs)) #'< :key #'output-serial))

(defun output-named-p (output name)
  "True when OUTPUT's connector name is NAME, case ignored."
  (let ((connector (output-connector-name output)))
    (and connector (stringp name) (string-equal connector name))))

(defun primary-screen ()
  "The primary screen: the output (:screens :primary) names by
connector, else the first output river listed. NIL with no outputs."
  (let ((outputs (outputs-in-listing-order))
        (name (config-getf '(:screens) :primary)))
    (or (and name
             (find-if (lambda (output) (output-named-p output name)) outputs))
        (first outputs))))

(defun spiral-key (origin output)
  "OUTPUT's place on the spiral around ORIGIN as (VALUES DISTANCE
ANGLE): the squared distance between doubled centres, and the angle
counter-clockwise from straight up, in radians from 0 below 2pi."
  (flet ((centre (o)
           (values (+ (* 2 (output-x o)) (output-width o))
                   (+ (* 2 (output-y o)) (output-height o)))))
    (multiple-value-bind (ox oy) (centre origin)
      (multiple-value-bind (x y) (centre output)
        (let* ((dx (- x ox))
               (dy (- y oy))
               (angle (if (and (zerop dx) (zerop dy))
                          0
                          (atan (- dx) (- dy)))))
          (values (+ (* dx dx) (* dy dy))
                  (if (minusp angle) (+ angle (* 2 pi)) angle)))))))

(defun spiral-order (origin outputs)
  "OUTPUTS in spiral order around ORIGIN: nearest centre first, then
counter-clockwise from the top, then listing order."
  (flet ((before-p (a b)
           (multiple-value-bind (da aa) (spiral-key origin a)
             (multiple-value-bind (db ab) (spiral-key origin b)
               (cond ((/= da db) (< da db))
                     ((/= aa ab) (< aa ab))
                     (t (< (output-serial a) (output-serial b))))))))
    (sort (copy-list outputs) #'before-p)))

(defun screen-order ()
  "Every output in screen-number order: the connected screens
(:screens :order) names, in its order, else the primary screen; then
the rest on the spiral from the first of those."
  (let* ((outputs (outputs-in-listing-order))
         (order (config-getf '(:screens) :order))
         (listed '()))
    (when (listp order)
      (dolist (name order)
        (let ((output (find-if (lambda (o) (output-named-p o name)) outputs)))
          (when (and output (not (member output listed)))
            (push output listed)))))
    (setf listed (nreverse listed))
    (let ((head (or listed
                    (let ((primary (primary-screen)))
                      (and primary (list primary))))))
      (when head
        (append head
                (spiral-order (first head)
                              (remove-if (lambda (o) (member o head))
                                         outputs)))))))

(defun screen-ordinal (output)
  "OUTPUT's zero-based position in screen-order, or NIL."
  (and output (position output (screen-order))))

(defun screen-number (output &key (index-base (index-base)))
  "OUTPUT's screen number under INDEX-BASE, or NIL when OUTPUT is not
a registered output."
  (let ((ordinal (screen-ordinal output)))
    (and ordinal (display-index ordinal :index-base index-base))))

(defun screen-by-number (designator &key (index-base (index-base)))
  "The output DESIGNATOR names: a screen number under INDEX-BASE
(an integer or a string of digits), or a connector name, case
ignored. NIL when it names no connected screen."
  (let ((ordinal (index-ordinal designator :index-base index-base)))
    (cond (ordinal
           (and (>= ordinal 0) (nth ordinal (screen-order))))
          ((stringp designator)
           (let ((name (string-trim " " designator)))
             (find-if (lambda (o) (output-named-p o name)) (all-outputs)))))))
