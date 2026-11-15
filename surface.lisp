;;; Shared software-surface pixel primitives
;;;
;;; ONE place for the row-major offset formula. The transposed
;;; form (y + W·x instead of y·W + x) has struck this codebase five
;;; times — bar, launcher (twice), decorations, and once in a test
;;; reader — because both forms look plausible, compile clean, and
;;; pass small-coordinate tests; only wide/tall draws fault past
;;; the buffer. Every surface now draws through these helpers, so
;;; the formula cannot be transposed per-surface and out-of-range
;;; writes are caught instead of corrupting memory.
;;;
;;; Buffers are premultiplied ARGB u32 (wl_shm argb8888):
;;; little-endian layout byte0=B byte1=G byte2=R byte3=A.

(in-package #:rhizome)

(declaim (inline surface-index))

(defun surface-index (width height x y)
  "Row-major byte-4 pixel index for (X Y) in a WIDTHxHEIGHT buffer,
or NIL when out of range. The ONLY definition of the offset
formula in the codebase."
  (declare (type integer width height x y))
  (if (and (>= x 0) (< x width)
           (>= y 0) (< y height))
      (+ (* width y) x)
      nil))

(declaim (inline put-pixel-argb))

(defun put-pixel-argb (ptr width height x y argb)
  "Store a premultiplied ARGB u32 pixel at (X Y); out-of-range
writes are a NO-OP (logged once per surface, not per pixel, by
the caller if it cares). PTR is the mmap'd shm base."
  (let ((idx (surface-index width height x y)))
    (unless (null idx)
      (setf (cffi:mem-aref ptr :unsigned-int idx) argb))))

(declaim (inline get-pixel-argb))

(defun get-pixel-argb (ptr width height x y)
  "Read a premultiplied ARGB u32 pixel at (X Y), or 0 out of
range."
  (let ((idx (surface-index width height x y)))
    (if (null idx)
        0
        (cffi:mem-aref ptr :unsigned-int idx))))

(defun make-argb (r g b &key (alpha 255))
  "A premultiplied pixel: channels are straight (0-255), alpha
0-255; channels are scaled by alpha here so the stored pixel
always satisfies c* <= a."
  (let ((a (min 255 (max 0 (round alpha))))
        (r (min 255 (max 0 (round r))))
        (g (min 255 (max 0 (round g))))
        (b (min 255 (max 0 (round b)))))
    (logior (ash a 24)
            (ash (round (* r a) 255) 16)
            (ash (round (* g a) 255) 8)
            (round (* b a) 255))))

(defun put-pixel (ptr width height x y r g b &key (alpha 255))
  "Store a straight-color pixel with the given alpha (premultiplied
on the way in). Out-of-range is a no-op."
  (put-pixel-argb ptr width height x y (make-argb r g b :alpha alpha)))

(defun blend-pixel (ptr width height x y r g b &key (alpha 255))
  "Alpha-BLEND a straight color over the existing pixel (premultiplied
over, per Wayland): result = src + dst·(1-src_a). For text and
icon antialiasing edges. ALPHA 0 leaves the destination intact."
  (let ((idx (surface-index width height x y)))
    (unless (null idx)
      (let* ((a (min 255 (max 0 (round alpha))))
              (r (min 255 (max 0 (round r))))
              (g (min 255 (max 0 (round g))))
              (b (min 255 (max 0 (round b))))
              (sa a)
             (old (cffi:mem-aref ptr :unsigned-int idx))
             (da (logand 255 (ash old -24)))
             (dr (logand 255 (ash old -16)))
             (dg (logand 255 (ash old -8)))
             (db (logand 255 old))
             (inv (- 255 sa))
             (out-a (min 255 (+ sa (round (* da inv) 255))))
             (out-r (min 255 (+ (round (* r sa) 255)
                                (round (* dr inv) 255))))
             (out-g (min 255 (+ (round (* g sa) 255)
                                (round (* dg inv) 255))))
             (out-b (min 255 (+ (round (* b sa) 255)
                                (round (* db inv) 255)))))
        (setf (cffi:mem-aref ptr :unsigned-int idx)
              (logior (ash out-a 24)
                      (ash out-r 16)
                      (ash out-g 8)
                      out-b))))))

;;; --- Rounded-rect primitives (the launcher treatment, shared) ---

(defvar *rounded-rect-distance-calls* 0
  "How many times rounded-rect-distance has run. A counter for tests
and measurements; no drawing path reads it.")

(declaim (type fixnum *rounded-rect-distance-calls*))

(declaim (inline rounded-rect-distance edge-coverage))

(defun rounded-rect-distance (x y w h radius)
  "Signed distance in px from the point (X Y) to the outline of the
W x H rect at the origin with corner RADIUS: negative inside, positive
outside, exact everywhere. RADIUS is clamped to half the shorter side.
Every rounded shape on a software surface derives its coverage from
this one field."
  (declare (type single-float x y w h radius)
           (optimize (speed 3) (safety 0)))
  (incf *rounded-rect-distance-calls*)
  (let* ((hw (* 0.5 w))
         (hh (* 0.5 h))
         (r (max 0.0 (min radius hw hh)))
         (qx (- (abs (- x hw)) (- hw r)))
         (qy (- (abs (- y hh)) (- hh r)))
         (ox (max qx 0.0))
         (oy (max qy 0.0)))
    (declare (type single-float hw hh r qx qy)
             (type (single-float 0.0) ox oy))
    (- (+ (sqrt (+ (* ox ox) (* oy oy)))
          (min (max qx qy) 0.0))
       r)))

(defun edge-coverage (d)
  "Area coverage of a pixel whose centre lies D px outside an edge:
1.0 at D <= -0.5, 0.0 at D >= 0.5, linear between."
  (declare (type single-float d)
           (optimize (speed 3) (safety 0)))
  (max 0.0 (min 1.0 (- 0.5 d))))

(defun rounded-rect-coverage (px py w h radius &key (stroke 0))
  "Coverage of pixel (PX PY) by the W x H rect at the origin with
corner RADIUS, as (VALUES FILL STROKE), each 0.0 to 1.0. STROKE is a
stroke width in px: the stroke is the fill less the fill of the shape
inset by STROKE, so arcs and straights carry one even weight. FILL
is 1.0 on every pixel inside a straight edge: the rect's edges lie on
pixel boundaries, so only the arcs take partial coverage."
  (let* ((d (rounded-rect-distance (+ (float px 1.0) 0.5)
                                   (+ (float py 1.0) 0.5)
                                   (float w 1.0) (float h 1.0)
                                   (float radius 1.0)))
         (fill (edge-coverage d)))
    (values fill
            (if (plusp stroke)
                (- fill (edge-coverage (+ d (float stroke 1.0))))
                0.0))))

(defvar *coverage-masks* (make-hash-table :test #'equal)
  "(W H RADIUS STROKE) -> (FILL . STROKE): cached coverage masks, see
rounded-rect-masks.")

(defvar *coverage-masks-lock* (bt:make-lock "coverage-masks")
  "Guards *coverage-masks*: the bar's tick and dbus threads both draw.")

(defparameter *coverage-mask-limit* 64
  "Entries *coverage-masks* holds before it is emptied and refilled.")

(defun rounded-rect-masks (w h radius &optional (stroke 0))
  "The coverage of a W x H rect with corner RADIUS, as (VALUES FILL
STROKE): two row-major (unsigned-byte 8) vectors of W*H bytes, 255
for a covered pixel, from rounded-rect-coverage with STROKE. Cached
per shape, so a redraw of a shape already drawn reads the cache and
leaves the distance field alone."
  (let ((key (list w h radius stroke)))
    (bt:with-lock-held (*coverage-masks-lock*)
      (let ((hit (gethash key *coverage-masks*)))
        (if hit
            (values (car hit) (cdr hit))
            (let* ((w (max 0 w))
                   (h (max 0 h))
                   (fill (make-array (* w h) :element-type '(unsigned-byte 8)))
                   (ring (make-array (* w h) :element-type '(unsigned-byte 8))))
              (dotimes (y h)
                (dotimes (x w)
                  (multiple-value-bind (f s)
                      (rounded-rect-coverage x y w h radius :stroke stroke)
                    (setf (aref fill (+ x (* y w))) (round (* 255 f))
                          (aref ring (+ x (* y w))) (round (* 255 s))))))
              (when (>= (hash-table-count *coverage-masks*)
                        *coverage-mask-limit*)
                (clrhash *coverage-masks*))
              (setf (gethash key *coverage-masks*) (cons fill ring))
              (values fill ring)))))))

(defun mix-argb (src dst c)
  "Premultiplied pixel SRC weighted by coverage C (0-255) plus DST
weighted by the rest: SRC where C is 255, DST where C is 0."
  (declare (type (unsigned-byte 32) src dst)
           (type (integer 0 255) c))
  (let ((k (- 255 c)))
    (flet ((channel (shift)
             (round (+ (* c (ldb (byte 8 shift) src))
                       (* k (ldb (byte 8 shift) dst)))
                    255)))
      (logior (ash (channel 24) 24) (ash (channel 16) 16)
              (ash (channel 8) 8) (channel 0)))))

(defun rounded-surface-pixel (fill stroke r g b border-r border-g border-b
                              alpha)
  "The premultiplied pixel of a fill in R G B under a border in
BORDER-R BORDER-G BORDER-B, at FILL and STROKE coverage (0-255,
STROKE <= FILL) and surface ALPHA (0-255)."
  (let ((inner (- fill stroke)))
    (flet ((channel (body border)
             (round (* alpha (+ (* body inner) (* border stroke)))
                    (* 255 255))))
      (logior (ash (round (* alpha fill) 255) 24)
              (ash (channel r border-r) 16)
              (ash (channel g border-g) 8)
              (channel b border-b)))))

(defun erf-approx (x)
  "The error function of X as a single-float, to within 1.5e-7
(Abramowitz and Stegun 7.1.26). Odd and monotonic, so a falloff built
on it steps smoothly at 8-bit alpha."
  (declare (type single-float x)
           (optimize (speed 3) (safety 0)))
  (let* ((s (abs x))
         (k (/ 1.0 (+ 1.0 (* 0.3275911 s))))
         (p 1.0614054))
    (declare (type single-float s k p))
    (setf p (+ -1.4531521 (* k p))
          p (+ 1.4214138 (* k p))
          p (+ -0.28449672 (* k p))
          p (+ 0.25482959 (* k p))
          p (* k p))
    (let ((e (- 1.0 (* p (exp (- (* s s)))))))
      (if (minusp x) (- e) e))))

(defun fill-rounded-rect (ptr width height x y w h r g b
                              &key (alpha 255) (radius 0))
  "Paint the rect at (X Y W H), rounded when RADIUS > 0, in R G B at
ALPHA: a covered pixel takes the colour, an edge pixel mixes it with
the pixel below by its coverage. The selection-pill primitive.
Clipped to the buffer; premultiplied."
  (let ((src (make-argb r g b :alpha alpha))
        (fill (rounded-rect-masks w h radius)))
    (loop for dy from (max 0 (- y)) below (min h (- height y)) do
      (loop for dx from (max 0 (- x)) below (min w (- width x))
            for c = (aref fill (+ dx (* dy w)))
            unless (zerop c) do
              (let ((px (+ x dx)) (py (+ y dy)))
                (put-pixel-argb ptr width height px py
                                (if (= c 255)
                                    src
                                    (mix-argb src
                                              (get-pixel-argb ptr width height
                                                              px py)
                                              c))))))))

(defun blend-rounded-rect (ptr width height x y w h r g b
                           &key (alpha 255) (radius 0))
  "Blend the rect at (X Y W H), rounded when RADIUS > 0, OVER the
existing pixels (source-over) at ALPHA scaled by each pixel's
coverage. Unlike fill-rounded-rect, which replaces covered pixels, it
keeps what lies below: the bar's hover veil must not punch a hole
through the modules it covers. Clipped to the buffer; premultiplied."
  (let ((fill (rounded-rect-masks w h radius)))
    (loop for dy from (max 0 (- y)) below (min h (- height y)) do
      (loop for dx from (max 0 (- x)) below (min w (- width x))
            for c = (aref fill (+ dx (* dy w)))
            unless (zerop c) do
              (blend-pixel ptr width height (+ x dx) (+ y dy) r g b
                           :alpha (round (* alpha c) 255))))))

(defun paint-rounded-surface (ptr width height r g b
                              &key (alpha 255) (radius 0)
                                (border-r 255) (border-g 255)
                                (border-b 255) (border-width 1))
  "Paint the whole surface as a rect with corner RADIUS, filled in
R G B under a BORDER-WIDTH px border in BORDER-R BORDER-G BORDER-B,
both at ALPHA. It paints the background of the bar and the launcher,
and of each menu, message window, and prompt. Every pixel is written; a pixel outside the shape becomes
transparent and an edge pixel takes its coverage. Premultiplied."
  (multiple-value-bind (fill ring)
      (rounded-rect-masks width height radius border-width)
    (let ((body (rounded-surface-pixel 255 0 r g b border-r border-g border-b
                                       alpha))
          (edge (rounded-surface-pixel 255 255 r g b
                                       border-r border-g border-b alpha)))
      (dotimes (y height)
        (dotimes (x width)
          (let* ((i (+ x (* y width)))
                 (f (aref fill i))
                 (s (aref ring i)))
            (put-pixel-argb ptr width height x y
                            (cond ((zerop f) 0)
                                  ((and (= f 255) (zerop s)) body)
                                  ((and (= f 255) (= s 255)) edge)
                                  (t (rounded-surface-pixel
                                      f s r g b border-r border-g border-b
                                      alpha))))))))))
