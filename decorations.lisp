;;; Decoration surfaces: shadows for floats, a glow for the focused
;;; tiled window
;;;
;;; River gives the WM a surface per window via
;;; river_window_v1.get_decoration_below/above: the WM sizes it,
;;; draws it, and positions it with river_decoration_v1.set_offset
;;; (relative to the window's top-left; negative offsets reach
;;; outside the window rect, which is how a shadow band works).
;;;
;;; Rhizome draws a soft neutral shadow under every floating window
;;; (decoration_below): the rect of the size the client committed,
;;; with its border and titlebar, blurred and shifted a few px down,
;;; and cut out where the window covers it. The focused tiled window
;;; takes a narrow glow in the gap, tinted toward the focus colour.
;;; Tiled windows otherwise keep river's crisp set_borders.
;;;
;;; Protocol discipline:
;;;   get_decoration_below   anytime, but the surface must be
;;;                          role-less and buffer-less (fresh)
;;;   set_offset             RENDER sequence only
;;;   sync_next_commit       RENDER sequence only, and the
;;;                          surface must commit before the
;;;                          render_finish that ends the pass
;;;   destroy                anytime
;;; Everything decoration-related therefore happens inside
;;; decorations-render-pass, called from on-render-sequence.

(in-package #:rhizome)

;;; --- State ---

(defvar *decorations* (make-hash-table :test #'eq)
  "WINDOW → decoration state list:
(:proxy ... :surface ... :ptr ... :fd ... :size ... :pool ...
 :buffer ... :w ... :h ... :focused ... :offset-sent ...)")

(defun decoration-config (&key (enabled t) (radius 16) (alpha 0.3)
                          (focused-alpha 0.4))
  "(:decorations ...) settings with defaults. ALPHA and FOCUSED-ALPHA
are the shadow's peak opacity under an unfocused and a focused float."
  (list :enabled (config-getf '(:decorations) :enabled :default enabled)
        :radius (config-getf '(:decorations) :radius :default radius)
        :alpha (config-getf '(:decorations) :alpha :default alpha)
        :focused-alpha (config-getf '(:decorations) :focused-alpha
                                    :default focused-alpha)))

(defun decoration-radius ()
  "The shadow band width in px (0 disables)."
  (let* ((cfg (decoration-config))
         (enabled (getf cfg :enabled))
         (radius (getf cfg :radius)))
    (if (and enabled (plusp radius) (realp radius))
        (round radius)
        0)))

(defun decoration-window-size (win)
  "The size WIN's client committed, as (VALUES W H): the dimensions of
its last river dimensions event, else the requested float size until
the first one arrives. A client may commit less than was requested
(a terminal snaps to its cell grid)."
  (values (if (plusp (window-width win))
              (window-width win)
              (or (window-float-width win) 100))
          (if (plusp (window-height win))
              (window-height win)
              (or (window-float-height win) 100))))

(defun decoration-target-rect (win)
  "The rect a decoration surrounds, relative to WIN's top-left, as
(VALUES X Y W H): the committed size (see decoration-window-size)
widened by the border on every side and raised by the titlebar when
WIN carries one."
  (multiple-value-bind (w h) (decoration-window-size win)
    (let ((bw (or (window-border-width win) 0))
          (th (if (window-titlebar-p win) (titlebar-height) 0)))
      (values (- bw) (- (+ bw th)) (+ w (* 2 bw)) (+ h (* 2 bw) th)))))

(defparameter *shadow-offset-y* 4
  "Px a float's shadow sits below the rect it surrounds.")

;;; --- Lifecycle ---

(defun cleanup-window-decoration (win)
  "Destroy WIN's decoration (proxy, surface, shm). Safe when
absent. Called from the :closed hook and when a window un-floats."
  (let ((state (gethash win *decorations*)))
    (when state
      (remhash win *decorations*)
      (handler-case
          (progn
            (when (getf state :proxy)
              (rwm:river-decoration-v1.destroy (getf state :proxy)))
            (when (getf state :surface)
              (wl:wl-surface.destroy (getf state :surface)))
            (when (getf state :buffer)
              (wl:wl-buffer.destroy (getf state :buffer)))
            (when (getf state :pool)
              (wl:wl-shm-pool.destroy (getf state :pool)))
            (when (getf state :ptr)
              (cffi:foreign-funcall "munmap" :pointer (getf state :ptr)
                                    :unsigned-long (getf state :size) :int))
            (when (getf state :fd)
              (cffi:foreign-funcall "close" :int (getf state :fd) :int)))
        (error (e)
          (format t "~&; Rhizome: decoration cleanup error: ~A~%" e))))))

(defun ensure-window-decoration (win)
  "Create the decoration (surface + river_decoration_below proxy)
for WIN if it lacks one. The surface must be fresh: no role, no
buffer. get_decoration_below is anytime-legal."
  (let ((state (gethash win *decorations*)))
    (when (and (null state)
               *compositor* (window-proxy win)
               (plusp (decoration-radius)))
      (let ((surface (wl:wl-compositor.create-surface *compositor*)))
        (handler-case
            (let ((proxy (rwm:river-window-v1.get-decoration-below
                           (window-proxy win) surface)))
              (setf (gethash win *decorations*)
                    (list :proxy proxy
                          :surface surface
                          :w 0 :h 0
                          :focused (window-focused-p win)
                          :offset-sent nil)))
          (error (e)
            (wl:wl-surface.destroy surface)
            (format t "~&; Rhizome: decoration creation failed: ~A~%" e)))))))

;;; --- Drawing ---

(defun decoration-shm (state width height)
  "The decoration's ARGB shm buffer, resized when needed."
  (let ((needed (* width height 4)))
    (if (and (getf state :ptr)
             (= (getf state :size) needed))
        state
        (progn
          ;; free the old buffer if any
          (when (getf state :buffer)
            (wl:wl-buffer.destroy (getf state :buffer)))
          (when (getf state :pool)
            (wl:wl-shm-pool.destroy (getf state :pool)))
          (when (getf state :ptr)
            (cffi:foreign-funcall "munmap" :pointer (getf state :ptr)
                                  :unsigned-long (getf state :size) :int))
          (when (getf state :fd)
            (cffi:foreign-funcall "close" :int (getf state :fd) :int))
          (let* ((fd (cffi:foreign-funcall "memfd_create" :string "deco" :int 0 :int))
                 (ptr (if (minusp fd)
                          nil
                          (progn
                            (cffi:foreign-funcall "ftruncate" :int fd
                                                  :long needed :int)
                            (cffi:foreign-funcall "mmap"
                                                  :pointer (cffi:null-pointer)
                                                  :unsigned-long needed
                                                  :int 3 :int 1
                                                  :int fd :long 0 :pointer)))))
            (if (or (minusp fd) (cffi:null-pointer-p ptr))
                nil
                (let* ((pool (wl:wl-shm.create-pool *shm* fd needed))
                       (buffer (wl:wl-shm-pool.create-buffer
                                 pool 0 width height (* width 4) :argb8888)))
                  (setf (getf state :ptr) ptr
                        (getf state :fd) fd
                        (getf state :size) needed
                        (getf state :pool) pool
                        (getf state :buffer) buffer)
                  state)))))))

(defun decoration-falloff (count band lo hi)
  "COUNT samples, one per pixel centre, of a box from LO to HI blurred
by a Gaussian of sigma BAND/3, as a vector of single-floats from 0.0
to 1.0. 0.5 at each box edge, under 0.002 BAND px outside it."
  (let ((v (make-array count :element-type 'single-float))
        (k (/ 1.0 (* (/ (max 1 band) 3.0) (sqrt 2.0)))))
    (dotimes (i count v)
      (let ((c (+ i 0.5)))
        (setf (aref v i)
              (* 0.5 (- (erf-approx (float (* k (- hi c)) 1.0))
                        (erf-approx (float (* k (- lo c)) 1.0)))))))))

(defun decoration-paint (ptr width height band hole-x hole-y hole-w hole-h
                         peak r g b)
  "Paint a soft shadow into the WIDTH x HEIGHT buffer at PTR: the rect
inset BAND px from every buffer edge, blurred over BAND px (see
decoration-falloff), in R G B at PEAK alpha (0-255), and transparent
inside the HOLE-W x HOLE-H rect at (HOLE-X HOLE-Y), the part a window
covers. Every pixel is written. A row equal to the one above it is
copied, so the per-pixel computation grows with the perimeter, not
the area, which keeps a redraw inside a render sequence short. Premultiplied."
  (let ((fx (decoration-falloff width band band (- width band)))
        (fy (decoration-falloff height band band (- height band)))
        (ramp (make-array 256 :element-type '(unsigned-byte 32)))
        (row-bytes (* width 4)))
    (dotimes (a 256)
      (setf (aref ramp a) (make-argb r g b :alpha a)))
    (flet ((hole-row-p (y) (and (<= hole-y y) (< y (+ hole-y hole-h)))))
      (dotimes (y height)
        (if (and (plusp y)
                 (= (aref fy y) (aref fy (1- y)))
                 (eq (hole-row-p y) (hole-row-p (1- y))))
            (cffi:foreign-funcall
             "memcpy"
             :pointer (cffi:inc-pointer ptr (* 4 (surface-index width height 0 y)))
             :pointer (cffi:inc-pointer ptr (* 4 (surface-index width height
                                                                0 (1- y))))
             :unsigned-long row-bytes
             :pointer)
            (let ((ky (* peak (aref fy y)))
                  (in-hole-row (hole-row-p y)))
              (dotimes (x width)
                (put-pixel-argb
                 ptr width height x y
                 (if (and in-hole-row (<= hole-x x) (< x (+ hole-x hole-w)))
                     0
                     (aref ramp (min 255 (max 0 (round (* ky (aref fx x))))))))))))))
  ptr)

(defun decoration-draw (state width height band peak-alpha
                        hole-x hole-y hole-w hole-h r g b)
  "Paint the decoration in STATE's WIDTH x HEIGHT buffer: a shadow of
BAND px in R G B at PEAK-ALPHA (0.0 to 1.0) with the HOLE rect cut
out, see decoration-paint."
  (let ((ptr (getf state :ptr)))
    (unless (cffi:null-pointer-p ptr)
      (decoration-paint ptr width height band hole-x hole-y hole-w hole-h
                        (* 255 peak-alpha) r g b))))

;;; --- Render pass (render-sequence only) ---

(defun decoration-redraw (state dw dh paint)
  "Redraw the DW x DH decoration buffer in STATE as PAINT, the list
(BAND PEAK-ALPHA HOLE-X HOLE-Y HOLE-W HOLE-H R G B) decoration-draw
takes, and commit it. Called only inside a render sequence:
sync_next_commit is render-only and the surface commit must land
before the render_finish that ends this pass."
  (let ((buffered (decoration-shm state dw dh)))
    (when buffered
      (setf state buffered)
      (apply #'decoration-draw state dw dh paint)
      (rwm:river-decoration-v1.sync-next-commit (getf state :proxy))
      (wl:wl-surface.attach (getf state :surface) (getf state :buffer) 0 0)
      (wl:wl-surface.damage (getf state :surface) 0 0 dw dh)
      (wl:wl-surface.commit (getf state :surface))))
    ;; the updated plist: threaded back to the caller, which writes it
    ;; into *decorations* (setf/getf rebinds, not mutates)
    state)

(defun decoration-needs-redraw-p (state dw dh focused colour
                                  &optional geometry)
  "True when the decoration in STATE no longer matches the size DW x DH,
the focus FOCUSED, COLOUR (a list of the shadow's r g b and its theme
colour), or GEOMETRY (the band, the cut-out rect, and the offset)."
  (or (/= (getf state :w) dw)
      (/= (getf state :h) dh)
      (not (eql (getf state :focused) focused))
      (not (equal (getf state :colour) colour))
      (not (equal (getf state :geometry) geometry))))

(defun decoration-update-for-window (win band peak-alpha offset-y r g b)
  "Keep WIN's decoration current (render pass only): a shadow of BAND
px in R G B at PEAK-ALPHA around WIN's target rect (see
decoration-target-rect), shifted OFFSET-Y px down, with the rect
itself cut out. The state is a plist: setf/getf REBINDS rather than
mutates, so the updated list is threaded and written back to the
hash. Redraws when the size or the cut-out changed, and when the
focus or the colour changed. Moves the surface when its offset
changed."
  (ensure-window-decoration win)
  (let ((state (gethash win *decorations*)))
    (when state
      (multiple-value-bind (rx ry rw rh) (decoration-target-rect win)
        (let* ((dw (+ rw (* 2 band)))
               (dh (+ rh (* 2 band)))
               (offset (list (- rx band) (- (+ ry offset-y) band)))
               (hole (list band (- band offset-y) rw rh))
               (geometry (list band offset hole))
               (focused (window-focused-p win))
               (colour (list r g b (theme-color :decoration-shadow))))
          (when (decoration-needs-redraw-p state dw dh focused colour geometry)
            (setf (getf state :w) dw
                  (getf state :h) dh
                  (getf state :focused) focused
                  (getf state :colour) colour
                  (getf state :geometry) geometry)
            (setf state (decoration-redraw
                         state dw dh
                         (list* band peak-alpha (append hole (list r g b))))))
          (unless (equal (getf state :offset-sent) offset)
            (rwm:river-decoration-v1.set-offset
             (getf state :proxy) (first offset) (second offset))
            (setf (getf state :offset-sent) offset))
          ;; write the (possibly rethreaded) plist back
          (setf (gethash win *decorations*) state)))))
  nil)

(declaim (ftype (function () null) decorations-render-pass))

(defun decorations-render-pass ()
  "Called from on-render-sequence: keep every floating window's shadow
and the focused tiled window's glow current. A float's shadow is the
neutral :decoration-shadow colour, shifted *shadow-offset-y* px down;
the glow is that colour tinted halfway toward :decoration-tint. All
river_decoration_v1 requests here are render-sequence legal:
set_offset and sync_next_commit are render-only, and the commit lands
before this render sequence's render_finish. Returns NIL."
  (let ((radius (decoration-radius)))
    (when (plusp radius)
      (let ((cfg (decoration-config)))
        (multiple-value-bind (sr sg sb) (theme-rgb :decoration-shadow)
          (multiple-value-bind (tr tg tb) (theme-rgb :decoration-tint)
            (loop for win being the hash-values of *windows*
                  ;; A fullscreen window covers its decoration and river
                  ;; clips it to the output: keep it as it is, undrawn,
                  ;; until the window leaves fullscreen
                  if (window-fullscreen-p win)
                    do (progn)
                  else
                  if (and (window-floating-p win)
                          (not (window-minimized-p win))
                          (gethash (window-proxy win) *windows*))
                    do (decoration-update-for-window
                        win radius
                        (getf cfg (if (window-focused-p win)
                                      :focused-alpha :alpha))
                        *shadow-offset-y* sr sg sb)
                  else
                  if (and (gethash (window-proxy win) *windows*)
                          (not (window-minimized-p win))
                          (not (window-floating-p win))
                          (window-focused-p win))
                    do (decoration-update-for-window
                        win
                        (config-getf '(:decorations) :glow-radius :default 6)
                        (config-getf '(:decorations) :glow-alpha :default 0.3)
                        0
                        (round (+ sr tr) 2) (round (+ sg tg) 2)
                        (round (+ sb tb) 2))
                  else
                    do (cleanup-window-decoration win))))))
    nil))

;;; Cleanup on window close
(add-hook :window-closed #'cleanup-window-decoration)
