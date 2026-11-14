;;; Titlebars for floating windows
;;;
;;; A strip above each qualifying float shows the window's title and
;;; gives the pointer something to drag: a press on it starts the
;;; same interactive move a client titlebar requests. The strip is a
;;; river_window_v1 decoration above the window, offset upward so it
;;; sits on the window's top border and spans the border's width. Its
;;; fill is the window's border colour (:focused-border or
;;; :unfocused-border), so strip and border read as one frame.
;;;
;;; A float gets no strip when any of these holds:
;;;   it has a parent (dialogs, file pickers, transients)
;;;   its size is fixed (min and max size hints equal)
;;;   it supports only client-side decorations
;;;   the first window rule carrying :titlebar sets it NIL
;;;   it is fullscreen, maximized, or minimized
;;;   decorations are left to clients, (:windows (:decorations :client))
;;;   titlebars are off, (:theme (:titlebar nil))
;;; Tiled windows never get one.
;;;
;;; Protocol discipline (river-window-management-v1):
;;;   get_decoration_above  anytime; the surface must be fresh
;;;   set_offset            render sequence only
;;;   sync_next_commit      render sequence only; the surface must
;;;                         commit before render_finish
;;;   destroy               anytime
;;; All of it runs from titlebars-render-pass, inside
;;; on-render-sequence. The buffer is redrawn only when the title,
;;; size, focus, or colours change, and reallocated on any size
;;; change.
;;;
;;; Settings, under :theme:
;;;   :titlebar         T (the default) or NIL (off)
;;;   :titlebar-height  strip height in px; NIL or absent derives it
;;;                     from the font's line height
;;; Per window rule: (:match "pavucontrol" :titlebar nil).

(in-package #:rhizome)

(defvar *titlebars* (make-hash-table :test #'eq)
  "WINDOW -> titlebar state plist:
(:proxy :surface :ptr :fd :size :pool :buffer :w :h :drawn :offset).")

(defparameter *titlebar-padding* 4
  "Pixels between the strip's edges and its text.")

;;; --- Qualification ---

(defun window-fixed-size-p (win)
  "True when WIN's size hints pin both dimensions: min equals max and
neither is 0 (no preference)."
  (let ((min-w (window-hint-min-width win))
        (max-w (window-hint-max-width win))
        (min-h (window-hint-min-height win))
        (max-h (window-hint-max-height win)))
    (and (integerp min-w) (integerp max-w) (integerp min-h) (integerp max-h)
         (plusp max-w) (plusp max-h)
         (= min-w max-w) (= min-h max-h))))

(defun window-rule-titlebar (win)
  "The :titlebar value of the first window rule that matches WIN and
carries :titlebar, or T when none does."
  (dolist (rule (config-get '(:window-rules)) t)
    (when (listp rule)
      (let ((value (getf rule :titlebar :absent)))
        (when (and (not (eq value :absent))
                   (window-rule-matches-p win rule))
          (return value))))))

(defun titlebars-enabled-p ()
  "True unless (:theme (:titlebar nil)) switches titlebars off."
  (and (config-getf '(:theme) :titlebar :default t) t))

(defun window-titlebar-p (win)
  "True when WIN is a float that carries a titlebar (see the file
header for the exclusions)."
  (and (titlebars-enabled-p)
       (window-floating-p win)
       (not (window-minimized-p win))
       (not (window-fullscreen-p win))
       (not (window-maximized-p win))
       (not (window-has-parent-p win))
       (not (window-fixed-size-p win))
       (not (eq (window-decoration-hint win) :only-supports-csd))
       (eq (decoration-mode) :server)
       (window-rule-titlebar win)
       t))

;;; --- Geometry and colours ---

(defun titlebar-height ()
  "Strip height in px: (:theme :titlebar-height), else the font's line
height plus padding above and below."
  (let ((h (config-getf '(:theme) :titlebar-height :default nil)))
    (if (and (integerp h) (plusp h))
        h
        (+ (font-line-height) (* 2 *titlebar-padding*)))))

(defun titlebar-border-width (win)
  "WIN's float border width: 2 px focused, 1 px unfocused, as
float-render-decisions sets it."
  (if (window-focused-p win) 2 1))

(defun titlebar-geometry (win)
  "The strip relative to WIN's top-left corner, as
(VALUES X-OFFSET Y-OFFSET WIDTH HEIGHT): the border box's width, its
bottom edge on the top border. The width is the one the client
committed, else the float width; a client may snap to a smaller size
than proposed (a terminal's cell grid)."
  (let ((bw (titlebar-border-width win))
        (h (titlebar-height))
        (w (if (plusp (window-width win))
               (window-width win)
               (or (window-float-width win) 100))))
    (values (- bw) (- (+ h bw)) (+ w (* 2 bw)) h)))

(defun titlebar-hit-p (win px py)
  "True when global point PX PY lies on WIN's strip, at WIN's float
position."
  (let ((x (window-float-x win)) (y (window-float-y win)))
    (when (and x y)
      (multiple-value-bind (dx dy w h) (titlebar-geometry win)
        (let ((left (+ x dx)) (top (+ y dy)))
          (and (<= left px) (< px (+ left w))
               (<= top py) (< py (+ top h))))))))

(defun titlebar-colours (focused-p)
  "Fill and text colours as (VALUES FR FG FB TR TG TB). The fill is
the theme border colour; the text is the bar background on a light
fill and the bar foreground on a dark one."
  (multiple-value-bind (fr fg fb) (theme-border-rgb focused-p)
    (multiple-value-bind (tr tg tb)
        (parse-hex-color
         (if (> (+ (* 299 fr) (* 587 fg) (* 114 fb)) 140000)
             (config-getf '(:bar) :background :default "#1a1b26")
             (config-getf '(:bar) :foreground :default "#c0caf5")))
      (values fr fg fb tr tg tb))))

(defun titlebar-fit-title (title max-width &optional (measure #'font-text-width))
  "TITLE cut to MAX-WIDTH px as measured by MEASURE, ending in \"...\"
when cut. Empty when TITLE is NIL or not even \"...\" fits."
  (cond ((null title) "")
        ((<= (funcall measure title) max-width) title)
        (t (loop for n from (1- (length title)) downto 0
                 for candidate = (concatenate 'string (subseq title 0 n) "...")
                 when (<= (funcall measure candidate) max-width)
                   return candidate
                 finally (return "")))))

;;; --- Pixels ---

(defun titlebar-free (state)
  "Release STATE's shm buffer, pool, mapping, and memfd."
  (when (getf state :buffer)
    (wl:wl-buffer.destroy (getf state :buffer)))
  (when (getf state :pool)
    (wl:wl-shm-pool.destroy (getf state :pool)))
  (when (getf state :ptr)
    (cffi:foreign-funcall "munmap" :pointer (getf state :ptr)
                          :unsigned-long (getf state :size) :int))
  (when (getf state :fd)
    (cffi:foreign-funcall "close" :int (getf state :fd) :int))
  nil)

(defun titlebar-draw (state w h title focused-p)
  "Fill a W x H strip in the theme colour for FOCUSED-P and write
TITLE on it, into STATE's shm buffer, reallocated whenever W or H
changes. Returns the updated STATE, or NIL when no buffer could be
allocated."
  (unless (and (eql (getf state :w) w) (eql (getf state :h) h))
    ;; decoration-shm keeps a buffer of equal byte size, whose stride
    ;; would be wrong for a strip of the same area and a new shape
    (titlebar-free state)
    (setf (getf state :ptr) nil (getf state :buffer) nil
          (getf state :pool) nil (getf state :fd) nil
          (getf state :size) nil (getf state :w) nil (getf state :h) nil))
  (let ((state (decoration-shm state w h)))
    (when state
      (setf (getf state :w) w (getf state :h) h)
      (multiple-value-bind (fr fg fb tr tg tb) (titlebar-colours focused-p)
        (let ((ptr (getf state :ptr))
              (pixel (make-argb fr fg fb)))
          (dotimes (y h)
            (dotimes (x w)
              (put-pixel-argb ptr w h x y pixel)))
          (font-ensure-initialized)
          (let ((text (titlebar-fit-title
                       title (- w (* 2 *titlebar-padding*)))))
            (font-draw-text ptr w h text *titlebar-padding*
                            (max 0 (floor (- h (font-line-height)) 2))
                            tr tg tb))))
      state)))

;;; --- Lifecycle ---

(defun cleanup-window-titlebar (win)
  "Destroy WIN's titlebar decoration, surface, and buffer. Safe when
WIN has none."
  (let ((state (gethash win *titlebars*)))
    (when state
      (remhash win *titlebars*)
      (handler-case
          (progn
            (when (getf state :proxy)
              (rwm:river-decoration-v1.destroy (getf state :proxy)))
            (when (getf state :surface)
              (wl:wl-surface.destroy (getf state :surface)))
            (titlebar-free state))
        (error (e)
          (format t "~&; Rhizome: titlebar cleanup error: ~A~%" e)))))
  nil)

(defun ensure-window-titlebar (win)
  "WIN's titlebar state, creating the decoration above WIN with a
fresh surface when it has none. NIL when the globals are missing."
  (or (gethash win *titlebars*)
      (when (and *compositor* *shm* (window-proxy win))
        (let ((surface (wl:wl-compositor.create-surface *compositor*)))
          (handler-case
              (setf (gethash win *titlebars*)
                    (list :proxy (rwm:river-window-v1.get-decoration-above
                                  (window-proxy win) surface)
                          :surface surface
                          :drawn nil :offset nil))
            (error (e)
              (wl:wl-surface.destroy surface)
              (format t "~&; Rhizome: titlebar creation failed: ~A~%" e)
              nil))))))

(defun titlebar-update (win)
  "Keep WIN's strip current: redraw and commit when the title, size,
focus, or colours changed; set the offset when it changed. Render
sequence only."
  (let ((state (ensure-window-titlebar win)))
    (when state
      (multiple-value-bind (dx dy w h) (titlebar-geometry win)
        (let* ((focused (and (window-focused-p win) t))
               (key (list (window-title win) w h focused
                          (multiple-value-list (titlebar-colours focused)))))
          (unless (equal key (getf state :drawn))
            (let ((drawn (titlebar-draw state w h (window-title win) focused)))
              (when drawn
                (setf state drawn
                      (getf state :drawn) key)
                (rwm:river-decoration-v1.sync-next-commit (getf state :proxy))
                (wl:wl-surface.attach (getf state :surface)
                                      (getf state :buffer) 0 0)
                (wl:wl-surface.damage (getf state :surface) 0 0 w h)
                (wl:wl-surface.commit (getf state :surface)))))
          (unless (equal (getf state :offset) (list dx dy))
            (rwm:river-decoration-v1.set-offset (getf state :proxy) dx dy)
            (setf (getf state :offset) (list dx dy)))
          (setf (gethash win *titlebars*) state)))))
  nil)

;;; --- Render pass ---

(defun titlebars-render-pass ()
  "Give every qualifying float a current strip and destroy the strips
of windows that no longer qualify or are gone. Render sequence only.
Returns NIL."
  (handler-case
      (progn
        (loop for win being the hash-keys of *titlebars*
              unless (and (gethash (window-proxy win) *windows*)
                          (window-titlebar-p win))
                do (cleanup-window-titlebar win))
        (loop for win being the hash-values of *windows*
              when (window-titlebar-p win)
                do (titlebar-update win)))
    (error (e)
      (format t "~&; Rhizome: titlebar error: ~A~%" e)))
  nil)

;;; --- Pointer ---

(defun titlebar-pressed (win seat)
  "Start an interactive move of WIN on SEAT when SEAT's pointer is on
WIN's strip. Returns T when a move was requested."
  (when (and win (window-titlebar-p win)
             (titlebar-hit-p win (seat-pointer-x seat) (seat-pointer-y seat)))
    (float-op-request win seat :move)
    t))

(add-hook :window-closed #'cleanup-window-titlebar)
(add-hook :window-closed #'forget-window-parent)
