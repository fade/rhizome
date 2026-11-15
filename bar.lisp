;;; Rhizome bar - a configurable status bar / mode-line
;;;
;;; A mode-line in the StumpWM tradition, rendered directly from the
;;; window manager's own state. The bar runs on its own Wayland
;;; connection in its own thread so its tick loop never blocks the WM
;;; event loop; it is a zwlr layer-shell surface with an exclusive zone
;;; so river reserves space for it and tiled windows avoid it.
;;;
;;; Configuration lives in *config* (see config.lisp). Modules are
;;; functions returning strings; the :left/:center/:right module lists
;;; can be changed live from the REPL.

(in-package #:rhizome)

;;; --- State ---

(defparameter *bar-thread* nil
  "The bar's tick thread.")
(defparameter *bar-running* nil
  "Whether the bar thread should keep running.")
(defparameter *bar-state* nil
  "Connection state hash for the running bar: :display :compositor
:shm :layer-shell :seat :pointer, :outputs (every bound wl_output) and
:output-names (output -> wl_output global name). Each output's bar
lives in its own ostate (see *bar-output-states*).")

(defvar *bar-cache* (make-hash-table)
  "Maps module names to (cons timestamp string) for rate-limited modules.")

;;; --- WM snapshot ---
;;;
;;; The bar runs as its own process, so it reads the WM's state
;;; snapshot from the file written by update-wm-snapshot in
;;; wm-protocol.lisp.

(defvar *wm-snapshot* '(:layout nil :windows 0 :workspace 1
                        :workspace-count 9 :occupied nil :focused nil)
  "Fallback snapshot, used until the WM writes a real one.")

(defun bar-snapshot ()
  "The WM's state snapshot (workspace, windows, layout, focus),
read from the snapshot file. Refreshed at most once per tick."
  (bar-cached :snapshot 0
              (lambda ()
                (or (handler-case
                      (with-open-file (s (snapshot-pathname))
                        (read s))
                      (error () nil))
                    *wm-snapshot*))))

;;; --- One bar per output ---
;;;
;;; The bar process binds every wl_output (the root state's :outputs)
;;; and gives each output whose bar is open an OSTATE: a hash table
;;; holding that output's layer surface, shm buffer and click regions.
;;; *bar-output-states* finds an ostate by output, *bar-surface-states*
;;; by wl_surface. The root state keeps each output's wl_output global
;;; name under :output-names; the WM snapshot's :outputs entries carry
;;; the same name under :wl-output.
;;;
;;; By default every bar draws the same content. Opt-in (:bar ...)
;;; keys: :per-output-content (each bar shows its own output),
;;; :active-screen (the current output's bar takes the active
;;; background and an underline :active-underline-width px wide),
;;; :tray-output :primary and :global-modules :primary (other bars
;;; drop the tray, or every global module), and :outputs (which
;;; outputs carry a bar).

(defparameter *bar-output-states* (make-hash-table)
  "Per-output bar state: output proxy -> the ostate hash holding that
output's :output :global-name :surface :layer-surface :shm :width
:height :mapped :shm-fd :shm-ptr :shm-size :wl-pool :wl-buffer
:hitboxes :last-content. One bar process, one surface (and one shm
buffer) per output whose bar is open.")

(defparameter *bar-surface-states* (make-hash-table)
  "wl_surface proxy -> that output's bar ostate (pointer routing).")

(defparameter *bar-pointer-ostate* nil
  "The ostate whose surface holds the pointer, or NIL.")

(defparameter *bar-closed-at* (make-hash-table)
  "Output proxy -> the universal time the compositor last closed that
output's bar surface.")

(defparameter *bar-output-entry* nil
  "The snapshot :outputs entry of the output whose bar is rendering,
or NIL.")

(defparameter *bar-output-view* nil
  "The snapshot keys that shadow the global ones for the rendering bar
(see bar-view): the output's whole entry under
(:bar :per-output-content), else only the current output's workspace
numbering (:labels :index-base :connector).")

(defparameter *bar-output-primary* t
  "True while rendering the primary bar, the one that keeps every
module under (:bar :tray-output :primary) or (:bar :global-modules
:primary).")

(defparameter *bar-per-output-modules* '(:workspaces :layout :windows)
  "Modules that describe one output. Under (:bar :global-modules
:primary) these are the only modules on bars other than the primary.")

(defparameter *bar-reopen-delay* 5
  "Seconds an output's bar stays closed after the compositor closes
its surface.")

(defun bar-view ()
  "The snapshot as the rendering bar reads it: *bar-output-view*
shadows the global keys (see bar-ostate-view)."
  (append *bar-output-view* (bar-snapshot)))

(defun bar-output-entry (snapshot name)
  "SNAPSHOT's :outputs entry for wl_output global NAME, or NIL."
  (find name (getf snapshot :outputs)
        :key (lambda (entry) (getf entry :wl-output))
        :test #'eql))

(defun bar-ostates ()
  "The open ostates in wl_output global-name order."
  (sort (loop for ostate being the hash-values of *bar-output-states*
              collect ostate)
        #'< :key (lambda (ostate) (or (gethash :global-name ostate) 0))))

(defun bar-output-global-name (state output)
  "The wl_output global name STATE bound OUTPUT under, or NIL."
  (let ((names (gethash :output-names state)))
    (and names (values (gethash output names)))))

(defun bar-output-named (state name)
  "The output STATE bound as wl_output global NAME, or NIL."
  (find name (gethash :outputs state)
        :key (lambda (output) (bar-output-global-name state output))
        :test #'eql))

(defun bar-forget-output (state name)
  "Forget the output STATE bound as wl_output global NAME: close its
bar and drop it from STATE's :outputs. No effect for an unknown NAME."
  (let ((output (bar-output-named state name)))
    (when output
      (let ((ostate (gethash output *bar-output-states*)))
        (when ostate
          (bar-close-surface ostate)))
      (remhash output (gethash :output-names state))
      (remhash output *bar-closed-at*)
      (setf (gethash :outputs state)
            (remove output (gethash :outputs state))))))

(defun bar-ostate-primary-p (ostate)
  "True when OSTATE is the primary bar: the snapshot marks its output
:primary, or, with a snapshot that lists no outputs, OSTATE is the
first open ostate."
  (let ((snapshot (bar-snapshot)))
    (if (eq (getf snapshot :outputs :absent) :absent)
        (eq ostate (first (bar-ostates)))
        (getf (bar-output-entry snapshot (gethash :global-name ostate))
              :primary))))

(defun bar-output-wanted-p (state output snapshot)
  "True when OUTPUT, bound in STATE, should show a bar under SNAPSHOT:
the bar is not hidden, the snapshot marks OUTPUT :bar (every output
when the snapshot lists no outputs), and the compositor has not closed
OUTPUT's bar within *bar-reopen-delay* seconds."
  (let ((closed-at (gethash output *bar-closed-at*))
        (outputs (getf snapshot :outputs :absent)))
    (and (not (getf snapshot :bar-hidden))
         (or (eq outputs :absent)
             (getf (bar-output-entry snapshot
                                     (bar-output-global-name state output))
                   :bar))
         (or (null closed-at)
             (>= (- (get-universal-time) closed-at) *bar-reopen-delay*))
         t)))

(defun bar-per-output-content-p ()
  "True under (:bar :per-output-content t): each bar shows its own
output's workspaces, layout and window count, and a workspace click
switches that output. NIL (the default): every bar shows the same
content, that of the current output."
  (config-getf '(:bar) :per-output-content :default nil))

(defun bar-ostate-view (entry &optional (snapshot (bar-snapshot)))
  "The keys that shadow SNAPSHOT's for the bar of ENTRY's output: ENTRY
itself under (:bar :per-output-content), else the current output's
:labels, :index-base, and :connector, so every bar numbers the
workspaces the way the current output does."
  (if (bar-per-output-content-p)
      entry
      (let ((current (find-if (lambda (e) (getf e :current))
                              (getf snapshot :outputs))))
        (when current
          (list :labels (getf current :labels)
                :index-base (getf current :index-base 1)
                :connector (getf current :connector))))))

(defmacro with-bar-ostate ((ostate) &body body)
  "Run BODY as OSTATE's bar: *bar-output-entry* holds OSTATE's snapshot
entry, bar-view reads the keys bar-ostate-view picks, and
bar-ostate-segment-modules applies OSTATE's primary status."
  (let ((o (gensym "OSTATE")))
    `(let* ((,o ,ostate)
            (*bar-output-entry* (bar-output-entry (bar-snapshot)
                                                  (gethash :global-name ,o)))
            (*bar-output-view* (bar-ostate-view *bar-output-entry*))
            (*bar-output-primary* (bar-ostate-primary-p ,o)))
       ,@body)))

(defun bar-active-screen-p (&optional (entry *bar-output-entry*)
                                      (snapshot (bar-snapshot)))
  "True when ENTRY, a snapshot :outputs entry, is the current output
and SNAPSHOT lists more than one output."
  (and (getf entry :current)
       (> (length (getf snapshot :outputs)) 1)))

(defun bar-ostate-active-p ()
  "True when the rendering bar takes the active background: under
(:bar :active-screen t), its output is the current one of several.
NIL (the default): every bar takes the inactive background, which
follows :bar-background."
  (and (config-getf '(:bar) :active-screen :default nil)
       (bar-active-screen-p)
       t))

(defun bar-background-color (active-p)
  "The bar background as \"#rrggbb\": the :bar-background-active theme
element when ACTIVE-P, otherwise :bar-background-inactive, which
follows :bar-background unless set."
  (theme-color (if active-p
                   :bar-background-active
                   :bar-background-inactive)))

(defun bar-active-underline-width ()
  "The active bar's underline width in pixels: (:bar
:active-underline-width) clamped to 1-5. A value that is not an
integer gives 2."
  (let ((width (config-getf '(:bar) :active-underline-width :default 2)))
    (if (integerp width) (max 1 (min 5 width)) 2)))

(defun bar-active-underline-rows (height line-width)
  "The underline's rows in a bar HEIGHT px tall, as (VALUES FIRST-ROW
ROWS): LINE-WIDTH rows along the edge that faces the windows. The bar
is anchored to the top of its output, so that edge is the bottom."
  (let ((rows (max 0 (min line-width height))))
    (values (- height rows) rows)))

(defun bar-draw-active-underline (ptr width height radius r g b alpha)
  "Draw the active bar's underline into the WIDTH x HEIGHT buffer at
PTR: R G B rows at ALPHA, the bar surface alpha, across the bar's full
width along its window-facing edge, clipped to the bar's shape of
corner RADIUS. Returns NIL."
  (multiple-value-bind (first-row rows)
      (bar-active-underline-rows height (bar-active-underline-width))
    (let ((pixel (make-argb r g b :alpha alpha)))
      (loop for y from first-row below (+ first-row rows)
            do (loop for x below width
                     when (rounded-rect-test x y width height radius)
                       do (put-pixel-argb ptr width height x y pixel)))))
  nil)

;;; --- Pointer state (module click/scroll support) ---

(defparameter *bar-pointer-x* nil
  "Pointer x position on the bar surface, NIL when outside.")
(defparameter *bar-axis-accumulator* 0
  "Accumulated vertical scroll axis values since the last axis-stop.")
(defparameter *tray-render-stamp* 0
  "The *tray-changed* value at the last render; a mismatch forces a
bar re-render even when text modules are unchanged.")

(defparameter *bar-hover-rendered* '%none
  "The bar-hover-key value as of the last render: the pointer's bar
and the module under the pointer ('%none when it is off the bar). Pointer motion alone must re-render: the
hover veil otherwise bakes into the buffer at click time and stays
over the module until unrelated content changes.")

(defparameter *bar-hitboxes* nil
  "Module click regions: list of (module-name start-x end-x),
recorded during rendering.")

(defun bar-pointer-hitboxes ()
  "The hitboxes of the bar the pointer is over: its own ostate's, or,
when the pointer's surface is unknown, any bar's, else *bar-hitboxes*."
  (if *bar-pointer-ostate*
      (gethash :hitboxes *bar-pointer-ostate*)
      (or (loop for o being the hash-values of *bar-output-states*
                  thereis (gethash :hitboxes o))
          *bar-hitboxes*)))

;;; --- Module helpers ---

(defun bar-cached (module interval thunk)
  "Return the cached value for MODULE, refreshing via THUNK at most
once per INTERVAL seconds."
  (let* ((entry (gethash module *bar-cache*))
         (now (get-universal-time)))
    (when (or (null entry) (>= (- now (car entry)) interval))
      (setf entry (cons now
                        (handler-case
                            (or (funcall thunk) "")
                          (error () ""))))
      (setf (gethash module *bar-cache*) entry))
    (cdr entry)))

(defun bar-read-file (path)
  "Return the first line of a file, trimmed, or NIL."
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (let ((line (read-line s nil nil)))
        (when line (string-trim " " line))))))

(defun bar-join (strings)
  "Join non-empty strings with a double space separator."
  (let ((parts (remove "" strings :test #'string=)))
    (with-output-to-string (out)
      (loop for part in parts
            for first = t then nil
            unless first do (princ "  " out)
            do (princ part out)))))

;;; --- Modules ---
;;;
;;; Each module is a function of no arguments returning either a
;;; string, or a list of (text . color) parts where color is a hex
;;; string ("#7aa2f7") or NIL for the segment's base color.

(defun bar-module-tray ()
  "The tray module's text part: none (icons only)."
  nil)

(defun tray-item-icon (item)
  "An item's drawable icon: (w h data) from IconPixmap (ARGB32
converted to RGBA, scaled to the bar's icon size), or the theme
icon resolved through load-icon from IconName. NIL when the item
has nothing drawable."
  (or (and (getf item :icon)
           (destructuring-bind (w h data) (getf item :icon)
             (when (and (plusp w) (plusp h)
                        (>= (length data) (* 4 w h)))
               (scale-icon w h data *bar-icon-size*))))
      (and (getf item :icon-name)
           (load-icon (getf item :icon-name)))
      (and (getf item :icon-name)
           (load-icon (pathname-name (getf item :icon-name))))))

(defun bar-draw-tray (ptr width height x y base-r base-g base-b)
  "Draw every tray item's icon at (X, Y); one hitbox per item so
clicks/scrolls route to tray-click / tray-scroll. ALWAYS returns
a number: an empty or iconless tray returning NIL poisons the
x-threading for every module after it — the bar vanishes (the
2026-11-04 invisible-bar bug)."
  (dolist (item *tray-items*)
    (let* ((icon (tray-item-icon item))
           (start x))
      (when icon
        (destructuring-bind (iw ih idata) icon
          (loop for iy below ih
                for py = (+ y iy)
                while (< py height)
                do (loop for ix below *bar-icon-size*
                         for px = (+ x ix)
                         while (< px width)
                         do (let* ((idx (* (+ (* iy *bar-icon-size*) ix) 4))
                                   (a (aref idata (+ idx 3)))
                                   (r (aref idata idx))
                                   (g (aref idata (+ idx 1)))
                                   (b (aref idata (+ idx 2))))
                              (when (plusp a)
                                (bar-set-pixel
                                  ptr width height px py
                                  (floor (/ (+ (* a r) (* (- 255 a) base-r)) 255))
                                  (floor (/ (+ (* a g) (* (- 255 a) base-g)) 255))
                                  (floor (/ (+ (* a b) (* (- 255 a) base-b))
                                            255)))))))))
      (incf x *bar-icon-size*)
      (push (list item start x) *bar-hitboxes*)
      (incf x 4)))
  (+ x *bar-module-gap*))

(defun bar-module-layout ()
  "The layout name of the bar's output."
  (let ((layout (getf (bar-view) :layout)))
    (when layout (string-downcase (symbol-name layout)))))

(defun bar-module-windows ()
  "The window count of the bar's output's current workspace, e.g.
\"2 wins\"; empty at zero."
  (let ((count (getf (bar-view) :windows)))
    (when (and count (plusp count))
      (format nil "~D win~:P" count))))

(defun bar-module-focused ()
  (getf (bar-snapshot) :focused))

(defun bar-module-recording ()
  "A REC marker while any screen capture session is active
(screencasts, portals; v5 capture_sessions events via the
snapshot's :recording flag). Add :recording to a bar segment to
use it."
  (if (getf (bar-snapshot) :recording)
      (list (list* "REC " (theme-color :bar-recording)))
      ""))

(defun bar-module-echo ()
  "The WM's echo text (the snapshot's :echo, e.g. fselect's hint) in
the accent colour, or \"\" when there is none. It takes the centre
segment while shown unless the bar configuration places :echo itself."
  (let ((text (getf (bar-snapshot) :echo)))
    (if (and (stringp text) (plusp (length text)))
        (list (list* text (theme-color :bar-accent)
                     nil))
        "")))

(defun bar-module-workspaces ()
  "One cell per workspace of the bar's output, labelled by name or
display number, coloured by the :bar-workspace-current,
:bar-workspace-occupied, and :bar-workspace-empty elements. Occupancy
comes from the output's :occupied list (workspace numbers with visible
windows). Each state also takes its non-hue cue from *state-cues*: the
current workspace sits on a pill, and an occupied one (current or not)
is bold with a dot below it. Each part carries :cell (DISPLAY-NUMBER
CONNECTOR) for clicks."
  (let* ((snapshot (bar-view))
         (current (getf snapshot :workspace 1))
         (count (getf snapshot :workspace-count 9))
         (base (getf snapshot :index-base 1))
         (connector (getf snapshot :connector))
         (labels (or (getf snapshot :labels)
                     (loop for i from 1 to count
                           collect (princ-to-string (+ i base -1)))))
         (occupied (getf snapshot :occupied))
         (accent (theme-color :bar-workspace-current))
         (fg (theme-color :bar-workspace-occupied))
         (muted (theme-color :bar-workspace-empty))
         (gap (config-getf '(:bar :workspaces) :gap :default 8)))
    (loop for label in labels
          for i from 1
          collect (list* label
                         (cond ((= i current) accent)
                               ((member i occupied) fg)
                               (t muted))
                         (append (when (= i current)
                                   (state-cue-flags :workspace-current))
                                 (if (member i occupied)
                                     (state-cue-flags :workspace-occupied)
                                     (state-cue-flags :workspace-empty))
                                 (list :gap gap
                                       :cell (list (+ i base -1)
                                                   connector)))))))

(defun bar-module-battery ()
  "Battery percentage from /sys/class/power_supply, e.g. \"75%\"."
  (bar-cached :battery
              (config-getf '(:modules :battery) :interval :default 5)
              (lambda ()
                (let ((cap (bar-read-file "/sys/class/power_supply/BAT1/capacity"))
                      (status (bar-read-file "/sys/class/power_supply/BAT1/status")))
                  (when cap
                    (let ((charging (string-equal status "Charging")))
                      (list (list (concatenate 'string cap
                                               (if charging "%" ""))
                                  nil
                                  :icon :battery
                                  :level (parse-integer cap :junk-allowed t)
                                  :charging charging))))))))

(defun bar-module-volume ()
  "Volume percentage via wpctl, e.g. \"VOL 45%\" or \"MUTED\"."
  (bar-cached :volume
              (config-getf '(:modules :volume) :interval :default 5)
              (lambda ()
                (let* ((output (uiop:run-program
                                 '("wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@")
                                 :output :string :ignore-error-status t))
                       (line (with-input-from-string (s output)
                               (read-line s nil nil))))
                  (cond
                    ((and line (search "MUTED" line))
                     (list (list "MUTED" nil :icon :volume :muted t)))
                    ((and line (search "Volume:" line))
                     (let* ((num (string-trim " " (subseq line (1+ (search ":" line)))))
                            (vol (read-from-string num)))
                       (list (list (format nil "~A%" (round (* 100 vol)))
                                   nil :icon :volume :muted nil))))
                    (t ""))))))

(defun bar-module-display ()
  "Display brightness percentage with a monitor icon."
  (bar-cached :display
              (config-getf '(:modules :display) :interval :default 10)
              (lambda ()
                (handler-case
                    (let* ((out (uiop:run-program '("brightnessctl" "info")
                                                   :output :string
                                                   :ignore-error-status t))
                           (pct (when (and out (search "%" out))
                                  (let* ((pc (position #\( out))
                                         (pe (position #\% out)))
                                    (when (and pc pe (> pe pc))
                                      (subseq out (1+ pc) pe))))))
                      (when pct
                        (list (list pct nil :icon :display))))
                  (error () nil)))))

(defun bar-module-network ()
  "Active network connection name via nmcli, e.g. \"WIFI MyWifi\"."
  (bar-cached :network
              (config-getf '(:modules :network) :interval :default 10)
              (lambda ()
                (let ((output (uiop:run-program
                                '("nmcli" "-t" "-f" "NAME,TYPE"
                                  "connection" "show" "--active")
                                :output :string :ignore-error-status t)))
                  (with-input-from-string (s output)
                    (let ((line (read-line s nil nil)))
                      (when (and line (plusp (length line)))
                        (let* ((colon (position #\: line))
                               (name (if colon (subseq line 0 colon) line))
                               (type (if colon (subseq line (1+ colon)) "")))
                          (cond
                            ((search "wireless" type)
                             (list (list name nil :icon :wifi)))
                            ((search "ethernet" type)
                             (list (list "ETH" nil :icon :ethernet)))
                            (t name))))))))))

(defun bar-module-clock ()
  "Clock formatted per the config's :format (a small strftime subset:
%H %M %S %d %m %y %Y %a %A %b %B). Typography hierarchy: the date
part renders muted, the time part accent — split at the format's
last space. A single-segment format stays one part."
  (let* ((fmt (config-getf '(:modules :clock) :format
                           :default "%a %d %b %H:%M"))
         (space (position #\Space fmt :from-end t)))
    (if space
        (list (list (bar-format-clock (subseq fmt 0 space))
                    (theme-color :bar-clock-date)
                    :gap 10)
              (list (bar-format-clock (subseq fmt (1+ space)))
                    (theme-color :bar-clock-time)))
        (bar-format-clock fmt))))

(defun bar-format-clock (format)
  "Format the current time according to FORMAT, supporting a small
strftime subset: %H %M %S %d %m %y %Y %a %A %b %B."
  (multiple-value-bind (second minute hour date month year day-of-week)
      (get-decoded-time)
    ;; Common Lisp counts day-of-week from 0 = Monday
    (let ((short-days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
          (long-days #("Monday" "Tuesday" "Wednesday" "Thursday"
                       "Friday" "Saturday" "Sunday"))
          (short-months #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
          (long-months #("January" "February" "March" "April" "May" "June"
                         "July" "August" "September" "October" "November" "December")))
      (with-output-to-string (out)
        (loop with i = 0
              while (< i (length format))
              for char = (char format i)
              do (cond
                   ((and (char= char #\%) (< (1+ i) (length format)))
                    (let ((code (char format (1+ i))))
                      (princ (case code
                               (#\H (format nil "~2,'0D" hour))
                               (#\M (format nil "~2,'0D" minute))
                               (#\S (format nil "~2,'0D" second))
                               (#\d (format nil "~2,'0D" date))
                               (#\m (format nil "~2,'0D" month))
                               (#\y (format nil "~2,'0D" (mod year 100)))
                               (#\Y (format nil "~D" year))
                               (#\a (aref short-days day-of-week))
                               (#\A (aref long-days day-of-week))
                               (#\b (aref short-months (1- month)))
                               (#\B (aref long-months (1- month)))
                               (t (string code)))
                             out))
                    (incf i 2))
                   (t (princ char out) (incf i 1))))))))

(defparameter *bar-modules*
  '((:workspaces . bar-module-workspaces)
    (:layout . bar-module-layout)
    (:windows . bar-module-windows)
    (:focused . bar-module-focused)
    (:recording . bar-module-recording)
    (:echo . bar-module-echo)
    (:clock . bar-module-clock)
    (:battery . bar-module-battery)
    (:volume . bar-module-volume)
    (:network . bar-module-network)
    (:display . bar-module-display))
  "Maps configured module names to their rendering functions.")

(defparameter *bar-module-gap* 16
  "Pixels of empty space between modules.")

(defun bar-module-parts (module)
  "Render one module to a list of parts. A part is
(text color . plist): color NIL means the segment's base color,
and the plist tail carries render flags — :gap N adds N pixels of
breathing room after the part, :pill T blends the part's colour
into a rounded fill behind the text (the active-workspace marker),
:bold T draws the text bold, and :dot T puts a small dot below it
(the occupied-workspace marker).
Unknown or failing modules render empty. EMPTY STRINGS are
dropped: bar-cached substitutes \"\" when a thunk errors, and
empty text renders nothing."
  (let ((fn (cdr (assoc module *bar-modules*))))
    (when fn
      (handler-case
          (let ((out (funcall fn)))
            (typecase out
              (string (when (plusp (length out))
                        (list (cons out nil))))
              (list (remove-if (lambda (part)
                                 (or (null (first part))
                                     (zerop (length (first part)))))
                               out))
              (t nil)))
        (error () nil)))))

(defun bar-part-flags (part)
  "The plist tail of a part. NIL for the classic dotted shape
((text . color)) — (cdr part) being an atom, not a list, is the
telltale."
  (let ((rest (cdr part)))
    (when (listp rest) (cddr part))))

(defun bar-part-width (part)
  "Pixel width of one part: icon + text advance + gap."
  (+ (if (getf (bar-part-flags part) :icon)
         (bar-icon-width) 0)
     (bar-text-width (first part) :bold (getf (bar-part-flags part) :bold))
     (or (getf (bar-part-flags part) :gap 0) 0)))

(defun bar-segment-modules (position)
  "The configured module list for :left, :center or :right. While the
snapshot carries an :echo and no segment names :echo, the centre
segment is (:echo) alone, so the text has the middle of the bar on a
narrow output."
  (let ((modules (config-getf '(:bar :modules) position :default nil)))
    (if (and (eq position :center)
             (getf (bar-snapshot) :echo)
             (notany (lambda (segment)
                       (member :echo (config-getf '(:bar :modules) segment
                                                  :default nil)))
                     '(:left :center :right)))
        '(:echo)
        modules)))

(defun bar-ostate-segment-modules (position)
  "The modules the rendering bar draws in POSITION. The primary bar
draws them all, and so does every bar by default. Under
(:bar :tray-output :primary) another bar drops the tray; under
(:bar :global-modules :primary) it keeps only *bar-per-output-modules*."
  (let ((modules (bar-segment-modules position)))
    (if *bar-output-primary*
        modules
        (let ((global (config-getf '(:bar) :global-modules :default :all))
              (tray (config-getf '(:bar) :tray-output :default :all)))
          (remove-if-not (lambda (module)
                           (and (or (eq tray :all)
                                    (not (tray-module-p module)))
                                (or (eq global :all)
                                    (member module *bar-per-output-modules*))))
                         modules)))))

(defun bar-segment-width (modules)
  "Total pixel width of a module list, including inter-module gaps
and per-part :gap flags, so centered/right segments line up with
what is actually drawn."
  (let ((content
          (reduce #'+
                  (mapcar (lambda (module)
                            (cond
                              ((icons-module-p module)
                               (bar-icons-width module))
                              ((tray-module-p module)
                               (bar-tray-width))
                              (t
                               (reduce #'+
                                       (mapcar #'bar-part-width
                                               (bar-module-parts module))
                                       :initial-value 0))))
                          modules)
                  :initial-value 0)))
    (+ content (* *bar-module-gap* (length modules)))))

;;; --- Icons module (application icons with click actions) ---
;;;
;;; A module spec of the form (:icons (entry ...)) renders each
;;; entry's icon as a 16x16 image; entries carry their own click
;;; actions inline:
;;;
;;;   (:icons ((:icon "flameshot" :on-click "flameshot gui")
;;;            (:icon "variety" :on-click "variety")
;;;            (:icon "/path/to/custom.png" :on-click "rhizome-cmd lock")))

(defparameter *bar-icon-size* 16
  "Rendered icon size (fits the 20px default bar).")
(defparameter *bar-icon-cache* (make-hash-table :test 'equal)
  "Maps icon specs to scaled RGBA data (list width height vector).")
(defparameter *bar-icon-dirs*
  '("/usr/share/icons/hicolor/128x128/apps/"
    "/usr/share/icons/hicolor/64x64/apps/"
    "/usr/share/icons/hicolor/48x48/apps/"
    "/usr/share/icons/hicolor/256x256/apps/"
    "/usr/share/icons/hicolor/scalable/apps/"
    "/usr/share/pixmaps/")
  "Icon search order for name specs (PNG only; SVG skipped).")

(defun icons-module-p (module)
  "Whether MODULE is an icons module spec: (:icons (entry ...))."
  (and (listp module) (eq (first module) :icons)))

(defun tray-module-p (module)
  "Whether MODULE is the tray module (:tray)."
  (eq module :tray))

(defun bar-tray-width ()
  "Pixel width of the tray module: every item's icon plus spacing."
  (+ (* (length *tray-items*) (+ *bar-icon-size* 4))
     (if *tray-items* *bar-module-gap* 0)))

(defun bar-icons-entries (module)
  "The icon entries of an icons module."
  (second module))

(defun bar-icons-width (module)
  "Pixel width of an icons module (icons + inter-icon gaps)."
  (* (+ *bar-icon-size* 4) (length (bar-icons-entries module))))

(defun icon-svg-cache-path (svg)
  "Where SVG's rasterized PNG lives: icons/ under the cache directory."
  (let ((dir (merge-pathnames #P"icons/" (cache-directory))))
    (ensure-directories-exist dir)
    (merge-pathnames (make-pathname :name (pathname-name svg) :type "png")
                     dir)))

(defun rasterize-icon-svg (svg)
  "Rasterize SVG to a 64x64 PNG via rsvg-convert (cached by name).
Returns the PNG path, or NIL when the tool is missing or fails."
  (let ((png (icon-svg-cache-path svg)))
    (or (and (probe-file png) png)
        (handler-case
            (progn
              (uiop:run-program
                (list "rsvg-convert" "-w" "64" "-h" "64"
                      "--output" (namestring png) (namestring svg))
                :ignore-error-status t)
              (and (probe-file png) png))
          (error () nil)))))

(defun icon-theme-search (spec)
  "Search every installed icon theme's category directories for
SPEC.png or SPEC.svg: the user's icons/ under the XDG data directory,
then /usr/share/icons/*/. SNI items that ship only themed icons
(variety-indicator, for one) live in panel and status categories the
hicolor apps search never reaches. Returns a PNG path or NIL."
  (labels ((find-ext (type)
             (loop for root in (cons (merge-pathnames #P"icons/"
                                                      (base-home :data))
                                     (uiop:directory* #P"/usr/share/icons/*/"))
                   for hits = (directory
                                (merge-pathnames
                                  (make-pathname
                                    :directory '(:relative :wild-inferiors)
                                    :name spec :type type)
                                  root))
                   when hits return (first hits))))
    (or (find-ext "png")
        (let ((svg (find-ext "svg")))
          (and svg (rasterize-icon-svg svg))))))

(defun find-icon-file (spec)
  "Resolve an :icon spec: an absolute path, or an icon name searched
in the standard hicolor apps directories (PNG), then every theme's
category directories (PNG or rasterized SVG)."
  (cond ((and (find #\/ spec) (probe-file spec)) spec)
        (t (or (loop for dir in *bar-icon-dirs*
                     for file = (concatenate 'string dir spec ".png")
                     when (probe-file file)
                       return file)
               (icon-theme-search spec)))))

(defun scale-icon (source-w source-h source-data target-size)
  "Scale decoded RGBA data to TARGET-SIZE x TARGET-SIZE,
nearest-neighbor. Returns (width height data). Handles any source
bit depth — some .desktop icons are 16-bit PNGs, whose samples
exceed 255; those shift down to 8 bits."
  (let ((out (make-array (* target-size target-size 4)
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (flet ((sample (idx)
             (let ((v (row-major-aref source-data idx)))
               (if (> v 255)
                   (ash v -8)             ; 16-bit PNG: shift to 8-bit
                   v))))
      (loop for ty below target-size
            for sy = (floor (* ty source-h) target-size)
            do (loop for tx below target-size
                     for sx = (floor (* tx source-w) target-size)
                     for sidx = (* (+ (* sy source-w) sx) 4)
                     for tidx = (* (+ (* ty target-size) tx) 4)
                     do (loop for c below 4
                              do (setf (aref out (+ tidx c))
                                       (sample (+ sidx c)))))))
    (list target-size target-size out)))

(defun load-icon (spec &key (size *bar-icon-size*))
  "SPEC's icon, decoded and scaled to SIZE px; cached. NIL if the
icon cannot be found or decoded."
  (or (gethash (cons spec size) *bar-icon-cache*)
      (let ((file (find-icon-file spec)))
        (when file
          (setf (gethash (cons spec size) *bar-icon-cache*)
                (handler-case
                    (let ((png (pngload:load-file file :flatten t)))
                      (scale-icon (pngload:width png)
                                  (pngload:height png)
                                  (pngload:data png)
                                  size))
                  (error ()
                    (setf (gethash (cons spec size) *bar-icon-cache*)
                          nil))))))))

(defun bar-draw-icons (ptr width height x y module base-r base-g base-b)
  "Draw an icons module's entries at (X, Y); one hitbox per entry so
the existing click/scroll chain runs each entry's :on-click /
:on-scroll-* commands. Returns the x after the module plus gap."
  (dolist (entry (bar-icons-entries module))
    (let* ((icon (load-icon (getf entry :icon)))
           (start x))
      (when icon
        (destructuring-bind (iw ih idata) icon
          (declare (ignore iw))
          (loop for iy below ih
                for py = (+ y iy)
                while (< py height)
                do (loop for ix below *bar-icon-size*
                         for px = (+ x ix)
                         while (< px width)
                         do (let* ((idx (* (+ (* iy *bar-icon-size*) ix) 4))
                                   (a (aref idata (+ idx 3)))
                                   (r (aref idata idx))
                                   (g (aref idata (+ idx 1)))
                                   (b (aref idata (+ idx 2))))
                              (when (plusp a)
                                (bar-set-pixel
                                  ptr width height px py
                                  (floor (/ (+ (* a r) (* (- 255 a) base-r)) 255))
                                  (floor (/ (+ (* a g) (* (- 255 a) base-g)) 255))
                                  (floor (/ (+ (* a b) (* (- 255 a) base-b))
                                            255))))))))
        (incf x *bar-icon-size*)
        (push (list entry start x) *bar-hitboxes*)))
    (incf x 4))
  (+ x *bar-module-gap*))

;;; --- Rendering ---

(defun bar-set-pixel (ptr width height px py r g b)
  "Set pixel at (PX, PY) to RGB color (BGRA layout in the buffer),
premultiplied by the surface alpha (ARGB buffers). The offset
comes from surface-index — the ONE row-major formula, bounds-
checked; out-of-range writes are no-ops."
  (let ((idx (surface-index width height px py)))
    (unless (null idx)
      (let* ((a (bar-surface-alpha))
             (offset (* 4 idx)))
        (setf (cffi:mem-aref ptr :unsigned-char offset)
                (round (* b a) 255)
              (cffi:mem-aref ptr :unsigned-char (1+ offset))
                (round (* g a) 255)
              (cffi:mem-aref ptr :unsigned-char (+ offset 2))
                (round (* r a) 255)
              (cffi:mem-aref ptr :unsigned-char (+ offset 3)) a)))))

(defun bar-surface-alpha ()
  "The bar/message/prompt surfaces' alpha byte. (:bar :opacity
1.0) = opaque; the buffers are ARGB8888, which Wayland requires
PREMULTIPLIED — every writer premultiplies by this."
  (round (* 255 (min 1.0 (max 0.0
            (config-getf '(:bar) :opacity :default 0.9))))))

(defun bar-clear (ptr width height r g b)
  "Fill the bar buffer with a solid, translucent-capable color
(premultiplied ARGB)."
  (let* ((a (bar-surface-alpha))
         (pixel (logior (ash a 24)
                        (ash (round (* r a) 255) 16)
                        (ash (round (* g a) 255) 8)
                        (round (* b a) 255))))
    (loop for i below (* width height) do
      (setf (cffi:mem-aref ptr :unsigned-int i) pixel))))

(defun bar-draw-text (ptr width height text x y r g b &key bold)
  "Draw TEXT at (X, Y), the top of the text box, in RGB colour,
overstruck one pixel wider when BOLD.
Delegates to the font module: TrueType via freetype when available,
the 8x16 bitmap font otherwise (identical legacy metrics). The
buffer is premultiplied ARGB, so the color is scaled by the
surface alpha down in the font module."
  (font-ensure-initialized)
  (font-draw-text ptr width height text x y r g b
                  :alpha (bar-surface-alpha) :bold bold))

(defun bar-text-width (text &key bold)
  (font-text-width text :bold bold))

;;; --- Status icons (wifi, battery, volume) ---

(defun bar-icon-width ()
  "Width reserved for a status icon, including its text gap."
  20)

(defun bar-draw-battery-icon (ptr width height x y level charging r g b)
  "A 15x9 battery: outline, tip, fill proportional to LEVEL,
bolt when CHARGING."
  (let ((body-w 13) (body-h 9))
    (loop for dx below body-w
          do (bar-set-pixel ptr width height (+ x dx) y r g b)
             (bar-set-pixel ptr width height (+ x dx) (+ y body-h -1) r g b))
    (loop for dy below body-h
          do (bar-set-pixel ptr width height x (+ y dy) r g b))
    (loop for dy from 3 to 5
          do (bar-set-pixel ptr width height (+ x body-w) (+ y dy) r g b)
             (bar-set-pixel ptr width height (+ x body-w 1) (+ y dy) r g b))
    (let ((fill-w (min (- body-w 2)
                       (floor (* (max 0 (min 100 level)) (- body-w 2))
                              100))))
      (when (plusp fill-w)
        (loop for dy from 2 below (- body-h 2)
              do (loop for dx below fill-w
                       do (bar-set-pixel ptr width height
                                         (+ x 2 dx) (+ y dy 2) r g b)))))
    (when charging
      (loop for dy from 2 below (- body-h 2)
            do (bar-set-pixel ptr width height (+ x 6) (+ y dy) r g b)))))

(defun bar-draw-volume-icon (ptr width height x y muted r g b)
  "A 13x10 speaker with arcs, or a slash when MUTED."
  (loop for dy from 3 to 6
        do (bar-set-pixel ptr width height x (+ y dy) r g b)
           (bar-set-pixel ptr width height (+ x 1) (+ y dy) r g b))
  (loop for dx from 2 to 5
        do (let ((half (1+ (- dx 1))))
             (loop for dy from (- 4 half) to (+ 3 half)
                   do (bar-set-pixel ptr width height
                                     (+ x dx) (+ y 4 dy) r g b))))
  (if muted
      (loop for i from 0 to 5
            do (bar-set-pixel ptr width height (+ x 6 i) (+ y 2 i) r g b))
      (loop for (ar ax) in '((4 7) (6 9))
            do (loop for dy from (- ar) to ar
                     do (let ((dx (round (sqrt (- (* ar ar) (* dy dy))))))
                          (bar-set-pixel ptr width height
                                        (+ x ax) (+ y 4 dy) r g b))))))

(defun bar-draw-wifi-icon (ptr width height x y r g b)
  "A 12x10 wifi glyph: dot plus three ascending arcs."
  (loop for dy from 0 to 1
        do (loop for dx from 0 to 1
                 do (bar-set-pixel ptr width height
                                   (+ x 5 dx) (+ y 8 dy) r g b)))
  (loop for ar in '(3 6 9)
        do (loop for dx from (- ar) to ar
                 do (let ((dy (round (sqrt (- (* ar ar) (* dx dx))))))
                      (bar-set-pixel ptr width height
                                     (+ x 6 dx) (- (+ y 9) dy) r g b)))))

(defun bar-draw-ethernet-icon (ptr width height x y r g b)
  "A 10x8 ethernet jack: bracket with two prongs."
  (loop for dx from 0 to 9
        do (bar-set-pixel ptr width height (+ x dx) y r g b))
  (loop for dy from 0 to 7
        do (bar-set-pixel ptr width height x (+ y dy) r g b)
           (bar-set-pixel ptr width height (+ x 9) (+ y dy) r g b))
  (loop for dy from 4 to 7
        do (bar-set-pixel ptr width height (+ x 3) (+ y dy) r g b)
           (bar-set-pixel ptr width height (+ x 6) (+ y dy) r g b)))

(defun bar-draw-display-icon (ptr width height x y r g b)
  "A 14x10 monitor: screen outline, stand, base."
  ;; screen outline: top, bottom, left, right
  (loop for dx from 0 to 13
        do (bar-set-pixel ptr width height (+ x dx) y r g b)
           (bar-set-pixel ptr width height (+ x dx) (+ y 6) r g b))
  (loop for dy from 0 to 6
        do (bar-set-pixel ptr width height x (+ y dy) r g b)
           (bar-set-pixel ptr width height (+ x 13) (+ y dy) r g b))
  ;; stand
  (loop for dy from 7 to 8
        do (bar-set-pixel ptr width height (+ x 6) (+ y dy) r g b)
           (bar-set-pixel ptr width height (+ x 7) (+ y dy) r g b))
  ;; base
  (loop for dx from 4 to 9
        do (bar-set-pixel ptr width height (+ x dx) (+ y 9) r g b)))

(defun bar-draw-status-icon (ptr width height x y kind flags r g b)
  "Dispatch a status icon; vertically centered in the row."
  (let* ((row-h (font-line-height))
         (icon-h 10)
         (iy (+ y (floor (- row-h icon-h) 2))))
    (case kind
      (:battery (bar-draw-battery-icon
                  ptr width height x iy (getf flags :level 50)
                  (getf flags :charging) r g b))
      (:volume (bar-draw-volume-icon
                 ptr width height x iy (getf flags :muted) r g b))
      (:wifi (bar-draw-wifi-icon ptr width height x iy r g b))
      (:display (bar-draw-display-icon ptr width height x iy r g b))
      (:ethernet (bar-draw-ethernet-icon ptr width height x iy r g b)))))

(defun bar-draw-module (ptr width height x y parts base-r base-g base-b module)
  "Draw one module's PARTS starting at X, using the base color for
parts without an explicit color. Parts may carry :gap, :pill,
:bold, and :dot flags (see bar-module-parts). Records a click hitbox
for MODULE. Returns the x after the module plus gap."
  (let ((start x))
    (dolist (part parts)
      (let* ((text (first part))
             (color (second part))
             (flags (bar-part-flags part))
             (r base-r) (g base-g) (b base-b))
        (when color
          (setf (values r g b) (parse-hex-color color)))
        ;; Status icon before the text (wifi, battery, volume)
        (when (getf flags :icon)
          (bar-draw-status-icon ptr width height x y
                                (getf flags :icon) flags r g b)
          (incf x (bar-icon-width)))
        (bar-draw-text ptr width height text x y r g b
                       :bold (getf flags :bold))
        (let ((tw (bar-text-width text :bold (getf flags :bold))))
          ;; The occupied-workspace dot: 2x2, centred horizontally,
          ;; one pixel below the baseline, kept inside the line box.
          (when (getf flags :dot)
            (let ((dx0 (+ x (floor tw 2) -1))
                  (dy0 (min (+ y (font-ascent) 1)
                            (+ y (font-line-height) -2))))
              (dotimes (dy 2)
                (dotimes (dx 2)
                  (bar-set-pixel ptr width height (+ dx0 dx) (+ dy0 dy)
                                 r g b)))))
          ;; A workspace cell's click region: its drawn text plus
          ;; half the gap on either side
          (when (getf flags :cell)
            (let* ((gap (or (getf flags :gap) 0))
                   (half (floor gap 2)))
              (push (list (list :workspace-cell (getf flags :cell))
                          (- x half)
                          (+ x tw (- gap half)))
                    *bar-hitboxes*)))
          ;; The active-workspace marker: a rounded accent PILL
          ;; blended behind the text (radius 4, inset).
          (when (getf flags :pill)
            (let* ((px0 (- x 5))
                   (py0 (+ y 2))
                   (pw (+ tw 10))
                   (ph (- (font-line-height) 4))
                   (radius 4))
              (loop for dy below ph
                    for py = (+ py0 dy)
                    while (< py height) do
                (loop for dx below pw
                      for pxx = (+ px0 dx)
                      while (< pxx width) do
                  ;; rounded-rect containment test (radius on the
                  ;; vertical ends; full height in the middle)
                  (let* ((cx (cond ((< dx radius) radius)
                                   ((> dx (- pw radius 1)) (- pw radius 1))
                                   (t dx)))
                         (cy (cond ((< dy radius) radius)
                                   ((> dy (- ph radius 1)) (- ph radius 1))
                                   (t dy)))
                         (in-corner (or (and (< dx radius) (< dy radius))
                                       (and (< dx radius) (> dy (- ph radius 1)))
                                       (and (> dx (- pw radius 1)) (< dy radius))
                                       (and (> dx (- pw radius 1))
                                            (> dy (- ph radius 1)))))
                         (d (sqrt (+ (expt (- cx dx) 2) (expt (- cy dy) 2)))))
                    (when (and (or (not in-corner) (<= d radius))
                               (>= px0 0))
                      (blend-pixel ptr width height pxx py
                                   r g b :alpha 70)))))))
          (incf x (+ tw (or (getf flags :gap 0) 0))))))
    (let ((end x))
      (when (and module (> end start))
        (push (list module start end) *bar-hitboxes*)))
    (+ x *bar-module-gap*)))

(defun bar-draw-segment-item (ptr width height x y module fr fg fb br bg bb)
  "Draw one module of a bar segment (text or icons); returns the x
after it."
  (cond
    ((tray-module-p module)
     (bar-draw-tray ptr width height x y br bg bb))
    ((icons-module-p module)
     (bar-draw-icons ptr width height x y module br bg bb))
    (t (bar-draw-module ptr width height x y
                         (bar-module-parts module)
                         fr fg fb module))))

(defun %bar-render (state)
  "Render the bar content into the shm buffer and commit the surface."
  (let* ((ptr (gethash :shm-ptr state))
         (width (gethash :width state))
         (height (gethash :height state))
         (padding (max (config-getf '(:bar) :padding :default 6)
                       (+ (config-getf '(:bar) :corner-radius :default 10)
                          (config-getf '(:bar) :padding :default 6)))))
    (when (or (null ptr) (cffi:null-pointer-p ptr))
      (return-from %bar-render))
    (multiple-value-bind (br bg bb) (parse-hex-color
                                       (bar-background-color
                                        (bar-ostate-active-p)))
      (multiple-value-bind (fr fg fb) (theme-rgb :bar-foreground)
        (let ((margin (config-getf '(:bar) :margin :default 8)))
          (if (plusp (config-getf '(:bar) :corner-radius :default 10))
              (multiple-value-bind (rdr rdg rdb) (theme-rgb :bar-border)
                (paint-rounded-surface ptr width height br bg bb
                                        :alpha (bar-surface-alpha)
                                        :radius (config-getf '(:bar)
                                                  :corner-radius :default 10)
                                        :border-r rdr :border-g rdg
                                        :border-b rdb))
              ;; margin 0: the classic full-strip bar
              (bar-clear ptr width height br bg bb))
          (when (bar-ostate-active-p)
            (multiple-value-bind (ur ug ub) (theme-rgb :bar-active-underline)
              (bar-draw-active-underline
               ptr width height
               (max 0 (config-getf '(:bar) :corner-radius :default 10))
               ur ug ub (bar-surface-alpha)))))
        (let ((y (floor (- height (font-line-height)) 2))
              (*bar-hitboxes* nil))
          ;; Left segment
          (let ((x padding))
            (dolist (module (bar-ostate-segment-modules :left))
              (setf x (bar-draw-segment-item ptr width height x y module
                                            fr fg fb br bg bb))))
          ;; Center segment
          (let* ((modules (bar-ostate-segment-modules :center))
                 (total (bar-segment-width modules))
                 (x (floor (- width total) 2)))
            (dolist (module modules)
              (setf x (bar-draw-segment-item ptr width height x y module
                                            fr fg fb br bg bb))))
          ;; Right segment
          (let* ((modules (bar-ostate-segment-modules :right))
                 (total (bar-segment-width modules))
                 (x (- width padding total)))
            (dolist (module modules)
              (setf x (bar-draw-segment-item ptr width height x y module
                                            fr fg fb br bg bb))))
          ;; Hover: a subtle rounded accent fill under the pointer
          (when (and *bar-pointer-x*
                     (or (null *bar-pointer-ostate*)
                         (eq state *bar-pointer-ostate*))
                     (>= *bar-pointer-x* 0) (< *bar-pointer-x* width))
            (multiple-value-bind (ar ag ab) (theme-rgb :bar-hover)
              (dolist (hb *bar-hitboxes*)
                ;; workspace cells sit inside their module's box
                (when (and (not (and (consp (first hb))
                                     (eq (first (first hb)) :workspace-cell)))
                           (<= (second hb) *bar-pointer-x*)
                           (< *bar-pointer-x* (third hb)))
                  (blend-rounded-rect ptr width height
                                      (second hb) 2
                                      (- (third hb) (second hb)) (- height 4)
                                      ar ag ab :alpha 35 :radius 4)))))
          (setf (gethash :hitboxes state) *bar-hitboxes*))))
    ;; stamp the hovered module even when no veil was drawn, so the
    ;; tick loop re-renders when the pointer leaves the bar
    (setf *bar-hover-rendered* (bar-hover-key))
    (wl:wl-surface.attach (gethash :surface state) (gethash :wl-buffer state) 0 0)
    (wl:wl-surface.damage (gethash :surface state) 0 0 width height)
    (wl:wl-surface.commit (gethash :surface state))))

(defvar *bar-render-lock* (bt:make-lock "bar-render")
  "Serializes bar rendering. Two threads call bar-render: the main
tick loop and the dbus-thread redraws (tray registrations).
Concurrent renders on one shm buffer were always a latent race —
visual glitches and double commits — that the font engine turned
fatal (a freetype face corrupts under concurrent FT_Load_Char, see
font.lisp's *font-lock*).")

(defun bar-render (ostate)
  "Render ONE output's bar into its own buffer and commit it, as that
output's bar (see with-bar-ostate). Its hitboxes land in OSTATE."
  (bt:with-lock-held (*bar-render-lock*)
    (with-bar-ostate (ostate)
      (%bar-render ostate))))

(defun bar-render-all ()
  "Render every mapped output's bar, each into its own buffer with its
own hitboxes."
  (dolist (ostate (bar-ostates))
    (when (gethash :mapped ostate)
      (bar-render ostate))))

(defun bar-any-mapped-p ()
  "Whether any output's bar is mapped."
  (loop for o being the hash-values of *bar-output-states*
        thereis (gethash :mapped o)))

;;; --- Shm buffer management ---

(defun bar-destroy-shm (state)
  "Release the bar's shared memory resources."
  (when (gethash :wl-buffer state)
    (wl:wl-buffer.destroy (gethash :wl-buffer state)))
  (when (gethash :wl-pool state)
    (wl:wl-shm-pool.destroy (gethash :wl-pool state)))
  (when (and (gethash :shm-ptr state)
             (not (cffi:null-pointer-p (gethash :shm-ptr state))))
    (cffi:foreign-funcall "munmap"
      :pointer (gethash :shm-ptr state)
      :unsigned-long (gethash :shm-size state)
      :int)
    (cffi:foreign-funcall "close" :int (gethash :shm-fd state) :int))
  (setf (gethash :wl-buffer state) nil
        (gethash :wl-pool state) nil
        (gethash :shm-ptr state) nil
        (gethash :shm-fd state) nil
        (gethash :shm-size state) 0))

(defun bar-ensure-shm (state)
  "Create the shm buffer for STATE's current :width and :height,
releasing any buffer STATE already holds first (see bar-destroy-shm).
Return T, or NIL when the shm file or mapping cannot be created."
  (bar-destroy-shm state)
  (let* ((width (gethash :width state))
         (height (gethash :height state))
         (size (* width height 4))
         (filename (format nil "rhizome-bar-~A-~A"
                           (get-universal-time) (random 100000)))
         (path (concatenate 'string "/dev/shm/" filename))
         (fd (cffi:foreign-funcall "open"
               :string path
               :int (logior 2 64)             ; O_RDWR | O_CREAT
               :int 384                        ; 0600
               :int)))
    (when (< fd 0)
      (format t "~&; Rhizome: bar: failed to create shm file~%")
      (return-from bar-ensure-shm nil))
    (cffi:foreign-funcall "unlink" :string path :int)
    (cffi:foreign-funcall "ftruncate" :int fd :long size :int)
    (let ((ptr (cffi:foreign-funcall "mmap"
                  :pointer (cffi:null-pointer)
                  :unsigned-long size
                  :int 3                       ; PROT_READ | PROT_WRITE
                  :int 1                       ; MAP_SHARED
                  :int fd
                  :long 0
                  :pointer)))
      (when (cffi:null-pointer-p ptr)
        (cffi:foreign-funcall "close" :int fd :int)
        (return-from bar-ensure-shm nil))
      (setf (gethash :shm-fd state) fd
            (gethash :shm-ptr state) ptr
            (gethash :shm-size state) size)
      (setf (gethash :wl-pool state)
            (wl:wl-shm.create-pool (gethash :shm state) fd size))
      (setf (gethash :wl-buffer state)
            (wl:wl-shm-pool.create-buffer
              (gethash :wl-pool state) 0 width height (* width 4)
              :argb8888))                    ; premultiplied alpha
      t)))

;;; --- Connection lifecycle ---

(defun bar-bind-global (state registry name interface version)
  "Bind a registry global for the bar. Every wl_output joins STATE's
:outputs, its global name kept under :output-names."
  (let ((iface-class (wl:find-interface-named interface)))
    (when iface-class
      (cond
        ((string= interface "wl_compositor")
         ;; Cap at version 5: wl_surface v6 adds
         ;; preferred_buffer_scale/transform events the wayflan
         ;; bindings cannot demarshal, and one such event desyncs the
         ;; connection's whole event stream.
         (let ((v (min version 5)))
           (setf (gethash :compositor state)
                 (wl:wl-registry.bind registry name iface-class v))))
        ((string= interface "wl_shm")
         (setf (gethash :shm state)
               (wl:wl-registry.bind registry name iface-class version)))
        ((string= interface "zwlr_layer_shell_v1")
         (setf (gethash :layer-shell state)
               (wl:wl-registry.bind registry name iface-class version)))
        ((string= interface "wl_output")
         ;; per-output bars: bind EVERY output (hotplug arrivals come
         ;; through here too, the registry hook keeps firing)
         (unless (bar-output-named state name)
           (let ((output (wl:wl-registry.bind registry name iface-class
                                              version))
                 (names (or (gethash :output-names state)
                            (setf (gethash :output-names state)
                                  (make-hash-table)))))
             (setf (gethash output names) name)
             (pushnew output (gethash :outputs state)))))
        ((and (string= interface "wl_seat")
              (null (gethash :seat state)))
         (setf (gethash :seat state)
               (wl:wl-registry.bind registry name iface-class
                                    (min version 5))))))))

(defun bar-layer-surface-handler (state layer-surface event-name args)
  "Handle events on the bar's layer surface."
  (case event-name
    (:configure
     (destructuring-bind (serial width height) args
       (let ((w (if (plusp width) width 0))
             (h (config-getf '(:bar) :height :default 28)))
         (setf (gethash :width state) w
               (gethash :height state) h)
         (rwm:zwlr-layer-surface-v1.ack-configure layer-surface serial)
         (when (plusp w)
           (bar-ensure-shm state)
           (bar-render state)
           (setf (gethash :mapped state) t)))))
    (:closed
     ;; Only this output's bar goes; the others keep running. It
     ;; reopens after *bar-reopen-delay* while its output remains.
     (format t "~&; Rhizome: bar: compositor closed the bar on wl_output ~A~%"
             (gethash :global-name state))
     (setf (gethash (gethash :output state) *bar-closed-at*)
           (get-universal-time))
     (bar-close-surface state))))

;;; --- Change detection ---
;;;
;;; The bar polls for state changes (WM snapshot file, clock, cached
;;; module values) at the configured :interval and only re-renders the
;;; buffer when something actually changed, so a fast poll is cheap.

(defun bar-content-changed-p (ostate)
  "True when a module OSTATE's bar draws rendered differently, or its
active background changed, since OSTATE's last check. Records the new
content on OSTATE."
  (with-bar-ostate (ostate)
    (let* ((modules (append (bar-ostate-segment-modules :left)
                            (bar-ostate-segment-modules :center)
                            (bar-ostate-segment-modules :right)))
           (sig (with-output-to-string (s)
                  (prin1 (bar-ostate-active-p) s)
                  (terpri s)
                  (dolist (module modules)
                    (princ module s)
                    (princ " = " s)
                    (prin1 (bar-module-parts module) s)
                    (terpri s)))))
      (unless (equal sig (gethash :last-content ostate))
        (setf (gethash :last-content ostate) sig)
        t))))

;;; --- Pointer handling (clickable modules) ---

(defun bar-hit-at (x)
  "The hitbox (module start-x end-x) containing X on the pointer's
bar (see bar-pointer-hitboxes), or NIL."
  (find-if (lambda (hb)
             (and (<= (second hb) x)
                  (< x (third hb))))
           (bar-pointer-hitboxes)))

(defun bar-pointer-entry ()
  "The snapshot entry of the output under the pointer's bar, or NIL."
  (when *bar-pointer-ostate*
    (bar-output-entry (bar-snapshot)
                      (gethash :global-name *bar-pointer-ostate*))))

(defun bar-hover-module-now ()
  "The module under the pointer (or '%none), from the current
hitboxes and pointer position."
  (or (and *bar-pointer-x* (bar-module-at *bar-pointer-x*))
      '%none))

(defun bar-hover-key ()
  "What the hover veil depends on: the pointer's bar and the module
under the pointer. Compared with EQUAL against *bar-hover-rendered*,
so moving between two bars over modules of the same name re-renders."
  (list *bar-pointer-ostate* (bar-hover-module-now)))

(defun bar-module-at (x)
  "The module whose rendered area on the pointer's bar contains X, or NIL."
  (let ((hit (bar-hit-at x)))
    (when hit (first hit))))

(defun bar-run-command (command)
  "Run a shell COMMAND asynchronously."
  (handler-case
      (uiop:launch-program (list "bash" "-c" command))
    (error (e)
      (format t "~&; Rhizome: bar: command failed: ~A~%" e))))

(defun bar-module-action (module key)
  "The configured shell command for MODULE's action KEY
(:on-click, :on-scroll-up, :on-scroll-down), if any. Icons-module
entries carry their actions inline."
  (cond (;; an icons entry: (:icon "name" :on-click "command" ...)
         (and (listp module) (getf module :icon))
         (getf module key))
        (t (config-getf (list :modules module) key :default nil))))

(defun bar-handle-click (x)
  "Handle a left click at X: switch to a clicked workspace cell, act
on a tray item, or run the module's :on-click command."
  (let* ((hit (bar-hit-at x))
         (module (first hit))
         (cell (find-if (lambda (hb)
                          (and (consp (first hb))
                               (eq (first (first hb)) :workspace-cell)
                               (<= (second hb) x)
                               (< x (third hb))))
                        (bar-pointer-hitboxes))))
    (when (or module cell)
      (cond
        ;; Workspace cell: switch to it via IPC. Under
        ;; (:bar :per-output-content) on this bar's screen, by connector
        ;; name, else by wl_output global name; otherwise on the
        ;; current screen.
        (cell
         (destructuring-bind (number connector) (second (first cell))
           (let ((entry (bar-pointer-entry)))
             (format t "~&; Rhizome: bar: click workspace ~A~%" number)
             (bar-run-command
               (if (or connector
                       (not (bar-per-output-content-p))
                       (null (getf entry :wl-output)))
                   (format nil "rhizome-cmd switch-workspace ~A~@[ ~A~]"
                           number connector)
                   (format nil "rhizome-cmd \"(rhizome:bar-switch-workspace ~D ~D)\""
                           (- number (getf entry :index-base 1) -1)
                           (getf entry :wl-output)))))))
        ;; Tray item: Activate / SecondaryActivate on the item
        ((and (listp module) (getf module :name))
         (format t "~&; Rhizome: bar: tray click ~A~%" (getf module :name))
         (tray-click module))
        ;; The workspaces module outside any cell does nothing
        ((eq module :workspaces))
        (t
         (format t "~&; Rhizome: bar: click on ~A~%" module)
         (let ((command (bar-module-action module :on-click)))
           (when command
             (bar-run-command command))))))))

(defun bar-handle-right-click (x)
  "Handle a right click at X: a tray item opens its dbusmenu via
the WM's prompt machinery (IPC round-trip: the menu data lives
here, the prompt surfaces live there)."
  (let* ((hit (bar-hit-at x))
         (module (first hit)))
    (when (and (listp module) (getf module :name) (getf module :menu))
      (format t "~&; Rhizome: bar: tray menu ~A~%" (getf module :name))
      ;; the Menu property is (o . "/path") — an object-path variant
      (let ((menu-path (if (consp (getf module :menu))
                           (cdr (getf module :menu))
                           (getf module :menu))))
        ;; The click position rides along: the WM's pointer menu
        ;; opens right here (x = click, y = below the bar strip)
        (bar-run-command
          (format nil
                  "rhizome-cmd \"(rhizome:tray-show-menu \\\"~A\\\" \\\"~A\\\" ~D ~D)\""
                  (getf module :name) menu-path
                  (round x)
                  (round (config-getf '(:bar) :height :default 28))))))))

(defun bar-handle-scroll (amount)
  "Handle a vertical scroll gesture over a module."
  (let* ((module (and *bar-pointer-x* (bar-module-at *bar-pointer-x*)))
         (key (if (plusp amount) :on-scroll-up :on-scroll-down))
         (command (and module (bar-module-action module key))))
    (cond
      ;; Tray item: Scroll(delta, vertical) on the item
      ((and (listp module) (getf module :name))
       (tray-scroll module (if (plusp amount) 1 -1)))
      (command
       (bar-run-command command)))))

(defun bar-pointer-event (event-name args)
  "Handle wl_pointer events on the bar surface."
  (handler-case
      (case event-name
        (:enter
         ;; wayflan's arity here has varied (the serial may ride
         ;; along); take the LAST three args: surface x y. The surface
         ;; names the bar.
         (let* ((a (last args 3))
                (surface (first a))
                (x (second a)))
           (setf *bar-pointer-ostate*
                 (values (gethash surface *bar-surface-states*))
                 *bar-pointer-x* (and x (round x)))))
        (:leave
         (setf *bar-pointer-ostate* nil
               *bar-pointer-x* nil))
        (:motion
         (destructuring-bind (time x y) args
           (declare (ignore time y))
           (setf *bar-pointer-x* (round x))))
        (:button
         (destructuring-bind (serial time button state) args
           (declare (ignore serial time))
           ;; 272 = BTN_LEFT, 273 = BTN_RIGHT
           (when (eq state :pressed)
             (let ((x *bar-pointer-x*))
               (when x
                 (cond
                   ((= button 272) (bar-handle-click x))
                   ((= button 273) (bar-handle-right-click x))))))))
        (:axis
         (destructuring-bind (time axis value) args
           (declare (ignore time))
           (when (eq axis :vertical-scroll)
             (incf *bar-axis-accumulator* value))))
        (:axis-stop
         ;; Gesture ended: act on the accumulated scroll if it was
         ;; significant, then reset.
         (let ((amount *bar-axis-accumulator*))
           (setf *bar-axis-accumulator* 0)
           (when (>= (abs amount) 10)
             (bar-handle-scroll amount))))
        (t nil))
    (error (e)
      (declare (ignore e))
      nil)))

(defun bar-setup-pointer (state)
  "Set up a wl_pointer on the bar's seat for clickable modules."
  (let ((seat (gethash :seat state)))
    (when seat
      (push (lambda (event-name &rest args)
              (when (eq event-name :capabilities)
                (destructuring-bind (caps) args
                  (when (and (member :pointer caps)
                             (null (gethash :pointer state)))
                    (let ((pointer (wl:wl-seat.get-pointer seat)))
                      (setf (gethash :pointer state) pointer)
                      (push (lambda (event-name &rest args)
                              (bar-pointer-event event-name args))
                            (wl:wl-proxy-hooks pointer)))))))
            (wl:wl-proxy-hooks seat)))))

(defun bar-open-surface (state output)
  "Create the bar's layer surface on OUTPUT: anchored to the top,
inset by (:bar :margin), with an exclusive zone of twice the margin
plus the height. The compositor's configure event maps it. The output
gets its own ostate (surface, shm buffer, hitboxes), registered in
*bar-output-states* and *bar-surface-states*. Returns the ostate."
  (let* ((ostate (make-hash-table))
         (surface (wl:wl-compositor.create-surface
                    (gethash :compositor state)))
         (layer-surface
           (rwm:zwlr-layer-shell-v1.get-layer-surface
             (gethash :layer-shell state) surface
             output :top "rhizome-bar"))
         (margin (config-getf '(:bar) :margin :default 8))
         (height (config-getf '(:bar) :height :default 28)))
    (setf (gethash :output ostate) output
          (gethash :global-name ostate) (bar-output-global-name state output)
          (gethash :shm ostate) (gethash :shm state)
          (gethash :height ostate) height
          (gethash :surface ostate) surface
          (gethash :layer-surface ostate) layer-surface)
    (setf (gethash output *bar-output-states*) ostate
          (gethash surface *bar-surface-states*) ostate)
    (push (lambda (event-name &rest args)
            (bar-layer-surface-handler ostate layer-surface
                                       event-name args))
          (wl:wl-proxy-hooks layer-surface))
    (rwm:zwlr-layer-surface-v1.set-anchor layer-surface '(:top :left :right))
    (rwm:zwlr-layer-surface-v1.set-margin layer-surface margin margin 0 margin)
    (rwm:zwlr-layer-surface-v1.set-exclusive-zone layer-surface
                                                  (+ (* 2 margin) height))
    (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity layer-surface :none)
    (rwm:zwlr-layer-surface-v1.set-size layer-surface 0 height)
    (wl:wl-surface.commit surface)
    ostate))

(defun bar-close-surface (ostate)
  "Destroy one output's bar: layer surface, wl_surface and shm buffer,
and forget OSTATE. River drops that output's exclusive zone with the
surface."
  (when (gethash :layer-surface ostate)
    (rwm:zwlr-layer-surface-v1.destroy (gethash :layer-surface ostate)))
  (when (gethash :surface ostate)
    (wl:wl-surface.destroy (gethash :surface ostate)))
  (bar-destroy-shm ostate)
  (when (eq *bar-pointer-ostate* ostate)
    (setf *bar-pointer-ostate* nil *bar-pointer-x* nil))
  (remhash (gethash :output ostate) *bar-output-states*)
  (remhash (gethash :surface ostate) *bar-surface-states*)
  (setf (gethash :layer-surface ostate) nil
        (gethash :surface ostate) nil
        (gethash :mapped ostate) nil
        (gethash :last-content ostate) nil))

(defun bar-sync-visibility (state)
  "Open or close each bound output's bar in STATE to match the WM
snapshot (see bar-output-wanted-p): by default a bar on every output,
or none while the bar is hidden."
  (let ((snapshot (bar-snapshot)))
    (dolist (output (gethash :outputs state))
      (let ((want (bar-output-wanted-p state output snapshot))
            (ostate (gethash output *bar-output-states*)))
        (cond ((and ostate (not want))
               (bar-close-surface ostate))
              ((and want (null ostate))
               (bar-open-surface state output)))))))

(defvar *bar-applied-settings* nil
  "The :settings snapshot entry the bar process last applied.")

(defun bar-geometry ()
  "The :bar settings that size the layer surface: (HEIGHT MARGIN)."
  (list (config-getf '(:bar) :height :default 28)
        (config-getf '(:bar) :margin :default 8)))

(defun bar-adopt-settings (state settings)
  "Apply SETTINGS, the :settings entry of the WM snapshot (see
BAR-PUSHED-SETTINGS), in the bar process: replace the :bar, :modules,
and :theme config groups, draw from the pushed theme, re-resolve the
font, and render every bar again. Reopens each open bar's surface
when the height or margin changed. Returns T when SETTINGS differ from
the last applied, NIL when they match or are NIL."
  (when (and settings (not (equal settings *bar-applied-settings*)))
    (let ((geometry (bar-geometry))
          (config (copy-list *config*))
          (theme (getf settings :theme-table))
          (name (getf (getf settings :theme) :name)))
      (setf (getf config :bar) (getf settings :bar)
            (getf config :modules) (getf settings :modules)
            (getf config :theme) (getf settings :theme))
      (setf *config* config
            *bar-applied-settings* settings)
      (clear-theme-cache)
      (when (and (stringp name)
                 (listp theme)
                 (theme-table-p (getf theme :roles))
                 (theme-table-p (getf theme :elements)))
        (bt:with-recursive-lock-held (*theme-cache-lock*)
          (setf (gethash name *theme-cache*) theme)))
      (clrhash *bar-cache*)
      (font-apply-config)
      (let ((reopen (not (equal geometry (bar-geometry)))))
        (dolist (ostate (bar-ostates))
          (setf (gethash :last-content ostate) nil)
          (when reopen
            (let ((output (gethash :output ostate)))
              (bar-close-surface ostate)
              (bar-open-surface state output)))))
      t)))

(defun bar-run-loop (state)
  "Open the bar's Wayland connection and drive its tick loop. Each
tick applies the settings the WM pushed in its snapshot, then opens or
closes each output's bar to follow the snapshot and re-renders the bars
whose content changed, every bar when the tray changed or the pointer
moved onto another module."
  (flet ((safe-roundtrip (display)
           ;; Wayflan raises on events it cannot decode (e.g. wl_shm
           ;; advertises non-standard fourcc formats); skip and carry on.
           (handler-case
               (wl:wl-display-roundtrip display)
             (error (e)
               (format t "~&; Rhizome: bar: ignored event error: ~A~%" e))))
         (safe-dispatch (display)
           (handler-case
               (wl:wl-display-dispatch-event display)
             (error (e)
               (format t "~&; Rhizome: bar: ignored event error: ~A~%" e))))
         (settled-p ()
           (every (lambda (ostate) (gethash :mapped ostate))
                  (bar-ostates))))
    (handler-case
        (wl:with-open-display (display)
          (setf (gethash :display state) display)
          (let ((registry (wl:wl-display.get-registry display)))
            (push (lambda (event-name &rest args)
                    (case event-name
                      (:global
                       (destructuring-bind (name interface version) args
                         (bar-bind-global state registry name interface version)))
                      ((:global-remove :global_remove)
                       (bar-forget-output state (first args)))))
                  (wl:wl-proxy-hooks registry))
            (safe-roundtrip display)
            (unless (and (gethash :compositor state)
                         (gethash :shm state)
                         (gethash :layer-shell state))
              (format t "~&; Rhizome: bar: missing compositor/shm/layer-shell globals~%")
              (return-from bar-run-loop))
            ;; Pointer input for clickable modules
            (bar-setup-pointer state)
            (bar-adopt-settings state (getf (bar-snapshot) :settings))
            (bar-sync-visibility state)
            (loop for i below 100
                  until (or (settled-p) (not *bar-running*))
                  do (safe-roundtrip display)
                     (sleep 0.05))
            (loop while *bar-running*
                  do (bar-adopt-settings state
                                         (getf (bar-snapshot) :settings))
                     (bar-sync-visibility state)
                     (let ((tray-was *tray-changed*))
                       (tray-drain)
                       (when (/= tray-was *tray-changed*)
                         ;; new/removed items: invalidate content
                         (dolist (ostate (bar-ostates))
                           (setf (gethash :last-content ostate) nil))))
                     ;; pointer motion alone re-renders: the hover veil
                     ;; follows the module under the pointer
                     (let ((tray-dirty (/= *tray-render-stamp* *tray-changed*))
                           (hover-moved (not (equal (bar-hover-key)
                                                    *bar-hover-rendered*))))
                       (dolist (ostate (bar-ostates))
                         (when (and (gethash :mapped ostate)
                                    (or (bar-content-changed-p ostate)
                                        tray-dirty
                                        hover-moved))
                           (bar-render ostate)))
                       (setf *tray-render-stamp* *tray-changed*))
                     (loop while (and *bar-running*
                                      (wl:wl-display-listen display))
                           do (safe-dispatch display))
                     (sleep (config-getf '(:bar) :interval :default 0.15)))
            (dolist (ostate (bar-ostates))
              (bar-close-surface ostate))))
      (error (e)
        (format t "~&; Rhizome: bar thread error: ~A~%" e)))))

(defparameter *bar-process-mode* nil
  "T in the bar process itself (set by run-bar BEFORE anything else).
Guards all bar process spawning: the bar loads the user config at
startup, and any (rhizome:configure ...) in it must never reach
bar-apply-config — the bar applying config used to spawn another bar,
which loaded the config and spawned another: exponential bars on
screen. Only the WM process ever manages bar processes.")

(defun run-bar ()
  "Entry point for the standalone bar process. Loads the user config,
then runs the bar's connection loop on this process's main thread.
Runs in *bar-process-mode*: this process never spawns or stops bars,
no matter what the user config asks for."
  (setf *bar-process-mode* t)
  (setf *bar-running* t)
  ;; the WM's environment, so the WM's directories; only the WM moves
  ;; files
  (resolve-base-directories)
  (load-user-config)
  (font-ensure-initialized)
  ;; The tray lives here: this process owns the StatusNotifierWatcher
  ;; bus name, serves registrations, and renders the items.
  (handler-case (tray-start)
    (error (e)
      (format t "~&; Rhizome: tray: startup failed: ~A~%" e)))
  (let ((state (make-hash-table)))
    (setf *bar-state* state)
    (bar-run-loop state)
    (setf *bar-state* nil)
    (format t "~&; Rhizome: bar loop ended~%")))

;;; --- Process lifecycle (the bar runs as its own process) ---

(defvar *bar-process* nil
  "The running bar process, a uiop:process object. DEFVAR: reloading
  bar.lisp must not orphan a running bar (bar-reserved-height no
  longer depends on this, but toggle-bar still uses it).")

(defvar *bar-start-lock* (bt:make-lock "bar-start")
  "Serializes start-bar's alive check and launch: the supervisor
thread and the main thread both call it.")

(defvar *bar-starting* nil
  "True while start-bar is launching a bar process.")

(defun bar-alive-p ()
  "Whether a bar process is running or being launched — our own
child, or one adopted from a previous WM."
  (or *bar-starting*
      (and *bar-process*
           (uiop:process-alive-p *bar-process*))
      (and *adopted-bar-pid*
           (external-pid-alive-p *adopted-bar-pid*))))

(defun bar-log-path ()
  "Pathname of the bar process's log file, in the cache directory."
  (merge-pathnames #P"bar.log" (cache-directory)))

(defun start-bar ()
  "Start the bar as its own process (no-op if one is running or being
launched). The WM process only, and it first reaps any orphan bars
left behind by past incidents (they would stack on screen)."
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from start-bar))
  (when *bar-process-mode*
    (return-from start-bar))
  (bt:with-lock-held (*bar-start-lock*)
    (when (bar-alive-p)
      (return-from start-bar))
    ;; A surviving bar (detach, crash recovery) is adopted rather
    ;; than cold-started: a fresh SBCL is the 40-second black strip
    (let ((pids (external-bar-pids)))
      (when pids
        ;; the adopted bar takes this WM's settings from the snapshot
        (setf *adopted-bar-pid* (first pids))
        (reap-extra-bars *adopted-bar-pid*)
        (format t "~&; Rhizome: bar adopted (pid ~A)~%" *adopted-bar-pid*)
        (return-from start-bar)))
    (setf *bar-starting* t)
    (unwind-protect
         (progn
           ;; Reap orphan bar processes: exact-match the run-bar eval,
           ;; never the WM (its arg is (rhizome:run))
           (ignore-errors
             (uiop:run-program '("pkill" "-f" "rhizome:run-bar")
                               :ignore-error-status t))
           (ensure-directories-exist (bar-log-path))
           (with-open-file (s (bar-log-path) :direction :output
                              :if-exists :supersede))
           (setf *bar-process*
                 (uiop:launch-program
                   (list "sbcl"
                         "--non-interactive"
                         "--eval" "(ql:quickload :rhizome)"
                         "--eval" "(rhizome:run-bar)")
                   :output (bar-log-path)
                   :error-output :output))
           (format t "~&; Rhizome: bar started~%"))
      (setf *bar-starting* nil))))

(defun stop-bar ()
  "Stop the bar process. Kills tracked AND orphan bars (the latter
left by past incidents); WM process only."
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from stop-bar))
  (when *bar-process-mode*
    (return-from stop-bar))
  (when (and *bar-process* (uiop:process-alive-p *bar-process*))
    (handler-case
        (uiop:terminate-process *bar-process*)
      (error (e)
        (format t "~&; Rhizome: error stopping bar: ~A~%" e))))
  (when *bar-process*
    (ignore-errors (uiop:wait-process *bar-process*)))
  ;; Reap orphans too (exact-match, never the WM)
  (ignore-errors
    (uiop:run-program '("pkill" "-f" "rhizome:run-bar")
                      :ignore-error-status t))
  (setf *bar-process* nil
        *adopted-bar-pid* nil)
  (format t "~&; Rhizome: bar stopped~%"))

(defvar *adopted-bar-pid* nil
  "PID of a bar process this WM found already running at startup and
adopted instead of launching a fresh one. A WM restart (detach, crash
recovery) leaves the bar running — the successor adopts it, and the
~40-second cold-start black strip (#9) never happens.")

(defun external-bar-pids ()
  "PIDs of running bar processes this WM did not launch, oldest
first. Exact-matches the run-bar eval, never the WM (its arg is
(rhizome:run))."
  (handler-case
      (let* ((out (uiop:run-program '("pgrep" "-f" "rhizome:run-bar")
                                    :output :string
                                    :ignore-error-status t))
             (lines (uiop:split-string out :separator '(#\newline))))
        (loop for line in lines
              for trimmed = (string-trim " " line)
              for pid = (and (plusp (length trimmed))
                             (parse-integer trimmed :junk-allowed t))
              when pid collect pid))
    (error () nil)))

(defun external-pid-alive-p (pid)
  "Whether the process PID still exists (/proc/PID)."
  (and pid
       (probe-file (make-pathname
                     :directory (list :absolute
                                      "proc" (princ-to-string pid))))
       t))

(defun reap-extra-bars (keep-pid)
  "Kill every running bar except KEEP-PID (extras stack on screen)."
  (dolist (pid (external-bar-pids))
    (unless (= pid keep-pid)
      (ignore-errors
        (uiop:run-program (list "kill" (princ-to-string pid))
                          :ignore-error-status t)))))

(defvar *bar-non-exclusive-heights* (make-hash-table)
  "Maps each output to the height of its non-exclusive area, as river
last reported it.")

(defun bar-output-selected-p (output)
  "True when the config puts a bar on OUTPUT. (:bar :outputs) is :all
(the default), :primary (the leftmost output), or a list of 0-based
indices into the outputs in left-to-right order. NIL while the bar is
disabled."
  (and (config-getf '(:bar) :enabled :default t)
       (let ((spec (config-getf '(:bar) :outputs :default :all))
             (sorted (all-outputs-sorted)))
         (cond ((eq spec :primary) (eq output (first sorted)))
               ((consp spec)
                (let ((index (position output sorted :test #'eq)))
                  (and index (member index spec) t)))
               (t t)))))

(defun bar-primary-output ()
  "The leftmost output carrying a bar: the primary bar (see
bar-ostate-segment-modules). NIL when no output carries one."
  (find-if #'bar-output-selected-p (all-outputs-sorted)))

(defun bar-zone-shown-p (&optional output)
  "True when river reports an exclusive zone at least as tall as the
bar on OUTPUT, or has reported no area for OUTPUT. Without OUTPUT:
on some output, or no area reported at all."
  (let ((bar-height (config-getf '(:bar) :height :default 28)))
    (if output
        (let ((height (gethash output *bar-non-exclusive-heights*)))
          (or (null height)
              (>= (- (output-height output) height) bar-height)))
        (or (zerop (hash-table-count *bar-non-exclusive-heights*))
            (loop for out being the hash-keys of *bar-non-exclusive-heights*
                    using (hash-value height)
                  thereis (>= (- (output-height out) height) bar-height))))))

(defun bar-reserved-height (&optional output)
  "Vertical pixels the layout leaves for the bar on OUTPUT: margin
above + height + margin below, so windows tile with the same gap under
the floating bar as above it. 0 when OUTPUT carries no bar (see
bar-output-selected-p), or when river reports an area for OUTPUT but
no exclusive zone at least as tall as the bar (hidden or still
starting). Without OUTPUT, the same test applies to any output. The
WM reserves this itself because it positions windows explicitly."
  (if (and (config-getf '(:bar) :enabled :default t)
           (if output
               (and (bar-output-selected-p output)
                    (bar-zone-shown-p output))
               (bar-zone-shown-p)))
      (+ (* 2 (config-getf '(:bar) :margin :default 8))
         (config-getf '(:bar) :height :default 28))
      0))

(defun bar-reservation (&optional output)
  "The reserved bar space of every output, of OUTPUT, and the
output-free value, as one list to compare across a change."
  (list* (bar-reserved-height)
         (and output (bar-reserved-height output))
         (mapcar #'bar-reserved-height (all-outputs))))

(defun bar-retile ()
  "Rescale every output's frame trees to the current reserved bar
space and request a manage sequence."
  (dolist (out (all-outputs))
    (refit-output-frame-trees out))
  (when *wm-proxy*
    (request-manage-dirty)))

(defun bar-note-non-exclusive-area (output height)
  "Record river's non-exclusive area HEIGHT for OUTPUT, or forget
OUTPUT when HEIGHT is NIL. Re-tile when the reserved bar space
changes."
  (let ((before (bar-reservation output)))
    (if height
        (setf (gethash output *bar-non-exclusive-heights*) height)
        (remhash output *bar-non-exclusive-heights*))
    (unless (equal before (bar-reservation output))
      (bar-retile))))

(defun toggle-bar ()
  "Hide or show the status bar without restarting its process.
Persists the intent into (:bar :enabled) so the supervisor does not
restart a hidden bar fifteen seconds later.
bar-apply-config passes the intent to the bar and re-tiles."
  (when *bar-process-mode*
    (return-from toggle-bar))
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from toggle-bar))
  (configure (list :bar (list :enabled
                              (not (config-getf '(:bar) :enabled
                                                :default t))))))

(defvar *bar-supervisor* nil
  "The WM's bar supervisor thread. The bar's deaths were silent and
manual for far too long.")

(defun start-bar-supervisor ()
  "Keep the bar alive (WM process only, idempotent): every 15
seconds, when the config wants a bar and none is alive, start one
and say why. Honors a deliberate toggle-off because toggle-bar
persists the intent in (:bar :enabled)."
  (when (or *bar-process-mode* *bar-supervisor*)
    (return-from start-bar-supervisor))
  (setf *bar-supervisor*
        (bt:make-thread
          (lambda ()
            (loop
              (handler-case
                  (progn
                    (sleep 15)
                    (when (and (config-getf '(:bar) :enabled :default t)
                               (not (bar-alive-p)))
                      (format t "~&; Rhizome: bar supervisor: bar is down, restarting~%")
                      (start-bar)))
                (error () nil))))
          :name "rhizome-bar-supervisor")))

(defun bar-apply-config ()
  "Apply the current config to the bar. A running bar process, launched
or adopted, keeps running: it reads the :bar, :modules, and :theme
settings and the active theme from the WM snapshot written here (see
BAR-PUSHED-SETTINGS), and :bar-hidden when the bar is disabled.
Enabling starts a bar when none is alive. WM-process only: any other
process, the bar process (see *bar-process-mode*) or a test run
included, must never start or stop bars, because stop-bar's pkill
would reap the user's live bar."
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from bar-apply-config))
  (when *bar-process-mode*
    (return-from bar-apply-config))
  (let ((before (bar-reservation)))
    (when (and (config-getf '(:bar) :enabled :default t)
               (not (bar-alive-p)))
      (start-bar))
    (when *wm-proxy*
      (update-wm-snapshot))
    (unless (equal before (bar-reservation))
      (bar-retile))))

(defun bar-switch-workspace (workspace &optional wl-output-name)
  "Switch the output whose wl_output global name is WL-OUTPUT-NAME to
WORKSPACE; the current output when no output has that name. The bar
sends this when a workspace number is clicked on an output's bar."
  (let ((output (or (and wl-output-name
                         (find-output-by-wl-name wl-output-name))
                    (current-output))))
    (when output
      (switch-workspace workspace output))))
