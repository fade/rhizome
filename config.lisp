;;; Rhizome configuration
;;;
;;; Configuration is a nested plist stored in *config*. Defaults are
;;; defined by DEFAULT-CONFIG; user overrides live in
;;; ~/.config/rhizome/config.lisp, which typically contains calls to
;;; (rhizome:configure '(...)). Configuration can be changed live from
;;; the REPL - the bar and other consumers pick changes up immediately.

(in-package #:rhizome)

;;; --- Config data structure helpers ---

(defun config-plist-p (list)
  "True when LIST is a non-empty property list: even length, with a
keyword at every key position. Values may be anything, keywords
included."
  (and (listp list)
       (plusp (length list))
       (evenp (length list))
       (every #'keywordp (loop for x on list by #'cddr collect (first x)))))

(defun keyword-list-p (list)
  "True when every element of LIST is a keyword."
  (every #'keywordp list))

(defun merge-plist (base override &key (groups-p t))
  "Merge OVERRIDE into BASE key by key and return a new plist. BASE is
not modified.

A value present on both sides merges recursively when both are
config plists (CONFIG-PLIST-P) and either
  - GROUPS-P is true: the values are groups, as at the top level of
    *CONFIG*, so a group merges whatever its values are; or
  - either side holds a value that is not a keyword.
Otherwise the OVERRIDE value replaces the BASE value. Below the top
level, a list of bare keywords is a value, not a group: a bar module
list such as (:layout :windows) or a preference list such as
(:empty :focused) replaces wholesale. A list of plists, such as
:window-rules, is never a config plist and also replaces."
  (loop with merged = (copy-list base)
        for (key value) on override by #'cddr
        do (let ((existing (getf merged key)))
             (setf (getf merged key)
                   (if (and (config-plist-p existing)
                            (config-plist-p value)
                            (or groups-p
                                (not (keyword-list-p existing))
                                (not (keyword-list-p value))))
                       (merge-plist existing value :groups-p nil)
                       value)))
        finally (return merged)))

(defun config-get (path &key (config *config*))
  "Look up a config value by a list of keys, e.g. (:bar :height).
Returns (values value found-p)."
  (labels ((walk (keys plist)
             (let ((value (getf plist (first keys) '%missing)))
               (cond
                 ((eq value '%missing) (values nil nil))
                 ((rest keys)
                  (if (listp value)
                      (walk (rest keys) value)
                      (values nil nil)))
                 (t (values value t))))))
    (walk path config)))

(defun config-getf (path key &key (config *config*) (default nil default-p))
  "Convenience: get KEY inside the config group at PATH."
  (multiple-value-bind (group foundp)
      (config-get path :config config)
    (if foundp
        (multiple-value-bind (value vfoundp)
            (getf group key '%missing)
          (if (eq value '%missing)
              (if default-p default nil)
              (values value vfoundp)))
        (if default-p default nil))))

(defun parse-hex-color (string)
  "Parse \"#1a1b26\" into (values r g b)."
  (let ((clean (string-trim '(#\#) string)))
    (values
      (parse-integer clean :start 0 :end 2 :radix 16)
      (parse-integer clean :start 2 :end 4 :radix 16)
      (parse-integer clean :start 4 :end 6 :radix 16))))

;;; --- Defaults ---

(defun default-config ()
  "Return the default configuration plist."
  '(:bar (:enabled t
          :height 20
          :interval 0.15
          :padding 6
          :opacity 0.9
          :background "#1a1b26"
          :foreground "#c0caf5"
          :accent "#7aa2f7"
          :muted "#565f89"
          :modules (:left (:workspaces :layout :windows)
                    :center (:clock)
                    :right (:tray :network :volume :battery)))
    :modules (:clock (:format "%a %d %b %H:%M")
              :battery (:interval 5)
              :volume (:interval 5
                       :on-click "pavucontrol"
                       :on-scroll-up "wpctl set-volume -l 1.0 @DEFAULT_AUDIO_SINK@ 5%+"
                       :on-scroll-down "wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-")
              :network (:interval 10
                        :on-click "nm-connection-editor"))
    :window-rules ()
    :workspaces (:types nil
                 ;; (2 :floating 5 :scroll) — per-workspace GROUP
                 ;; TYPES: :tile (frame trees, default), :floating
                 ;; (StumpWM's floating group), :scroll (the
                 ;; ribbon). Also set-workspace-type /
                 ;; cycle-workspace-type (C-t l)
                 :default-layout :master-stack
                 ;; the layout untouched workspaces use;
                 ;; switch-layout stores per-workspace
                 )
    :gaps (:inner 6
           ;; Pixels of breathing room around each tiled window.
           ;; Adjacent windows show twice this between them, screen
           ;; edges once. 0 restores the classic flush tiling.
           )
    :animations (:enabled t
                 :duration-ms 200
                 ;; :easing :ease-out-cubic (default) | :linear |
                 ;; :ease-in-out — window glide: workspace slide-ins,
                 ;; expose gliding into the grid, float/maximize
                 :easing :ease-out-cubic)
    :window-state (:enabled t
                   ;; Remember identifier-based window placements
                   ;; across WM restarts (river v4 identifiers)
                   )
    :output (:presentation-mode :vsync
             ;; :async for tearing/lowest latency on high-refresh
             ;; panels; a render-sequence request (river v4)
             )
    ;; :warp-on-switch t warps the pointer to the output center on
    ;; workspace switch (river_seat_v1.pointer_warp); set it nil if
    ;; pointer jumps bother you. xcursor theme/size default to the
    ;; XCURSOR_THEME/XCURSOR_SIZE the session exports.
    :pointer (:warp-on-switch t)
    :theme (:font-family "monospace"
            :font-size 14
            :focused-border "#7aa2f7"
            :unfocused-border "#404040")
    :launcher (:background "#23142d"
               :opacity 0.9
               :icon-size 20
               :accent "#7aa2f7"
               :prompt-color "#8c64aa"
               :query-color "#dcc8f0"
               :cursor-color "#b4a0d2"
               :selection-background "#503278"
               :selected-color "#f0e6ff"
               :entry-color "#aa96c8")
    :ipc (:enabled t)
    :swank (:enabled t
            :port 4005)))

(defun theme-border-rgb (focused-p)
  "Focused/unfocused window border color as (values r g b), from
(:theme :focused-border/:unfocused-border)."
  (parse-hex-color
    (config-getf '(:theme)
                 (if focused-p :focused-border :unfocused-border)
                 :default (if focused-p "#7aa2f7" "#404040"))))

(defvar *config* (default-config)
  "The current rhizome configuration.")

;;; --- Public config API ---

(defun configure (plist)
  "Merge PLIST into *config* and apply it. For example:

  (rhizome:configure '(:bar (:modules (:left (:layout :windows)
                                          :center (:clock)
                                          :right (:battery))
                           :height 24)))
  (rhizome:configure '(:bar (:enabled nil)))   ; disable the bar

Restarting running components (e.g. the bar) is handled automatically."
  (setf *config* (merge-plist *config* plist))
  (apply-config)
  *config*)

(defun apply-config ()
  "Apply the current configuration to running components. Never
runs component restarts inside the bar process (the bar loading the
user config would otherwise spawn bars — see *bar-process-mode*)."
  (unless (and (boundp '*bar-process-mode*) *bar-process-mode*)
    (when (fboundp 'bar-apply-config)
      (funcall (symbol-function 'bar-apply-config)))
    (when (fboundp 'font-apply-config)
      (funcall (symbol-function 'font-apply-config)))
    (when (fboundp 'output-apply-config)
      (funcall (symbol-function 'output-apply-config)))))

(defun config-pathname ()
  "Pathname of the user config file."
  (merge-pathnames #P".config/rhizome/config.lisp"
                   (user-homedir-pathname)))

;;; --- Modifier keys (define once, use many) ---
;;;
;;; River's xkb bindings take modifier lists; these variables name the
;;; common ones so bindings read as intent, not bitmasks. *mod* is THE
;;; user modifier — every direct binding (MOD+Return terminal,
;;; MOD+1..9 workspaces, MOD+drag window moves, ...) routes through
;;; it. Default Super (:mod4); change at runtime with
;;; (rhizome:set-user-mod :alt) or :super.

(defparameter *mod-none*  '(:none)
  "No modifier.")
(defparameter *mod-shift* '(:shift)
  "Shift.")
(defparameter *mod-ctrl*  '(:ctrl)
  "Control (the C-t prefix chord).")
(defparameter *mod-alt*   '(:mod1)
  "Alt.")
(defparameter *mod-super* '(:mod4)
  "Super / Logo / Windows key.")

(defparameter *mod* *mod-super*
  "The user modifier: what direct keybindings and pointer grabs
(MOD+drag moves a window) are bound to. Defaults to Super; set to
*mod-alt* for the classic StumpWM-on-X11 Alt feel.")

(defun resolve-mod (mod)
  "Map a friendly modifier name (:alt, :super, :ctrl) to its modifier
list, or NIL if unknown."
  (case mod
    ((:alt :mod1) *mod-alt*)
    ((:super :logo :windows :mod4) *mod-super*)
    ((:ctrl :control) *mod-ctrl*)
    (t nil)))

;;; --- Fuzzy matching (prompt + launcher filtering) ---

(defun fuzzy-match-p (query candidate)
  "Whether QUERY's characters appear in CANDIDATE in order,
case-insensitively — a subsequence match that subsumes substring
matching (\"ff\" matches \"Firefox\", \"xf\" does not)."
  (loop with i = 0
        for ch across query
        for pos = (position ch candidate :start i :test #'char-equal)
        unless pos
          return nil
        do (setf i (1+ pos))
        finally (return t)))

(defun fuzzy-filter (query candidates)
  "Candidates matching QUERY, substring matches first, fuzzy-only
matches after; original order preserved within each group. An empty
query returns the candidates unchanged."
  (if (zerop (length query))
      candidates
      (let ((direct '()) (fuzzy '()))
        (dolist (c candidates)
          (cond ((null c))
                ((search query c :test #'char-equal) (push c direct))
                ((fuzzy-match-p query c) (push c fuzzy))))
        (nconc (nreverse direct) (nreverse fuzzy)))))

(defun load-user-config ()
  "Load the user's config file if it exists."
  (let ((path (config-pathname)))
    (when (probe-file path)
      (format t "~&; Rhizome: loading config from ~A~%" path)
      (handler-case
          (load path)
        (error (e)
          (format t "~&; Rhizome: error loading config: ~A~%" e))))))

(defun reload-config ()
  "Reset to defaults, reload the user config file, and re-apply."
  (setf *config* (default-config))
  (load-user-config)
  (apply-config)
  *config*)
