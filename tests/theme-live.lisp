;;; Tests for live theme switching and for the settings the WM pushes
;;; to the running bar process: set-theme from Lisp, the theme
;;; commands over IPC, the redraw request a theme change makes, the
;;; decoration redraw on a colour change, the :settings entry in the
;;; WM snapshot, the bar adopting pushed settings, and bar-apply-config
;;; keeping a running bar instead of restarting it. River requests and
;;; process launches are stubbed; no bar process starts.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: configures below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(defparameter *fixture-theme-directory*
  (asdf:system-relative-pathname :rhizome "tests/fixtures/themes/"))

(let ((pass 0) (fail 0)
      (saved-config *config*)
      (saved-search-path *theme-search-path*))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (unwind-protect
         (handler-bind ((warning #'muffle-warning))
           (setf *theme-search-path*
                 (list *fixture-theme-directory* *shipped-theme-directory*))
           (clear-theme-cache)

           ;; --- available themes ---
           (ok "available-theme-names lists shipped and fixture themes"
               (available-theme-names)
               '("deepsky-moonunit" "ef-summer" "kusanagi"
                 "modus-operandi" "modus-vivendi" "test-fixture-light"
                 "tokyo-night" "tokyo-night-emacs"))

           ;; --- set-theme from Lisp ---
           (ok "default theme draws the tokyo-night background"
               (theme-color :bar-background) "#1a1b26")
           (ok "set-theme returns the name it switched to"
               (set-theme "test-fixture-light") "test-fixture-light")
           (ok "set-theme sets (:theme :name)"
               (config-getf '(:theme) :name) "test-fixture-light")
           (ok "set-theme keeps the font settings"
               (config-getf '(:theme) :font-family) "monospace")
           (ok "elements draw from the new theme"
               (list (theme-color :bar-background)
                     (theme-color :border-focused)
                     (theme-color :launcher-match))
               '("#f4f1e8" "#b3005f" "#b3005f"))
           (ok "set-theme refuses a theme it cannot find"
               (set-theme "no-such-theme") nil)
           (ok "a refused switch leaves the theme selected"
               (config-getf '(:theme) :name) "test-fixture-light")
           (ok "set-theme refuses a name that leaves the theme directories"
               (set-theme "../tokyo-night") nil)
           (ok "set-theme switches back"
               (set-theme "tokyo-night") "tokyo-night")
           (ok "back on the tokyo-night background"
               (theme-color :bar-background) "#1a1b26")

           ;; --- set-theme reads an edited theme file again ---
           (let ((cached (load-theme "test-fixture-light")))
             (set-theme "test-fixture-light")
             (ok "set-theme reloads the theme file it selects"
                 (eq cached (load-theme "test-fixture-light")) nil))
           (set-theme "tokyo-night")

           ;; --- commands and IPC ---
           (ok "set-theme, select-theme, and list-themes are commands"
               (mapcar (lambda (k) (and (gethash k *command-table*) t))
                       '(:set-theme :select-theme :list-themes))
               '(t t t))
           (let ((reply (ipc-dispatch-line "set-theme test-fixture-light")))
             (ok "IPC set-theme switches the theme"
                 (config-getf '(:theme) :name) "test-fixture-light")
             (ok "IPC set-theme replies with the theme"
                 reply "theme test-fixture-light"))
           (ok "IPC set-theme names the choices for an unknown theme"
               (ipc-dispatch-line "set-theme no-such-theme")
               (format nil "no theme no-such-theme (themes: ~A)"
                       "deepsky-moonunit ef-summer kusanagi modus-operandi modus-vivendi test-fixture-light tokyo-night tokyo-night-emacs"))
           (ok "IPC set-theme with no name gives its usage"
               (ipc-dispatch-line "set-theme")
               (format nil "usage: set-theme NAME (themes: ~A)"
                       "deepsky-moonunit ef-summer kusanagi modus-operandi modus-vivendi test-fixture-light tokyo-night tokyo-night-emacs"))
           (ok "IPC list-themes marks the current theme"
               (ipc-dispatch-line "list-themes")
               "deepsky-moonunit ef-summer kusanagi modus-operandi modus-vivendi test-fixture-light* tokyo-night tokyo-night-emacs")
           (ok "an IPC Lisp form switches the theme"
               (progn (ipc-dispatch-line "(set-theme \"tokyo-night\")")
                      (config-getf '(:theme) :name))
               "tokyo-night")
           (ok "configure with (:theme NAME) still switches"
               (progn (configure '(:theme "test-fixture-light"))
                      (theme-color :bar-background))
               "#f4f1e8")
           (set-theme "tokyo-night")

           ;; --- select-theme opens a picker of the themes ---
           (let ((opened nil)
                 (real-open (symbol-function 'prompt-open)))
             (setf (symbol-function 'prompt-open)
                   (lambda (title &key candidates on-complete initial)
                     (declare (ignore initial))
                     (setf opened (list title candidates))
                     (funcall on-complete "test-fixture-light")))
             (unwind-protect
                  (funcall (gethash :select-theme *command-table*) nil nil)
               (setf (symbol-function 'prompt-open) real-open))
             (ok "select-theme offers every theme"
                 opened '("Theme:" ("deepsky-moonunit" "ef-summer" "kusanagi"
                             "modus-operandi" "modus-vivendi"
                             "test-fixture-light" "tokyo-night"
                             "tokyo-night-emacs")))
             (ok "choosing in the picker switches the theme"
                 (config-getf '(:theme) :name) "test-fixture-light"))
           (set-theme "tokyo-night")

           ;; --- a theme change requests a redraw ---
           (let ((dirty 0)
                 (real-dirty (symbol-function 'request-manage-dirty)))
             (setf (symbol-function 'request-manage-dirty)
                   (lambda () (incf dirty)))
             (unwind-protect
                  (let ((*wm-proxy* :fake-proxy))
                    (theme-apply-config)
                    (setf dirty 0)
                    (theme-apply-config)
                    (ok "no redraw while the theme is unchanged" dirty 0)
                    (set-theme "test-fixture-light")
                    (ok "a theme switch requests a manage sequence"
                        (plusp dirty) t)
                    ;; other components may request one per configure:
                    ;; count the theme's own request directly
                    (setf dirty 0
                          *config* (merge-plist
                                    *config*
                                    '(:theme (:roles (:accent "#00ff00")))))
                    (theme-apply-config)
                    (ok "a role override requests one manage sequence"
                        dirty 1)
                    (setf dirty 0
                          *config* (merge-plist *config*
                                                '(:gaps (:inner 7))))
                    (theme-apply-config)
                    (ok "an unrelated change requests no redraw"
                        dirty 0))
               (setf (symbol-function 'request-manage-dirty) real-dirty)))
           (setf *config* saved-config)
           (clear-theme-cache)

           ;; --- decorations redraw when their colours change ---
           (let ((state (list :w 100 :h 80 :focused t
                              :colour (list 1 2 3 "#1e1e24"))))
             (ok "decoration current when nothing changed"
                 (decoration-needs-redraw-p state 100 80 t
                                            (list 1 2 3 "#1e1e24"))
                 nil)
             (ok "decoration redraws on a tint change"
                 (decoration-needs-redraw-p state 100 80 t
                                            (list 9 2 3 "#1e1e24"))
                 t)
             (ok "decoration redraws on a shadow change"
                 (decoration-needs-redraw-p state 100 80 t
                                            (list 1 2 3 "#000000"))
                 t)
             (ok "decoration redraws on a size change"
                 (decoration-needs-redraw-p state 120 80 t
                                            (list 1 2 3 "#1e1e24"))
                 t)
             (ok "decoration redraws on a focus change"
                 (decoration-needs-redraw-p state 100 80 nil
                                            (list 1 2 3 "#1e1e24"))
                 t))

           ;; --- the snapshot carries the bar's settings ---
           (let* ((snapshot (wm-snapshot-plist))
                  (settings (getf snapshot :settings)))
             (ok "snapshot carries the :bar group"
                 (getf settings :bar) (config-get '(:bar)))
             (ok "snapshot carries the :modules group"
                 (getf settings :modules) (config-get '(:modules)))
             (ok "snapshot carries the :theme group"
                 (getf settings :theme) (config-get '(:theme)))
             (ok "snapshot carries the active theme's table"
                 (getf settings :theme-table) (active-theme))
             (ok "the snapshot prints readably"
                 (handler-case
                     (let ((*print-readably* t))
                       (and (read-from-string (prin1-to-string snapshot)) t))
                   (error () :unreadable))
                 t))
           (configure '(:bar (:modules (:left (:clock)))))
           (ok "a :bar change reaches the snapshot"
               (getf (getf (getf (getf (wm-snapshot-plist) :settings) :bar)
                           :modules)
                     :left)
               '(:clock))
           (set-theme "test-fixture-light")
           (ok "a theme change reaches the snapshot"
               (getf (getf (getf (getf (wm-snapshot-plist) :settings)
                                 :theme-table)
                           :roles)
                     :background)
               "#f4f1e8")
           (configure (list :bar (list :on-click-hook (lambda () nil))))
           (ok "unprintable settings leave the snapshot readable"
               (handler-case
                   (let ((*print-readably* t))
                     (and (read-from-string
                           (prin1-to-string (wm-snapshot-plist)))
                          t))
                 (error () :unreadable))
               t)
           (ok "unprintable settings are not pushed"
               (getf (wm-snapshot-plist) :settings) nil)
           (setf *config* saved-config)
           (clear-theme-cache)

           ;; --- the bar adopts pushed settings ---
           (let* ((pushed-theme
                    (list :format 1 :name "pushed-only"
                          :roles (merge-plist (getf *default-theme* :roles)
                                              '(:background "#123456"))
                          :elements nil))
                  (settings
                    (list :bar (merge-plist (config-get '(:bar))
                                            '(:height 30 :modules
                                              (:left (:clock))))
                          :modules (config-get '(:modules))
                          :theme (merge-plist (config-get '(:theme))
                                              '(:name "pushed-only"))
                          :theme-table pushed-theme))
                  (state (make-hash-table))
                  (closed 0) (opened 0) (fonts 0)
                  (real-close (symbol-function 'bar-close-surface))
                  (real-open (symbol-function 'bar-open-surface))
                  (real-font (symbol-function 'font-apply-config)))
             (setf (gethash :surface state) :fake-surface
                   (gethash :height state) 20)
             (setf (symbol-function 'bar-close-surface)
                   (lambda (s) (incf closed) (setf (gethash :surface s) nil))
                   (symbol-function 'bar-open-surface)
                   (lambda (s) (incf opened)
                     (setf (gethash :surface s) :fake-surface))
                   (symbol-function 'font-apply-config)
                   (lambda () (incf fonts)))
             (unwind-protect
                  (let ((*bar-applied-settings* nil)
                        (*bar-last-content* "rendered"))
                    (ok "the bar applies new settings"
                        (bar-adopt-settings state settings) t)
                    (ok "the bar's :bar group follows the push"
                        (list (config-getf '(:bar) :height)
                              (config-getf '(:bar :modules) :left))
                        '(30 (:clock)))
                    (ok "the bar draws from the pushed theme"
                        (theme-color :bar-background) "#123456")
                    (ok "the bar re-renders after a push"
                        *bar-last-content* nil)
                    (ok "a height change reopens the bar surface"
                        (list closed opened) '(1 1))
                    (ok "the bar re-resolves its font after a push"
                        fonts 1)
                    (setf *bar-last-content* "rendered")
                    (ok "the same settings apply once"
                        (bar-adopt-settings state settings) nil)
                    (ok "an unchanged push renders nothing new"
                        *bar-last-content* "rendered")
                    (ok "a snapshot without settings changes nothing"
                        (bar-adopt-settings state nil) nil)
                    (let ((colour-only
                            (list :bar (getf settings :bar)
                                  :modules (getf settings :modules)
                                  :theme (getf settings :theme)
                                  :theme-table
                                  (list :format 1 :name "pushed-only"
                                        :roles (merge-plist
                                                (getf pushed-theme :roles)
                                                '(:background "#654321"))
                                        :elements nil))))
                      (setf closed 0 opened 0)
                      (ok "a pushed colour change applies"
                          (bar-adopt-settings state colour-only) t)
                      (ok "the bar draws the pushed colour"
                          (theme-color :bar-background) "#654321")
                      (ok "a colour change keeps the bar surface"
                          (list closed opened) '(0 0))))
               (setf (symbol-function 'bar-close-surface) real-close
                     (symbol-function 'bar-open-surface) real-open
                     (symbol-function 'font-apply-config) real-font)))
           (setf *config* saved-config)
           (clear-theme-cache)

           ;; --- bar-apply-config keeps a running bar ---
           (let ((stops 0) (starts 0)
                 (real-stop (symbol-function 'stop-bar))
                 (real-start (symbol-function 'start-bar)))
             (setf (symbol-function 'stop-bar) (lambda () (incf stops))
                   (symbol-function 'start-bar) (lambda () (incf starts)))
             (unwind-protect
                  (let ((*wm-process-mode* t)
                        (*bar-process-mode* nil)
                        (*adopted-bar-pid* 1)
                        (*wm-proxy* nil))
                    (configure '(:bar (:enabled t)))
                    (configure '(:bar (:height 31 :modules
                                       (:left (:layout)))))
                    (configure '(:theme "test-fixture-light"))
                    (ok "a :bar or theme change never restarts a running bar"
                        (list stops starts) '(0 0))
                    (setf *adopted-bar-pid* nil)
                    (configure '(:bar (:height 32)))
                    (ok "a bar that is down still starts"
                        (list stops starts) '(0 1)))
               (setf (symbol-function 'stop-bar) real-stop
                     (symbol-function 'start-bar) real-start))))
      (setf *config* saved-config
            *theme-search-path* saved-search-path)
      (clear-theme-cache)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
