;;; Tests for the colours of the active-screen bars, the active-screen
;;; edge line, frame outlines, fselect labels, float titlebars, and the
;;; dialog notice: each reads a theme element, the default theme draws
;;; the colours these surfaces had before themes, a theme switch
;;; recolours every one of them, and their old colour keys still work
;;; with a deprecation warning while their on and off switches draw
;;; none.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: configures below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(defun site-warnings (thunk)
  "Call THUNK with the once-per-process warning record cleared; return
the text of every warning it signals."
  (clrhash *warned-colour-keys*)
  (let ((texts '()))
    (handler-bind ((warning (lambda (w)
                              (push (princ-to-string w) texts)
                              (muffle-warning w))))
      (funcall thunk))
    (nreverse texts)))

(defun site-defaults ()
  "Reset *config* to the defaults with the bar off and forget loaded themes."
  (setf *config* (merge-plist (default-config) '(:bar (:enabled nil))))
  (clear-theme-cache))

(defun site-colours ()
  "Every ported site's colour under the current config, as a plist."
  (list :bar-active (bar-background-color t)
        :bar-inactive (bar-background-color nil)
        :edge (multiple-value-bind (edge width r g b) (active-edge-style)
                (declare (ignore edge width))
                (list r g b))
        :outline (multiple-value-list (frame-outline-style nil))
        :outline-empty (multiple-value-list (frame-outline-style t))
        :fselect (multiple-value-list (fselect-label-colours))
        :titlebar-focused (multiple-value-list (titlebar-colours t))
        :titlebar-unfocused (multiple-value-list (titlebar-colours nil))
        :notice (theme-color :dialog-notice)
        :message (theme-color :message-text)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; --- the default theme draws the colours from before themes ---

    (site-defaults)
    (let ((colours (site-colours)))
      (ok "active bar background" (getf colours :bar-active) "#392e7a")
      (ok "inactive bar background" (getf colours :bar-inactive) "#1a1b26")
      (ok "active-screen edge line" (getf colours :edge) '(122 162 247))
      (ok "focused frame outline" (getf colours :outline) '(122 162 247 2))
      (ok "focused empty frame outline" (getf colours :outline-empty)
          '(255 158 100 1))
      (ok "fselect label: border-blue fill, dark text on it"
          (getf colours :fselect) '(122 162 247 26 27 38))
      (ok "focused titlebar: border-blue fill, dark text on it"
          (getf colours :titlebar-focused) '(122 162 247 26 27 38))
      (ok "unfocused titlebar: grey fill, light text on it"
          (getf colours :titlebar-unfocused) '(64 64 64 192 202 245))
      (ok "dialog notice" (getf colours :notice) "#ff5fd7")
      (ok "message text" (getf colours :message) "#7aa2f7"))
    (ok "the default config sets no deprecated colour key"
        (deprecated-colour-keys-in (default-config)) nil)
    (ok "the default config sets no colour string anywhere"
        (labels ((walk (x)
                   (cond ((stringp x) (and (theme-colour-string-p x) (list x)))
                         ((consp x) (append (walk (car x)) (walk (cdr x))))
                         (t nil))))
          (walk (default-config)))
        nil)

    ;; --- a config that sets only fonts and switches warns about nothing ---

    (site-defaults)
    (ok "fonts, pointer, and frame settings: no warning"
        (site-warnings
          (lambda ()
            (configure '(:theme (:font-family "EnvyCodeR Nerd Font"
                                 :font-size 13)))
            (configure '(:frames (:focus-empty t)))
            (configure '(:pointer (:focus-policy :sloppy)))))
        nil)
    (ok "on and off switches in old colour keys: no warning"
        (site-warnings
          (lambda ()
            (configure '(:theme (:frame-outline t :frame-outline-empty nil)
                         :screens (:active-edge-color t)))))
        nil)
    (ok ":frame-outline-empty nil gives an empty frame the outline colour"
        (multiple-value-list (frame-outline-style t)) '(122 162 247 1))
    (configure '(:theme (:frame-outline nil)))
    (ok ":frame-outline nil turns the outline off"
        (multiple-value-list (frame-outline-style nil)) '(nil))

    ;; --- old colour keys still colour their site, and warn once ---

    (site-defaults)
    (let ((warnings
            (site-warnings
              (lambda ()
                (configure '(:bar (:background-active "#444444"
                                   :background-inactive "#111111")
                             :screens (:active-edge-color "#ffffff")
                             :theme (:frame-outline "#00ff00"
                                     :frame-outline-empty "#ff0000")
                             :dialogs (:notify-color "#0000ff")))))))
      (ok "each old colour key warns once" (length warnings) 6)
      (ok "the warning names the element"
          (and (find-if (lambda (w) (search ":screen-active-edge" w)) warnings) t)
          t))
    (let ((colours (site-colours)))
      (ok "old key: active bar background" (getf colours :bar-active) "#444444")
      (ok "old key: inactive bar background" (getf colours :bar-inactive) "#111111")
      (ok "old key: edge line" (getf colours :edge) '(255 255 255))
      (ok "old key: frame outline" (getf colours :outline) '(0 255 0 2))
      (ok "old key: empty frame outline" (getf colours :outline-empty)
          '(255 0 0 1))
      (ok "old key: fselect labels follow the outline"
          (subseq (getf colours :fselect) 0 3) '(0 255 0))
      (ok "old key: dialog notice" (getf colours :notice) "#0000ff"))

    ;; --- an element override beats the old key ---

    (configure '(:theme (:elements (:bar-background-active "#123456"))))
    (ok "element override wins over the old key"
        (bar-background-color t) "#123456")

    ;; --- a theme switch recolours every site ---

    (site-defaults)
    (let ((before (site-colours)))
      (ok "modus-operandi is available"
          (and (member "modus-operandi" (available-theme-names)
                       :test #'string=)
               t)
          t)
      (ok "set-theme modus-operandi" (set-theme "modus-operandi")
          "modus-operandi")
      (let ((after (site-colours)))
        (ok "modus-operandi: active bar takes the selection colour"
            (getf after :bar-active) "#c0deff")
        (ok "modus-operandi: inactive bar takes the bar background"
            (getf after :bar-inactive) "#ffffff")
        (ok "modus-operandi: edge line takes the accent"
            (getf after :edge) '(53 72 207))
        (ok "modus-operandi: outline takes the accent"
            (getf after :outline) '(53 72 207 2))
        (ok "modus-operandi: empty outline takes the warning colour"
            (getf after :outline-empty) '(136 73 0 1))
        (ok "modus-operandi: unfocused titlebar takes the unfocus colour"
            (subseq (getf after :titlebar-unfocused) 0 3) '(159 159 159))
        (ok "modus-operandi: message text takes the accent"
            (getf after :message) "#3548cf")
        (ok "every site except the dialog notice changes colour"
            (loop for (key value) on before by #'cddr
                  unless (equal value (getf after key))
                    collect key)
            '(:bar-active :bar-inactive :edge :outline :outline-empty
              :fselect :titlebar-focused :titlebar-unfocused :message))
        (ok "no converted theme maps the notice role: it keeps the default"
            (getf after :notice) "#ff5fd7")))
    (site-defaults))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
