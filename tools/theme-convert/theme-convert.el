;;; theme-convert.el --- Convert an Emacs colour theme to a rhizome theme  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Brian O'Reilly <fade@deepsky.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Commentary:

;; A developer tool, run once per theme; its output is committed under
;; themes/.  Rhizome never runs it.
;;
;;   emacs -Q --batch -l tools/theme-convert/theme-convert.el \
;;         RECIPE THEME-DIR [EXTRA-LOAD-DIR ...]
;;
;; RECIPE is a file under tools/theme-convert/recipes/ holding one plist:
;; the Emacs theme to load, the rhizome theme name, the header lines
;; (origin, authors, licence), and optional settings, corrections, and
;; notice text.  THEME-DIR holds the Emacs theme; each EXTRA-LOAD-DIR
;; holds a package it requires (modus-themes for the Modus derivatives).
;;
;; Environment:
;;   TC_OUT              directory for NAME.theme (default: the current
;;                       directory)
;;   TC_REPORT           directory for NAME.report.txt, the per-pair and
;;                       per-role detail (default: TC_OUT)
;;   TC_RHIZOME          the rhizome source tree, read for its role,
;;                       element, and state cue tables (default: two
;;                       directories above this file)
;;   TC_ALLOW_HUE_ONLY   when set, write a theme that fails the hue-only
;;                       check instead of refusing it, flagged in its header
;;
;; Faces are resolved inside Emacs, with inheritance, palette
;; indirection, and the default face applied, against one fixed display
;; description: a 24-bit graphic display with the theme's own
;; background mode.  Each rhizome role takes the first candidate face
;; the theme styles; a role no styled face fills follows another role
;; and is reported.  Contrast is reported, never used to choose.
;;
;; Exit status: 0 on success, 2 when the theme fails the hue-only check
;; (no file written), 1 on any other error.

;;; Code:

(require 'cl-lib)
(require 'color)

;;;; Display description

(defvar tc-display-colors 16777216)
(defvar tc-display-types '(graphic pgtk x))
(defvar tc-background 'dark)

(defun tc-match-display (display &optional _frame)
  "Match DISPLAY, a defface display spec, against the fixed description."
  (or (eq display t)
      (cl-every
       (lambda (conjunct)
         (let ((req (car conjunct)) (opts (cdr conjunct)))
           (pcase req
             ('type (cl-some (lambda (o) (memq o tc-display-types)) opts))
             ('class (memq 'color opts))
             ('min-colors (>= tc-display-colors (car opts)))
             ('background (memq tc-background opts))
             ('supports t)
             (_ nil))))
       display)))

(advice-add 'face-spec-set-match-display :override #'tc-match-display)

;;;; Colour arithmetic

(defvar tc-colour-corrections nil
  "Alist of (WRITTEN . READ-AS) colour strings from the recipe.")

(defun tc-hex (colour)
  "COLOUR as \"#rrggbb\", or nil when it is unspecified or unknown."
  (let ((colour (or (cdr (assoc colour tc-colour-corrections)) colour)))
    (cond
     ((not (stringp colour)) nil)
     ((string-prefix-p "unspecified" colour) nil)
     ((string-match "\\`#\\([[:xdigit:]]+\\)\\'" colour)
      (let* ((digits (match-string 1 colour))
             (n (/ (length digits) 3)))
        (when (and (memq n '(1 2 3 4)) (= (* n 3) (length digits)))
          (apply #'format "#%02x%02x%02x"
                 (cl-loop for i below 3
                          for v = (string-to-number
                                   (substring digits (* i n) (* (1+ i) n)) 16)
                          collect (round (* 255.0 (/ v (float (1- (expt 16 n)))))))))))
     (t (let ((entry (cl-find (downcase (replace-regexp-in-string " " "" colour))
                              color-name-rgb-alist
                              :key (lambda (e) (downcase (replace-regexp-in-string " " "" (car e))))
                              :test #'string=)))
          (when entry
            (apply #'format "#%02x%02x%02x"
                   (mapcar (lambda (v) (round (* 255.0 (/ v 65535.0)))) (cdr entry)))))))))

(defun tc-luminance (hex)
  "WCAG relative luminance of HEX."
  (let ((rgb (mapcar (lambda (i)
                       (/ (string-to-number (substring hex (1+ (* i 2)) (+ 3 (* i 2))) 16)
                          255.0))
                     '(0 1 2))))
    (cl-destructuring-bind (r g b)
        (mapcar (lambda (c) (if (<= c 0.04045) (/ c 12.92) (expt (/ (+ c 0.055) 1.055) 2.4)))
                rgb)
      (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)))))

(defun tc-contrast (a b)
  "WCAG contrast ratio of hex colours A and B, or nil."
  (when (and a b)
    (let ((la (tc-luminance a)) (lb (tc-luminance b)))
      (/ (+ (max la lb) 0.05) (+ (min la lb) 0.05)))))

;;;; Face resolution

(defun tc-attr (face attr)
  "ATTR of FACE, through inheritance and the default face, or nil."
  (let ((v (face-attribute face attr nil 'default)))
    (if (eq v 'unspecified) nil v)))

(defun tc-own-attr (face attr)
  "ATTR as stated on FACE itself, or nil."
  (let ((v (face-attribute face attr nil nil)))
    (if (eq v 'unspecified) nil v)))

(defun tc-inherit-chain (face &optional seen)
  "The faces FACE inherits from, depth first, cycle safe."
  (let ((inh (tc-own-attr face :inherit)))
    (cl-loop for f in (if (listp inh) inh (list inh))
             when (and (symbolp f) (facep f) (not (memq f seen)))
             append (cons f (tc-inherit-chain f (cons f seen))))))

(defun tc-styled-p (face)
  "Non-nil when an enabled theme or a recipe setting styles FACE or a
face it inherits from."
  (cl-some (lambda (f) (or (get f 'theme-face) (get f 'tc-set)))
           (cons face (tc-inherit-chain face))))

(defun tc-resolve (face)
  "Plist of FACE's resolved :fg, :bg, :box colour, and :cued flag."
  (when (facep face)
    (let* ((inverse (eq (tc-attr face :inverse-video) t))
           (fg (tc-hex (tc-attr face :foreground)))
           (bg (tc-hex (tc-attr face :background)))
           (eff-fg (if inverse bg fg))
           (eff-bg (if inverse fg bg))
           (box (tc-attr face :box))
           (box-colour (and (consp box) (tc-hex (plist-get box :color)))))
      (list :fg eff-fg
            :bg eff-bg
            ;; A box drawn in the face's own background is invisible.
            :box (and box-colour (not (equal box-colour eff-bg)) box-colour)
            :cued (or (memq (tc-attr face :weight)
                            '(bold semi-bold extra-bold ultra-bold heavy black))
                      (tc-attr face :underline)
                      (and box-colour (not (equal box-colour eff-bg))))))))

;;;; Rhizome tables, read as data from the rhizome source

(defun tc-read-defparameter (file name)
  "The quoted value of (defparameter NAME '...) in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (unless (search-forward (format "(defparameter %s" name) nil t)
      (error "%s not found in %s" name file))
    (let ((form (read (current-buffer))))
      (if (eq (car-safe form) 'quote) (cadr form) form))))

(defun tc-read-theme-file (file)
  "The plist in the rhizome theme FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

;;;; Roles

(defvar tc-roles
  '((:background base (default :bg))
    (:foreground base (default :fg))
    (:accent nil (link :fg) (minibuffer-prompt :fg) (font-lock-keyword-face :fg))
    (:muted nil (shadow :fg) (completions-annotations :fg) (font-lock-comment-face :fg))
    (:focus alias :accent)
    (:unfocus nil (vertical-border :fg) (window-divider :fg))
    (:hairline nil (vertical-border :fg) (window-divider :fg))
    (:surface nil (tab-bar :bg) (header-line :bg) (mode-line-inactive :bg))
    (:surface-foreground paired :surface)
    (:surface-border nil)
    (:selection nil (vertico-current :bg) (completions-highlight :bg)
                (icomplete-selected-match :bg) (region :bg))
    (:selection-foreground paired :selection)
    (:match nil (completions-common-part :fg) (orderless-match-face-0 :fg) (match :fg))
    (:error nil (error :fg) (compilation-error :fg))
    (:shadow nil))
  "Each rhizome role as (ROLE CONTEXT CANDIDATE...), CANDIDATE being
(FACE SLOT), SLOT one of :fg, :bg, or :box.  CONTEXT base accepts the
default face's own values; nil rejects a candidate equal to the
default face unless the face carries a cue; (paired ROLE) takes the
foreground of the face that filled ROLE; (alias ROLE) follows ROLE by
design, as rhizome's default theme does: the focused border is the
accent.")

(defvar tc-role-fallbacks
  '((:unfocus . :muted) (:hairline . :muted)
    (:surface . :background) (:surface-foreground . :foreground)
    (:surface-border . :hairline) (:selection-foreground . :foreground)
    (:match . :accent))
  "The role an unfilled role follows.  A role absent here takes the
default rhizome theme's value.")

(defvar tc-elements
  '((:launcher-prompt minibuffer-prompt :fg)
    (:prompt-title minibuffer-prompt :fg)
    (:launcher-cursor cursor :bg))
  "Element overrides taken from a face when the theme styles it.")

(defun tc-slot (r slot)
  (plist-get r slot))

(defun tc-pick-roles ()
  "Each role as (ROLE VALUE SOURCE), SOURCE a face, (follows ROLE), or
(default)."
  (let ((default (tc-resolve 'default))
        (picks nil)
        (filled nil))
    (dolist (row tc-roles)
      (cl-destructuring-bind (role context . candidates) row
        (let ((picked nil))
          (cond
           ((eq context 'alias)
            (setq picked (list role (car candidates) (list 'alias (car candidates)))))
           ((eq context 'paired)
            (let* ((face (cdr (assq (car candidates) filled)))
                   (r (and face (tc-resolve face))))
              (when (and r (plist-get r :fg))
                (setq picked (list role (plist-get r :fg) face)))))
           (t
            (dolist (c candidates)
              (unless picked
                (let* ((face (car c)) (slot (cadr c))
                       (r (tc-resolve face))
                       (v (and r (tc-slot r slot))))
                  (when (and v
                             (or (eq context 'base) (tc-styled-p face))
                             (or (eq context 'base)
                                 (plist-get r :cued)
                                 (not (equal v (tc-slot default (if (eq slot :box) :fg slot))))))
                    (setq picked (list role v face))
                    (push (cons role face) filled)))))))
          (push (or picked
                    (let ((fb (cdr (assq role tc-role-fallbacks))))
                      (if fb (list role fb (list 'follows fb))
                        (list role nil '(default)))))
                picks))))
    (nreverse picks)))

(defun tc-pick-elements ()
  "Element overrides as (ELEMENT VALUE FACE)."
  (cl-loop for (element face slot) in tc-elements
           for r = (and (tc-styled-p face) (tc-resolve face))
           for v = (and r (tc-slot r slot))
           when v collect (list element v face)))

;;;; Resolution as rhizome does it

(defun tc-rz-colour (name theme element-table default-roles &optional depth)
  "NAME's colour under THEME, as rhizome resolves it."
  (let ((depth (or depth 16)))
    (when (> depth 0)
      (let ((v (or (plist-get (plist-get theme :elements) name)
                   (cadr (assq name element-table))
                   (plist-get (plist-get theme :roles) name)
                   (plist-get default-roles name))))
        (if (keywordp v)
            (tc-rz-colour v theme element-table default-roles (1- depth))
          v)))))

(defun tc-hue-only-pairs (cues colour-of)
  "Each state pair in CUES as (STATE AGAINST COLOURS NON-HUE RATIO).
NON-HUE lists the non-hue attributes whose values differ."
  (cl-loop for (state . plist) in cues
           for elements = (plist-get plist :elements)
           for colours = (mapcar colour-of elements)
           collect (list state (plist-get plist :against) colours
                         (cl-loop for (attr a b) in (plist-get plist :cues)
                                  unless (equal a b) collect attr)
                         (apply #'tc-contrast colours))))

;;;; Main

(defun tc-git-version (dir)
  "A version string for the source checkout DIR, or nil."
  (let ((default-directory (file-name-as-directory dir)))
    (ignore-errors
      (car (process-lines "git" "describe" "--tags" "--always")))))

(defun tc-format-ratio (r)
  (format "%.1f:1" r))

(defun tc-main ()
  (let* ((args command-line-args-left)
         (recipe-file (expand-file-name (pop args)))
         (theme-dir (expand-file-name (pop args)))
         (extra (mapcar #'expand-file-name args))
         (here (file-name-directory (or load-file-name buffer-file-name)))
         (rhizome (file-name-as-directory
                   (or (getenv "TC_RHIZOME") (expand-file-name "../.." here))))
         (out (file-name-as-directory (or (getenv "TC_OUT") default-directory)))
         (recipe (with-temp-buffer (insert-file-contents recipe-file)
                                   (read (current-buffer))))
         (name (plist-get recipe :name))
         (emacs-theme (plist-get recipe :emacs-theme))
         (role-table (tc-read-defparameter (concat rhizome "theme.lisp") "*theme-roles*"))
         (element-table (tc-read-defparameter (concat rhizome "theme.lisp") "*theme-elements*"))
         (cues (tc-read-defparameter (concat rhizome "cues.lisp") "*state-cues*"))
         (default-theme (tc-read-theme-file (concat rhizome "themes/tokyo-night.theme")))
         (default-roles (plist-get default-theme :roles))
         (correction-notes nil))
    (setq command-line-args-left nil)
    (dolist (d (reverse (cons theme-dir extra)))
      (push d load-path)
      (push d custom-theme-load-path))
    (setq tc-colour-corrections
          (cl-loop for c in (plist-get recipe :corrections)
                   when (plist-get c :colour) collect (plist-get c :colour)))
    (dolist (form (plist-get recipe :settings)) (eval form t))
    (load-theme emacs-theme t)
    (setq tc-background
          (or (plist-get recipe :background)
              (and (memq frame-background-mode '(dark light)) frame-background-mode)
              (let ((bg (tc-hex (face-attribute 'default :background))))
                (if (and bg (< (tc-luminance bg) 0.18)) 'dark 'light))))
    (dolist (f '(vertico-current orderless-match-face-0))
      (when (and (not (facep f)) (get f 'theme-face)) (make-empty-face f)))
    (dolist (f (face-list)) (face-spec-recalc f (selected-frame)))
    ;; Recipe settings that act after the theme loads, as the
    ;; operator's init does; the faces they set count as styled.
    (dolist (form (plist-get recipe :after-load))
      (eval form t)
      (when (and (eq (car-safe form) 'set-face-attribute)
                 (eq (car-safe (cadr form)) 'quote))
        (put (cadr (cadr form)) 'tc-set t)))
    ;; Attribute corrections: a misspelt keyword in a face spec read as
    ;; the keyword meant.
    (dolist (c (plist-get recipe :corrections))
      (let ((fix (plist-get c :face-attribute)))
        (when fix
          (cl-destructuring-bind (face written meant) fix
            (when (and (not (facep face)) (get face 'theme-face))
              (make-empty-face face))
            (let* ((spec (cadr (car (get face 'theme-face))))
                   (attrs (and spec (cadr (car spec))))
                   (v (and (listp attrs) (plist-get attrs written))))
              (when (and v (facep face))
                (set-face-attribute face nil meant v)))))))
    (dolist (c (plist-get recipe :corrections))
      (let ((check (plist-get c :check)))
        (push (list (plist-get c :text)
                    (if check (if (eval check t) "holds" "DOES NOT HOLD") "applied"))
              correction-notes)))
    (setq correction-notes (nreverse correction-notes))
    (let* ((picks (tc-pick-roles))
           (elements (tc-pick-elements))
           (roles-plist (cl-loop for (role v _src) in picks
                                 append (list role (or v (plist-get default-roles role)))))
           (elements-plist (cl-loop for (e v _f) in elements append (list e v)))
           (theme (list :roles roles-plist :elements elements-plist))
           (colour-of (lambda (n) (tc-rz-colour n theme element-table default-roles)))
           (pairs (tc-hue-only-pairs cues colour-of))
           (hue-only (cl-remove-if #'cadddr pairs))
           (unmapped (cl-loop for (role v src) in picks
                              unless (or (symbolp src) (eq (car src) 'alias))
                              collect (list role v src)))
           (contrast
            (cl-loop for (label fg bg)
                     in '(("foreground on background" :foreground :background)
                          ("bar text on bar" :bar-foreground :bar-background)
                          ("entry on launcher" :launcher-entry :launcher-background)
                          ("selected entry on selection" :launcher-selected :launcher-selection)
                          ("match on launcher" :launcher-match :launcher-background)
                          ("match on selection" :launcher-match :launcher-selection)
                          ("focused border on bar" :border-focused :bar-background)
                          ("unfocused border on bar" :border-unfocused :bar-background))
                     for a = (funcall colour-of fg)
                     for b = (funcall colour-of bg)
                     for r = (tc-contrast a b)
                     collect (list label a b r)))
           (roles-missing (cl-remove-if (lambda (r) (plist-get roles-plist (car r)))
                                        role-table))
           (theme-file (locate-file (format "%s-theme" emacs-theme)
                                    (custom-theme--load-path) '(".el"))))
      (when roles-missing
        (error "Roles left without a value: %S" (mapcar #'car roles-missing)))
      (when (and hue-only (not (getenv "TC_ALLOW_HUE_ONLY")))
        (message "REFUSED %s: hue-only state pairs %S" name
                 (mapcar (lambda (p) (list (nth 0 p) (nth 1 p))) hue-only))
        (kill-emacs 2))
      (with-temp-file (concat out name ".theme")
        (insert (format ";;;; %s.theme: rhizome colour theme, format 1\n;;;;\n" name))
        (dolist (line (plist-get recipe :header))
          (insert (if (string-empty-p line) ";;;;\n" (format ";;;; %s\n" line))))
        (insert ";;;;\n")
        (insert (format ";;;; Generated by tools/theme-convert from the Emacs theme %s\n" emacs-theme))
        (insert (format ";;;;   %s, version %s, resolved in Emacs %s\n"
                        (plist-get recipe :url)
                        (or (tc-git-version theme-dir) "unknown") emacs-version))
        (insert (format ";;;;   for a 24-bit graphic display, background %s.\n" tc-background))
        (when (plist-get recipe :settings-note)
          (dolist (line (plist-get recipe :settings-note))
            (insert (format ";;;; %s\n" line))))
        (when correction-notes
          (insert ";;;; Corrections to the source, applied as if fixed there:\n")
          (cl-loop for (text state) in correction-notes for i from 1
                   do (insert (format ";;;;   %d. %s (%s)\n" i text state))))
        (insert ";;;; Roles with no face in the Emacs theme:\n")
        (if unmapped
            (cl-loop for (role v src) in unmapped
                     do (insert (format ";;;;   %s %s\n" role
                                        (if (eq (car src) 'follows)
                                            (format "follows %s" (cadr src))
                                          (format "takes the default theme's %s"
                                                  (plist-get default-roles role))))))
          (insert ";;;;   none\n"))
        (insert ";;;; Contrast (WCAG 2), pairs below 7:1 kept as the source renders them:\n")
        (cl-loop for (label a b r) in contrast
                 do (insert (format ";;;;   %-28s %s on %s %7s%s\n" label a b (tc-format-ratio r)
                                    (cond ((< r 4.5) "  BELOW 4.5:1")
                                          ((< r 7.0) "  below 7:1")
                                          (t "")))))
        (insert (format ";;;; Hue-only state pairs: %s\n"
                        (if hue-only
                            (format "%S (FLAGGED)" (mapcar #'car hue-only))
                          (format "none; each of %d pairs carries a non-hue cue" (length pairs)))))
        (dolist (line (plist-get recipe :notice))
          (insert (if (string-empty-p line) ";;;;\n" (format ";;;; %s\n" line))))
        (insert "\n")
        (insert (format "(:format 1\n :name %S\n :roles (" name))
        (cl-loop for (role v src) in picks
                 for first = t then nil
                 for value = (or v (plist-get default-roles role))
                 do (insert (if first "" "\n         ")
                            (format "%-29s ; %s"
                                    (format "%s %S" role value)
                                    (cond ((eq (car-safe src) 'follows)
                                           (format "no face: follows %s" (cadr src)))
                                          ((eq (car-safe src) 'alias)
                                           (format "follows %s, as in the default theme"
                                                   (cadr src)))
                                          ((equal src '(default)) "no face: default theme")
                                          (t (format "%s" src))))))
        ;; A trailing comment would swallow a closing parenthesis.
        (insert "\n         )\n :elements (")
        (cl-loop for (e v f) in elements
                 for first = t then nil
                 do (insert (if first "" "\n            ")
                            (format "%-29s ; %s" (format "%s %S" e v) f)))
        (insert (if elements "\n            ))\n" "))\n")))
      (with-temp-file (concat (file-name-as-directory (or (getenv "TC_REPORT") out))
                              name ".report.txt")
        (insert (format "%s from %s (%s)\n" name emacs-theme theme-file))
        (insert (format "background %s\n\nstate pairs:\n" tc-background))
        (cl-loop for (state against colours non-hue ratio) in pairs
                 do (insert (format "  %-22s vs %-22s %s non-hue %S luminance step %s\n"
                                    state against colours non-hue
                                    (if ratio (tc-format-ratio ratio) "-"))))
        (insert "\nroles:\n")
        (cl-loop for (role v src) in picks
                 do (insert (format "  %-22s %-9s %S\n" role v src)))))
    (message "wrote %s.theme" name)))

(condition-case err
    (tc-main)
  (error (message "theme-convert: %s" (error-message-string err))
         (kill-emacs 1)))

;;; theme-convert.el ends here
