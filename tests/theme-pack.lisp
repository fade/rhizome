;;; Tests for the themes rhizome ships: each one loads without a
;;; warning, gives every role a colour, resolves every element to a
;;; colour, shows no state by colour alone, and names its origin,
;;; authors, and licence in its header. themes/CREDITS names each one.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: configures below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(defparameter *shipped-theme-licences*
  '(("deepsky-moonunit" "GPL-3.0-or-later")
    ("ef-summer" "GPL-3.0-or-later")
    ("kusanagi" "GPL-3.0-or-later")
    ("modus-operandi" "GPL-3.0-or-later")
    ("modus-vivendi" "GPL-3.0-or-later")
    ("tokyo-night" "MIT")
    ("tokyo-night-emacs" "GPL-3.0-or-later"))
  "Every shipped theme with the licence its header must name.")

(defun theme-file-header (path)
  "The leading comment lines of the file at PATH, as one string."
  (with-open-file (in path :external-format :utf-8)
    (with-output-to-string (out)
      (loop for line = (read-line in nil)
            while (and line (or (zerop (length line))
                                (char= (char line 0) #\;)))
            do (write-line line out)))))

(defun header-field (header field)
  "The text of the first line in HEADER that starts with FIELD after
the comment semicolons, or NIL."
  (with-input-from-string (in header)
    (loop for line = (read-line in nil)
          while line
          for text = (string-left-trim "; " line)
          when (and (>= (length text) (length field))
                    (string= field text :end2 (length field)))
            return text)))

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
         (progn
           ;; Only the shipped directory: a user's themes must not
           ;; shadow the files under test.
           (setf *theme-search-path* (list *shipped-theme-directory*))
           (clear-theme-cache)
           (ok "every shipped theme is offered"
               (available-theme-names)
               (sort (mapcar #'first *shipped-theme-licences*) #'string<))
           (loop
             for (name licence) in *shipped-theme-licences*
             for path = (merge-pathnames (format nil "~A.theme" name)
                                         *shipped-theme-directory*)
             do (ok (format nil "~A: the file exists" name)
                    (and (probe-file path) t) t)
                (when (probe-file path)
                  (let ((theme (handler-case (read-theme-file path)
                                 (theme-file-error () nil)))
                        (header (theme-file-header path))
                        (warnings '()))
                    (ok (format nil "~A: reads as a format 1 theme" name)
                        (and theme (getf theme :format)) 1)
                    (ok (format nil "~A: its :name matches the file" name)
                        (and theme (getf theme :name)) name)
                    (ok (format nil "~A: gives every role a value" name)
                        (and theme
                             (remove-if (lambda (role)
                                          (getf (getf theme :roles) role))
                                        (theme-role-names)))
                        '())
                    (handler-bind ((warning
                                     (lambda (w)
                                       (push (princ-to-string w) warnings)
                                       (muffle-warning w))))
                      (clear-theme-cache)
                      (configure (list :theme name))
                      (ok (format nil "~A: loads without a warning" name)
                          warnings '())
                      (ok (format nil "~A: is the active theme" name)
                          (getf (active-theme) :name) name)
                      (ok (format nil "~A: every element and role resolves"
                                  name)
                          (remove-if
                           (lambda (element)
                             (let ((colour (resolve-theme-colour
                                            element nil (active-theme) 16)))
                               (theme-colour-string-p colour)))
                           (append (theme-role-names) (theme-element-names)))
                          '())
                      (ok (format nil "~A: no state pair is hue-only" name)
                          (hue-only-state-pairs) '()))
                    (ok (format nil "~A: the header names its origin" name)
                        (and (header-field header "Origin:") t) t)
                    (ok (format nil "~A: the header names its authors" name)
                        (and (header-field header "Authors:") t) t)
                    (ok (format nil "~A: the header names licence ~A"
                                name licence)
                        (let ((field (header-field header "Licence:")))
                          (and field (search licence field) t))
                        t)
                    (when (string= licence "GPL-3.0-or-later")
                      (ok (format nil "~A: the licence is not given as ~
                                       the AGPL" name)
                          (let ((field (header-field header "Licence:")))
                            (and field (search "Licence: AGPL" field) t))
                          nil))
                    (when (search "tokyo-night" name)
                      (ok (format nil "~A: keeps Enkia's MIT notice" name)
                          (and (search "Copyright (c) 2018-present Enkia"
                                       header)
                               (search "Permission is hereby granted"
                                       header)
                               t)
                          t)))))
           (let ((credits (uiop:read-file-string
                           (merge-pathnames "CREDITS"
                                            *shipped-theme-directory*))))
             (loop for (name licence) in *shipped-theme-licences*
                   do (ok (format nil "CREDITS names ~A.theme and ~A"
                                  name licence)
                          (let ((at (search (format nil "~A.theme~%" name)
                                            credits)))
                            (and at (search licence credits :start2 at) t))
                          t))))
      (setf *config* saved-config
            *theme-search-path* saved-search-path)
      (clear-theme-cache)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
