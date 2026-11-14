;;; User modules for Rhizome
;;;
;;; A module is an ASDF system in its own directory under the modules
;;; directory, which sits beside the user config file:
;;;
;;;   ~/.config/rhizome/modules/
;;;     my-placement/
;;;       my-placement.asd
;;;       policy.lisp
;;;
;;;   (rhizome:load-module "my-placement")
;;;
;;; Nothing is loaded unless the config or the REPL asks. Module code
;;; runs inside the window manager process and uses the exported
;;; registration points: define-placement-policy, keymaps, hooks, and
;;; window rules through configure.

(in-package #:rhizome)

(defun modules-directory ()
  "Return the user modules directory: modules/ in the directory of
CONFIG-PATHNAME."
  (merge-pathnames "modules/"
                   (uiop:pathname-directory-pathname (config-pathname))))

(define-condition module-not-found (error)
  ((name :initarg :name :reader module-not-found-name)
   (pathname :initarg :pathname :reader module-not-found-pathname))
  (:report (lambda (condition stream)
             (format stream "Rhizome module ~S not found: no system definition at ~A"
                     (module-not-found-name condition)
                     (namestring (module-not-found-pathname condition)))))
  (:documentation "Signalled by LOAD-MODULE when the module's system
definition file does not exist."))

(defun module-system-pathname (name directory)
  "Return the pathname of NAME's system definition: NAME/NAME.asd
under DIRECTORY."
  (merge-pathnames (make-pathname :directory (list :relative name)
                                  :name name
                                  :type "asd")
                   (uiop:ensure-directory-pathname directory)))

(defun load-module-system (name)
  "Load the ASDF system NAME: through ql:quickload when Quicklisp is
present, so dependencies on Quicklisp libraries are fetched, else
through asdf:load-system."
  (let ((quickload (and (find-package "QL")
                        (find-symbol "QUICKLOAD" "QL"))))
    (if (and quickload (fboundp quickload))
        (funcall quickload name :silent t)
        (asdf:load-system name))))

(defun load-module (name &key (directory (modules-directory)))
  "Load the user module NAME (a string naming a subdirectory of
DIRECTORY and the ASDF system defined in NAME/NAME.asd there) and
return NAME.

Push the module's directory onto asdf:*central-registry* for this
session only, then load the system. A call with unchanged sources
loads nothing; changed sources are recompiled and loaded, so state
held in DEFVAR survives.

Signals MODULE-NOT-FOUND, naming the path searched, when the system
definition is absent, and an error when NAME is empty or contains a
directory separator, or when the system NAME resolves to a file
other than the module's."
  (check-type name string)
  (when (or (zerop (length name))
            (find #\/ name)
            (find #\\ name)
            (member name '("." "..") :test #'string=))
    (error "Rhizome module name ~S is not a directory name." name))
  (let* ((asd (module-system-pathname name directory))
         (home (uiop:pathname-directory-pathname asd)))
    (unless (probe-file asd)
      (error 'module-not-found :name name :pathname asd))
    (pushnew home asdf:*central-registry* :test #'equal)
    (let ((found (asdf:system-source-file (asdf:find-system name))))
      (unless (equal (truename found) (truename asd))
        (error "Rhizome module ~S resolves to ~A, not ~A." name
               (namestring found) (namestring asd))))
    (load-module-system name)
    name))
