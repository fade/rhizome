;;; modules.lisp — user modules: load-module, the StumpWM way
;;;
;;; A module is an ASDF system that lives in its own directory under
;;; the user's module directory, modules/ in the config directory
;;; ($XDG_CONFIG_HOME/rhizome/, by default ~/.config/rhizome/):
;;;
;;;   ~/.config/rhizome/modules/
;;;     my-placement/
;;;       my-placement.asd
;;;       policy.lisp
;;;
;;; (rhizome:load-module "my-placement") makes the module's directory
;;; visible to ASDF for this session (a plain *central-registry* push,
;;; nothing global is rewritten), quickloads the system, and is
;;; idempotent: loading an already-loaded module is a no-op, like
;;; Quicklisp. Works from config.lisp at startup and live from the
;;; REPL. Nothing is loaded unless the config asks for it.
;;;
;;; A module may register placement policies (define-placement-policy),
;;; commands (define-command), keymaps, hooks, and window rules. It
;;; should not redefine core functions — a missing seam is an issue
;;; first, a patch second. State a module keeps across reloads belongs
;;; in defvars (the live-reload rule the WM itself follows).

(in-package #:rhizome)

(defparameter *module-directory* nil
  "The directory load-module scans for modules. NIL until first use;
rebind it in tests. Defaults to the config directory's modules/.")

(defun module-directory ()
  "The user's module directory (see *module-directory*): modules/ in
the config directory, beside config.lisp."
  (or *module-directory*
      (setf *module-directory*
            (merge-pathnames (make-pathname :directory
                                            '(:relative "modules"))
                             (config-directory)))))

(defun module-subdirectories ()
  "One directory per module in the module directory, in name order."
  (sort (remove-if-not
          (lambda (dir)
            ;; a module dir carries its own <name>.asd
            (probe-file (merge-pathnames
                          (make-pathname
                            :name (car (last (pathname-directory dir)))
                            :type "asd")
                          dir)))
          (or (ignore-errors (uiop:subdirectories (module-directory)))
              '()))
        #'string< :key (lambda (dir) (car (last (pathname-directory dir))))))

(defun register-module-directories ()
  "Make every module's directory visible to ASDF for this session.
A plain *central-registry* push per module directory: additive,
session-local, and Quicklisp's own registrations are untouched."
  (dolist (dir (module-subdirectories))
    (pushnew dir asdf:*central-registry* :test #'equal)))

(defun module-loaded-p (name)
  "Whether the ASDF system NAME is already loaded."
  (let ((system (asdf:find-system (string-downcase name) nil)))
    (and system (asdf:component-loaded-p system))))

(defun load-module (name)
  "Load the module (an ASDF system) NAME from the user's module
directory. Returns :LOADED, or :ALREADY-LOADED for a system that is
loaded, or NIL after logging why a load failed. Safe from
config.lisp at startup and live from the REPL."
  (let ((system (string-downcase (string name))))
    (cond ((module-loaded-p system)
           :already-loaded)
          (t
           (register-module-directories)
           ;; asdf:load-system, not ql:quickload: quicklisp's own
           ;; index never sees *central-registry* modules, while
           ;; ASDF sees both the registry AND quicklisp's dists
           (handler-case
               (progn
                 (asdf:load-system system)
                 (format t "~&; Rhizome: module ~A loaded~%" system)
                 :loaded)
             (error (e)
               (format t "~&; Rhizome: module ~A failed to load: ~A~%"
                       system e)
               nil))))))
