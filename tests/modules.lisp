;;; Tests for user modules (LOAD-MODULE): the modules directory sits
;;; beside the user config file, a module loads by name from its own
;;; directory, a second call with unchanged sources loads nothing, a
;;; changed source reloads while DEFVAR state survives, a missing
;;; module signals MODULE-NOT-FOUND naming the path searched, a
;;; config file loads a module at startup and again through
;;; reload-config, and an example module's placement policy, key
;;; binding, and hook are callable. Everything runs under a temporary
;;; directory: the config pathname is redirected there, so the real
;;; ~/.config is never read.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defvar *modules-test-loads* 0
  "Incremented by each load of the test module's source file.")

(defvar *modules-test-hook-calls* nil
  "Arguments the test module's hook handler received.")

(let* ((pass 0) (fail 0)
       (suffix (format nil "~(~36R~)" (random (expt 36 6) (make-random-state t))))
       (root (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "rhizome-modules-test-~A" suffix)
                               (uiop:temporary-directory))))
       (config-file (merge-pathnames "rhizome/config.lisp" root))
       (modules (merge-pathnames "rhizome/modules/" root))
       (other (merge-pathnames "elsewhere/" root))
       (name (format nil "rhz-test-module-~A" suffix))
       (other-name (format nil "rhz-test-other-~A" suffix))
       (policy (intern (string-upcase name) :keyword))
       (real-config-pathname (symbol-function 'config-pathname))
       (registry-before (copy-list asdf:*central-registry*)))
  (labels ((ok (label actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" label))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                label actual expected))))
           (write-file (path text)
             (ensure-directories-exist path)
             (with-open-file (s path :direction :output :if-exists :supersede)
               (write-string text s)))
           (write-module (dir system-name version)
             (let ((home (merge-pathnames (format nil "~A/" system-name) dir)))
               (write-file (merge-pathnames (format nil "~A.asd" system-name) home)
                           (format nil "(asdf:defsystem ~S :components ((:file \"policy\")))~%"
                                   system-name))
               (write-file (merge-pathnames "policy.lisp" home)
                           (format nil "~
(in-package #:rhizome)
(incf *modules-test-loads*)
(defvar *~A-state* (list :fresh))
(defun ~A-version () ~D)
(define-placement-policy ~S ~A-policy () ())
(defmethod placement-new-window-frame ((policy ~A-policy) output workspace window history)
  (declare (ignore output workspace window history))
  :module-frame)
(defun ~A-install () (define-key *top-map* \"s-F12\" '~A-version))
(defun ~A-note-window (win) (push win *modules-test-hook-calls*))
(add-hook :window-created '~A-note-window)
"
                                   system-name system-name version policy system-name
                                   system-name system-name system-name system-name system-name))))
           (call (system-name suffix-name &rest args)
             (apply (symbol-function
                     (find-symbol (string-upcase (format nil "~A-~A" system-name suffix-name))
                                  :rhizome))
                    args))
           (state (system-name)
             (symbol-value (find-symbol (string-upcase (format nil "*~A-STATE*" system-name))
                                        :rhizome))))
    (unwind-protect
         (progn
           (setf (symbol-function 'config-pathname) (lambda () config-file))
           (write-module modules name 1)

           ;; --- where modules live ---
           (ok "modules directory is modules/ beside the config file"
               (namestring (modules-directory)) (namestring modules))
           (ok "modules directory is not under the real home config"
               (null (search (namestring (merge-pathnames ".config/" (user-homedir-pathname)))
                             (namestring (modules-directory))))
               t)

           ;; --- first load ---
           (ok "load-module returns the system name"
               (load-module name) name)
           (ok "module source loaded once"
               *modules-test-loads* 1)
           (ok "system resolves to the module's own directory"
               (namestring (asdf:system-source-directory name))
               (namestring (merge-pathnames (format nil "~A/" name) modules)))
           (ok "module directory is on the session's central registry"
               (and (member (merge-pathnames (format nil "~A/" name) modules)
                            asdf:*central-registry* :test #'equal)
                    t)
               t)

           ;; --- registration points the module used ---
           (ok "module function is callable"
               (call name "version") 1)
           (ok "module placement policy is registered by name"
               (gethash policy *placement-policies*)
               (find-symbol (string-upcase (format nil "~A-POLICY" name)) :rhizome))
           (ok "module placement policy is selected through configure"
               (let ((*config* (merge-plist (default-config)
                                            `(:tiling (:placement (:policy ,policy))))))
                 (placement-new-window-frame (current-placement-policy) nil 1 nil nil))
               :module-frame)
           (call name "install")
           (ok "module key binding is in the top map"
               (keymap-lookup *top-map* (first (kbd "s-F12")))
               (find-symbol (string-upcase (format nil "~A-VERSION" name)) :rhizome))
           (setf *modules-test-hook-calls* nil)
           (run-hook :window-created :a-window)
           (ok "module hook handler runs"
               *modules-test-hook-calls* '(:a-window))

           ;; --- second call with unchanged sources ---
           (setf (cdr (state name)) (list :running))
           (ok "second load-module returns the system name"
               (load-module name) name)
           (ok "second load-module loads nothing"
               *modules-test-loads* 1)

           ;; --- a newer version reloads; DEFVAR state survives ---
           (sleep 1.1)
           (write-module modules name 2)
           (load-module name)
           (ok "a changed module source is reloaded"
               (list *modules-test-loads* (call name "version"))
               '(2 2))
           (ok "DEFVAR state survives the reload"
               (state name) '(:fresh :running))
           (setf *modules-test-hook-calls* nil)
           (run-hook :window-created :b-window)
           (ok "a hook handler named by a symbol is not added twice by a reload"
               *modules-test-hook-calls* '(:b-window))

           ;; --- an explicit directory ---
           (write-module other other-name 7)
           (ok "load-module takes an explicit directory"
               (list (load-module other-name :directory other)
                     (call other-name "version"))
               (list other-name 7))

           ;; --- from the user config file at startup ---
           (let ((startup-name (format nil "rhz-test-startup-~A" suffix)))
             (write-module modules startup-name 3)
             (write-file config-file
                         (format nil "(rhizome:load-module ~S)~%" startup-name))
             (load-user-config)
             (ok "load-module from config.lisp loads the module"
                 (call startup-name "version") 3))

           ;; --- reload-config reruns config.lisp: load-module loads
           ;; nothing, the install function restores the key binding ---
           (write-file config-file
                       (format nil "(rhizome:load-module ~S)~%(rhizome::~A-install)~%"
                               name name))
           (let ((loads *modules-test-loads*))
             (reload-config)
             (ok "reload-config: the module is not loaded again"
                 *modules-test-loads* loads))
           (ok "reload-config: the install function rebinds the module key"
               (keymap-lookup *top-map* (first (kbd "s-F12")))
               (find-symbol (string-upcase (format nil "~A-VERSION" name)) :rhizome))
           (ok "reload-config: the placement policy stays registered"
               (and (gethash policy *placement-policies*) t) t)

           ;; --- a missing module ---
           (let ((condition (handler-case (progn (load-module "rhz-no-such-module") nil)
                              (module-not-found (c) c))))
             (ok "a missing module signals module-not-found"
                 (typep condition 'module-not-found) t)
             (ok "the condition names the module"
                 (and condition (module-not-found-name condition))
                 "rhz-no-such-module")
             (ok "the report names the path searched"
                 (and condition
                      (search (namestring
                               (merge-pathnames "rhz-no-such-module/rhz-no-such-module.asd"
                                                modules))
                              (princ-to-string condition))
                      t)
                 t))

           ;; --- names that are not a directory name ---
           (ok "a name with a directory separator signals an error"
               (handler-case (progn (load-module "../x") :loaded)
                 (error () :error))
               :error))
      (setf (symbol-function 'config-pathname) real-config-pathname)
      (setf asdf:*central-registry* registry-before)
      (define-key *top-map* "s-F12" nil)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
