;;; window-rules-reload.lisp: window rules owned by their source.
;;; A reload of a source replaces that source's whole rule set: a
;;; removed rule is gone, a changed rule is replaced, an unchanged rule
;;; stays a single copy, and other sources are untouched. Uses a
;;; throwaway config file and module under /tmp; the user's own config
;;; and module directory are never read.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun reload-test-matches ()
  "The :match strings of the effective rule list, in order."
  (mapcar (lambda (rule) (getf rule :match)) (window-rules)))

(let ((pass 0) (fail 0)
      (tmp (format nil "/tmp/rhizome-test-rules-~A-~A/"
                   (get-universal-time) (random 100000)))
      (real-config-pathname (symbol-function 'config-pathname)))
  (labels ((ok (name thunk expected)
             (let ((actual (handler-case (funcall thunk)
                             (error (e) (list :error (princ-to-string e))))))
               (if (equal actual expected)
                   (progn (incf pass)
                          (format t "~&PASS: ~A~%" name))
                   (progn (incf fail)
                          (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                  name actual expected)))))
           (config-file ()
             (format nil "~Aconfig.lisp" tmp))
           (write-config (text)
             (with-open-file (s (config-file) :direction :output
                                              :if-exists :supersede)
               (write-string text s)))
           (write-module (rule-text)
             (with-open-file (s (format nil "~Amodules/rules-module/rules-module.lisp" tmp)
                                :direction :output :if-exists :supersede)
               (format s "(in-package #:rhizome)~%(set-window-rules '(~A))~%"
                       rule-text)))
           (quiet-reload ()
             (let ((*standard-output* (make-broadcast-stream)))
               (reload-config)))
           (window-with-app-id (proxy app-id)
             (let ((win (make-instance 'rhizome-window :proxy proxy)))
               (setf (gethash proxy *windows*) win
                     (window-app-id win) app-id)
               win)))
    (unwind-protect
         (progn
           (ensure-directories-exist
             (format nil "~Amodules/rules-module/" tmp))
           (with-open-file (s (format nil "~Amodules/rules-module/rules-module.asd" tmp)
                              :direction :output :if-exists :supersede)
             (format s "(asdf:defsystem #:rules-module~@
                        :components ((:file \"rules-module\")))~%"))
           (write-module "(:match \"from-module\" :workspace 8)")
           (setf (symbol-function 'config-pathname)
                 (lambda () (pathname (config-file))))

           ;; --- the config file's rules, set through configure ---
           (write-config
             "(rhizome:configure '(:window-rules ((:match \"keep\" :workspace 1)
                                                  (:match \"change\" :workspace 2)
                                                  (:match \"drop\" :workspace 3))))")
           (quiet-reload)
           (quiet-reload)
           (ok "unchanged config rules stay one copy over two reloads"
               #'reload-test-matches '("keep" "change" "drop"))
           (write-config
             "(rhizome:configure '(:window-rules ((:match \"keep\" :workspace 1)
                                                  (:match \"change\" :workspace 5))))")
           (quiet-reload)
           (ok "a rule removed from the config file is gone after reload"
               #'reload-test-matches '("keep" "change"))
           (ok "a changed config rule is replaced, not duplicated"
               (lambda ()
                 (mapcar (lambda (rule) (getf rule :workspace))
                         (remove "change" (window-rules)
                                 :key (lambda (rule) (getf rule :match))
                                 :test-not #'equal)))
               '(5))

           ;; --- the config file's rules, set through set-window-rules ---
           (write-config
             "(rhizome:set-window-rules '((:match \"a\" :workspace 1)
                                          (:match \"b\" :workspace 2)))")
           (quiet-reload)
           (quiet-reload)
           (ok "set-window-rules in the config file: one copy over two reloads"
               #'reload-test-matches '("a" "b"))
           (write-config
             "(rhizome:set-window-rules '((:match \"a\" :workspace 4)))")
           (quiet-reload)
           (ok "set-window-rules in the config file: removed rule gone, changed rule replaced"
               (lambda () (window-rules))
               '((:match "a" :workspace 4)))
           (write-config "")
           (quiet-reload)
           (ok "a config file that sets no rules leaves none after reload"
               (lambda () (window-rules)) '())

           ;; --- the README append idiom in the config file ---
           (write-config
             "(rhizome:configure `(:window-rules ,(append (getf rhizome:*config* :window-rules)
                                                          (list '(:match \"appended\" :workspace 2)))))")
           (quiet-reload)
           (quiet-reload)
           (ok "the append idiom in the config file stays one copy over two reloads"
               #'reload-test-matches '("appended"))

           ;; --- other sources ---
           (write-config
             "(rhizome:configure '(:window-rules ((:match \"cfg\" :workspace 1))))")
           (quiet-reload)
           (set-window-rules '((:match "other" :workspace 6)) :source :other)
           (set-window-rules '((:match "other" :workspace 7)) :source :other)
           (ok "setting a source twice replaces its set"
               (lambda () (window-rules))
               '((:match "cfg" :workspace 1) (:match "other" :workspace 7)))
           (quiet-reload)
           (quiet-reload)
           (ok "another source's rules survive config reloads, after the config's"
               (lambda () (window-rules))
               '((:match "cfg" :workspace 1) (:match "other" :workspace 7)))
           (set-window-rules '((:match "other2" :workspace 7)) :source :other)
           (ok "replacing another source leaves the config's rules alone"
               #'reload-test-matches '("cfg" "other2"))
           (set-window-rules nil :source :other)
           (ok "setting a source to NIL removes its rules"
               #'reload-test-matches '("cfg"))

           ;; --- a module loaded with load-module ---
           (write-config
             "(rhizome:configure '(:window-rules ((:match \"cfg\" :workspace 1))))
              (rhizome:load-module \"rules-module\")")
           (let ((*module-directory* (pathname (format nil "~Amodules/" tmp))))
             (quiet-reload)
             (ok "a module's top-level rules land under the module's name"
                 #'reload-test-matches '("cfg" "from-module"))
             (quiet-reload)
             (quiet-reload)
             (ok "module rules survive config reloads as one copy"
                 #'reload-test-matches '("cfg" "from-module"))
             (write-module "(:match \"from-module\" :workspace 9)")
             (let ((*standard-output* (make-broadcast-stream)))
               (let ((*window-rules-source* :rules-module))
                 (asdf:load-system "rules-module" :force '("rules-module"))))
             (ok "reloading the module replaces its rule and keeps the config's"
                 (lambda () (window-rules))
                 '((:match "cfg" :workspace 1)
                   (:match "from-module" :workspace 9)))
             (ok "the module's set is keyed by its name"
                 (lambda () (window-rules-for-source :rules-module))
                 '((:match "from-module" :workspace 9))))

           ;; --- matching uses the effective list, config first ---
           (set-window-rules '((:match "shared" :workspace 6)
                               (:match "only-module" :workspace 7))
                             :source :rules-module)
           (configure '(:window-rules ((:match "shared" :workspace 2))))
           (let ((win (window-with-app-id :reload-w1 "shared")))
             (let ((*standard-output* (make-broadcast-stream)))
               (apply-window-rules win))
             (ok "a config rule wins over another source's rule"
                 (lambda () (window-workspace win)) 2))
           (let ((win (window-with-app-id :reload-w2 "only-module")))
             (let ((*standard-output* (make-broadcast-stream)))
               (apply-window-rules win))
             (ok "another source's rule applies when no config rule matches"
                 (lambda () (window-workspace win)) 7))

           ;; --- a source keeps its place in the matching order ---
           (let ((*window-rule-sets* '()))
             (write-config
               "(rhizome:set-window-rules '((:match \"ordered\" :workspace 1)))")
             (quiet-reload)
             (set-window-rules '((:match "ordered" :workspace 6))
                               :source :rules-module)
             (quiet-reload)
             (quiet-reload)
             (ok "after reloads the config file's set still matches before a module's"
                 (lambda () (mapcar #'car *window-rule-sets*))
                 '(:config :rules-module))
             (let ((win (window-with-app-id :reload-w3 "ordered")))
               (let ((*standard-output* (make-broadcast-stream)))
                 (apply-window-rules win))
               (ok "after reloads a config set-window-rules rule wins over a module's"
                   (lambda () (window-workspace win)) 1))
             (set-window-rules '((:match "first" :workspace 1)) :source :first)
             (set-window-rules '((:match "second" :workspace 2)) :source :second)
             (set-window-rules nil :source :first)
             (set-window-rules '((:match "first" :workspace 3)) :source :first)
             (ok "a source set to NIL and set again keeps its place"
                 (lambda ()
                   (remove-if-not (lambda (match) (member match '("first" "second")
                                                          :test #'equal))
                                  (reload-test-matches)))
                 '("first" "second"))))

      ;; --- cleanup ---
      (setf (symbol-function 'config-pathname) real-config-pathname)
      (set-window-rules nil :source :other)
      (set-window-rules nil :source :rules-module)
      (remhash :reload-w1 *windows*)
      (remhash :reload-w2 *windows*)
      (remhash :reload-w3 *windows*)
      (ignore-errors (uiop:delete-directory-tree
                       (pathname tmp) :validate t))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
