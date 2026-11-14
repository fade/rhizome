;;; Tests that every default key binding can be overridden from a user
;;; init file: rebound to another command, unbound, or moved to a new
;;; key through the public keymap API, and that the user's choice holds
;;; through the events that rebuild or reapply the defaults: manage
;;; sequences, set-user-mod, set-prefix-key, a rebuild of the default
;;; keymaps, reload-config, an output hotplug, and configure. The table
;;; is every binding in the default keymaps, enumerated at run time, and
;;; each case runs twice: with the init loaded through load-user-config
;;; before the defaults exist (WM start order), and evaluated after
;;; they are built (a runtime edit).
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defvar *bindings-test-init* nil
  "The user init function the config file under test calls.")

(let* ((pass 0) (fail 0)
       (init-file (merge-pathnames
                   (format nil "rhizome-bindings-init-~36R.lisp" (random (expt 36 8)
                                                                         (make-random-state t)))
                   (uiop:temporary-directory)))
       (prefix-key (make-key 116 '(:control)))
       (moved-modifiers '(:control :meta :super)))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (manage-cycles (n)
             (dotimes (i n) (on-manage-sequence)))
           (walk (map keys resolve)
             ;; The binding KEYS reach from MAP, or NIL when a key on
             ;; the way is unbound or :undefined
             (loop for (key . more) on keys
                   for binding = (keymap-lookup map key)
                   do (cond ((or (null binding) (undefined-binding-p binding))
                             (return nil))
                            ((null more)
                             (return binding))
                            (t
                             (setf map (funcall resolve binding))
                             (unless map (return nil))))))
           (default-submap (binding)
             (if (eq binding '*prefix-map*)
                 *default-prefix-map*
                 (binding-keymap binding)))
           (effective (keys)
             (walk *top-map* keys #'binding-keymap))
           (default-at (keys)
             (walk *default-top-map* keys #'default-submap))
           (leaf-p (binding)
             (null (binding-keymap binding)))
           (same-binding-p (a b)
             (or (eq a b)
                 (and a b (binding-keymap a)
                      (eq (binding-keymap a) (binding-keymap b)))))
           (spec (keys)
             (sequence-description keys))
           (default-entries ()
             ;; (KEYS . BINDING) for every binding in the default
             ;; keymaps, sub-keymaps included
             (let ((out '()))
               (labels ((visit (map path)
                          (map-keymap (lambda (key binding)
                                        (let ((keys (append path (list key))))
                                          (push (cons keys binding) out)
                                          (let ((sub (default-submap binding)))
                                            (when sub (visit sub keys)))))
                                      map)))
                 (visit *default-top-map* nil))
               (sort out #'string< :key (lambda (e) (spec (car e))))))
           (fresh-start ()
             ;; A process that has loaded rhizome and not yet managed
             (clear-keymap *top-map*)
             (clear-keymap *prefix-map*)
             (clear-keymap *default-top-map*)
             (clear-keymap *default-prefix-map*)
             (setf *default-keymaps-built* nil
                   *default-top-map-mod* nil
                   *mod* *mod-super*
                   *prefix-key-binding* (cons 116 *mod-ctrl*)
                   *minor-mode-stack* nil))
           (moved (keys)
             (let ((last (car (last keys))))
               (append (butlast keys)
                       (list (make-key (key-keysym last)
                                       (union (key-modifiers last) moved-modifiers))))))
           (under-p (keys path)
             ;; True when PATH lies strictly below KEYS
             (and (> (length path) (length keys))
                  (equal keys (subseq path 0 (length keys)))))
           (prefix-internal-p (keys)
             (and (rest keys) (equal (first keys) prefix-key)))
           (user-target (keys)
             ;; The keymap and spec a user init names KEYS by
             (if (prefix-internal-p keys)
                 (values *prefix-map* (spec (rest keys)))
                 (values *top-map* (spec keys))))
           (make-init (op keys binding user-fn)
             (multiple-value-bind (map key-spec) (user-target keys)
               (let ((new-spec (nth-value 1 (user-target (moved keys)))))
                 (ecase op
                   (:rebind (lambda () (define-key map key-spec user-fn)))
                   (:unbind (lambda () (undefine-key map key-spec)))
                   (:move (lambda ()
                            (undefine-key map key-spec)
                            (define-key map new-spec binding)))
                   (:rebind-chord (lambda () (define-key *top-map* (spec keys) user-fn)))))))
           (broken (entries op keys binding user-fn &key (remap #'identity))
             ;; Descriptions of what no longer holds after OP on KEYS;
             ;; NIL when the user's choice and every other default hold.
             ;; REMAP rewrites every path before it is looked up.
             (let ((bad '())
                   (grabs (key-grab-set)))
               (flet ((bad (what keys)
                        (push (format nil "~A ~A" what (spec keys)) bad))
                      (at (keys) (effective (funcall remap keys))))
                 (ecase op
                   ((:rebind :rebind-chord)
                    (unless (eq (at keys) user-fn) (bad "lost-rebind" keys)))
                   (:unbind
                    (when (at keys) (bad "default-back" keys)))
                   (:move
                    (when (at keys) (bad "default-back" keys))
                    (unless (same-binding-p (at (moved keys)) binding)
                      (bad "lost-move" (moved keys)))))
                 (when (null (rest keys))
                   (let ((key (first (funcall remap keys))))
                     (ecase op
                       ((:rebind :rebind-chord)
                        (unless (member key grabs :test #'equal) (bad "ungrabbed" keys)))
                       (:unbind
                        (when (member key grabs :test #'equal) (bad "still-grabbed" keys)))
                       (:move
                        (when (member key grabs :test #'equal) (bad "still-grabbed" keys))
                        (unless (member (first (funcall remap (moved keys))) grabs
                                        :test #'equal)
                          (bad "ungrabbed" (moved keys)))))))
                 (loop for (other . nil) in entries
                       for default = (default-at (funcall remap other))
                       when (and (not (equal other keys))
                                 (leaf-p default))
                         do (cond ((not (under-p keys other))
                                   (unless (same-binding-p (at other) default)
                                     (bad "sibling" other)))
                                  ((eq op :move)
                                   (let ((there (append (moved keys)
                                                        (nthcdr (length keys) other))))
                                     (unless (same-binding-p (at there) default)
                                       (bad "moved-sibling" there)))))))
               (nreverse bad)))
           (run-init (order init)
             (setf *bindings-test-init* init)
             (ecase order
               (:start (load-user-config) (manage-cycles 2))
               (:runtime (manage-cycles 2) (funcall init))))
           (hotplug ()
             (let ((out (make-instance 'rhizome-output :proxy :bindings-hotplug)))
               (setf (gethash :bindings-hotplug *outputs*) out)
               (run-hook :output-created out)
               (handle-output-event out :dimensions 1920 1080)
               (apply-config)
               (handle-output-event out :removed)
               (manage-cycles 2))))

    ;; River-touching stubs
    (dolist (fn '(message show-message message-hide request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf *xkb-bindings-proxy* nil
          *prefix-mode-active* nil
          *prefix-current-map* nil
          *prefix-state* nil
          *outputs* (make-hash-table)
          *windows* (make-hash-table))
    ;; The user config file: it runs *bindings-test-init*
    (with-open-file (s init-file :direction :output :if-exists :supersede)
      (write-string "(funcall rhizome::*bindings-test-init*)" s)
      (terpri s))
    (setf (symbol-function 'config-pathname) (lambda () init-file))

    (fresh-start)
    (manage-cycles 1)
    (let* ((entries (default-entries))
           (direct (count-if (lambda (e) (null (rest (car e)))) entries))
           (nested (count-if (lambda (e) (rest (rest (car e)))) entries)))
      (format t "~&DEFAULTS ~D: ~D direct, ~D under the prefix key (~D of them in sub-keymaps)~%"
              (length entries) direct (- (length entries) direct) nested)
      (dolist (e entries)
        (format t "~&DEFAULT ~A~@[ (~A)~]~%" (spec (car e))
                (and (rest (car e))
                     (keymap-binding-doc (or (default-submap
                                              (default-at (butlast (car e))))
                                             *default-top-map*)
                                         (car (last (car e)))))))

      ;; --- The table names the bindings a user overrides ---
      (ok "C-t f is fselect and C-t F the float toggle"
          (list (keymap-binding-doc *default-prefix-map* 102)
                (keymap-binding-doc *default-prefix-map* 70))
          '("select frame by number" "float/tile window"))
      (ok "every default key reads back from its spec string"
          (loop for (keys . nil) in entries
                unless (equal (kbd (spec keys)) keys)
                  collect (spec keys))
          nil)
      (ok "every moved key is free in the defaults"
          (loop for (keys . nil) in entries
                when (default-at (moved keys))
                  collect (spec (moved keys)))
          nil)

      ;; --- Every default: rebind, unbind, move; both load orders ---
      (let ((events '(:manage :set-user-mod :set-prefix-key :defaults-rebuilt
                      :reload-config :hotplug :configure))
            (results (make-hash-table :test 'equal)))
        (dolist (order '(:start :runtime))
          (dolist (op '(:rebind :unbind :move :rebind-chord))
            (loop for (keys . nil) in entries
                  when (or (not (eq op :rebind-chord)) (prefix-internal-p keys))
                    do (fresh-start)
                       (manage-cycles 1)
                       (let* ((binding (default-at keys))
                              (user-fn (lambda () (list :user (spec keys))))
                              (init (make-init op keys binding user-fn)))
                         (fresh-start)
                         (flet ((note (event &rest more)
                                  (let ((bad (apply #'broken entries op keys binding
                                                    user-fn more)))
                                    (when bad
                                      (push (format nil "~A: ~{~A~^, ~}" (spec keys) bad)
                                            (gethash (list order op event) results))))))
                           (run-init order init)
                           (note :init)
                           (dolist (event events)
                             (ecase event
                               (:manage (manage-cycles 3))
                               (:set-user-mod
                                (set-user-mod :alt) (manage-cycles 1)
                                (set-user-mod :super) (manage-cycles 1))
                               (:set-prefix-key
                                (set-prefix-key "C-z") (manage-cycles 1)
                                (unless (equal keys (list prefix-key))
                                  (note :prefix-moved
                                        :remap (lambda (path)
                                                 (if (equal (first path) prefix-key)
                                                     (cons (make-key 122 '(:control))
                                                           (rest path))
                                                     path))))
                                (set-prefix-key "C-t") (manage-cycles 1))
                               (:defaults-rebuilt (setup-default-keybindings))
                               (:reload-config
                                (reload-config)
                                (configure '(:bar (:enabled nil)))
                                (manage-cycles 1))
                               (:hotplug (hotplug))
                               (:configure
                                (configure '(:bar (:enabled nil)))
                                (manage-cycles 1)))
                             (note event)))))))
        (dolist (order '(:start :runtime))
          (dolist (op '(:rebind :unbind :move :rebind-chord))
            (dolist (event (list* :init (append (subseq events 0 2)
                                                (list :prefix-moved)
                                                (subseq events 2))))
              (ok (format nil "~(~A~) ~(~A~): user choice holds after ~(~A~)"
                          order op event)
                  (reverse (gethash (list order op event) results))
                  nil))))))

    ;; --- The C-t f / C-t F pair, swapped by command name ---
    (fresh-start)
    (run-init :start (lambda ()
                       (define-key *prefix-map* "f" 'toggle-float-focused)
                       (define-key *prefix-map* "F" 'enter-fselect)))
    (set-user-mod :alt)
    (set-user-mod :super)
    (setup-default-keybindings)
    (reload-config)
    (configure '(:bar (:enabled nil)))
    (manage-cycles 2)
    (ok "C-t f and C-t F swapped by a user init stay swapped"
        (list (effective (kbd "C-t f")) (effective (kbd "C-t F")))
        '(toggle-float-focused enter-fselect))

    ;; --- A user binding inside a default sub-keymap keeps its siblings ---
    (fresh-start)
    (run-init :start (lambda ()
                       (define-key *prefix-map* "h b" 'describe-prefix-bindings)))
    (ok "a user key in C-t h keeps the other help keys"
        (loop for k in '("C-t h c" "C-t h f" "C-t h v" "C-t h k" "C-t h w")
              unless (functionp (effective (kbd k)))
                collect k)
        nil)

    ;; --- A default added to a sub-keymap reaches a user's copy of it ---
    ;; A hot load rebuilds the defaults with the new definition of
    ;; bind-default-prefix-keys
    (fresh-start)
    (run-init :runtime (lambda ()
                         (undefine-key *prefix-map* "h w")))
    (let ((original (fdefinition 'bind-default-prefix-keys))
          (added (lambda () :added)))
      (setf (fdefinition 'bind-default-prefix-keys)
            (lambda ()
              (funcall original)
              (define-prefix-key "h z" added "added by a hot load")))
      (unwind-protect (setup-default-keybindings)
        (setf (fdefinition 'bind-default-prefix-keys) original))
      (ok "a rebuilt default sub-keymap reaches the user's edit of it"
          (list (effective (kbd "C-t h z")) (effective (kbd "C-t h w")))
          (list added nil)))
    (setup-default-keybindings)

    ;; --- An override on a literal modifier stays on that modifier ---
    (fresh-start)
    (run-init :start (lambda () (undefine-key *top-map* "s-Return")))
    (set-user-mod :alt)
    (ok "set-user-mod leaves a user unbind on its literal modifier"
        (list (effective (kbd "s-Return"))
              (functionp (effective (kbd "M-Return"))))
        '(nil t))
    (set-user-mod :super)
    (ok "the unbind holds again once the modifier is back"
        (effective (kbd "s-Return")) nil)

    (ignore-errors (delete-file init-file)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
