;;; Tests for the StumpWM commands gnext, gprev, gnext-with-window,
;;; gprev-with-window, eval, and exec, run through execute-command and
;;; the IPC dispatcher: workspace cycling wraps both ways on one
;;; output; the with-window variants carry the focused window and keep
;;; it focused, and act as the plain commands with no focused window;
;;; exec starts a real process; eval replies with the printed value or
;;; the error text, bounded; every reply is NIL or a string.
;;; River requests and on-screen messages are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))
;; No slide-in animation in a test run
(configure '(:animations (:enabled nil)))
;; Three workspaces per output
(configure '(:workspaces (:count 3)))

(defparameter *group-eval-exec-commands*
  '(:gnext :gprev :gnext-with-window :gprev-with-window :eval :exec)
  "The StumpWM commands under test.")

(defun wait-for-file (path &optional (seconds 5))
  "Return T once PATH exists, or NIL after SECONDS."
  (loop repeat (* seconds 20)
        when (probe-file path) return t
        do (sleep 0.05)
        finally (return (and (probe-file path) t))))

(defun run-group-eval-exec-tests ()
  "Run the checks; return the pass and fail counts."
  (let ((pass 0) (fail 0) (messages '()) (out nil))
    (labels ((ok (name actual expected)
               (if (equal actual expected)
                   (progn (incf pass)
                          (format t "~&PASS: ~A~%" name))
                   (progn (incf fail)
                          (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                  name actual expected))))
             (reset ()
               (setf *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *output-workspace* (make-hash-table)
                     *window-workspace* (make-hash-table)
                     *minimized-windows* (make-hash-table)
                     *marked-windows* (make-hash-table)
                     *window-number-map* (make-hash-table)
                     *frame-trees* (make-hash-table :test 'equal)
                     *current-frames* (make-hash-table :test 'equal)
                     *scroll-states* (make-hash-table :test 'equal)
                     *workspace-last-focus* (make-hash-table :test 'equal)
                     *workspace-count-overrides* (make-hash-table)
                     *pending-focus-requests* nil
                     *pending-close-requests* nil
                     *pending-pointer-warps* nil
                     *last-registered-window* nil
                     *pending-autofocus* nil
                     messages nil)
               (setf out (make-instance 'rhizome-output :proxy :out-1
                                                        :width 1920
                                                        :height 1080))
               (setf (gethash out *output-workspace*) 1))
             (make-seat (proxy)
               (setf (gethash proxy *seats*)
                     (make-instance 'rhizome-seat :proxy proxy)))
             (make-win (proxy workspace)
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out
                       *last-registered-window* win)
                 (assign-window-workspace win workspace)
                 win))
             (reply-type-ok (r)
               (or (null r) (stringp r)))
             (log-text (thunk)
               (with-output-to-string (*standard-output*)
                 (funcall thunk))))
      (macrolet ((check (name actual expected)
                   `(handler-case (ok ,name ,actual ,expected)
                      (error (e)
                        (incf fail)
                        (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

        ;; Record messages and river requests instead of sending them
        (setf (symbol-function 'show-message)
              (lambda (text &rest args)
                (declare (ignore args))
                (push text messages)
                (incf *message-serial*)
                nil)
              (symbol-function 'request-manage-dirty)
              (lambda (&rest args) (declare (ignore args)) nil)
              (symbol-function 'rwm:river-seat-v1.focus-window)
              (lambda (&rest args) (declare (ignore args)) nil)
              (symbol-function 'rwm:river-seat-v1.clear-focus)
              (lambda (&rest args) (declare (ignore args)) nil)
              (symbol-function 'rwm:river-seat-v1.pointer-warp)
              (lambda (&rest args) (declare (ignore args)) nil)
              (symbol-function 'current-output)
              (lambda () out))

        ;; Native commands, not aliases
        (dolist (name *group-eval-exec-commands*)
          (check (format nil "~(~A~) is a registered command" name)
                 (and (gethash name *command-table*) t)
                 t)
          (check (format nil "~(~A~) is not an alias" name)
                 (command-alias-target name)
                 nil))

        ;; --- gnext and gprev wrap across three workspaces ---

        (reset)
        (let ((replies '()))
          (dolist (expected '(2 3 1))
            (push (execute-command out :gnext nil) replies)
            (check (format nil "gnext from execute-command reaches ~D"
                           expected)
                   (current-workspace out) expected))
          (dolist (expected '(3 2 1))
            (push (execute-command out :gprev nil) replies)
            (check (format nil "gprev from execute-command reaches ~D"
                           expected)
                   (current-workspace out) expected))
          (check "gnext and gprev reply with strings"
                 (every #'stringp replies) t)
          (check "gnext replies with the workspace it shows"
                 (car (last replies)) "workspace 2"))

        (reset)
        (dolist (expected '(2 3 1))
          (ipc-dispatch-line "gnext")
          (check (format nil "gnext from the IPC line reaches ~D" expected)
                 (current-workspace out) expected))
        (dolist (expected '(3 2 1))
          (ipc-dispatch-line "gprev")
          (check (format nil "gprev from the IPC line reaches ~D" expected)
                 (current-workspace out) expected))
        (check "gnext from the IPC line replies with a string"
               (stringp (ipc-dispatch-line "gnext")) t)

        ;; Control: a lone workspace stays put
        (reset)
        (setf (gethash out *workspace-count-overrides*) 1)
        (let ((r (execute-command out :gnext nil)))
          (check "gnext with one workspace stays put"
                 (current-workspace out) 1)
          (check "gnext with one workspace says so"
                 r "no other workspace"))

        ;; --- The with-window variants ---

        (reset)
        (let* ((seat (make-seat :seat-1))
               (a (make-win :a 1))
               (b (make-win :b 1)))
          (focus-window a seat)
          (let ((r (execute-command out :gnext-with-window nil)))
            (check "gnext-with-window replies with a string" (stringp r) t))
          (check "gnext-with-window shows the next workspace"
                 (current-workspace out) 2)
          (check "gnext-with-window carries the focused window"
                 (window-workspace a) 2)
          (check "gnext-with-window keeps the window focused"
                 (seat-focused-window seat) a)
          (check "gnext-with-window leaves the other window behind"
                 (window-workspace b) 1)
          (execute-command out :gnext-with-window nil)
          (execute-command out :gnext-with-window nil)
          (check "gnext-with-window wraps to the first workspace"
                 (list (current-workspace out) (window-workspace a))
                 '(1 1))
          (execute-command out :gprev-with-window nil)
          (check "gprev-with-window wraps to the last workspace"
                 (list (current-workspace out) (window-workspace a))
                 '(3 3))
          (check "gprev-with-window keeps the window focused"
                 (seat-focused-window seat) a)
          (ipc-dispatch-line "gprev-with-window")
          (check "gprev-with-window from the IPC line carries the window"
                 (list (current-workspace out) (window-workspace a))
                 '(2 2))
          (ipc-dispatch-line "gnext-with-window")
          (check "gnext-with-window from the IPC line carries the window"
                 (list (current-workspace out) (window-workspace a)
                       (eq (seat-focused-window seat) a))
                 '(3 3 t)))

        ;; No focused window: the plain commands' behaviour
        (reset)
        (let* ((seat (make-seat :seat-1))
               (a (make-win :a 1)))
          (check "no window is focused" (seat-focused-window seat) nil)
          (let ((r (execute-command out :gnext-with-window nil)))
            (check "gnext-with-window without focus replies like gnext"
                   r "workspace 2"))
          (check "gnext-with-window without focus moves the view"
                 (current-workspace out) 2)
          (check "gnext-with-window without focus moves no window"
                 (window-workspace a) 1)
          (ipc-dispatch-line "gprev-with-window")
          (check "gprev-with-window without focus moves the view back"
                 (list (current-workspace out) (window-workspace a))
                 '(1 1)))

        ;; --- exec ---

        (reset)
        (let* ((dir (uiop:temporary-directory))
               (tag (format nil "~D-~D" (get-universal-time)
                           (random 1000000000 (make-random-state t))))
               (file-1 (merge-pathnames
                        (format nil "rhizome-exec-a-~A" tag) dir))
               (file-2 (merge-pathnames
                        (format nil "rhizome-exec-b-~A" tag) dir)))
          (unwind-protect
               (let* ((line-1 (format nil "touch ~A" (namestring file-1)))
                      (r (execute-command out :exec (list line-1))))
                 (check "exec from execute-command replies running"
                        r (format nil "running: ~A" line-1))
                 (check "exec from execute-command starts the process"
                        (wait-for-file file-1) t)
                 (let ((r2 (ipc-dispatch-line
                            (format nil "exec touch ~A"
                                    (namestring file-2)))))
                   (check "exec from the IPC line replies running"
                          r2 (format nil "running: touch ~A"
                                     (namestring file-2)))
                   (check "exec from the IPC line starts the process"
                          (wait-for-file file-2) t)))
            (ignore-errors (delete-file file-1))
            (ignore-errors (delete-file file-2))))
        (let* ((r :unset)
               (log (log-text (lambda ()
                                (setf r (execute-command out :exec
                                                         '("   ")))))))
          (check "exec with a blank line replies NIL" r nil)
          (check "exec with a blank line opens the shell prompt"
                 (and (search "cannot show prompt" log) t) t))
        (let* ((r :unset)
               (log (log-text (lambda ()
                                (setf r (execute-command out :exec nil))))))
          (check "exec with no argument replies NIL" r nil)
          (check "exec with no argument opens the shell prompt"
                 (and (search "cannot show prompt" log) t) t))

        ;; --- eval ---

        (check "eval from execute-command replies with the value"
               (execute-command out :eval '("(+ 1 2)")) "3")
        (check "eval from the IPC line replies with the value"
               (ipc-dispatch-line "eval (+ 1 2)") "3")
        (check "eval reads a form that is not a list"
               (ipc-dispatch-line "eval *reply-max-length*")
               (princ-to-string *reply-max-length*))
        (check "eval replies with every value"
               (ipc-dispatch-line "eval (values 1 2)") "1 2")
        (dolist (bad '("(+ 1" ")" "(1 . . 2)"))
          (let ((r (execute-command out :eval (list bad))))
            (check (format nil "eval of ~S replies with a string" bad)
                   (stringp r) t)
            (check (format nil "eval of ~S replies with the error" bad)
                   (and (stringp r) (eql 0 (search "error: " r))) t)))
        (let ((r (ipc-dispatch-line "eval (error \"boom\")")))
          (check "eval of a signalling form replies with its text"
                 (and (stringp r) (search "boom" r) t) t))
        (let ((r (ipc-dispatch-line
                  "eval (let ((x (list 1 2))) (setf (cdr (last x)) x) x)")))
          (check "eval of a circular value replies with its printed start"
                 (and (stringp r) (eql 0 (search "(1 2 1 2" r))) t)
          (check "eval of a circular value replies within the bound"
                 (and (stringp r) (<= (length r) (+ 3 *reply-max-length*)))
                 t))
        (let* ((r :unset)
               (log (log-text (lambda ()
                                (setf r (execute-command out :eval nil))))))
          (check "eval with no argument replies NIL" r nil)
          (check "eval with no argument opens the eval prompt"
                 (and (search "cannot show eval prompt" log) t) t))
        ;; Control: a bare form on the IPC line evaluates as before
        (check "a form on the IPC line still evaluates"
               (ipc-dispatch-line "(+ 1 2)") "3")

        ;; --- Reply types ---

        (reset)
        (let ((seat (make-seat :seat-1))
              (a (make-win :a 1)))
          (focus-window a seat)
          (dolist (call `((:gnext nil) (:gprev nil)
                          (:gnext-with-window nil)
                          (:gprev-with-window nil)
                          (:eval ("(list 1 2)"))
                          (:eval ("(make-hash-table)"))
                          (:eval ("(+ 1"))
                          (:exec ("true"))
                          (:exec ("  "))))
            (destructuring-bind (name args) call
              (check (format nil "~(~A~) ~S replies with NIL or a string"
                             name args)
                     (reply-type-ok (execute-command out name args))
                     t))))))
    (values pass fail)))

(multiple-value-bind (pass fail) (run-group-eval-exec-tests)
  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
