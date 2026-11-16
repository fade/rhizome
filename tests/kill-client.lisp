;;; Tests for kill-client and the kill and delete commands: the guards
;;; on river's unreliable PID refuse without sending a signal, a real
;;; child process given to a fake window is signalled and reaped, the
;;; commands reply with a string or NIL, and the commands and symbols
;;; are public. No compositor: messages are recorded, not drawn.
(require :sb-posix)
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun kill-client-test-window (pid)
  "A window with a placeholder proxy and PID as its unreliable PID."
  (make-instance 'rhizome-window :proxy :fake-proxy :unreliable-pid pid))

(defun kill-client-test-spawn ()
  "Start a child process that sleeps for 30 seconds; return the process."
  (sb-ext:run-program "/bin/sleep" '("30") :wait nil :output nil :error nil))

(defun kill-client-test-focus-only (win)
  "Make WIN the only window and mark it focused."
  (setf *windows* (make-hash-table))
  (setf (gethash 1 *windows*) win
        (window-focused-p win) t))

(defun kill-client-test-ending (process)
  "Wait for PROCESS and return its status and exit code as a list."
  (sb-ext:process-wait process)
  (list (sb-ext:process-status process) (sb-ext:process-exit-code process)))

(defun kill-client-test-reply-p (reply)
  "True when REPLY is safe for the IPC dispatcher to print: NIL or a
string."
  (or (null reply) (stringp reply)))

(let ((pass 0) (fail 0) (messages '()) (sent '())
      (real-sender (and (fboundp 'send-process-signal)
                        (symbol-function 'send-process-signal))))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    (setf (symbol-function 'message)
          (lambda (fmt &rest args)
            (push (apply #'format nil fmt args) messages)
            nil)
          (symbol-function 'request-manage-dirty)
          (lambda (&rest args) (declare (ignore args)) nil))

    (labels ((record-signals ()
               ;; Guard cases: record any signal instead of sending it
               (setf (symbol-function 'send-process-signal)
                     (lambda (pid signo)
                       (push (list pid signo) sent)
                       0)))
             (real-signals ()
               (when real-sender
                 (setf (symbol-function 'send-process-signal) real-sender)))
             (call (thunk)
               (handler-case (funcall thunk)
                 (error (e) (list :error (type-of e)))))
             (kill-with-pid (pid &rest keys)
               (call (lambda ()
                       (apply 'kill-client (kill-client-test-window pid) keys))))
             (unstick (p)
               ;; Without the implementation nothing ends the child
               (unless real-sender (sb-ext:process-kill p 1)))
             (refuses (label pid)
               (setf sent '() messages '())
               (ok (format nil "~A: kill-client returns NIL" label)
                   (kill-with-pid pid) nil)
               (ok (format nil "~A: no signal sent" label) sent nil)
               (ok (format nil "~A: a message is shown" label)
                   (and messages t) t)))

      ;; Public API
      (dolist (name '("KILL-CLIENT" "WINDOW-UNRELIABLE-PID"))
        (ok (format nil "~A is external" name)
            (nth-value 1 (find-symbol name "RHIZOME"))
            :external))

      ;; Guards
      (record-signals)
      (refuses "nil pid" nil)
      (refuses "pid 0" 0)
      (refuses "pid 1" 1)
      (refuses "own pid" (sb-posix:getpid))
      (refuses "parent pid" (sb-posix:getppid))
      (let ((gone (sb-ext:process-pid
                   (sb-ext:run-program "/bin/true" '() :wait t))))
        (ok "precondition: the reaped pid has no /proc entry"
            (probe-file (format nil "/proc/~D/" gone)) nil)
        (refuses "nonexistent /proc entry" gone))
      (setf sent '())
      (let ((p (kill-client-test-spawn)))
        (ok "unknown signal is refused"
            (kill-with-pid (sb-ext:process-pid p) :signal :hup) nil)
        (sb-ext:process-kill p 9)
        (sb-ext:process-wait p))
      (ok "unknown signal: no signal sent" sent nil)

      ;; A real child process: SIGTERM by default
      (real-signals)
      (let* ((p (kill-client-test-spawn)) (pid (sb-ext:process-pid p)))
        (ok "kill-client on a live child returns T" (kill-with-pid pid) t)
        (unstick p)
        (ok "the child ended on SIGTERM"
            (kill-client-test-ending p) '(:signaled 15))
        (ok "the child is reaped"
            (probe-file (format nil "/proc/~D/" pid)) nil))

      ;; :signal :kill sends SIGKILL
      (let ((p (kill-client-test-spawn)))
        (ok "kill-client :signal :kill returns T"
            (kill-with-pid (sb-ext:process-pid p) :signal :kill) t)
        (unstick p)
        (ok "the child ended on SIGKILL"
            (kill-client-test-ending p) '(:signaled 9)))

      ;; Command table. The IPC dispatcher prints each reply, so a
      ;; reply is NIL or a string, never a window.
      (ok "kill command exists" (functionp (find-command :kill)) t)
      (ok "delete command exists" (functionp (find-command :delete)) t)
      (let* ((p (kill-client-test-spawn))
             (reply (progn
                      (kill-client-test-focus-only
                       (kill-client-test-window (sb-ext:process-pid p)))
                      (call (lambda () (execute-command nil :kill '()))))))
        (unstick p)
        (ok "kill command signals the focused window's process"
            (kill-client-test-ending p) '(:signaled 15))
        (ok "kill command replies with a string"
            (stringp reply) t))
      (let* ((p (kill-client-test-spawn))
             (reply (progn
                      (kill-client-test-focus-only
                       (kill-client-test-window (sb-ext:process-pid p)))
                      (call (lambda () (ipc-dispatch-line "kill 9"))))))
        (unstick p)
        (ok "kill 9 sends SIGKILL"
            (kill-client-test-ending p) '(:signaled 9))
        (ok "kill 9 over IPC replies with a string"
            (stringp reply) t))
      (record-signals)
      (kill-client-test-focus-only (kill-client-test-window 1))
      (ok "refused kill replies with NIL or a string"
          (kill-client-test-reply-p
           (call (lambda () (execute-command nil :kill '()))))
          t)
      (real-signals)
      (setf *windows* (make-hash-table) messages '())
      (ok "kill with no focused window returns NIL"
          (call (lambda () (execute-command nil :kill '()))) nil)
      (ok "kill with no focused window says so"
          (and (member "no focused window" messages :test #'string=) t) t)
      (let ((win (kill-client-test-window nil)))
        (kill-client-test-focus-only win)
        (setf *pending-close-requests* nil)
        (ok "delete replies with NIL or a string"
            (kill-client-test-reply-p
             (call (lambda () (execute-command nil :delete '()))))
            t)
        (ok "delete queues a polite close for the focused window"
            (and (member win *pending-close-requests*) t) t)
        (setf *pending-close-requests* nil))))

  (format t "~&~%=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
