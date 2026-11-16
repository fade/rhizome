;;; ipc-slow-client.lisp: a client that sends commands and never reads
;;; their replies cannot stall the thread that drains the command
;;; queue (the WM's main thread). The client is one end of a socket
;;; pair whose send buffer on the served end is shrunk below one
;;; reply. Every drain runs in a thread with a deadline, so a drain
;;; that blocks fails its case instead of hanging the run.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun set-buffer-size (fd option octets)
  "Set the SOL_SOCKET OPTION (7 send, 8 receive) of FD to OCTETS."
  (cffi:with-foreign-object (value :int)
    (setf (cffi:mem-ref value :int) octets)
    (cffi:foreign-funcall "setsockopt" :int fd :int 1 :int option
                          :pointer value :uint 4 :int)))

(defun write-line-octets (fd line)
  "Write LINE as UTF-8 plus a newline to FD in one call."
  (let ((octets (sb-ext:string-to-octets
                 (concatenate 'string line (string #\Newline))
                 :external-format :utf-8)))
    (cffi:with-pointer-to-vector-data (ptr octets)
      (cffi:foreign-funcall "write" :int fd :pointer ptr
                            :long (length octets) :long))))

(defun queued-count ()
  "Number of thunks in the command queue."
  (bt:with-lock-held (*ipc-lock*) (length *ipc-queue*)))

(defun timed-drain (seconds)
  "Run IPC-DRAIN in a fresh thread. Return the seconds it took, or
:TIMEOUT when it is still running after SECONDS."
  (let* ((start (get-internal-real-time))
         (elapsed nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (let ((*standard-output* (make-broadcast-stream)))
                      (ipc-drain))
                    (setf elapsed (/ (- (get-internal-real-time) start)
                                     internal-time-units-per-second)))
                  :name "ipc-slow-client-drain")))
    (sb-thread:join-thread thread :timeout seconds :default nil)
    (or elapsed :timeout)))

(defun with-silent-client (commands fn)
  "Serve one end of a socket pair with IPC-HANDLE-CONNECTION, its send
buffer shrunk, and write each line of COMMANDS from the other end,
which never reads. Once every command is queued, call FN with the
handler thread and return its value. Close the client end afterwards,
which also releases a writer blocked on it."
  (cffi:with-foreign-object (fds :int 2)
    (assert (zerop (cffi:foreign-funcall "socketpair" :int 1 :int 1 :int 0
                                         :pointer fds :int)))
    (let* ((client (cffi:mem-aref fds :int 0))
           (server (cffi:mem-aref fds :int 1))
           (handler nil))
      (set-buffer-size server 7 4096)
      (set-buffer-size client 8 4096)
      (setf handler (sb-thread:make-thread
                     (lambda ()
                       (let ((*standard-output* (make-broadcast-stream)))
                         (ipc-handle-connection server)))
                     :name "ipc-slow-client-handler"))
      (unwind-protect
           (progn
             (dolist (line commands) (write-line-octets client line))
             (loop repeat 300
                   until (>= (queued-count) (length commands))
                   do (sleep 0.01))
             (funcall fn handler))
        (cffi:foreign-funcall "close" :int client :int)
        (sb-thread:join-thread handler :timeout 5 :default nil)
        (bt:with-lock-held (*ipc-lock*) (setf *ipc-queue* nil))))))

(defparameter *big-reply-form*
  "(make-string 4096 :initial-element (code-char 10003))"
  "Eval form whose reply encodes to 12289 octets, past the shrunk
send buffer.")

(defun over-cap-disconnects-p (cap)
  "Set *IPC-CLIENT-OUTPUT-CAP* globally to CAP, send four big replies
to a client that never reads, and return true when the drain returns
and the handler then ends. Return :NO-OUTPUT-CAP when the variable is
not defined."
  (unless (boundp '*ipc-client-output-cap*)
    (return-from over-cap-disconnects-p :no-output-cap))
  (let ((saved (symbol-value '*ipc-client-output-cap*)))
    (setf (symbol-value '*ipc-client-output-cap*) cap)
    (unwind-protect
         (with-silent-client
             (make-list 4 :initial-element *big-reply-form*)
           (lambda (handler)
             (and (realp (timed-drain 3))
                  (progn (sb-thread:join-thread handler :timeout 3
                                                        :default nil)
                         (not (sb-thread:thread-alive-p handler))))))
      (setf (symbol-value '*ipc-client-output-cap*) saved))))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (let ((elapsed (with-silent-client
                       (make-list 4 :initial-element *big-reply-form*)
                     (lambda (handler)
                       (declare (ignore handler))
                       (timed-drain 3)))))
      (ok "drain returns while a client does not read its replies"
          (realp elapsed) t)
      (ok "drain with an unread client returns within 200 ms"
          (and (realp elapsed) (< elapsed 0.2))
          t))

    (ok "a client over the output cap is disconnected"
        (over-cap-disconnects-p 20000) t)

    (let ((elapsed (with-silent-client
                       (list "(+ 1 2)")
                     (lambda (handler)
                       (declare (ignore handler))
                       (timed-drain 3)))))
      (ok "control: drain of a small reply returns within 200 ms"
          (and (realp elapsed) (< elapsed 0.2))
          t)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
