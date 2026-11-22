;;; Idle wait: check that WAIT-FOR-FD-INPUT returns as soon as its fd is
;;; readable or at end-of-file, and after about the timeout when not,
;;; and that WAIT-FOR-DISPLAY-INPUT does the same on a compositor socket.
(ql:quickload :rhizome)
(require :sb-bsd-sockets)

(in-package #:rhizome)

(defparameter *idle-wait-timeout* 0.2
  "Timeout passed to each wait, in seconds.")

(defun idle-wait-seconds (function &rest args)
  "Call FUNCTION with ARGS. Return its first value and the elapsed
real time in seconds, or :UNDEFINED and NIL when FUNCTION is unbound."
  (if (fboundp function)
      (let* ((start (get-internal-real-time))
             (value (apply function args)))
        (values value
                (/ (- (get-internal-real-time) start)
                   (float internal-time-units-per-second))))
      (values :undefined nil)))

(defun call-with-pipe (function)
  "Call FUNCTION with the read fd and write fd of a fresh pipe, and a
function of no arguments that closes the write fd. Close any fd still
open on exit."
  (multiple-value-bind (read-fd write-fd) (sb-posix:pipe)
    (let ((open-fds (list read-fd write-fd)))
      (unwind-protect
           (funcall function read-fd write-fd
                    (lambda ()
                      (sb-posix:close write-fd)
                      (setf open-fds (remove write-fd open-fds))))
        (dolist (fd open-fds)
          (ignore-errors (sb-posix:close fd)))))))

(defun write-one-octet (fd)
  "Write a single octet to FD."
  (let ((octet (make-array 1 :element-type '(unsigned-byte 8)
                             :initial-element 1)))
    (sb-sys:with-pinned-objects (octet)
      (sb-posix:write fd (sb-sys:vector-sap octet) 1))))

(defun idle-wait-socket-path ()
  "A fresh Unix socket path for a stand-in compositor."
  (format nil "~A/rhizome-idle-wait-~D-~D"
          (string-right-trim "/" (or (uiop:getenv "XDG_RUNTIME_DIR")
                                     (uiop:getenv "TMPDIR")
                                     "/tmp"))
          (sb-posix:getpid) (random 1000000)))

(defun call-with-idle-compositor (function)
  "Listen on a fresh Unix socket, connect a wayflan display to it, and
call FUNCTION with the display and the accepted server-side socket.
Close everything on exit."
  (let ((path (idle-wait-socket-path))
        (server (make-instance 'sb-bsd-sockets:local-socket :type :stream))
        (display nil)
        (peer nil))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind server path)
           (sb-bsd-sockets:socket-listen server 1)
           (setf display (wl:wl-display-connect (pathname path))
                 peer (sb-bsd-sockets:socket-accept server))
           (funcall function display peer))
      (ignore-errors (sb-bsd-sockets:socket-close server))
      (when peer (ignore-errors (sb-bsd-sockets:socket-close peer)))
      (when display (ignore-errors (wl:wl-display-disconnect display)))
      (ignore-errors (delete-file path)))))

(let ((pass 0) (fail 0)
      (prompt (/ *idle-wait-timeout* 4))
      (late (* *idle-wait-timeout* 0.8)))
  (labels ((check (name ok detail)
             (if ok
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (~A)~%" name detail))))
           (prompt-p (value elapsed)
             (and (eq value t) elapsed (< elapsed prompt)))
           (timed-out-p (value elapsed)
             (and (null value) elapsed
                  (>= elapsed late) (< elapsed (* 5 *idle-wait-timeout*)))))

    (call-with-pipe
     (lambda (read-fd write-fd close-write)
       (declare (ignore close-write))
       (write-one-octet write-fd)
       (multiple-value-bind (value elapsed)
           (idle-wait-seconds 'wait-for-fd-input read-fd *idle-wait-timeout*)
         (check "wait on a readable fd returns at once"
                (prompt-p value elapsed)
                (format nil "got ~S after ~S s" value elapsed)))))

    (call-with-pipe
     (lambda (read-fd write-fd close-write)
       (declare (ignore write-fd close-write))
       (multiple-value-bind (value elapsed)
           (idle-wait-seconds 'wait-for-fd-input read-fd *idle-wait-timeout*)
         (check "wait on an idle fd times out"
                (timed-out-p value elapsed)
                (format nil "got ~S after ~S s" value elapsed)))))

    (call-with-pipe
     (lambda (read-fd write-fd close-write)
       (declare (ignore write-fd))
       (funcall close-write)
       (multiple-value-bind (value elapsed)
           (idle-wait-seconds 'wait-for-fd-input read-fd *idle-wait-timeout*)
         (check "wait on an fd at end-of-file returns at once"
                (prompt-p value elapsed)
                (format nil "got ~S after ~S s" value elapsed)))))

    (call-with-idle-compositor
     (lambda (display peer)
       (declare (ignore peer))
       (multiple-value-bind (value elapsed)
           (idle-wait-seconds 'wait-for-display-input
                              display *idle-wait-timeout*)
         (check "wait on an idle display times out"
                (timed-out-p value elapsed)
                (format nil "got ~S after ~S s" value elapsed)))))

    (call-with-idle-compositor
     (lambda (display peer)
       (sb-bsd-sockets:socket-send
        peer (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-element 0)
        8)
       (multiple-value-bind (value elapsed)
           (idle-wait-seconds 'wait-for-display-input
                              display *idle-wait-timeout*)
         (check "wait on a display with input waiting returns at once"
                (prompt-p value elapsed)
                (format nil "got ~S after ~S s" value elapsed)))))

    (call-with-idle-compositor
     (lambda (display peer)
       (sb-bsd-sockets:socket-close peer)
       (multiple-value-bind (value elapsed)
           (idle-wait-seconds 'wait-for-display-input
                              display *idle-wait-timeout*)
         (check "wait on a display whose compositor hung up returns at once"
                (prompt-p value elapsed)
                (format nil "got ~S after ~S s" value elapsed))
         (check "display-connection-lost-p then returns true"
                (and (display-connection-lost-p display) t)
                "display-connection-lost-p returned NIL"))))

    (call-with-idle-compositor
     (lambda (display peer)
       (declare (ignore peer))
       ;; Disconnecting clears the socket fd: this takes the no-socket
       ;; branch of WAIT-FOR-DISPLAY-INPUT.
       (wl:wl-display-disconnect display)
       (multiple-value-bind (value elapsed)
           (idle-wait-seconds 'wait-for-display-input
                              display *idle-wait-timeout*)
         (check "wait on a display with no socket times out"
                (timed-out-p value elapsed)
                (format nil "got ~S after ~S s" value elapsed))))))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
