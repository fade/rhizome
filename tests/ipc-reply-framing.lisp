;;; ipc-reply-framing.lisp: a reply that spans several lines reaches
;;; the Lisp client and the rhizome-cmd shell client whole. On the
;;; wire every reply is one line: a newline inside the reply travels
;;; as \n and a backslash as \\, so a reply with neither a newline nor
;;; a backslash keeps its exact octets.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun drain-while (thread seconds)
  "Drain the command queue until THREAD ends or SECONDS pass. Return
true when THREAD ended."
  (loop with deadline = (+ (get-internal-real-time)
                           (* seconds internal-time-units-per-second))
        while (and (sb-thread:thread-alive-p thread)
                   (< (get-internal-real-time) deadline))
        do (let ((*standard-output* (make-broadcast-stream)))
             (ipc-drain))
           (sleep 0.01))
  (not (sb-thread:thread-alive-p thread)))

(defun client-reply (path line &optional (seconds 3))
  "Send LINE with IPC-SEND-COMMAND from a second thread and return its
reply, :CRASHED when the client signals an error, or :TIMEOUT."
  (let* ((reply :timeout)
         (client (sb-thread:make-thread
                  (lambda ()
                    (setf reply (handler-case (ipc-send-command path line)
                                  (error () :crashed))))
                  :name "ipc-framing-client")))
    (unless (drain-while client seconds)
      (sb-thread:terminate-thread client))
    reply))

(defun raw-reply (path line &optional (seconds 3))
  "Send LINE to the socket at PATH and return, as a list, the octets
received up to the first newline octet, or those received when
SECONDS pass."
  (let ((fd (%ipc-socket))
        (received (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
    (cffi:with-foreign-object (addr :uint8 110)
      (cffi:foreign-funcall "memset" :pointer addr :int 0 :long 110 :pointer)
      (setf (cffi:mem-aref addr :uint16 0) 1)
      (loop for i from 0 for c across path
            do (setf (cffi:mem-aref addr :uint8 (+ 2 i)) (char-code c)))
      (assert (zerop (cffi:foreign-funcall "connect" :int fd :pointer addr
                                           :uint 110 :int))))
    (let ((reader
            (sb-thread:make-thread
             (lambda ()
               (cffi:with-foreign-object (buf :uint8 4096)
                 (loop
                   (let ((n (cffi:foreign-funcall "read" :int fd :pointer buf
                                                  :long 4096 :long)))
                     (when (<= n 0) (return))
                     (dotimes (i n)
                       (vector-push-extend (cffi:mem-aref buf :uint8 i)
                                           received))
                     (when (find 10 received) (return))))))
             :name "ipc-framing-raw-reader")))
      (%ipc-write-string fd line)
      (unless (drain-while reader seconds)
        (sb-thread:terminate-thread reader))
      (cffi:foreign-funcall "close" :int fd :int)
      (coerce received 'list))))

(defun shell-reply (runtime-dir line &optional (seconds 10))
  "Run session/rhizome-cmd with XDG_RUNTIME_DIR set to RUNTIME-DIR and
LINE as its argument. Return its standard output, or :TIMEOUT."
  (let* ((script (namestring (asdf:system-relative-pathname
                              :rhizome "session/rhizome-cmd")))
         (output :timeout)
         (runner (sb-thread:make-thread
                  (lambda ()
                    (setf output
                          (uiop:run-program
                           (list "env" (format nil "XDG_RUNTIME_DIR=~A"
                                               runtime-dir)
                                 script line)
                           :output :string :ignore-error-status t)))
                  :name "ipc-framing-shell")))
    (unless (drain-while runner seconds)
      (sb-thread:terminate-thread runner))
    output))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (let* ((dir (format nil "/tmp/rhizome-test-reply-framing-~D/"
                        (cffi:foreign-funcall "getpid" :int)))
           (path (concatenate 'string dir "rhizome.sock"))
           (three-lines (format nil "one~%two~%three"))
           (three-lines-form "(format nil \"one~%two~%three\")"))
      (ensure-directories-exist dir)
      (stop-ipc-server)
      (setf *ipc-thread* nil *ipc-fd* nil)
      (start-ipc-server path)
      (sleep 0.2)

      ;; --- The Lisp client ---
      (ok "client receives a three-line reply whole"
          (client-reply path three-lines-form) three-lines)
      (ok "client receives a reply holding a backslash and n unchanged"
          (client-reply path "(format nil \"a\\\\nb\\\\\\\\c\")")
          (format nil "a\\nb\\\\c"))
      (ok "client receives a reply ending in a newline"
          (client-reply path "(format nil \"x~%\")")
          (format nil "x~%"))
      (ok "client receives a single-line reply"
          (client-reply path "(+ 40 2)") "42")

      ;; --- The wire ---
      (ok "a three-line reply is one line on the wire"
          (count 10 (raw-reply path three-lines-form)) 1)
      (ok "a single-line ASCII reply keeps its octets"
          (raw-reply path "(+ 1 2)") (list 51 10))
      (ok "a single-line non-ASCII reply keeps its octets"
          (raw-reply path "(identity \"café ✓\")")
          (coerce (sb-ext:string-to-octets (format nil "café ✓~%")
                                           :external-format :utf-8)
                  'list))

      ;; --- The shell client ---
      (ok "rhizome-cmd prints a three-line reply whole"
          (shell-reply dir three-lines-form)
          (format nil "~A~%" three-lines))
      (ok "rhizome-cmd prints a single-line reply"
          (shell-reply dir "(* 6 7)")
          (format nil "42~%"))

      (stop-ipc-server)
      (cffi:foreign-funcall "unlink" :string path :int)
      (ignore-errors (uiop:delete-empty-directory dir))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
