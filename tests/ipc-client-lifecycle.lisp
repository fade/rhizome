;;; ipc-client-lifecycle.lisp: a reply queued for a client that has
;;; gone is dropped, and never reaches a descriptor that reused the
;;; client's number; a client that shuts its write side after the
;;; request still gets the reply. Each client is one end of a socket
;;; pair whose other end IPC-HANDLE-CONNECTION serves.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun write-line-octets (fd line)
  "Write LINE as UTF-8 plus a newline to FD in one call."
  (let ((octets (sb-ext:string-to-octets
                 (concatenate 'string line (string #\Newline))
                 :external-format :utf-8)))
    (cffi:with-pointer-to-vector-data (ptr octets)
      (cffi:foreign-funcall "write" :int fd :pointer ptr
                            :long (length octets) :long))))

(defun make-socket-pair ()
  "Return the two descriptors of a new AF_UNIX stream socket pair."
  (cffi:with-foreign-object (fds :int 2)
    (assert (zerop (cffi:foreign-funcall "socketpair" :int 1 :int 1 :int 0
                                         :pointer fds :int)))
    (values (cffi:mem-aref fds :int 0) (cffi:mem-aref fds :int 1))))

(defun pending-octets (fd)
  "Read whatever FD holds now without blocking. Return the count of
octets read, 0 when none are waiting."
  (cffi:with-foreign-object (buf :uint8 4096)
    (max 0 (cffi:foreign-funcall "recv" :int fd :pointer buf :long 4096
                                 :int #x40 :long)))) ; MSG_DONTWAIT

(defun queued-count ()
  "Number of thunks in the command queue."
  (bt:with-lock-held (*ipc-lock*) (length *ipc-queue*)))

(defun serve (fd)
  "Start a thread running IPC-HANDLE-CONNECTION on FD."
  (sb-thread:make-thread
   (lambda ()
     (let ((*standard-output* (make-broadcast-stream)))
       (ipc-handle-connection fd)))
   :name "ipc-lifecycle-handler"))

(defun quiet-drain ()
  "Run IPC-DRAIN with its output discarded."
  (let ((*standard-output* (make-broadcast-stream)))
    (ipc-drain)))

(defun wait-for-queue (count)
  "Wait up to 3 seconds for COUNT thunks in the command queue."
  (loop repeat 300 until (>= (queued-count) count) do (sleep 0.01)))

(defun gone-client-case ()
  "Queue a reply for a client, close the client, and open a socket
pair that may reuse the served descriptor's number. Drain the queue.
Return the octets the new pair received and whether it reused the
number, as a list."
  (multiple-value-bind (client server) (make-socket-pair)
    (let ((handler (serve server)))
      (write-line-octets client "(identity \"late reply\")")
      (wait-for-queue 1)
      (cffi:foreign-funcall "close" :int client :int)
      (sleep 0.3)
      (multiple-value-bind (a b) (make-socket-pair)
        (quiet-drain)
        (sleep 0.3)
        (let ((received (+ (pending-octets a) (pending-octets b)))
              (reused (or (= a server) (= b server))))
          (sb-thread:join-thread handler :timeout 3 :default nil)
          (cffi:foreign-funcall "close" :int a :int)
          (cffi:foreign-funcall "close" :int b :int)
          (bt:with-lock-held (*ipc-lock*) (setf *ipc-queue* nil))
          (list received reused (sb-thread:thread-alive-p handler)))))))

(defun half-closed-client-reply ()
  "Send a request, shut the client's write side, drain, and return
the reply octets decoded up to the newline, or :TIMEOUT."
  (multiple-value-bind (client server) (make-socket-pair)
    (let ((handler (serve server))
          (received (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
      (write-line-octets client "(identity \"half closed\")")
      (cffi:foreign-funcall "shutdown" :int client :int 1 :int) ; SHUT_WR
      (wait-for-queue 1)
      (sleep 0.2)
      (quiet-drain)
      (loop with deadline = (+ (get-internal-real-time)
                               (* 3 internal-time-units-per-second))
            until (or (find 10 received)
                      (> (get-internal-real-time) deadline))
            do (cffi:with-foreign-object (buf :uint8 4096)
                 (let ((n (cffi:foreign-funcall "recv" :int client :pointer buf
                                                :long 4096 :int #x40 :long)))
                   (if (plusp n)
                       (dotimes (i n)
                         (vector-push-extend (cffi:mem-aref buf :uint8 i)
                                             received))
                       (sleep 0.01)))))
      (cffi:foreign-funcall "close" :int client :int)
      (sb-thread:join-thread handler :timeout 3 :default nil)
      (bt:with-lock-held (*ipc-lock*) (setf *ipc-queue* nil))
      (if (find 10 received)
          (let ((s (sb-ext:octets-to-string
                    (coerce received '(simple-array (unsigned-byte 8) (*)))
                    :external-format :utf-8)))
            (subseq s 0 (position #\Newline s)))
          :timeout))))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (destructuring-bind (received reused alive) (gone-client-case)
      (format t "~&; served descriptor number reused by the new pair: ~A~%"
              reused)
      (ok "a reply to a gone client reaches no other descriptor"
          received 0)
      (ok "the handler of a gone client ends" alive nil))
    (ok "a client that shuts its write side gets its reply"
        (half-closed-client-reply) "half closed"))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
