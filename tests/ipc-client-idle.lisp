;;; ipc-client-idle.lisp: a client that neither reads its replies nor
;;; sends for *IPC-CLIENT-IDLE-SECONDS* is dropped, so it cannot keep
;;; other clients from being served; a client that reads slowly but
;;; steadily, even 16 KB at a time, is kept and gets every reply; every
;;; drop writes one log line naming its reason, the kernel's unread
;;; count, and the WM's queue. Each case runs a listening socket served
;;; by IPC-ACCEPT-LOOP and a thread that drains the command queue, as
;;; the WM's main thread does. Every wait has a deadline, so a client
;;; that is never served fails its case instead of hanging the run.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defparameter *idle-seconds* 1
  "Idle deadline used by every case, in seconds.")

(setf (symbol-value '*ipc-client-idle-seconds*) *idle-seconds*)

(defparameter *big-reply-form*
  "(make-string 4096 :initial-element (code-char 10003))"
  "Eval form whose reply encodes to 12289 octets with its newline.")

(defconstant +big-reply-octets+ 12289)

(defun write-line-octets (fd line)
  "Write LINE as UTF-8 plus a newline to FD in one call."
  (let ((octets (sb-ext:string-to-octets
                 (concatenate 'string line (string #\Newline))
                 :external-format :utf-8)))
    (cffi:with-pointer-to-vector-data (ptr octets)
      (cffi:foreign-funcall "write" :int fd :pointer ptr
                            :long (length octets) :long))))

(defun connect-client (path)
  "Return a new socket fd connected to the unix socket at PATH."
  (let ((fd (%ipc-socket)))
    (cffi:with-foreign-object (addr :uint8 110)
      (cffi:foreign-funcall "memset" :pointer addr :int 0 :long 110 :pointer)
      (setf (cffi:mem-aref addr :uint16 0) 1)
      (loop for i below (length path)
            do (setf (cffi:mem-aref addr :uint8 (+ 2 i))
                     (char-code (char path i))))
      (assert (zerop (cffi:foreign-funcall "connect" :int fd :pointer addr
                                           :uint 110 :int))))
    fd))

(defun recv-now (fd buf size)
  "Read up to SIZE octets from FD into BUF without blocking. Return the
count read, 0 when none are waiting."
  (max 0 (cffi:foreign-funcall "recv" :int fd :pointer buf :long size
                               :int #x40 :long))) ; MSG_DONTWAIT

(defun queued-count ()
  "Number of thunks in the command queue."
  (bt:with-lock-held (*ipc-lock*) (length *ipc-queue*)))

(defvar *case-number* 0)

(defun call-with-server (fn)
  "Listen on a fresh socket path, serve it with IPC-ACCEPT-LOOP, and
drain the command queue from a second thread. Call FN with the path.
Return FN's value and the text the server and drain threads wrote to
*STANDARD-OUTPUT*, as two values."
  (let* ((path (format nil "/tmp/rhizome-idle-test-~D-~D.sock"
                       (sb-unix:unix-getpid) (incf *case-number*)))
         (fd (%ipc-socket))
         (accept-log (make-string-output-stream))
         (drain-log (make-string-output-stream))
         (running t)
         (server nil)
         (drainer nil))
    (cffi:foreign-funcall "unlink" :string path :int)
    (assert (zerop (%ipc-bind fd path)))
    (cffi:foreign-funcall "listen" :int fd :int 8 :int)
    (setf server (sb-thread:make-thread
                  (lambda ()
                    (let ((*standard-output* accept-log))
                      (ipc-accept-loop fd)))
                  :name "ipc-idle-accept")
          drainer (sb-thread:make-thread
                   (lambda ()
                     (let ((*standard-output* drain-log))
                       (loop while running
                             do (ipc-drain)
                                (sleep 0.01))))
                   :name "ipc-idle-drain"))
    (unwind-protect
         (values (funcall fn path)
                 (progn
                   (setf running nil)
                   (sb-thread:join-thread drainer :timeout 3 :default nil)
                   (concatenate 'string
                                (get-output-stream-string accept-log)
                                (get-output-stream-string drain-log))))
      (setf running nil)
      (cffi:foreign-funcall "shutdown" :int fd :int 2 :int)
      (cffi:foreign-funcall "close" :int fd :int)
      (when (sb-thread:thread-alive-p server)
        (ignore-errors (sb-thread:terminate-thread server)))
      (sb-thread:join-thread server :timeout 3 :default nil)
      (cffi:foreign-funcall "unlink" :string path :int)
      (bt:with-lock-held (*ipc-lock*) (setf *ipc-queue* nil)))))

(defun timed-command (path line seconds)
  "Send LINE with IPC-SEND-COMMAND from a fresh thread. Return the reply
and the seconds it took, or :TIMEOUT when no reply arrives within
SECONDS."
  (let* ((start (get-internal-real-time))
         (reply :timeout)
         (elapsed nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (setf reply (handler-case (ipc-send-command path line)
                                  (error () :crashed))
                          elapsed (/ (- (get-internal-real-time) start)
                                     internal-time-units-per-second)))
                  :name "ipc-idle-client-b")))
    (sb-thread:join-thread thread :timeout seconds :default nil)
    (when (sb-thread:thread-alive-p thread)
      (ignore-errors (sb-thread:terminate-thread thread))
      (setf reply :timeout elapsed nil))
    (values reply elapsed)))

(defun drop-lines (log)
  "Return the lines of LOG that record a dropped IPC client."
  (remove-if-not (lambda (line) (search "ipc: dropped client" line))
                 (uiop:split-string log :separator '(#\Newline))))

(defun unread-client-case ()
  "Client A sends enough big requests to queue more than 300 KB of
replies and never reads or closes. Client B then sends (+ 1 2).
Return B's reply, whether it came within *IDLE-SECONDS* + 2 seconds,
and the drop log lines, as a list."
  (let ((a nil))
    (multiple-value-bind (result log)
        (call-with-server
         (lambda (path)
           (setf a (connect-client path))
           (dotimes (i 40) (write-line-octets a *big-reply-form*))
           (loop repeat 300
                 until (zerop (queued-count))
                 do (sleep 0.01))
           (sleep 0.2)
           (multiple-value-bind (reply elapsed)
               (timed-command path "(+ 1 2)" (+ *idle-seconds* 2))
             (prog1 (list reply (and elapsed t))
               (cffi:foreign-funcall "close" :int a :int)))))
      (append result (list (drop-lines log))))))

(defun silent-client-case ()
  "Client A connects and sends nothing. Client B then sends (+ 1 2).
Return B's reply, whether it came within *IDLE-SECONDS* + 2 seconds,
and the drop log lines, as a list."
  (multiple-value-bind (result log)
      (call-with-server
       (lambda (path)
         (let ((a (connect-client path)))
           (sleep 0.1)
           (multiple-value-bind (reply elapsed)
               (timed-command path "(+ 1 2)" (+ *idle-seconds* 2))
             (prog1 (list reply (and elapsed t))
               (cffi:foreign-funcall "close" :int a :int))))))
    (append result (list (drop-lines log)))))

(defun steady-reader-case (count)
  "Client A sends COUNT big requests, then reads at most 64 KB every
half deadline until every reply is in or 20 seconds pass. Return the
newline count, the octets received, and the drop log lines, as a
list."
  (multiple-value-bind (result log)
      (call-with-server
       (lambda (path)
         (let ((a (connect-client path))
               (newlines 0)
               (octets 0)
               (deadline (+ (get-internal-real-time)
                            (* 20 internal-time-units-per-second))))
           (dotimes (i count) (write-line-octets a *big-reply-form*))
           (cffi:with-foreign-object (buf :uint8 65536)
             (loop until (or (= newlines count)
                             (> (get-internal-real-time) deadline))
                   do (sleep (/ *idle-seconds* 2))
                      (let ((n (recv-now a buf 65536)))
                        (incf octets n)
                        (dotimes (i n)
                          (when (= 10 (cffi:mem-aref buf :uint8 i))
                            (incf newlines))))))
           (cffi:foreign-funcall "close" :int a :int)
           (list newlines octets))))
    (append result (list (drop-lines log)))))

(defun over-cap-case ()
  "With the output cap at 100000 octets, client A sends 40 big requests,
more than the socket buffers and the cap hold, and never reads. Return
the drop log lines."
  (let ((saved *ipc-client-output-cap*))
    (setf *ipc-client-output-cap* 100000)
    (unwind-protect
         (drop-lines
          (nth-value
           1 (call-with-server
              (lambda (path)
                (let ((a (connect-client path)))
                  (dotimes (i 40) (write-line-octets a *big-reply-form*))
                  (loop repeat 300
                        until (zerop (queued-count))
                        do (sleep 0.01))
                  (sleep 0.5)
                  (cffi:foreign-funcall "close" :int a :int))))))
      (setf *ipc-client-output-cap* saved))))

(defun small-read-case (count)
  "Client A sends COUNT big requests, then reads at most 16384 octets
every 0.4 deadlines until every reply is in, the server closes, or 30
seconds pass. Each read is far smaller than the socket buffer, as with
rhizome-cmd reading a long reply. Return the newline count, the octets
received, and the drop log lines, as a list."
  (multiple-value-bind (result log)
      (call-with-server
       (lambda (path)
         (let ((a (connect-client path))
               (newlines 0)
               (octets 0)
               (deadline (+ (get-internal-real-time)
                            (* 30 internal-time-units-per-second))))
           (dotimes (i count) (write-line-octets a *big-reply-form*))
           (cffi:with-foreign-object (buf :uint8 16384)
             (loop until (or (= newlines count)
                             (> (get-internal-real-time) deadline))
                   do (sleep (* 0.4 *idle-seconds*))
                      (let ((n (cffi:foreign-funcall
                                "recv" :int a :pointer buf :long 16384
                                :int #x40 :long)))
                        (when (zerop n) (return))
                        (when (plusp n)
                          (incf octets n)
                          (dotimes (i n)
                            (when (= 10 (cffi:mem-aref buf :uint8 i))
                              (incf newlines)))))))
           (cffi:foreign-funcall "close" :int a :int)
           (list newlines octets))))
    (append result (list (drop-lines log)))))

(defun one-line-naming-p (lines reason)
  "True when LINES is one line that names REASON, the kernel's unread
count, and the WM's queue."
  (and (= 1 (length lines))
       (search reason (first lines))
       (search "kernel unread" (first lines))
       (search "WM queue" (first lines))
       t))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (destructuring-bind (reply in-time lines) (unread-client-case)
      (format t "~&; unread client drop log: ~S~%" lines)
      (ok "a client with unread replies does not keep another client waiting"
          (list reply in-time) '("3" t))
      (ok "dropping a client with unread replies logs one line"
          (one-line-naming-p lines "idle") t))

    (destructuring-bind (reply in-time lines) (silent-client-case)
      (format t "~&; silent client drop log: ~S~%" lines)
      (ok "a client that sends nothing does not keep another client waiting"
          (list reply in-time) '("3" t))
      (ok "dropping a client that sends nothing logs one line"
          (one-line-naming-p lines "idle") t))

    (destructuring-bind (newlines octets lines) (steady-reader-case 30)
      (ok "a slow but steady reader receives every reply"
          (list newlines octets) (list 30 (* 30 +big-reply-octets+)))
      (ok "a slow but steady reader is not dropped" lines '()))

    (destructuring-bind (newlines octets lines) (small-read-case 40)
      (format t "~&; small-read client drop log: ~S~%" lines)
      (ok "a reader taking 16 KB at a time receives every reply"
          (list newlines octets) (list 40 (* 40 +big-reply-octets+)))
      (ok "a reader taking 16 KB at a time is not dropped" lines '()))

    (let ((lines (over-cap-case)))
      (format t "~&; output cap drop log: ~S~%" lines)
      (ok "dropping a client over the output cap logs one line"
          (one-line-naming-p lines "output cap") t)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
