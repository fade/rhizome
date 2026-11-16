;;; ipc-request-decoding.lisp: a request whose multi-byte UTF-8
;;; character arrives split across two reads decodes intact. The
;;; request is written into one end of a socket pair in two parts,
;;; with a pause between them, while IPC-HANDLE-CONNECTION serves the
;;; other end.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun write-octets (fd octets)
  "Write the octet vector OCTETS to FD in one call."
  (let ((v (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (cffi:with-pointer-to-vector-data (ptr v)
      (cffi:foreign-funcall "write" :int fd :pointer ptr
                            :long (length v) :long))))

(defun split-request-reply (line split-at &optional (seconds 3))
  "Serve one end of a socket pair with IPC-HANDLE-CONNECTION. Write
LINE as UTF-8 plus a newline into the other end in two parts, the
first holding SPLIT-AT octets. Return the reply decoded as UTF-8 up
to its newline, or :TIMEOUT. A handler that signals an error closes
its end, which ends the case."
  (cffi:with-foreign-object (fds :int 2)
    (assert (zerop (cffi:foreign-funcall "socketpair" :int 1 :int 1 :int 0
                                         :pointer fds :int)))
    (let* ((client (cffi:mem-aref fds :int 0))
           (server (cffi:mem-aref fds :int 1))
           (octets (sb-ext:string-to-octets
                    (concatenate 'string line (string #\Newline))
                    :external-format :utf-8))
           (received (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0))
           (handler (sb-thread:make-thread
                     (lambda ()
                       (let ((*standard-output* (make-broadcast-stream)))
                         (handler-case (ipc-handle-connection server)
                           (error ()
                             (cffi:foreign-funcall "close" :int server
                                                   :int)))))
                     :name "ipc-decoding-handler"))
           (reader (sb-thread:make-thread
                    (lambda ()
                      (cffi:with-foreign-object (buf :uint8 4096)
                        (loop
                          (let ((n (cffi:foreign-funcall "read" :int client
                                                         :pointer buf
                                                         :long 4096 :long)))
                            (when (<= n 0) (return))
                            (dotimes (i n)
                              (vector-push-extend (cffi:mem-aref buf :uint8 i)
                                                  received))
                            (when (find 10 received) (return))))))
                    :name "ipc-decoding-reader")))
      (write-octets client (subseq octets 0 split-at))
      (sleep 0.3)
      (write-octets client (subseq octets split-at))
      (loop with deadline = (+ (get-internal-real-time)
                               (* seconds internal-time-units-per-second))
            while (and (sb-thread:thread-alive-p reader)
                       (< (get-internal-real-time) deadline))
            do (let ((*standard-output* (make-broadcast-stream)))
                 (ipc-drain))
               (sleep 0.01))
      (let ((timed-out (sb-thread:thread-alive-p reader)))
        (when timed-out (sb-thread:terminate-thread reader))
        (cffi:foreign-funcall "close" :int client :int)
        (sb-thread:join-thread handler :timeout 5 :default nil)
        (when (sb-thread:thread-alive-p handler)
          (sb-thread:terminate-thread handler))
        (if timed-out
            :timeout
            (let ((reply (sb-ext:octets-to-string
                          (coerce received '(simple-array (unsigned-byte 8) (*)))
                          :external-format '(:utf-8 :replacement #\?))))
              (subseq reply 0 (position #\Newline reply))))))))

(defun octet-offset-inside (line char &optional (into 1))
  "Return the octet offset INTO octets past the start of the first
CHAR in LINE encoded as UTF-8."
  (+ into (length (sb-ext:string-to-octets
                   (subseq line 0 (position char line))
                   :external-format :utf-8))))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (let ((line "(identity \"message héllo ✓\")"))
      (ok "a two-octet character split across reads decodes"
          (split-request-reply line (octet-offset-inside line #\é))
          "message héllo ✓")
      (ok "a three-octet character split after its first octet decodes"
          (split-request-reply line (octet-offset-inside line #\✓ 1))
          "message héllo ✓")
      (ok "a three-octet character split after its second octet decodes"
          (split-request-reply line (octet-offset-inside line #\✓ 2))
          "message héllo ✓")
      (ok "control: a request split between characters decodes"
          (split-request-reply line 5)
          "message héllo ✓")))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
