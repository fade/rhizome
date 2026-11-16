;;; ipc-reply-printing.lisp: IPC and eval-prompt replies print any
;;; result without killing or hanging the WM, keep ordinary replies
;;; unchanged, and reach the client whole when they hold non-ASCII
;;; text or run past one read. Each case runs in a thread with a
;;; deadline, so a reply path that loops or exhausts the stack fails
;;; its case instead of the run.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Remove this run's socket when the run exits, whether it passes,
;; fails a case, or crashes
(push (lambda ()
        (cffi:foreign-funcall
         "unlink" :string (format nil "/tmp/rhizome-test-reply-printing-~D.sock"
                                  (cffi:foreign-funcall "getpid" :int))
         :int))
      sb-ext:*exit-hooks*)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun run-bounded (thunk &optional (seconds 5))
  "Call THUNK in a fresh thread with output discarded. Return :OK and
its value, :CRASHED and the condition when it signals any serious
condition (a stack exhaustion included), or :TIMEOUT when it is still
running after SECONDS."
  (let* ((status :timeout)
         (value nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (let ((*standard-output* (make-broadcast-stream)))
                      (handler-case
                          (setf value (funcall thunk)
                                status :ok)
                        (serious-condition (c)
                          (setf value c
                                status :crashed)))))
                  :name "ipc-reply-printing-case")))
    (handler-case
        (sb-thread:join-thread thread :timeout seconds :default nil)
      (sb-thread:join-thread-error () nil))
    (when (sb-thread:thread-alive-p thread)
      (sb-thread:terminate-thread thread)
      (setf status :timeout))
    (values status value)))

(defun circular-frame ()
  "A root frame and its child, each pointing at the other."
  (let* ((parent (make-frame))
         (child (make-frame :parent parent)))
    (setf (frame-first-child parent) child)
    parent))

(defun raw-ipc-reply (path line &optional (seconds 3))
  "Send LINE to the IPC socket at PATH as UTF-8 and return the octets
received within SECONDS, as a vector of (unsigned-byte 8). Stop at the
first newline octet. The caller drains the command queue meanwhile."
  (let ((fd (%ipc-socket))
        (received (make-array 0 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0)))
    (assert (< (length path) 108))
    (cffi:with-foreign-object (addr :uint8 110)
      (cffi:foreign-funcall "memset" :pointer addr :int 0 :long 110 :pointer)
      (setf (cffi:mem-aref addr :uint16 0) 1)
      (loop for i from 0 for c across path
            do (setf (cffi:mem-aref addr :uint8 (+ 2 i)) (char-code c)))
      (assert (zerop (cffi:foreign-funcall "connect" :int fd :pointer addr
                                           :uint 110 :int))))
    (let* ((octets (sb-ext:string-to-octets
                    (concatenate 'string line (string #\Newline))
                    :external-format :utf-8))
           (reader
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
              :name "raw-ipc-reader")))
      (cffi:with-pointer-to-vector-data (ptr octets)
        (cffi:foreign-funcall "write" :int fd :pointer ptr
                              :long (length octets) :long))
      (loop with deadline = (+ (get-internal-real-time)
                               (* seconds internal-time-units-per-second))
            while (and (sb-thread:thread-alive-p reader)
                       (< (get-internal-real-time) deadline))
            do (ipc-drain) (sleep 0.01))
      (when (sb-thread:thread-alive-p reader)
        (sb-thread:terminate-thread reader))
      (cffi:foreign-funcall "close" :int fd :int)
      (coerce received '(simple-array (unsigned-byte 8) (*))))))

(defun client-reply (path line &optional (seconds 3))
  "Send LINE with IPC-SEND-COMMAND from a second thread and return its
reply, :CRASHED when the client signals an error, or :TIMEOUT. The
calling thread drains the command queue meanwhile."
  (let* ((reply :timeout)
         (client (sb-thread:make-thread
                  (lambda ()
                    (setf reply (handler-case (ipc-send-command path line)
                                  (error () :crashed))))
                  :name "ipc-client")))
    (loop with deadline = (+ (get-internal-real-time)
                             (* seconds internal-time-units-per-second))
          while (and (sb-thread:thread-alive-p client)
                     (< (get-internal-real-time) deadline))
          do (ipc-drain) (sleep 0.01))
    (when (sb-thread:thread-alive-p client)
      (sb-thread:terminate-thread client))
    reply))

(defun socketpair-transfer (string)
  "Write STRING with %IPC-WRITE-STRING into one end of a socket pair
while a second thread reads the other end to EOF. Return the value
%IPC-WRITE-STRING returned and the octets received."
  (cffi:with-foreign-object (fds :int 2)
    (assert (zerop (cffi:foreign-funcall "socketpair" :int 1 :int 1 :int 0
                                         :pointer fds :int)))
    (let* ((writer-fd (cffi:mem-aref fds :int 0))
           (reader-fd (cffi:mem-aref fds :int 1))
           (received (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0))
           (reader
             (sb-thread:make-thread
              (lambda ()
                (cffi:with-foreign-object (buf :uint8 4096)
                  (loop
                    (let ((n (cffi:foreign-funcall "read" :int reader-fd
                                                   :pointer buf
                                                   :long 4096 :long)))
                      (when (<= n 0) (return))
                      (dotimes (i n)
                        (vector-push-extend (cffi:mem-aref buf :uint8 i)
                                            received))))))
              :name "socketpair-reader"))
           (written (%ipc-write-string writer-fd string)))
      (cffi:foreign-funcall "close" :int writer-fd :int)
      (sb-thread:join-thread reader :timeout 10 :default nil)
      (cffi:foreign-funcall "close" :int reader-fd :int)
      (values written (coerce received '(simple-array (unsigned-byte 8) (*)))))))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (reply (line)
           "Dispatch LINE under a deadline: the reply string, or the
status keyword when the dispatch crashed or timed out."
           (multiple-value-bind (status value)
               (run-bounded (lambda () (ipc-dispatch-line line)))
             (if (eq status :ok) value status)))
         (prefixp (prefix string)
           (and (stringp string)
                (>= (length string) (length prefix))
                (string= prefix string :end2 (length prefix)))))

    ;; --- Circular, long, and deep results ---
    (let ((r (reply "(circular-frame)")))
      (ok "eval of a circular frame replies with a bounded string"
          (and (prefixp "#S(FRAME" r)
               (<= (length r) (+ *reply-max-length* 3)))
          t))
    (let ((r (reply "(let ((l (list 1 2 3))) (setf (cdr (last l)) l) l)")))
      (ok "eval of a circular list replies with a bounded string"
          (and (prefixp "(1 2 3 1 2 3" r) (< (length r) 2100)
               (search "...)" r :from-end t) t)
          t))
    (let ((r (reply "(make-list 5000 :initial-element 7)")))
      (ok "eval of a long list replies with a bounded string"
          (and (stringp r) (< (length r) 2100)
               (search "...)" r :from-end t) t)
          t))
    (let ((r (reply "(let ((d nil)) (dotimes (i 100000) (setf d (list d))) d)")))
      (ok "eval of a deeply nested list replies"
          (and (stringp r) (find #\# r) t) t))
    (let ((r (reply "(let ((x 1)) (dotimes (i 40) (setf x (list x x))) x)")))
      (ok "eval of a widely shared tree replies cut at the length cap"
          (and (stringp r) (= (length r) (+ *reply-max-length* 3))
               (search "..." r :from-end t) t)
          t))
    (ok "error whose report holds a circular value replies"
        (prefixp "error: "
                 (reply "(error 'type-error :datum (circular-frame) :expected-type 'integer)"))
        t)
    (ok "error while printing a result replies"
        (prefixp "error: "
                 (reply "(progn (defstruct (unprintable (:print-function (lambda (o s d) (declare (ignore o s d)) (error \"cannot print\"))))) (make-unprintable))"))
        t)

    ;; --- Command replies ---
    (setf (gethash :reply-printing-circular *command-table*)
          (lambda (output args)
            (declare (ignore output args))
            (circular-frame)))
    (setf (gethash :reply-printing-plain *command-table*)
          (lambda (output args)
            (declare (ignore output))
            (format nil "workspace ~A" (first args))))
    (ok "command returning a circular frame replies"
        (prefixp "#S(FRAME" (reply "reply-printing-circular")) t)
    (ok "command returning a string replies unchanged"
        (reply "reply-printing-plain 4") "workspace 4")
    (remhash :reply-printing-circular *command-table*)
    (remhash :reply-printing-plain *command-table*)

    ;; --- Ordinary eval replies stay byte-identical ---
    (ok "integer reply" (reply "(+ 1 2)") "3")
    (ok "multiple values reply" (reply "(values 1 2)") "1 2")
    (ok "string reply" (reply "(identity \"abc\")") "abc")
    (ok "NIL reply" (reply "(identity nil)") "NIL")
    (ok "mixed list reply" (reply "(list 1 \"a\" :b 2.5)") "(1 a B 2.5)")
    (ok "symbol reply" (reply "(quote foo)") "FOO")
    (ok "short list reply" (reply "(list 1 2 3)") "(1 2 3)")
    (ok "300-element list reply"
        (reply "(make-list 300 :initial-element 1)")
        (format nil "~A" (make-list 300 :initial-element 1)))
    (ok "plain error reply" (reply "(error \"nope\")") "error: nope")
    ;; Shared structure prints in full, without #n= labels
    (ok "shared string reply"
        (reply "(let ((s \"ab\")) (list s s))") "(ab ab)")
    (ok "shared list reply"
        (reply "(let ((x (list 1))) (list x x))") "((1) (1))")
    (ok "control: *print-circle* true labels the shared string"
        (let ((*print-circle* t))
          (format nil "~A" (let ((s "ab")) (list s s))))
        "(#1=ab #1#)")

    ;; --- Eval prompt and the command queue ---
    (setf *eval-prompt-result* nil)
    (multiple-value-bind (status)
        (run-bounded (lambda () (eval-prompt-eval "(rhizome::circular-frame)")))
      (ok "eval prompt survives a circular frame" status :ok))
    (ok "eval prompt shows the circular frame"
        (prefixp "#S(FRAME" *eval-prompt-result*) t)
    (multiple-value-bind (status)
        (run-bounded
         (lambda ()
           (ipc-enqueue
            (lambda ()
              (error 'type-error :datum (circular-frame)
                                 :expected-type 'integer)))
           (ipc-drain)))
      (ok "command queue survives an error holding a circular value"
          status :ok))

    ;; --- Replies over the socket ---
    (let ((path (format nil "/tmp/rhizome-test-reply-printing-~D.sock"
                        (cffi:foreign-funcall "getpid" :int))))
      (cffi:foreign-funcall "unlink" :string path :int)
      (stop-ipc-server)
      (setf *ipc-thread* nil *ipc-fd* nil)
      (start-ipc-server path)
      (sleep 0.2)
      ;; The pretty-printed frame spans several lines; the reader
      ;; returns once a newline has arrived
      (ok "circular frame reply arrives over the socket"
          (let ((octets (raw-ipc-reply path "(circular-frame)")))
            (and (find 10 octets)
                 (prefixp "#S(FRAME"
                          (sb-ext:octets-to-string octets
                                                   :external-format :utf-8))))
          t)
      (ok "non-ASCII reply arrives whole with its newline"
          (coerce (raw-ipc-reply path "(identity \"café ✓\")") 'list)
          (coerce (sb-ext:string-to-octets (format nil "café ✓~%")
                                           :external-format :utf-8)
                  'list))
      (ok "ASCII reply arrives with the same octets as before"
          (coerce (raw-ipc-reply path "(+ 1 2)") 'list)
          (list 51 10))
      (let ((long (make-string 3000 :initial-element #\LATIN_SMALL_LETTER_E_WITH_ACUTE)))
        (ok "non-ASCII reply longer than one read arrives whole"
            (equalp (raw-ipc-reply path "(make-string 3000 :initial-element (code-char 233))")
                    (sb-ext:string-to-octets (format nil "~A~%" long)
                                             :external-format :utf-8))
            t)
        (ok "client decodes a non-ASCII reply longer than one read"
            (client-reply path "(make-string 3000 :initial-element (code-char 233))")
            long))
      ;; --- A reply larger than the socket buffer is written in full ---
      (let* ((big (with-output-to-string (s)
                    (dotimes (i 400000) (write-string "é✓" s))))
             (expected (sb-ext:string-to-octets (format nil "~A~%" big)
                                                :external-format :utf-8)))
        (multiple-value-bind (written received) (socketpair-transfer big)
          (ok "large reply reports every encoded octet written"
              written (length expected))
          (ok "large reply arrives byte for byte"
              (equalp received expected) t)))
      (stop-ipc-server)
      (cffi:foreign-funcall "unlink" :string path :int)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
