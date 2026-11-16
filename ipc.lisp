;;; IPC for Rhizome: the stumpish equivalent
;;;
;;; A Unix domain socket ($XDG_RUNTIME_DIR/rhizome.sock, or
;;; /tmp/rhizome-UID.sock when XDG_RUNTIME_DIR is unset or empty) that
;;; accepts one command per line and replies with the result:
;;;
;;;   rhizome-cmd switch-workspace 3
;;;   rhizome-cmd current-workspace
;;;   rhizome-cmd "(+ 1 2)"            ; Lisp forms are evaluated
;;;
;;; Each reply is one line: a newline inside it is sent as backslash n
;;; and a backslash as two backslashes (see IPC-ESCAPE-REPLY).
;;;
;;; Safety model: the socket thread only reads and writes bytes.
;;; Commands are enqueued and executed by the WM's main thread (see
;;; the poll loop in wm-protocol.lisp), so no Wayland request is ever
;;; made from a foreign thread. The main thread never blocks on a
;;; client: a reply the socket does not take at once is held for the
;;; socket thread to send.

(in-package #:rhizome)

;;; --- Command queue (drained by the main loop) ---

(defvar *ipc-queue* nil
  "Pending command thunks, executed by the WM's main thread.")
(defvar *ipc-lock* (bt:make-lock "rhizome-ipc")
  "Protects *ipc-queue*.")

(defun ipc-enqueue (thunk)
  "Queue THUNK for execution by the WM's main thread."
  (bt:with-lock-held (*ipc-lock*)
    (push thunk *ipc-queue*)))

(defun ipc-drain ()
  "Execute all queued commands on the calling (main) thread."
  (loop
    (let ((thunk nil))
      (bt:with-lock-held (*ipc-lock*)
        (setf thunk (pop *ipc-queue*)))
      (unless thunk (return))
      (handler-case
          (funcall thunk)
        (serious-condition (e)
          (format t "~&; Rhizome: ipc command error: ~A~%"
                  (format-reply "~A" e)))))))

;;; --- Command dispatch ---

(defvar *command-reply-seconds* 1
  "Seconds ipc-dispatch-line shows a command's reply (see
show-command-reply).")

(defun parse-command-line (line)
  "Split a NAME ARGS... command LINE. Return the command keyword and
its argument list. Arguments are split on whitespace and each one
that reads as an integer becomes that integer; a command listed in
*rest-of-line-commands*, or an alias of one, gets the rest of the
line as one string."
  (let* ((name-and-rest (cl-ppcre:split "\\s+" line :limit 2))
         (command (intern (string-upcase (first name-and-rest)) :keyword))
         (rest (second name-and-rest)))
    (values command
            (cond ((or (null rest) (zerop (length rest))) nil)
                  ((member (canonical-command-name command)
                           *rest-of-line-commands*)
                   (list rest))
                  (t (mapcar (lambda (token)
                               (or (ignore-errors (parse-integer token))
                                   token))
                             (cl-ppcre:split "\\s+" rest)))))))

(defun eval-form-reply (text)
  "Read one form from TEXT, evaluate it in the RHIZOME package, and
return its values printed with FORMAT-REPLY, separated by spaces, and
cut to *REPLY-MAX-LENGTH* characters plus ... when longer. A serious
condition while reading or evaluating returns \"error: \" and the
condition text, cut the same way."
  (handler-case
      (let ((*package* (find-package "RHIZOME")))
        (format-reply "~{~A~^ ~}"
                      (multiple-value-list
                        (eval (read-from-string text)))))
    (serious-condition (e)
      (format-reply "error: ~A" e))))

(defun ipc-dispatch-line (line)
  "Execute one command LINE and return the result as a string.
A line starting with ( is evaluated as a Lisp form in the RHIZOME
package; otherwise it is NAME ARGS..., split by parse-command-line and
looked up with find-command.
The result is printed with FORMAT-REPLY, so any value, and any serious
condition from the evaluation or the command, yields a bounded string.
A command's reply is shown on screen only when it is non-empty and the
command showed no message of its own."
  (let ((line (string-trim " " (remove #\newline line))))
    (cond
      ((zerop (length line)) "")
      ((char= (char line 0) #\()
       (let ((result (eval-form-reply line)))
         (show-message result 1)
         result))
      (t
       (multiple-value-bind (command args) (parse-command-line line)
         (let ((handler (find-command command)))
           (cond
             (handler
               (let ((serial *message-serial*)
                      (result
                        (handler-case
                            (let ((r (funcall handler (current-output)
                                                   args)))
                              (format-reply "~A" (or r "")))
                          (serious-condition (e)
                            (format-reply "error: ~A" e)))))
                 (show-command-reply result serial *command-reply-seconds*)))
             (t
               (format nil "unknown command: ~A (try: ~{~A~^ ~})"
                       command (command-names))))))))))

;;; --- Unix domain socket via CFFI (no extra dependencies) ---

(defun ipc-socket-path ()
  "Path of the IPC socket: $XDG_RUNTIME_DIR/rhizome.sock, or
/tmp/rhizome-UID.sock when XDG_RUNTIME_DIR is unset or empty.
session/rhizome-cmd computes the same path."
  (let ((runtime-dir (uiop:getenv "XDG_RUNTIME_DIR")))
    (if (and runtime-dir (plusp (length runtime-dir)))
        (concatenate 'string runtime-dir "/rhizome.sock")
        (format nil "/tmp/rhizome-~A.sock"
                (cffi:foreign-funcall "getuid" :int)))))

(defparameter *ipc-thread* nil
  "The accept-thread for the IPC socket.")
(defparameter *ipc-fd* nil
  "Listening socket fd.")

(defun %ipc-socket ()
  (cffi:foreign-funcall "socket" :int 1 :int 1 :int 0 :int)) ; AF_UNIX, SOCK_STREAM

(defun %ipc-bind (fd path)
  "Bind FD to PATH (a unix socket address)."
  (cffi:with-foreign-object (addr :uint8 110)
    (cffi:foreign-funcall "memset" :pointer addr :int 0 :long 110 :pointer)
    (setf (cffi:mem-aref addr :uint16 0) 1) ; AF_UNIX
    (cffi:with-foreign-string (cpath path)
      (loop for i below (min 107 (length path))
            do (setf (cffi:mem-aref addr :uint8 (+ 2 i))
                     (cffi:mem-aref cpath :uint8 i))))
    (cffi:foreign-funcall "bind" :int fd :pointer addr :uint 110 :int)))

(defun %ipc-write-string (fd string)
  "Write STRING to FD as UTF-8 followed by a newline. Write every
encoded octet, retrying short writes and writes interrupted by a
signal. Return the number of octets written, or NIL when write fails."
  (let* ((octets (sb-ext:string-to-octets
                  (concatenate 'string string (string #\newline))
                  :external-format :utf-8))
         (total (length octets)))
    (cffi:with-pointer-to-vector-data (ptr octets)
      (loop with offset = 0
            while (< offset total)
            do (let ((n (cffi:foreign-funcall "write" :int fd
                                              :pointer (cffi:inc-pointer ptr offset)
                                              :long (- total offset) :long)))
                 (cond ((plusp n) (incf offset n))
                       ((and (minusp n) (= (sb-alien:get-errno) sb-unix:eintr)))
                       (t (return nil))))
            finally (return total)))))

;;; --- Reply framing ---
;;;
;;; Every reply is one line on the wire. A newline inside the reply is
;;; sent as backslash n and a backslash as two backslashes, so a reply
;;; without either keeps the octets it would have unescaped.

(defun ipc-escape-reply (string)
  "Return STRING with each backslash doubled and each newline written
as backslash n."
  (if (not (find-if (lambda (c) (or (char= c #\\) (char= c #\Newline)))
                    string))
      string
      (with-output-to-string (out)
        (loop for c across string
              do (case c
                   (#\\ (write-string "\\\\" out))
                   (#\Newline (write-string "\\n" out))
                   (t (write-char c out)))))))

(defun ipc-unescape-reply (string)
  "Return STRING with IPC-ESCAPE-REPLY undone: backslash n becomes a
newline and two backslashes become one. A backslash before any other
character, or at the end, is kept."
  (if (not (find #\\ string))
      string
      (with-output-to-string (out)
        (loop with end = (length string)
              with i = 0
              while (< i end)
              do (let ((c (char string i)))
                   (if (and (char= c #\\) (< (1+ i) end)
                            (member (char string (1+ i)) '(#\n #\\)))
                       (progn
                         (write-char (if (char= (char string (1+ i)) #\n)
                                         #\Newline
                                         #\\)
                                     out)
                         (incf i 2))
                       (progn (write-char c out)
                              (incf i))))))))

(defun ipc-reply-octets (reply)
  "Return REPLY as one wire line: escaped with IPC-ESCAPE-REPLY,
ended by a newline, and encoded as UTF-8."
  (sb-ext:string-to-octets
   (concatenate 'string (ipc-escape-reply reply) (string #\Newline))
   :external-format :utf-8))

;;; --- Connections ---
;;;
;;; The socket thread owns a client's fd: it alone closes it, and it
;;; marks the client :CLOSED, with the client's lock held, before it
;;; does. The main thread only appends replies and sends without
;;; blocking, under the same lock, so a reply for a closed client is
;;; dropped and never reaches an fd number the kernel has reused.

(defconstant +ipc-pollin+ #x001)

(defconstant +ipc-pollout+ #x004)

(defconstant +ipc-pollerr+ #x008)

(defconstant +ipc-pollhup+ #x010)

(defconstant +ipc-pollnval+ #x020)

(defconstant +ipc-send-flags+ (logior #x40 #x4000)
  "MSG_DONTWAIT and MSG_NOSIGNAL: send never blocks, and a send to a
gone peer returns EPIPE instead of raising SIGPIPE.")

(defparameter *ipc-client-output-cap* (* 1024 1024)
  "Most reply octets held for one client that is not reading. A reply
that would take a client past it disconnects the client.")

(defparameter *ipc-client-linger-seconds* 10
  "Seconds a client that has shut its write side is kept open for the
replies still queued for it.")

(defparameter *ipc-client-idle-seconds* 5
  "Seconds a client may go without sending an octet or reading a reply
octet before it is dropped; (:ipc :client-idle-seconds) overrides it.
A read counts once it finishes one send of up to +IPC-SEND-CHUNK+
octets.
The socket thread serves one client at a time, so this bounds how long
a stalled client keeps the others waiting. Time spent waiting for the
main thread to answer does not count. Five seconds is far longer than
rhizome-cmd needs to send and read, and short enough that a
rhizome-cmd queued behind a stalled client is served within its own
10 s timeout.")

(defun ipc-client-idle-seconds ()
  "Return the idle deadline in seconds: (:ipc :client-idle-seconds)
when configured, otherwise *IPC-CLIENT-IDLE-SECONDS*."
  (config-getf '(:ipc) :client-idle-seconds
               :default *ipc-client-idle-seconds*))

(defun %ipc-unread-octets (fd)
  "Return the octets sent on socket FD that its peer has not read yet,
as the kernel counts them (SIOCOUTQ, which includes buffer overhead),
or 0 when the kernel does not report it."
  (cffi:with-foreign-object (count :int)
    (if (zerop (cffi:foreign-funcall "ioctl" :int fd :unsigned-long #x5411
                                     :pointer count :int))
        (max 0 (cffi:mem-ref count :int))
        0)))

(defun %ipc-log-drop (fd reason unread held)
  "Log one line recording that the client on FD was dropped for
REASON, a string. UNREAD is the kernel's count for the socket (see
%IPC-UNREAD-OCTETS); HELD is the reply octets the WM held and had not
yet sent."
  (format t "~&; Rhizome: ipc: dropped client on fd ~D (~A), kernel unread ~D bytes, WM queue ~D bytes~%"
          fd reason unread held))

(defstruct (ipc-client (:constructor make-ipc-client (fd)))
  "One IPC connection. Every slot but FD and LOCK is read and written
with LOCK held. STATE is :OPEN, :DROPPED (the socket thread is to
close the client), or :CLOSED. PENDING counts requests queued and not
yet answered. OUTPUT holds OUTPUT-LENGTH octets not yet sent.
PROGRESS-AT is the internal real time the client last sent an octet,
read a reply octet, or was waiting on the main thread."
  (fd -1 :type fixnum)
  (lock (bt:make-lock "rhizome-ipc-client"))
  (state :open)
  (pending 0 :type fixnum)
  (output (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (output-length 0 :type fixnum)
  (progress-at (get-internal-real-time)))

(defun %ipc-take-lines (acc)
  "Remove every newline-terminated line from ACC, an octet vector with
a fill pointer, and return the lines decoded from UTF-8, in order.
Octets after the last newline stay in ACC. An invalid UTF-8 sequence
decodes as U+FFFD."
  (let ((start 0)
        (lines nil)
        (format (list :utf-8 :replacement (code-char #xfffd))))
    (loop for pos = (position 10 acc :start start)
          while pos
          do (push (sb-ext:octets-to-string acc :start start :end pos
                                                :external-format format)
                   lines)
             (setf start (1+ pos)))
    (when (plusp start)
      (replace acc acc :start2 start)
      (setf (fill-pointer acc) (- (fill-pointer acc) start)))
    (nreverse lines)))

(defun %ipc-client-append (client octets)
  "Append OCTETS to CLIENT's held output. Call with its lock held."
  (let* ((length (ipc-client-output-length client))
         (needed (+ length (length octets)))
         (buffer (ipc-client-output client)))
    (when (> needed (length buffer))
      (let ((grown (make-array (max needed (* 2 (length buffer)))
                               :element-type '(unsigned-byte 8))))
        (replace grown buffer :end2 length)
        (setf buffer grown
              (ipc-client-output client) grown)))
    (replace buffer octets :start1 length)
    (setf (ipc-client-output-length client) needed)))

(defconstant +ipc-send-chunk+ 8192
  "Most octets passed to one send call. The kernel's unread count for a
Unix stream socket falls only once the peer has read all of one send,
so a reader taking 16 KB at a time shows progress on every read only
when each send is smaller than that.")

(defun %ipc-client-flush (client)
  "Send as much of CLIENT's held output as its socket takes without
blocking, in sends of at most +IPC-SEND-CHUNK+ octets, and keep the
rest. Sending any octet sets PROGRESS-AT.
Return :GONE when a send fails for any reason but a full socket,
otherwise T. Call with its lock held, and only while CLIENT is not
:CLOSED."
  (let ((length (ipc-client-output-length client))
        (buffer (ipc-client-output client))
        (sent 0)
        (result t))
    (when (plusp length)
      (cffi:with-pointer-to-vector-data (ptr buffer)
        (loop while (< sent length)
              do (let ((n (cffi:foreign-funcall
                           "send" :int (ipc-client-fd client)
                                  :pointer (cffi:inc-pointer ptr sent)
                                  :long (min +ipc-send-chunk+
                                             (- length sent))
                                  :int +ipc-send-flags+ :long)))
                   (cond ((plusp n) (incf sent n))
                         ((= (sb-alien:get-errno) sb-unix:eintr))
                         ((= (sb-alien:get-errno) sb-unix:eagain)
                          (return))
                         (t (setf result :gone)
                            (return))))))
      (when (plusp sent)
        (setf (ipc-client-progress-at client) (get-internal-real-time)))
      (replace buffer buffer :start2 sent :end2 length)
      (setf (ipc-client-output-length client) (- length sent)))
    result))

(defun ipc-client-deliver (client reply)
  "Hand REPLY, a string or NIL, to CLIENT and count one of its queued
requests as answered. Runs on the main thread and never blocks on the
client: octets the socket does not take now are held for the socket
thread. REPLY is dropped when CLIENT is no longer :OPEN. A reply that
would take the held output past *IPC-CLIENT-OUTPUT-CAP* marks CLIENT
:DROPPED and logs the drop; a failed send marks it :DROPPED. The
socket thread closes a :DROPPED client."
  (let ((octets (and reply (ipc-reply-octets reply)))
        (over-cap nil))
    (bt:with-lock-held ((ipc-client-lock client))
      (decf (ipc-client-pending client))
      (when (and octets (eq (ipc-client-state client) :open))
        (cond ((> (+ (ipc-client-output-length client) (length octets))
                  *ipc-client-output-cap*)
               (setf over-cap (list (%ipc-unread-octets
                                     (ipc-client-fd client))
                                    (+ (ipc-client-output-length client)
                                       (length octets)))
                     (ipc-client-state client) :dropped
                     (ipc-client-output-length client) 0))
              (t
               (%ipc-client-append client octets)
               (when (eq (%ipc-client-flush client) :gone)
                 (setf (ipc-client-state client) :dropped
                       (ipc-client-output-length client) 0))))))
    (when over-cap
      (apply #'%ipc-log-drop (ipc-client-fd client) "output cap" over-cap))))

(defun ipc-client-queue-request (client line)
  "Queue LINE for the main thread, which dispatches it and hands the
reply to CLIENT with IPC-CLIENT-DELIVER."
  (bt:with-lock-held ((ipc-client-lock client))
    (incf (ipc-client-pending client)))
  (ipc-enqueue
   (lambda ()
     (let ((reply nil))
       (unwind-protect
            (setf reply (ipc-dispatch-line line))
         (ipc-client-deliver client reply))))))

(defun ipc-handle-connection (client-fd)
  "Serve CLIENT-FD until the client goes, then close it.
Each newline-terminated request is decoded once complete and queued
for the main thread, whose reply IPC-CLIENT-DELIVER hands back. Held
reply octets are sent as the socket takes them.

A client that neither sends an octet nor reads a reply octet for the
seconds IPC-CLIENT-IDLE-SECONDS returns is dropped, while time spent waiting for the
main thread to answer does not count. After the client shuts its
write side, queued replies are still sent for up to
*IPC-CLIENT-LINGER-SECONDS*, as long as the client keeps reading
them. A client that closes, fails, passes the output cap, or misses
either deadline is closed at once and its queued replies are dropped.
Every drop for a deadline logs one line naming the reason, the
kernel's unread count, and the octets the WM still holds."
  (let ((client (make-ipc-client client-fd))
        (acc (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0))
        (idle-limit (round (* (ipc-client-idle-seconds)
                              internal-time-units-per-second)))
        (eof-at nil)
        (unread 0)
        (drop-reason nil)
        (drop-counts nil))
    (unwind-protect
         (cffi:with-foreign-objects ((buf :uint8 4096) (pfd :uint8 8))
           (block serve
             (loop
               (let ((events (logior (if eof-at 0 +ipc-pollin+)
                                     (if (bt:with-lock-held
                                             ((ipc-client-lock client))
                                           (plusp (ipc-client-output-length
                                                   client)))
                                         +ipc-pollout+
                                         0))))
                 (setf (cffi:mem-ref pfd :int 0) client-fd
                       (cffi:mem-ref pfd :short 4) events
                       (cffi:mem-ref pfd :short 6) 0)
                 (cffi:foreign-funcall "poll" :pointer pfd :unsigned-long 1
                                       :int 50 :int)
                 (let ((revents (cffi:mem-ref pfd :short 6)))
                   (when (logtest revents (logior +ipc-pollerr+ +ipc-pollnval+))
                     (return-from serve))
                   (when (and (not eof-at)
                              (logtest revents (logior +ipc-pollin+
                                                       +ipc-pollhup+)))
                     (let ((n (cffi:foreign-funcall "read" :int client-fd
                                                    :pointer buf
                                                    :long 4096 :long)))
                       (cond
                         ((plusp n)
                          (bt:with-lock-held ((ipc-client-lock client))
                            (setf (ipc-client-progress-at client)
                                  (get-internal-real-time)))
                          (dotimes (i n)
                            (vector-push-extend (cffi:mem-aref buf :uint8 i)
                                                acc))
                          (dolist (line (%ipc-take-lines acc))
                            (ipc-client-queue-request client line)))
                         ((zerop n)
                          (setf eof-at (get-internal-real-time)))
                         ((not (member (sb-alien:get-errno)
                                       (list sb-unix:eintr sb-unix:eagain)))
                          (return-from serve)))))
                   (when (and eof-at (logtest revents +ipc-pollhup+))
                     (return-from serve)))
                 (let ((now (get-internal-real-time))
                       (queued (%ipc-unread-octets client-fd)))
                   (bt:with-lock-held ((ipc-client-lock client))
                     (when (< queued unread)
                       (setf (ipc-client-progress-at client) now))
                     (setf unread queued)
                     (when (or (eq (ipc-client-state client) :dropped)
                               (eq (%ipc-client-flush client) :gone)
                               (and eof-at
                                    (zerop (ipc-client-pending client))
                                    (zerop (ipc-client-output-length client))))
                       (return-from serve))
                     (when (and (plusp (ipc-client-pending client))
                                (zerop (ipc-client-output-length client)))
                       (setf (ipc-client-progress-at client) now))
                     (setf drop-counts
                           (list (%ipc-unread-octets client-fd)
                                 (ipc-client-output-length client))
                           drop-reason
                           (cond ((> (- now (ipc-client-progress-at client))
                                     idle-limit)
                                  "idle deadline")
                                 ((and eof-at
                                       (> (- now eof-at)
                                          (* *ipc-client-linger-seconds*
                                             internal-time-units-per-second)))
                                  "linger deadline"))))
                   (when drop-reason
                     (apply #'%ipc-log-drop client-fd drop-reason drop-counts)
                     (return-from serve)))))))
      (bt:with-lock-held ((ipc-client-lock client))
        (setf (ipc-client-state client) :closed
              (ipc-client-output-length client) 0)
        (cffi:foreign-funcall "close" :int client-fd :int)))))

(defun ipc-accept-loop (fd)
  "Accept connections on FD until the socket closes, and serve each one
in turn with IPC-HANDLE-CONNECTION. Runs on the socket thread."
  (handler-case
      (loop
        (let ((client (cffi:foreign-funcall "accept" :int fd
                                            :pointer (cffi:null-pointer)
                                            :pointer (cffi:null-pointer) :int)))
          (when (< client 0) (return))
          (ipc-handle-connection client)))
    (error (e)
      (format t "~&; Rhizome: ipc accept loop ended: ~A~%" e))))

(defun start-ipc-server (&optional (path (ipc-socket-path)))
  "Start the IPC command socket on PATH (the default location unless
given)."
  (when *ipc-thread* (return-from start-ipc-server))
  (let ((fd (%ipc-socket)))
    (when (< fd 0)
      (format t "~&; Rhizome: ipc: cannot create socket~%")
      (return-from start-ipc-server))
    (cffi:foreign-funcall "unlink" :string path :int)
    (when (< (%ipc-bind fd path) 0)
      (format t "~&; Rhizome: ipc: cannot bind ~A~%" path)
      (cffi:foreign-funcall "close" :int fd :int)
      (return-from start-ipc-server))
    (cffi:foreign-funcall "listen" :int fd :int 8 :int)
    (setf *ipc-fd* fd
          *ipc-thread*
          (bt:make-thread (lambda () (ipc-accept-loop fd))
                          :name "rhizome-ipc"))
    (format t "~&; Rhizome: ipc listening on ~A~%" path)))

(defun stop-ipc-server ()
  "Stop the IPC server and remove the socket."
  (when *ipc-fd*
    (cffi:foreign-funcall "shutdown" :int *ipc-fd* :int 2 :int)
    (cffi:foreign-funcall "close" :int *ipc-fd* :int)
    (setf *ipc-fd* nil))
  (when (and *ipc-thread* (bt:thread-alive-p *ipc-thread*))
    (handler-case (bt:destroy-thread *ipc-thread*)
      (error ())))
  (setf *ipc-thread* nil))

;;; --- Client side (used by tests; rhizome-cmd uses the shell) ---

(defun ipc-send-command (path line)
  "Connect to the IPC socket at PATH, send LINE, and return the reply
with its newlines and backslashes restored (see IPC-ESCAPE-REPLY)."
  (let ((fd (%ipc-socket)))
    (when (< fd 0) (error "cannot create socket"))
    (unwind-protect
         (progn
           (cffi:with-foreign-object (addr :uint8 110)
             (cffi:foreign-funcall "memset" :pointer addr :int 0 :long 110 :pointer)
             (setf (cffi:mem-aref addr :uint16 0) 1)
             (cffi:with-foreign-string (cpath path)
               (loop for i below (min 107 (length path))
                     do (setf (cffi:mem-aref addr :uint8 (+ 2 i))
                              (cffi:mem-aref cpath :uint8 i))))
             (when (< (cffi:foreign-funcall "connect" :int fd :pointer addr
                                            :uint 110 :int) 0)
               (error "cannot connect to ~A" path)))
           (%ipc-write-string fd line)
           ;; Read the reply (up to newline)
           ;; Decode once all octets are in: a multi-octet character
           ;; can straddle two reads
           (cffi:with-foreign-object (buf :uint8 4096)
             (let ((acc (make-array 0 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 0)))
               (loop
                 (let ((n (cffi:foreign-funcall "read" :int fd :pointer buf
                                                :long 4096 :long)))
                   (when (<= n 0) (return))
                   (dotimes (i n)
                     (vector-push-extend (cffi:mem-aref buf :uint8 i) acc))
                   (when (find 10 acc :start (- (length acc) n)) (return))))
               (let ((reply (sb-ext:octets-to-string
                             (coerce acc '(simple-array (unsigned-byte 8) (*)))
                             :external-format :utf-8)))
                 (string-trim " " (ipc-unescape-reply
                                   (subseq reply 0 (or (position #\newline reply)
                                                       (length reply)))))))))
      (cffi:foreign-funcall "close" :int fd :int))))
