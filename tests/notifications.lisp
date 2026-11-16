;;; Tests for the org.freedesktop.Notifications server: the bar side
;;; (ids, replacement, expiry, close reasons, the lines it sends the
;;; window manager), method dispatch on the server connection, the
;;; window manager's commands, and the server on a PRIVATE dbus-daemon
;;; started for this run (never the session bus). The private-bus
;;; section is skipped when dbus-daemon is not installed.
(ql:quickload :rhizome :silent t)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defvar *test-wm-lines* nil
  "Lines the server sent the window manager, newest first.")
(defvar *test-wm-lock* (bt:make-lock "test-wm-lines"))

;; The window manager is not running: record its lines instead
(setf (symbol-function 'notification-send-to-wm)
      (lambda (line)
        (bt:with-lock-held (*test-wm-lock*) (push line *test-wm-lines*))
        nil))

(defvar *test-pass* 0)
(defvar *test-fail* 0)

(defun ok (name actual expected)
  "Count and print one assertion: ACTUAL is EQUAL to EXPECTED."
  (if (equal actual expected)
      (progn (incf *test-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *test-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                     name actual expected))))

(defun test-wm-lines ()
  (bt:with-lock-held (*test-wm-lock*) (reverse *test-wm-lines*)))

(defun test-reset ()
  (bt:with-lock-held (*test-wm-lock*) (setf *test-wm-lines* nil))
  (setf *notifications* nil
        *notification-next-id* 1
        *notification-shown* nil
        *notification-on-screen* nil))

(defun test-show-request (line)
  "The plist a notification-show LINE carries."
  (notification-read-request
    (subseq line (length "notification-show "))))

(defun test-private-bus ()
  "Start a dbus-daemon with no activatable services on a socket in
/tmp. Return (values process socket-path config-path), or NIL when
dbus-daemon is not installed."
  (let ((daemon (ignore-errors
                  (string-trim '(#\Newline)
                               (uiop:run-program '("sh" "-c" "command -v dbus-daemon")
                                                 :output :string
                                                 :ignore-error-status t)))))
    (when (and daemon (plusp (length daemon)))
      (let* ((stem (format nil "/tmp/rhizome-notify-test-~D"
                           (sb-unix:unix-getpid)))
             (socket (concatenate 'string stem ".sock"))
             (config (concatenate 'string stem ".conf")))
        (with-open-file (s config :direction :output :if-exists :supersede)
          (format s "<!DOCTYPE busconfig PUBLIC \"-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN\"
 \"http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd\">
<busconfig><type>session</type><listen>unix:path=~A</listen>
<auth>EXTERNAL</auth><policy context=\"default\">
<allow send_destination=\"*\" eavesdrop=\"true\"/><allow eavesdrop=\"true\"/>
<allow own=\"*\"/></policy></busconfig>~%" socket))
        (let ((process (uiop:launch-program
                         (list daemon (format nil "--config-file=~A" config)
                               "--nofork")
                         :output nil :error-output nil)))
          (loop repeat 100 until (probe-file socket) do (sleep 0.05))
          (values process socket config))))))

(defun test-await (predicate &optional (seconds 3))
  "Poll PREDICATE until it returns true or SECONDS pass."
  (loop with deadline = (+ (get-internal-real-time)
                           (* seconds internal-time-units-per-second))
        for value = (funcall predicate)
        until (or value (> (get-internal-real-time) deadline))
        do (sleep 0.02)
        finally (return value)))

(defun test-closed-signals (conn)
  "Pop every NotificationClosed body queued on CONN."
  (let ((found nil))
    (bt:with-lock-held ((dbus-connection-queue-lock conn))
      (setf (dbus-connection-queue conn)
            (remove-if (lambda (msg)
                         (when (and (dbus-message-p msg)
                                    (equal (message-field msg 3)
                                           "NotificationClosed"))
                           (push (dbus-message-body msg) found)
                           t))
                       (dbus-connection-queue conn))))
    found))

(defun test-call-error (reply)
  "The error name of REPLY when it is an error reply, else NIL."
  (and reply
       (eql (dbus-message-type reply) +dbus-msg-error+)
       (message-field reply 4)))

(defun test-socketpair ()
  "A connected pair of AF_UNIX stream sockets, as two descriptors."
  (cffi:with-foreign-object (fds :int 2)
    (assert (zerop (cffi:foreign-funcall "socketpair" :int 1 :int 1 :int 0
                                         :pointer fds :int)))
    (values (cffi:mem-aref fds :int 0) (cffi:mem-aref fds :int 1))))

(defun test-notify-bytes (summary &key (serial 1) (signature "susssasa{sv}i")
                                       (values (list "app" 0 "" summary ""
                                                     nil nil 0)))
  "The bytes of a Notify call to the server as the bus delivers it,
from sender :1.42. BUILD-OUTGOING writes no sender field, so the
destination field (code 6) is relabelled as the sender (code 7)."
  (let* ((bytes (coerce (build-outgoing
                          (make-outgoing-message
                            :type +dbus-msg-call+ :destination ":1.42"
                            :path +notify-path+ :interface +notify-iface+
                            :member "Notify" :signature signature
                            :values values)
                          serial)
                        '(simple-array (unsigned-byte 8) (*))))
         (field (search (vector 6 1 (char-code #\s) 0) bytes)))
    (setf (aref bytes field) 7)
    bytes))

(defun test-fake-connection (fd)
  "A DBUS-CONNECTION on descriptor FD with its reader thread running,
as DBUS-CONNECT leaves one after Hello."
  (let ((conn (make-dbus-connection
                :fd fd :lock (bt:make-lock "test-dbus")
                :pending (make-hash-table) :queue nil
                :queue-lock (bt:make-lock "test-dbus-queue")
                :queue-cond (bt:make-condition-variable)
                :running t :unique-name ":1.1")))
    (setf (dbus-connection-thread conn)
          (bt:make-thread (lambda () (dbus-reader-loop conn))
                          :name "test-dbus-reader"))
    conn))

(defun test-ids-and-markup ()
  (test-reset)
  (ok "the first notification gets id 1"
      (notification-notify "app" 0 "One" "body" -1 0) 1)
  (ok "the next one gets id 2"
      (notification-notify "app" 0 "Two" "body" -1 0) 2)
  (ok "replaces_id returns the same id"
      (notification-notify "app" 1 "One again" "new body" -1 0) 1)
  (ok "replacing opens no new notification" (length *notifications*) 2)
  (ok "the replacement carries the new summary"
      (getf (notification-find 1) :summary) "One again")
  (ok "a new id after a replacement still increases"
      (notification-notify "app" 0 "Three" "" -1 0) 3)
  (ok "markup is stripped and entities decoded"
      (notification-plain-text "<b>Bold</b> &amp; &lt;tag&gt;")
      "Bold & <tag>"))

(defun test-timeouts ()
  (test-reset)
  (configure '(:notifications (:timeout-seconds 2)))
  (notification-notify "app" 0 "Default" "" -1 1000)
  (ok "-1 takes (:notifications :timeout-seconds)"
      (getf (notification-find 1) :expires) 3000)
  (notification-notify "app" 0 "Sticky" "" 0 1000)
  (ok "0 never expires" (getf (notification-find 2) :expires) nil)
  (notification-notify "app" 0 "Short" "" 500 1000)
  (ok "a positive timeout is in milliseconds"
      (getf (notification-find 3) :expires) 1500)
  (ok "an int32 -1 off the wire reads as -1"
      (notification-signed-32 #xFFFFFFFF) -1)
  (configure '(:notifications (:timeout-seconds 5))))

(defun test-expiry-and-lines ()
  (test-reset)
  (let ((emitted nil)
        (real-emit (symbol-function 'dbus-emit-signal)))
    (setf (symbol-function 'dbus-emit-signal)
          (lambda (conn path iface member &key signature values)
            (declare (ignore conn path signature))
            (push (list iface member values) emitted)))
    (setf *notify-conn* :fake)
    (unwind-protect
         (progn
           (notification-notify "Mail" 0 "Subject" "line one
line two" 1000 0)
           (notifications-tick 0)
           (let* ((lines (test-wm-lines))
                  (request (test-show-request (first lines))))
             (ok "one draw request goes to the window manager"
                 (length lines) 1)
             (ok "the draw request is one line"
                 (find #\Newline (first lines)) nil)
             (ok "the draw request carries the summary"
                 (getf request :summary) "Subject")
             (ok "the draw request carries the body lines"
                 (getf request :lines) '("line one" "line two"))
             (ok "the draw request carries the app name"
                 (getf request :app) "Mail")
             (ok "the window manager shows it past the bar's expiry"
                 (getf request :seconds) 2))
           (notifications-tick 500)
           (ok "an unchanged notification is not sent again"
               (length (test-wm-lines)) 1)
           (notifications-tick 1000)
           (ok "expiry emits NotificationClosed (id 1)"
               (first emitted)
               (list +notify-iface+ "NotificationClosed" '(1 1)))
           (ok "expiry hides it"
               (car (last (test-wm-lines))) "notification-hide 1")

           ;; the newest shows; when it closes, the next one returns
           (test-reset)
           (setf emitted nil)
           (notification-notify "a" 0 "Old" "" 0 0)
           (notification-notify "a" 0 "New" "" 0 0)
           (notifications-tick 0)
           (ok "the newest notification is the one shown"
               (getf (test-show-request (car (last (test-wm-lines))))
                     :summary)
               "New")
           (ok "a notification with no expiry is sent without seconds"
               (getf (test-show-request (car (last (test-wm-lines))))
                     :seconds)
               nil)
           (multiple-value-bind (signature values)
               (notification-handle-close
                 (make-dbus-message :type +dbus-msg-call+
                                    :fields '((7 . ":1.9"))
                                    :body '(2))
                 :fake)
             (ok "CloseNotification replies with no values"
                 (list signature values) '("" nil)))
           (ok "CloseNotification emits NotificationClosed (id 3)"
               (first emitted)
               (list +notify-iface+ "NotificationClosed" '(2 3)))
           (notifications-tick 0)
           (ok "the older notification shows again"
               (getf (test-show-request (car (last (test-wm-lines))))
                     :summary)
               "Old")
           (setf emitted nil)
           (notification-handle-close
             (make-dbus-message :type +dbus-msg-call+
                                :fields '((7 . ":1.9")) :body '(42))
             :fake)
           (ok "closing an id that is not open emits nothing" emitted nil)

           ;; Dismiss from the window manager: reason 2
           (notification-signal
             (make-dbus-message :type +dbus-msg-signal+
                                :fields `((2 . ,+notify-private-iface+)
                                          (3 . "Dismiss"))
                                :body '(1)))
           (ok "Dismiss emits NotificationClosed (id 2)"
               (first emitted)
               (list +notify-iface+ "NotificationClosed" '(1 2))))
      (setf *notify-conn* nil
            (symbol-function 'dbus-emit-signal) real-emit))))

(defun test-capabilities ()
  (ok "GetCapabilities lists body"
      (multiple-value-list (notification-handle-capabilities nil nil))
      '("as" (("body"))))
  (multiple-value-bind (signature values)
      (notification-handle-server-information nil nil)
    (ok "GetServerInformation replies ssss" signature "ssss")
    (ok "GetServerInformation names rhizome and spec 1.2"
        (list (first values) (fourth values)) '("rhizome" "1.2"))))

(defun test-dispatch ()
  "Method calls on the server connection, with the reply functions
recording instead of sending."
  (let ((replies nil)
        (real-reply (symbol-function 'dbus-reply))
        (real-error (symbol-function 'dbus-error-reply)))
    (setf (symbol-function 'dbus-reply)
          (lambda (conn msg &key signature values)
            (declare (ignore conn msg))
            (push (list :reply signature values) replies))
          (symbol-function 'dbus-error-reply)
          (lambda (conn msg name text)
            (declare (ignore conn msg text))
            (push (list :error name) replies)))
    (flet ((dispatch (path interface member signature body)
             (setf replies nil)
             (notification-dispatch
               (make-dbus-message :type +dbus-msg-call+
                                  :fields `((1 . ,path) (2 . ,interface)
                                            (3 . ,member) (7 . ":1.9")
                                            (8 . ,signature))
                                  :body body)
               :fake)
             (first replies)))
      (unwind-protect
           (progn
             (test-reset)
             (ok "Notify with the wrong signature gets InvalidArgs"
                 (dispatch +notify-path+ +notify-iface+ "Notify" "s" '("x"))
                 '(:error "org.freedesktop.DBus.Error.InvalidArgs"))
             (ok "Notify whose body fails its handler gets Failed"
                 (dispatch +notify-path+ +notify-iface+ "Notify"
                           "susssasa{sv}i" '("x"))
                 '(:error "org.freedesktop.DBus.Error.Failed"))
             (ok "CloseNotification with a string id gets InvalidArgs"
                 (dispatch +notify-path+ +notify-iface+ "CloseNotification"
                           "s" '("1"))
                 '(:error "org.freedesktop.DBus.Error.InvalidArgs"))
             (ok "Notify on another path gets UnknownObject"
                 (dispatch "/StatusNotifierWatcher" +notify-iface+ "Notify"
                           "susssasa{sv}i"
                           (list "a" 0 "" "s" "" nil nil 0))
                 '(:error "org.freedesktop.DBus.Error.UnknownObject"))
             (ok "an unknown method gets UnknownMethod"
                 (dispatch +notify-path+ +notify-iface+ "Frob" "" nil)
                 '(:error "org.freedesktop.DBus.Error.UnknownMethod"))
             (ok "Introspect elsewhere is an empty node"
                 (dispatch "/" "org.freedesktop.DBus.Introspectable"
                           "Introspect" "" nil)
                 '(:reply "s" ("<node/>")))
             (ok "a valid Notify replies with its id"
                 (dispatch +notify-path+ +notify-iface+ "Notify"
                           "susssasa{sv}i"
                           (list "a" 0 "" "s" "" nil nil 0))
                 '(:reply "u" (1))))
        (setf (symbol-function 'dbus-reply) real-reply
              (symbol-function 'dbus-error-reply) real-error)))))

(defun test-wm-commands ()
  (test-reset)
  (notification-notify "Mail" 0 "Subject" "Body text" 0 0)
  (notifications-tick 0)
  (let* ((line (first (test-wm-lines)))
         (shown (ipc-dispatch-line line)))
    (ok "notification-show over IPC replies with a string"
        (stringp shown) t)
    (ok "notification-show replies with nothing to show" shown "")
    (ok "the message window gets app, summary and body"
        *last-message* (format nil "Mail: Subject~%Body text"))
    (ok "the window manager records the notification on screen"
        (car *notification-on-screen*) 1))
  (ok "notification-show without a plist returns a string"
      (stringp (execute-command nil :notification-show '("junk"))) t)
  (ok "notification-show refuses #. reader evaluation"
      (stringp (execute-command nil :notification-show
                                '("#.(error \"evaluated\")")))
      t)
  (ok "notification-hide of another id returns NIL"
      (execute-command nil :notification-hide '(7)) nil)
  (ok "notification-hide of another id leaves it on screen"
      (car *notification-on-screen*) 1)
  (let ((dismissed nil)
        (real (symbol-function 'notification-signal-dismiss)))
    (setf (symbol-function 'notification-signal-dismiss)
          (lambda (id) (push id dismissed) nil))
    (unwind-protect
         (progn
           (ok "notification-dismiss returns NIL"
               (execute-command nil :notification-dismiss nil) nil)
           (ok "notification-dismiss reports the id to the bar"
               dismissed '(1))
           (ok "notification-dismiss with nothing shown returns a string"
               (execute-command nil :notification-dismiss nil)
               "no notification to dismiss"))
      (setf (symbol-function 'notification-signal-dismiss) real)))
  (ok "notification-hide returns NIL"
      (execute-command nil :notification-hide '(1)) nil))

(defun test-disabled ()
  (configure '(:notifications (:enabled nil)))
  (ok "(:notifications (:enabled nil)) starts no server"
      (notifications-start :path "/nonexistent/bus") nil)
  (configure '(:notifications (:enabled t))))

(defun test-bus ()
  "The server, and the tray watcher beside it, on a private bus."
  (multiple-value-bind (daemon socket config) (test-private-bus)
    (if (null daemon)
        (format t "~&SKIP: dbus-daemon not installed; private bus assertions skipped~%")
        (let ((rival nil) (client nil) (drainer nil) (running t))
          (unwind-protect
               (progn
                 (test-reset)
                 ;; the tray watcher, as the bar runs it
                 (ok "the tray watcher starts on the private bus"
                     (and (tray-start :path socket) t) t)
                 (setf drainer
                       (bt:make-thread
                         (lambda ()
                           (loop while running
                                 do (tray-drain)
                                    (notifications-drain)
                                    (sleep 0.01)))
                         :name "test-notify-drain"))
                 ;; another server already owns the name
                 (setf rival (dbus-connect :path socket))
                 (ok "a rival takes the name first"
                     (dbus-request-name rival +notify-name+ 4) 1)
                 (let* ((result nil)
                        (log (with-output-to-string (*standard-output*)
                               (setf result
                                     (notifications-start :path socket)))))
                   (ok "with the name owned, the server stays off"
                       (list result *notify-conn*) '(nil nil))
                   (ok "with the name owned, one line is logged"
                       (count #\Newline
                              (subseq log (search "notifications:" log)))
                       1)
                   (ok "the log line names the owned name"
                       (and (search "already owned" log) t) t))
                 (multiple-value-bind (reply body)
                     (dbus-call rival "org.freedesktop.DBus"
                                "/org/freedesktop/DBus"
                                "org.freedesktop.DBus" "GetNameOwner"
                                :signature "s"
                                :values (list +notify-name+))
                   (declare (ignore reply))
                   (ok "the rival keeps the name"
                       (first body)
                       (dbus-connection-unique-name rival)))
                 (flet ((tray-call (path interface member signature values)
                          (test-call-error
                            (dbus-call rival
                                       (dbus-connection-unique-name *tray-conn*)
                                       path interface member
                                       :signature signature :values values
                                       :timeout 2))))
                   (ok "with the server off, Notify on the tray's connection gets an error"
                       (tray-call +notify-path+ +notify-iface+ "Notify"
                                  "susssasa{sv}i"
                                  (list "a" 0 "" "s" "" nil nil 0))
                       "org.freedesktop.DBus.Error.UnknownMethod")
                   (ok "Introspect on the tray's path gets UnknownMethod"
                       (tray-call "/StatusNotifierWatcher"
                                  "org.freedesktop.DBus.Introspectable"
                                  "Introspect" "" nil)
                       "org.freedesktop.DBus.Error.UnknownMethod"))
                 (dbus-disconnect rival)
                 (setf rival nil)

                 ;; the name is free: the server takes it
                 (ok "with the name free, the server starts"
                     (and (notifications-start :path socket) t) t)
                 (setf client (dbus-connect :path socket))
                 (dbus-add-match client "type='signal',interface='org.freedesktop.Notifications',member='NotificationClosed'")
                 (ok "with the server on, Notify on the tray's connection gets an error"
                     (test-call-error
                       (dbus-call client
                                  (dbus-connection-unique-name *tray-conn*)
                                  +notify-path+ +notify-iface+ "Notify"
                                  :signature "susssasa{sv}i"
                                  :values (list "a" 0 "" "s" "" nil nil 0)
                                  :timeout 2))
                     "org.freedesktop.DBus.Error.UnknownMethod")
                 (ok "a malformed Notify gets an error reply at once"
                     (test-call-error
                       (dbus-call client +notify-name+ +notify-path+
                                  +notify-iface+ "Notify"
                                  :signature "s" :values (list "x")
                                  :timeout 2))
                     "org.freedesktop.DBus.Error.InvalidArgs")
                 (flet ((notify (replaces summary body timeout)
                          (multiple-value-bind (reply body)
                              (dbus-call client +notify-name+ +notify-path+
                                         +notify-iface+ "Notify"
                                         :signature "susssasa{sv}i"
                                         :values (list "test-app" replaces ""
                                                       summary body
                                                       '("default" "Open")
                                                       (list (list "urgency"
                                                                   (cons "y" 1)))
                                                       timeout))
                            (declare (ignore reply))
                            (first body))))
                   (ok "Notify over the bus returns id 1"
                       (notify 0 "Hello" "<i>World</i>" 0) 1)
                   (ok "Notify over the bus returns id 2"
                       (notify 0 "Second" "" 0) 2)
                   (ok "Notify with replaces_id 1 returns 1"
                       (notify 1 "Hello again" "" 0) 1)
                   (ok "the draw request reaches the window manager line"
                       (and (test-await
                              (lambda ()
                                (find-if (lambda (line)
                                           (search "Hello again" line))
                                         (test-wm-lines))))
                            t)
                       t)
                   (multiple-value-bind (reply body)
                       (dbus-call client +notify-name+ +notify-path+
                                  +notify-iface+ "GetCapabilities")
                     (declare (ignore reply))
                     (ok "GetCapabilities over the bus" body '(("body"))))
                   (multiple-value-bind (reply body)
                       (dbus-call client +notify-name+ +notify-path+
                                  +notify-iface+ "GetServerInformation")
                     (declare (ignore reply))
                     (ok "GetServerInformation over the bus"
                         (list (first body) (fourth body))
                         '("rhizome" "1.2")))
                   (multiple-value-bind (reply body)
                       (dbus-call client +notify-name+ +notify-path+
                                  "org.freedesktop.DBus.Introspectable"
                                  "Introspect")
                     (declare (ignore reply))
                     (ok "Introspect describes Notify"
                         (and (search "Notify" (first body)) t) t))
                   (dbus-call client +notify-name+ +notify-path+
                              +notify-iface+ "CloseNotification"
                              :signature "u" :values (list 2))
                   (ok "CloseNotification over the bus signals (2 3)"
                       (test-await (lambda () (test-closed-signals client)))
                       '((2 3)))
                   (dbus-emit-signal client +notify-path+
                                     +notify-private-iface+ "Dismiss"
                                     :signature "u" :values (list 1))
                   (ok "Dismiss over the bus signals (1 2)"
                       (test-await (lambda () (test-closed-signals client)))
                       '((1 2)))
                   (ok "a short timeout over the bus expires"
                       (notify 0 "Brief" "" 100) 3)
                   (ok "expiry over the bus signals (3 1)"
                       (test-await (lambda () (test-closed-signals client)))
                       '((3 1)))
                   (ok "after the last close the display is hidden"
                       (and (test-await
                              (lambda ()
                                (equal (car (last (test-wm-lines)))
                                       "notification-hide 3")))
                            t)
                       t)))
            (setf running nil)
            (when drainer (bt:join-thread drainer))
            (notifications-stop)
            (when *tray-conn*
              (dbus-disconnect *tray-conn*)
              (setf *tray-conn* nil))
            (when client (dbus-disconnect client))
            (when rival (dbus-disconnect rival))
            (uiop:terminate-process daemon)
            (uiop:wait-process daemon)
            (ignore-errors (delete-file socket))
            (ignore-errors (delete-file config)))))))

(defun test-wire-framing ()
  "Malformed messages on a socket pair, read by a connection's reader
thread: each is dropped, the thread keeps reading, and a valid Notify
after them is answered."
  (multiple-value-bind (ours theirs) (test-socketpair)
    (let ((conn (test-fake-connection ours))
          (limit *dbus-max-message-bytes*))
      (unwind-protect
           (progn
             (test-reset)
             (setf *dbus-max-message-bytes* 4096)
             ;; a signature naming a type that does not exist
             (let* ((bytes (test-notify-bytes "bad" :signature "s"
                                                    :values '("bad")))
                    (sig (search (vector 8 1 (char-code #\g) 0 1
                                         (char-code #\s) 0)
                                 bytes)))
               (setf (aref bytes (+ sig 5)) (char-code #\z))
               (sock-write-all theirs bytes))
             ;; a body shorter than its signature needs
             (let* ((full (test-notify-bytes "truncated" :serial 2))
                    (body-len (+ (aref full 4) (ash (aref full 5) 8)))
                    (cut (subseq full 0 (- (length full) (- body-len 4)))))
               (setf (aref cut 4) 4 (aref cut 5) 0 (aref cut 6) 0
                     (aref cut 7) 0)
               (sock-write-all theirs cut))
             ;; longer than *DBUS-MAX-MESSAGE-BYTES*
             (sock-write-all theirs (test-notify-bytes
                                      (make-string 8000 :initial-element #\x)
                                      :serial 3))
             ;; big-endian, which the parser does not read
             (sock-write-all theirs
                             (coerce (list (char-code #\B) 1 0 1 0 0 0 0
                                           0 0 0 4 0 0 0 0)
                                     '(vector (unsigned-byte 8))))
             ;; then a valid Notify
             (sock-write-all theirs (test-notify-bytes "valid" :serial 7))
             (let ((queued (test-await
                             (lambda ()
                               (bt:with-lock-held
                                   ((dbus-connection-queue-lock conn))
                                 (copy-list (dbus-connection-queue conn))))
                             2)))
               (sleep 0.2)
               (setf queued (bt:with-lock-held
                                ((dbus-connection-queue-lock conn))
                              (copy-list (dbus-connection-queue conn))))
               (ok "malformed and oversized messages are dropped"
                   (mapcar #'dbus-message-serial queued) '(7))
               (ok "the reader thread keeps reading after them"
                   (bt:thread-alive-p (dbus-connection-thread conn)) t))
             (setf *notify-conn* conn)
             (notifications-drain)
             (setf *notify-conn* nil)
             (let ((reply (dbus-read-message theirs)))
               (ok "the valid Notify after them gets its id"
                   (list (dbus-message-type reply) (message-field reply 5)
                         (dbus-message-body reply))
                   (list +dbus-msg-return+ 7 '(1)))))
        (setf *dbus-max-message-bytes* limit
              *notify-conn* nil)
        (dbus-disconnect conn)
        (%sock-close theirs))))
  ;; a length past the protocol maximum cannot be framed
  (multiple-value-bind (ours theirs) (test-socketpair)
    (unwind-protect
         (let* ((huge (+ +dbus-max-message-length+ 8))
                (result :unread)
                (log nil))
           (sock-write-all theirs
                           (coerce (list (char-code #\l) 1 0 1
                                         (ldb (byte 8 0) huge)
                                         (ldb (byte 8 8) huge)
                                         (ldb (byte 8 16) huge)
                                         (ldb (byte 8 24) huge)
                                         1 0 0 0 0 0 0 0)
                                   '(vector (unsigned-byte 8))))
           (setf log (with-output-to-string (*standard-output*)
                       (setf result (dbus-read-message ours))))
           (ok "a length past the protocol maximum ends the read"
               (list result (and (search "protocol maximum" log) t))
               '(nil t)))
      (%sock-close ours)
      (%sock-close theirs))))

(defun test-request-parsing ()
  "Application text crosses to the window manager as escaped strings,
and the window manager's parser interns nothing."
  (test-reset)
  (let* ((summary "foo) (:zz-probe-key . #.(bar \\ \"q\"")
         (body "zz-probe-symbol")
         (id (notification-notify "app" 0 summary body 0 0)))
    (notifications-tick 0)
    (let* ((line (car (last (test-wm-lines))))
           (request (test-show-request line)))
      (ok "a hostile summary crosses as the same string"
          (list (getf request :id) (getf request :summary)
                (getf request :lines))
          (list id summary (list body)))
      (ok "the request holds only known keys, strings, and integers"
          (loop for (key value) on request by #'cddr
                always (and (member key '(:id :app :summary :lines :seconds))
                            (or (null value) (integerp value)
                                (stringp value)
                                (and (listp value)
                                     (every #'stringp value)))))
          t)))
  (ok "an unknown key is refused"
      (notification-read-request "(:ID 1 :ZZ-PROBE-KEY-2 2)") nil)
  (ok "a bare symbol value is refused"
      (notification-read-request "(:ID 1 :APP ZZ-PROBE-BARE)") nil)
  (ok "a refused request interns no symbol"
      (list (find-symbol "ZZ-PROBE-KEY-2" :keyword)
            (find-symbol "ZZ-PROBE-BARE" :rhizome)
            (find-symbol "ZZ-PROBE-BARE" :cl-user)
            (find-symbol "ZZ-PROBE-KEY" :keyword)
            (find-symbol "ZZ-PROBE-SYMBOL" :rhizome)
            (find-symbol "ZZ-PROBE-SYMBOL" :cl-user))
      '(nil nil nil nil nil nil))
  (ok "a request past the length bound is refused"
      (notification-read-request
        (format nil "(:ID 1 :APP ~S)"
                (make-string (1+ *notification-request-max-length*)
                             :initial-element #\a)))
      nil)
  (test-reset)
  (notification-notify (make-string 300 :initial-element #\")
                       0 (make-string 300 :initial-element #\\)
                       (format nil "~{~A~^~%~}"
                               (loop repeat 12
                                     collect (make-string 300 :initial-element #\")))
                       0 0)
  (notifications-tick 0)
  (let ((line (car (last (test-wm-lines)))))
    (ok "the longest request the bar sends fits the bound and parses"
        (and (<= (length line) *notification-request-max-length*)
             (integerp (getf (test-show-request line) :id)))
        t)))

(defun test-wrap ()
  "Long lines wrap to the message window's text width."
  (let* ((width (notification-text-width))
         (words (format nil "~{~A~^ ~}"
                        (loop for i from 0 below 40
                              collect (format nil "word~D" i))))
         (long-word (make-string 300 :initial-element #\x))
         (wrapped (notification-wrap-line words width))
         (broken (notification-wrap-line long-word width)))
    (ok "a 200-character line of words wraps onto several lines"
        (and (> (length words) 199) (> (length wrapped) 1)) t)
    (ok "every wrapped line fits the message window"
        (every (lambda (line) (<= (font-text-width line) width))
               (append wrapped broken))
        t)
    (ok "a line wraps between words, losing nothing"
        (format nil "~{~A~^ ~}" wrapped) words)
    (ok "a word wider than the window breaks inside it, losing nothing"
        (list (> (length broken) 1) (apply #'concatenate 'string broken))
        (list t long-word))
    (ok "a short line stays whole" (notification-wrap-line "hi" width)
        '("hi"))
    (ok "an empty line stays one empty line"
        (notification-wrap-line "" width) '(""))
    (ok "the message window text wraps every line"
        (every (lambda (line) (<= (font-text-width line) width))
               (cl-ppcre:split
                 "\\n"
                 (notification-display-text
                   (list :id 1 :app "app" :summary words
                         :lines (list words long-word)))))
        t)))

(defun test-no-expiry ()
  "A notification with no expiry stays in the message window with no
deadline."
  (let ((shown nil)
        (real (symbol-function 'show-message)))
    (setf (symbol-function 'show-message)
          (lambda (text &optional (seconds 3) color)
            (declare (ignore text color))
            (push seconds shown)))
    (unwind-protect
         (progn
           (execute-command nil :notification-show
                            '("(:ID 5 :APP \"a\" :SUMMARY \"s\" :LINES NIL :SECONDS NIL)"))
           (execute-command nil :notification-show
                            '("(:ID 6 :APP \"a\" :SUMMARY \"s\" :LINES NIL :SECONDS 4)"))
           (ok "a notification without seconds is shown with no deadline"
               (reverse shown) '(nil 4)))
      (setf (symbol-function 'show-message) real)))
  (let ((*message-state* (make-hash-table))
        (*message-expiry* nil))
    (message-drain)
    (ok "a message with no deadline survives the expiry check"
        (and *message-state* t) t)))

(defun test-run-all ()
  (test-ids-and-markup)
  (test-timeouts)
  (test-expiry-and-lines)
  (test-capabilities)
  (test-dispatch)
  (test-wire-framing)
  (test-request-parsing)
  (test-wrap)
  (test-no-expiry)
  (test-wm-commands)
  (test-disabled)
  (test-bus))

(test-run-all)
(format t "~&=== ~A passed, ~A failed ===~%" *test-pass* *test-fail*)
(uiop:quit (if (zerop *test-fail*) 0 1))
