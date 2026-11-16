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

(defun test-run-all ()
  (test-ids-and-markup)
  (test-timeouts)
  (test-expiry-and-lines)
  (test-capabilities)
  (test-dispatch)
  (test-wm-commands)
  (test-disabled)
  (test-bus))

(test-run-all)
(format t "~&=== ~A passed, ~A failed ===~%" *test-pass* *test-fail*)
(uiop:quit (if (zerop *test-fail*) 0 1))
