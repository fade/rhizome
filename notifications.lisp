;;; Desktop notifications for Rhizome: an org.freedesktop.Notifications
;;; server (Desktop Notifications Specification 1.2).
;;;
;;; The bar process owns the bus name, as it owns the tray watcher's,
;;; and keeps the open notifications: ids, replacement, expiry, and the
;;; NotificationClosed signal. The window manager draws them. The bar
;;; sends each change over the IPC socket as a notification-show or
;;; notification-hide command, and the message window shows the newest
;;; open notification. The notification-dismiss command closes the one
;;; last shown and reports it to the bar with a Dismiss signal on the
;;; private org.rhizome.Notifications interface.
;;;
;;; Close reasons in NotificationClosed: 1 expired, 2 dismissed by the
;;; user, 3 closed by a CloseNotification call.

(in-package #:rhizome)

(defparameter +notify-name+ "org.freedesktop.Notifications")
(defparameter +notify-path+ "/org/freedesktop/Notifications")
(defparameter +notify-iface+ "org.freedesktop.Notifications")
(defparameter +notify-private-iface+ "org.rhizome.Notifications"
  "Interface of the Dismiss signal the window manager emits.")

(defconstant +notification-expired+ 1)
(defconstant +notification-dismissed+ 2)
(defconstant +notification-closed-by-call+ 3)

(defparameter *notification-max-lines* 10
  "Body lines a notification shows; the rest are dropped.")
(defparameter *notification-max-line-length* 200
  "Characters a summary or body line shows; the rest are dropped.")

;;; --- Server state (bar process) ---

(defparameter *notify-conn* nil
  "The bar process's connection serving org.freedesktop.Notifications,
or NIL when the server is off.")

(defparameter *notification-bus-path* nil
  "Socket path of the bus the server and the dismiss signal use; NIL
for the session bus (DBUS-BUS-PATH).")

(defparameter *notifications* nil
  "Open notifications, newest first. Each is a plist (:ID :APP
:SUMMARY :BODY :EXPIRES :VERSION); :EXPIRES is a NOTIFICATION-NOW-MS
time, or NIL for a notification that stays until it is closed.")

(defparameter *notification-next-id* 1
  "The id the next new notification gets.")

(defparameter *notification-shown* nil
  "(ID . VERSION) of the notification the window manager was last
asked to show, or NIL.")

(defun notification-now-ms ()
  "Monotonic time in milliseconds."
  (floor (* 1000 (get-internal-real-time))
         internal-time-units-per-second))

(defun notification-find (id)
  "The open notification with ID, or NIL."
  (find id *notifications* :key (lambda (n) (getf n :id))))

(defun notification-signed-32 (value)
  "VALUE, an int32 the wire layer decodes as unsigned, as a signed
integer."
  (if (>= value #x80000000)
      (- value #x100000000)
      value))

(defun notification-timeout-ms (timeout)
  "Milliseconds a notification with expire_timeout TIMEOUT stays open,
or NIL for one that stays until closed. -1 (any negative value) takes
(:notifications :timeout-seconds), where NIL or 0 also means it
stays."
  (cond ((zerop timeout) nil)
        ((plusp timeout) timeout)
        (t (let ((seconds (config-getf '(:notifications) :timeout-seconds
                                       :default 5)))
             (and (realp seconds) (plusp seconds)
                  (round (* 1000 seconds)))))))

(defun notification-plain-text (text)
  "TEXT with markup tags removed, the XML entities &lt; &gt; &quot;
&apos; and &amp; decoded, and control characters other than newline
replaced by spaces."
  (let ((plain (cl-ppcre:regex-replace-all "<[^>]*>" (or text "") "")))
    (loop for (entity . char) in '(("&lt;" . "<") ("&gt;" . ">")
                                   ("&quot;" . "\"") ("&apos;" . "'")
                                   ("&amp;" . "&"))
          do (setf plain (cl-ppcre:regex-replace-all entity plain char)))
    (map 'string (lambda (c)
                   (if (and (char/= c #\Newline) (< (char-code c) 32))
                       #\Space
                       c))
         plain)))

(defun notification-notify (app-name replaces-id summary body timeout
                            &optional (now (notification-now-ms)))
  "Open a notification, or replace the open one with REPLACES-ID, and
return its id. Either way it becomes the newest, the one shown. A REPLACES-ID of 0 takes a new id; ids only increase.
A nonzero REPLACES-ID is returned as given, and opens a new
notification under that id when none is open. TIMEOUT is the signed
expire_timeout in milliseconds (see NOTIFICATION-TIMEOUT-MS)."
  (let* ((ms (notification-timeout-ms timeout))
         (id (if (plusp replaces-id)
                 replaces-id
                 *notification-next-id*))
         (fields (list :app (notification-plain-text app-name)
                       :summary (notification-plain-text summary)
                       :body (notification-plain-text body)
                       :expires (and ms (+ now ms))))
         (existing (notification-find id)))
    (setf *notification-next-id* (max *notification-next-id* (1+ id)))
    ;; a replacement moves to the front: it is the newest content
    (setf *notifications* (remove existing *notifications*))
    (push (list* :id id
                 :version (if existing (1+ (getf existing :version 0)) 0)
                 fields)
          *notifications*)
    id))

(defun notification-close (id reason)
  "Remove the open notification ID and emit NotificationClosed (ID
REASON). Return T when ID was open, else NIL; a notification that is
not open emits nothing."
  (let ((open (notification-find id)))
    (when open
      (setf *notifications* (remove open *notifications*))
      (when *notify-conn*
        (dbus-emit-signal *notify-conn* +notify-path+ +notify-iface+
                          "NotificationClosed"
                          :signature "uu" :values (list id reason)))
      t)))

(defun notification-truncate (line)
  "LINE cut to *NOTIFICATION-MAX-LINE-LENGTH* characters."
  (if (> (length line) *notification-max-line-length*)
      (subseq line 0 *notification-max-line-length*)
      line))

(defun notification-show-line (notification now)
  "The IPC command line that asks the window manager to show
NOTIFICATION, NOW being the current NOTIFICATION-NOW-MS time. The
argument is a printed plist (:ID :APP :SUMMARY :LINES :SECONDS) on one
line: the body travels as a list of lines, and :SECONDS is NIL for a
notification with no expiry."
  (let* ((expires (getf notification :expires))
         (plist (list :id (getf notification :id)
                      :app (notification-truncate
                             (substitute #\Space #\Newline
                                         (getf notification :app)))
                      :summary (notification-truncate
                                 (substitute #\Space #\Newline
                                             (getf notification :summary)))
                      :lines (mapcar #'notification-truncate
                                     (let ((lines (remove ""
                                                          (cl-ppcre:split
                                                            "\\n"
                                                            (getf notification
                                                                  :body))
                                                          :test #'string=)))
                                       (subseq lines 0
                                               (min (length lines)
                                                    *notification-max-lines*))))
                      ;; one second past the bar's own expiry: the bar's
                      ;; hide is the one that counts
                      :seconds (and expires
                                    (1+ (max 1 (ceiling (- expires now)
                                                        1000)))))))
    (format nil "notification-show ~A"
            (with-standard-io-syntax
              (let ((*print-readably* nil))
                (prin1-to-string plist))))))

(defun notifications-tick (&optional (now (notification-now-ms)))
  "Close every expired notification with reason 1, then ask the window
manager to show the newest open notification when it is not the one
last shown, or to hide the last one when none is open."
  (dolist (notification (copy-list *notifications*))
    (let ((expires (getf notification :expires)))
      (when (and expires (<= expires now))
        (notification-close (getf notification :id)
                            +notification-expired+))))
  (let ((newest (first *notifications*)))
    (cond
      ((null newest)
       (when *notification-shown*
         (notification-send-to-wm
           (format nil "notification-hide ~D" (car *notification-shown*)))
         (setf *notification-shown* nil)))
      ((not (equal *notification-shown*
                   (cons (getf newest :id) (getf newest :version))))
       (notification-send-to-wm (notification-show-line newest now))
       (setf *notification-shown*
             (cons (getf newest :id) (getf newest :version)))))))

;;; --- Delivery to the window manager (bar process) ---
;;;
;;; One sender thread writes the lines to the IPC socket in order, so
;;; the bar's loop never waits for the window manager.

(defvar *notification-wm-lines* nil
  "Lines waiting for the sender thread, oldest first.")
(defvar *notification-wm-lock* (bt:make-lock "rhizome-notify-wm"))
(defvar *notification-wm-cond* (bt:make-condition-variable))
(defvar *notification-wm-thread* nil)

(defparameter *notification-wm-backlog* 64
  "Lines the sender holds at most; the oldest are dropped beyond it.")

(defun notification-sender-loop ()
  "Deliver queued lines to the window manager's IPC socket, forever."
  (loop
    (let ((line (bt:with-lock-held (*notification-wm-lock*)
                  (loop until *notification-wm-lines*
                        do (bt:condition-wait *notification-wm-cond*
                                              *notification-wm-lock*))
                  (pop *notification-wm-lines*))))
      (handler-case (ipc-send-command (ipc-socket-path) line)
        (error (e)
          (format t "~&; Rhizome: notifications: window manager ~
unreachable: ~A~%" e))))))

(defun notification-send-to-wm (line)
  "Queue the IPC command LINE for the window manager and return NIL."
  (bt:with-lock-held (*notification-wm-lock*)
    (setf *notification-wm-lines*
          (append *notification-wm-lines* (list line)))
    (when (> (length *notification-wm-lines*) *notification-wm-backlog*)
      (pop *notification-wm-lines*))
    (unless (and *notification-wm-thread*
                 (bt:thread-alive-p *notification-wm-thread*))
      (setf *notification-wm-thread*
            (bt:make-thread #'notification-sender-loop
                            :name "rhizome-notify-sender")))
    (bt:condition-notify *notification-wm-cond*))
  nil)

;;; --- D-Bus methods (bar process) ---

(defun notification-server-version ()
  "The version GetServerInformation reports."
  (or (ignore-errors
        (asdf:component-version (asdf:find-system :rhizome nil)))
      "0.1.0"))

(defparameter +notify-introspection+
  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
 \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
<node>
 <interface name=\"org.freedesktop.Notifications\">
  <method name=\"Notify\">
   <arg type=\"s\" direction=\"in\"/><arg type=\"u\" direction=\"in\"/>
   <arg type=\"s\" direction=\"in\"/><arg type=\"s\" direction=\"in\"/>
   <arg type=\"s\" direction=\"in\"/><arg type=\"as\" direction=\"in\"/>
   <arg type=\"a{sv}\" direction=\"in\"/><arg type=\"i\" direction=\"in\"/>
   <arg type=\"u\" direction=\"out\"/>
  </method>
  <method name=\"CloseNotification\"><arg type=\"u\" direction=\"in\"/></method>
  <method name=\"GetCapabilities\"><arg type=\"as\" direction=\"out\"/></method>
  <method name=\"GetServerInformation\">
   <arg type=\"s\" direction=\"out\"/><arg type=\"s\" direction=\"out\"/>
   <arg type=\"s\" direction=\"out\"/><arg type=\"s\" direction=\"out\"/>
  </method>
  <signal name=\"NotificationClosed\"><arg type=\"u\"/><arg type=\"u\"/></signal>
 </interface>
</node>"
  "Introspection data for the server's object path.")

(defun notification-handle-notify (msg conn)
  "Notify (susssasa{sv}i) -> u."
  (declare (ignore conn))
  (destructuring-bind (app-name replaces-id app-icon summary body
                       &optional actions hints (timeout -1))
      (dbus-message-body msg)
    (declare (ignore app-icon actions hints))
    (values "u" (list (notification-notify
                        app-name replaces-id summary body
                        (notification-signed-32 timeout))))))

(defun notification-handle-close (msg conn)
  "CloseNotification (u): close with reason 3. An id that is not open
gets an empty reply."
  (declare (ignore conn))
  (notification-close (first (dbus-message-body msg))
                      +notification-closed-by-call+)
  (values "" nil))

(defun notification-handle-capabilities (msg conn)
  "GetCapabilities -> as."
  (declare (ignore msg conn))
  (values "as" (list (list "body"))))

(defun notification-handle-server-information (msg conn)
  "GetServerInformation -> ssss: name, vendor, version, spec version."
  (declare (ignore msg conn))
  (values "ssss" (list "rhizome" "rhizome"
                       (notification-server-version) "1.2")))

(defun notification-handle-introspect (msg conn)
  "Introspect -> s: the server's interface on its object path, an
empty node elsewhere."
  (declare (ignore conn))
  (values "s" (list (if (equal (message-field msg 1) +notify-path+)
                        +notify-introspection+
                        "<node/>"))))

(defun notification-register-handlers ()
  "Install the server's method handlers in *DBUS-HANDLERS*."
  (setf (dbus-handler +notify-iface+ "Notify")
        #'notification-handle-notify
        (dbus-handler +notify-iface+ "CloseNotification")
        #'notification-handle-close
        (dbus-handler +notify-iface+ "GetCapabilities")
        #'notification-handle-capabilities
        (dbus-handler +notify-iface+ "GetServerInformation")
        #'notification-handle-server-information
        (dbus-handler "org.freedesktop.DBus.Introspectable" "Introspect")
        #'notification-handle-introspect))

;;; --- Lifecycle (bar process) ---

(defun notifications-start (&key (path (or *notification-bus-path*
                                            (dbus-bus-path))))
  "Serve org.freedesktop.Notifications from this process on the bus at
PATH and return the connection. Return NIL, with one log line, when
(:notifications :enabled) is NIL, no bus answers, or another process
owns the name: the request never replaces an owner and never queues
behind one."
  (when *notify-conn*
    (return-from notifications-start *notify-conn*))
  (unless (config-getf '(:notifications) :enabled :default t)
    (format t "~&; Rhizome: notifications: disabled in config~%")
    (return-from notifications-start nil))
  (let ((conn (dbus-connect :path path)))
    (unless conn
      (format t "~&; Rhizome: notifications: no session bus; ~
notification server off~%")
      (return-from notifications-start nil))
    (notification-register-handlers)
    (dbus-add-match conn (format nil "type='signal',interface='~A',~
member='Dismiss'" +notify-private-iface+))
    ;; flags 4: DO_NOT_QUEUE, without REPLACE_EXISTING
    (let ((result (ignore-errors (dbus-request-name conn +notify-name+ 4))))
      (cond
        ((eql result 1)
         (setf *notify-conn* conn)
         (format t "~&; Rhizome: notifications: server active as ~A~%"
                 +notify-name+)
         conn)
        (t
         (format t "~&; Rhizome: notifications: ~A already owned ~
(RequestName ~A); notification server off~%" +notify-name+ result)
         (dbus-disconnect conn)
         nil)))))

(defun notifications-stop ()
  "Stop serving: release the name by closing the connection, forget
every open notification without signals, and hide the one on screen.
Return NIL."
  (let ((conn *notify-conn*))
    (setf *notify-conn* nil
          *notifications* nil)
    (notifications-tick)
    (dbus-disconnect conn)))

(defun notification-signal (msg)
  "Handle one incoming signal: Dismiss (u) on the private interface
closes that notification with reason 2."
  (when (and (equal (message-field msg 2) +notify-private-iface+)
             (equal (message-field msg 3) "Dismiss"))
    (let ((id (first (dbus-message-body msg))))
      (when (integerp id)
        (notification-close id +notification-dismissed+)))))

(defun notifications-drain (&optional (now (notification-now-ms)))
  "One pass on the bar's tick: dispatch the method calls and signals
queued on the server connection, then run NOTIFICATIONS-TICK. A closed
connection turns the server off. Return NIL."
  (let ((conn *notify-conn*))
    (when conn
      (loop
        (let ((msg (bt:with-lock-held ((dbus-connection-queue-lock conn))
                     (pop (dbus-connection-queue conn)))))
          (unless msg (return))
          (when (eq msg :closed)
            (format t "~&; Rhizome: notifications: bus connection ~
closed; notification server off~%")
            (setf *notify-conn* nil
                  *notifications* nil)
            (return))
          (handler-case
              (cond
                ((eql (dbus-message-type msg) +dbus-msg-call+)
                 (dispatch-method-call msg conn))
                ((eql (dbus-message-type msg) +dbus-msg-signal+)
                 (notification-signal msg)))
            (error (e)
              (format t "~&; Rhizome: notifications: ~A.~A: ~A~%"
                      (message-field msg 2) (message-field msg 3) e)))))
      (notifications-tick now)))
  nil)

;;; --- Display (window manager process) ---

(defvar *notification-on-screen* nil
  "(ID . MESSAGE-SERIAL) of the notification the message window was
last given, or NIL. MESSAGE-SERIAL is *MESSAGE-SERIAL* just after it
was shown: a later message replaced it on screen when they differ.")

(defvar *notification-wm-conn* nil
  "The window manager's bus connection for the Dismiss signal.")

(defun notification-read-request (text)
  "The plist a notification-show line carries, read from TEXT with
*READ-EVAL* off, or NIL when TEXT is not a plist with an integer :ID."
  (let ((form (handler-case
                  (with-standard-io-syntax
                    (let ((*read-eval* nil))
                      (read-from-string text)))
                (error () nil))))
    (and (consp form)
         (evenp (length form))
         (integerp (getf form :id))
         form)))

(defun notification-display-text (request)
  "The message window text for REQUEST: \"APP: SUMMARY\" (just the
summary without an app name), then one line per body line."
  (flet ((text (key)
           (let ((value (getf request key)))
             (if (stringp value) value ""))))
    (let ((app (text :app))
          (summary (text :summary)))
      (format nil "~A~{~%~A~}"
              (cond ((zerop (length app)) summary)
                    ((zerop (length summary)) app)
                    (t (format nil "~A: ~A" app summary)))
              (remove-if-not #'stringp (getf request :lines))))))

(defun notification-wm-connection ()
  "The window manager's bus connection, connecting when there is none
or it has closed; NIL when no bus answers."
  (let ((conn *notification-wm-conn*))
    (unless (and conn (dbus-connection-running conn))
      (setf conn (dbus-connect :path (or *notification-bus-path*
                                         (dbus-bus-path)))
            *notification-wm-conn* conn))
    conn))

(defun notification-signal-dismiss (id)
  "Emit Dismiss (ID) for the bar's notification server. Return NIL."
  (let ((conn (notification-wm-connection)))
    (if conn
        (dbus-emit-signal conn +notify-path+ +notify-private-iface+
                          "Dismiss" :signature "u" :values (list id))
        (format t "~&; Rhizome: notifications: no session bus; ~
dismiss of ~D not sent~%" id)))
  nil)

(define-command :notification-show (output args)
  "Show a notification in the message window. The bar's notification
server sends this; the argument is a printed plist."
  (declare (ignore output))
  (let ((request (and (stringp (first args))
                      (notification-read-request (first args)))))
    (cond
      ((null request)
       "notification-show: expected (:id N :summary S :lines (...) :seconds N)")
      (t
       (let ((seconds (getf request :seconds)))
         (show-message (notification-display-text request)
                       (if (and (integerp seconds) (plusp seconds))
                           seconds
                           ;; no expiry: stays until hidden or dismissed
                           86400))
         (setf *notification-on-screen*
               (cons (getf request :id) *message-serial*))
         nil)))))

(define-command :notification-hide (output args)
  "Hide notification ID from the message window when it still shows
there. The bar's notification server sends this."
  (declare (ignore output))
  (let ((id (command-integer-argument (first args)))
        (shown *notification-on-screen*))
    (cond
      ((null id) "notification-hide: expected a notification id")
      (t
       (when (and shown (eql (car shown) id))
         (when (eql (cdr shown) *message-serial*)
           (message-hide))
         (setf *notification-on-screen* nil))
       nil))))

(define-command :notification-dismiss (output args)
  "Close the notification last shown; the next open one shows."
  (declare (ignore output args))
  (let ((shown *notification-on-screen*))
    (cond
      ((null shown) "no notification to dismiss")
      (t
       (when (eql (cdr shown) *message-serial*)
         (message-hide))
       (setf *notification-on-screen* nil)
       (notification-signal-dismiss (car shown))
       nil))))

(pushnew :notification-show *rest-of-line-commands*)
