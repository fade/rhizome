;;; Reduced motion from the desktop
;;;
;;; The settings portal (org.freedesktop.portal.Settings) publishes the
;;; desktop's org.freedesktop.appearance reduced-motion key: u, 0 for no
;;; preference, 1 for reduced. The WM process reads it once at start
;;; and follows SettingChanged afterwards, on a session-bus connection
;;; of its own made with rhizome's D-Bus client (dbus.lisp). The value
;;; lands in *desktop-reduced-motion*, which reduced-motion-p consults
;;; under (:animations :reduced-motion :follow).
;;;
;;; An absent key, a missing portal, no session bus, or any D-Bus error
;;; reads as no preference. Nothing here signals to the caller.

(in-package #:rhizome)

(defparameter +portal-name+ "org.freedesktop.portal.Desktop"
  "Bus name of the desktop portal.")

(defparameter +portal-path+ "/org/freedesktop/portal/desktop"
  "Object path of the desktop portal.")

(defparameter +portal-settings-iface+ "org.freedesktop.portal.Settings"
  "The portal's settings interface.")

(defparameter +appearance-namespace+ "org.freedesktop.appearance"
  "Settings namespace holding reduced-motion.")

(defvar *reduced-motion-conn* nil
  "The WM's session-bus connection for portal settings, or NIL.")

(defun portal-reduced-motion-value (value)
  "True when VALUE, a decoded variant (SIGNATURE . DATUM), possibly
nested, holds the uint32 1. Any other value is no preference."
  (loop while (and (consp value) (equal (car value) "v"))
        do (setf value (cdr value)))
  (and (consp value)
       (equal (car value) "u")
       (eql (cdr value) 1)))

(defun portal-read-reduced-motion (conn)
  "Read reduced-motion from the settings portal over CONN. ReadOne
first; when the portal lacks it (UnknownMethod), the deprecated Read.
Return T for reduced, NIL for no preference, an absent key, an error
reply, no reply, or a failed call."
  (handler-case
      (flet ((read-setting (member)
               (multiple-value-bind (reply body)
                   (dbus-call conn +portal-name+ +portal-path+
                              +portal-settings-iface+ member
                              :signature "ss"
                              :values (list +appearance-namespace+
                                            "reduced-motion")
                              :timeout 2)
                 (values reply (first body)))))
        (multiple-value-bind (reply value) (read-setting "ReadOne")
          (when (and reply
                     (eql (dbus-message-type reply) +dbus-msg-error+)
                     (equal (message-field reply 4)
                            "org.freedesktop.DBus.Error.UnknownMethod"))
            (multiple-value-setq (reply value) (read-setting "Read")))
          (and reply
               (eql (dbus-message-type reply) +dbus-msg-return+)
               (portal-reduced-motion-value value))))
    (error () nil)))

(defun reduced-motion-signal (msg)
  "Apply MSG when it is SettingChanged for the appearance namespace's
reduced-motion key: set *desktop-reduced-motion*. Ignore any other
message. Return T when MSG was applied."
  (let ((body (dbus-message-body msg)))
    (when (and (eql (dbus-message-type msg) +dbus-msg-signal+)
               (equal (message-field msg 2) +portal-settings-iface+)
               (equal (message-field msg 3) "SettingChanged")
               (listp body)
               (equal (first body) +appearance-namespace+)
               (equal (second body) "reduced-motion"))
      (setf *desktop-reduced-motion*
            (portal-reduced-motion-value (third body)))
      t)))

(defun reduced-motion-connect (&key (path (dbus-bus-path)))
  "Connect to the session bus at PATH, subscribe to the portal's
SettingChanged for the appearance namespace, and read reduced-motion
into *desktop-reduced-motion*. Return the connection, or NIL when no
bus answers or setup fails; either way nothing is signalled."
  (handler-case
      (let ((conn (dbus-connect :path path)))
        (when conn
          (dbus-add-match
           conn (format nil "type='signal',interface='~A',~
member='SettingChanged',arg0='~A'"
                        +portal-settings-iface+ +appearance-namespace+))
          (setf *desktop-reduced-motion* (portal-read-reduced-motion conn)
                *reduced-motion-conn* conn)
          (format t "~&; Rhizome: reduced motion: desktop ~
~:[has no preference~;asks for reduced motion~]~%"
                  *desktop-reduced-motion*)
          conn))
    (error (e)
      (format t "~&; Rhizome: reduced motion: portal unreadable: ~A~%" e)
      nil)))

(defun reduced-motion-start ()
  "Read the desktop's reduced-motion setting in a background thread,
so a slow or absent portal never delays the WM. Return NIL."
  (unless *reduced-motion-conn*
    (bt:make-thread #'reduced-motion-connect
                    :name "rhizome-reduced-motion"))
  nil)

(defun reduced-motion-drain ()
  "Apply the SettingChanged signals queued on *reduced-motion-conn*.
Runs on the WM main thread; a closed connection is dropped and the
last value kept. Return NIL."
  (let ((conn *reduced-motion-conn*))
    (when conn
      (loop
        (let ((msg (bt:with-lock-held ((dbus-connection-queue-lock conn))
                     (pop (dbus-connection-queue conn)))))
          (cond ((null msg) (return))
                ((eq msg :closed)
                 (setf *reduced-motion-conn* nil)
                 (return))
                ((dbus-message-p msg)
                 (handler-case (reduced-motion-signal msg)
                   (error () nil))))))))
  nil)
