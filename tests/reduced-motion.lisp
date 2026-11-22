;;; Reduced motion (reduced-motion.lisp, animate.lisp): with
;;; (:animations :reduced-motion :follow), the default, the desktop's
;;; org.freedesktop.appearance reduced-motion setting, read through the
;;; settings portal, turns every motion kind off; T always reduces and
;;; NIL ignores the desktop. Portal replies are faked: 0, 1, an absent
;;; key, a portal without ReadOne, no reply, and a failing call.
;;;
;;; Fixture shapes: the ReadOne return body (("u" . N)) and the
;;; org.freedesktop.portal.Error.NotFound error for an absent
;;; reduced-motion key match replies recorded from xdg-desktop-portal
;;; 1.22.1 through dbus.lisp. No portal at hand sets the key, and none
;;; lacks ReadOne: the Read double variant (("v" . ("u" . N))) follows
;;; the interface description in org.freedesktop.portal.Settings.xml.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (replies '()) (members '()))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (use (animations)
             (setf *config* (merge-plist (default-config)
                                         (list :bar '(:enabled nil)
                                               :animations animations))))
           (return-msg (&rest body)
             (make-dbus-message :type +dbus-msg-return+ :body body))
           (error-msg (name)
             (make-dbus-message :type +dbus-msg-error+
                                :fields (list (cons 4 name))
                                :body (list "fake error")))
           (read-with (script)
             ;; SCRIPT maps a method name to the reply message, :signal
             ;; to make the call signal, or NIL for no reply
             (setf replies script members '())
             (portal-read-reduced-motion :fake-conn))
           (kinds-on ()
             (mapcar #'animation-kind-enabled-p '(:workspace :expose :float)))
           (setting-changed (namespace key value)
             (make-dbus-message
              :type +dbus-msg-signal+
              :fields (list (cons 2 "org.freedesktop.portal.Settings")
                            (cons 3 "SettingChanged"))
              :body (list namespace key value))))
    (setf (symbol-function 'dbus-call)
          (lambda (conn destination path interface member &key values
                   &allow-other-keys)
            (declare (ignore conn destination path interface))
            (push (cons member values) members)
            (let ((reply (cdr (assoc member replies :test #'string=))))
              (when (eq reply :signal)
                (error "fake transport failure"))
              (values reply (and reply (dbus-message-body reply))))))

    ;; --- reading the portal ---

    (ok "ReadOne 0: no preference"
        (read-with (list (cons "ReadOne" (return-msg '("u" . 0))))) nil)
    (ok "ReadOne asks for org.freedesktop.appearance reduced-motion"
        (first members)
        '("ReadOne" "org.freedesktop.appearance" "reduced-motion"))
    (ok "ReadOne 1: reduced"
        (read-with (list (cons "ReadOne" (return-msg '("u" . 1))))) t)
    (ok "absent key (NotFound): no preference"
        (read-with (list (cons "ReadOne"
                               (error-msg "org.freedesktop.portal.Error.NotFound"))))
        nil)
    (ok "no ReadOne: Read's double variant 1 is reduced"
        (read-with (list (cons "ReadOne"
                               (error-msg "org.freedesktop.DBus.Error.UnknownMethod"))
                         (cons "Read" (return-msg '("v" . ("u" . 1))))))
        t)
    (ok "no ReadOne: Read's 0 is no preference"
        (read-with (list (cons "ReadOne"
                               (error-msg "org.freedesktop.DBus.Error.UnknownMethod"))
                         (cons "Read" (return-msg '("v" . ("u" . 0))))))
        nil)
    (ok "no portal (ServiceUnknown): no preference"
        (read-with (list (cons "ReadOne"
                               (error-msg "org.freedesktop.DBus.Error.ServiceUnknown"))))
        nil)
    (ok "no reply: no preference"
        (read-with '()) nil)
    (ok "a failing call: no preference, nothing signalled"
        (handler-case (read-with (list (cons "ReadOne" :signal)))
          (error () :signalled))
        nil)
    (ok "a reply of the wrong type: no preference"
        (read-with (list (cons "ReadOne" (return-msg '("s" . "1"))))) nil)

    ;; --- the setting ---

    (let ((*desktop-reduced-motion* nil))
      (use '())
      (ok "default is :follow"
          (getf (getf (default-config) :animations) :reduced-motion) :follow)
      (ok ":follow, desktop no preference: every kind on"
          (kinds-on) '(t t t))
      (setf *desktop-reduced-motion* t)
      (ok ":follow, desktop reduced: every kind off"
          (kinds-on) '(nil nil nil))
      (ok ":follow, desktop reduced: reduced-motion-p"
          (reduced-motion-p) t)
      (use '(:reduced-motion nil))
      (ok "NIL ignores the desktop: every kind on"
          (kinds-on) '(t t t))
      (setf *desktop-reduced-motion* nil)
      (use '(:reduced-motion t))
      (ok "T reduces without the desktop: every kind off"
          (kinds-on) '(nil nil nil))
      (let ((*animations* (make-hash-table))
            (win (make-instance 'rhizome-window :proxy :rm-win)))
        (setf (window-x win) 0 (window-y win) 0)
        (animation-start win :kind :float :from-x 9)
        (ok "reduced: animation-start starts nothing"
            (animation-active-p win) nil)))

    ;; --- SettingChanged ---

    (let ((*desktop-reduced-motion* nil))
      (reduced-motion-signal
       (setting-changed "org.freedesktop.appearance" "reduced-motion" '("u" . 1)))
      (ok "SettingChanged 1: reduced" *desktop-reduced-motion* t)
      (reduced-motion-signal
       (setting-changed "org.freedesktop.appearance" "color-scheme" '("u" . 0)))
      (ok "another key leaves the setting alone" *desktop-reduced-motion* t)
      (reduced-motion-signal
       (setting-changed "org.freedesktop.appearance" "reduced-motion" '("u" . 0)))
      (ok "SettingChanged 0: no preference" *desktop-reduced-motion* nil)
      ;; The main-loop drain dispatches queued signals
      (let ((*reduced-motion-conn*
              (make-dbus-connection
               :queue (list (setting-changed "org.freedesktop.appearance"
                                             "reduced-motion" '("u" . 1)))
               :queue-lock (bt:make-lock "fake-queue")
               :running t)))
        (reduced-motion-drain)
        (ok "the drain applies a queued SettingChanged"
            *desktop-reduced-motion* t)
        (ok "the drain empties the queue"
            (dbus-connection-queue *reduced-motion-conn*) nil)))

    ;; --- no bus ---

    (let ((*desktop-reduced-motion* nil)
          (*reduced-motion-conn* nil))
      (ok "no session bus: no connection, nothing signalled"
          (handler-case (reduced-motion-connect :path "/nonexistent/bus")
            (error () :signalled))
          nil)
      (ok "no session bus: no preference" *desktop-reduced-motion* nil)))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
