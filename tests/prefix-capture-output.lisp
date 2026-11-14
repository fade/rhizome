;;; Tests for the output of the keyboard capture surfaces on a
;;; multi-head host: the prefix capture, its which-key message, and the
;;; prompt open on the current output's wl_output, never on whichever
;;; output the compositor picks for a surface without one. Also covers
;;; focus_none while the prefix capture is open, and the log line for a
;;; key the capture receives. River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (layers '()) (binds '()) (manage-requests 0)
      (bound (make-hash-table)))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (try (name thunk expected)
             ;; An error is a failure, not a crash: every case reports
             (ok name
                 (handler-case (funcall thunk)
                   (error (e) (list :error (princ-to-string e))))
                 expected)))

    ;; Record layer surfaces and registry binds instead of touching river
    (dolist (fn '(rwm:zwlr-layer-surface-v1.set-anchor
                  rwm:zwlr-layer-surface-v1.set-size
                  rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
                  rwm:zwlr-layer-surface-v1.set-margin
                  rwm:zwlr-layer-surface-v1.destroy
                  wl:wl-surface.commit wl:wl-surface.destroy
                  bar-destroy-shm))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    (setf (symbol-function 'request-manage-dirty)
          (lambda (&rest args)
            (declare (ignore args))
            (incf manage-requests)
            nil)
          (symbol-function 'wl:wl-compositor.create-surface)
          (lambda (compositor)
            (declare (ignore compositor))
            (make-instance 'wl:wl-proxy))
          (symbol-function 'rwm:zwlr-layer-shell-v1.get-layer-surface)
          (lambda (shell surface output layer namespace)
            (declare (ignore shell surface layer))
            (push (list namespace output) layers)
            (make-instance 'wl:wl-proxy))
          (symbol-function 'wl:wl-registry.bind)
          (lambda (registry name interface version)
            (declare (ignore registry interface))
            (push (list name version) binds)
            (setf (gethash name bound) (make-instance 'wl:wl-proxy))))

    (labels ((reset ()
               (setf *outputs* (make-hash-table)
                     *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *selected-output* nil
                     *pending-focus-requests* nil
                     *prefix-mode-active* nil
                     *prefix-state* nil
                     *prefix-command-mode* nil
                     *expose-active* nil
                     *iresize-active* nil
                     *prompt-active* nil
                     *prompt-state* nil
                     *eval-prompt-active* nil
                     *launcher-active* nil
                     *message-state* nil
                     *registry* :registry
                     *compositor* :compositor
                     *shm* :shm
                     *layer-shell* :layer-shell
                     layers nil
                     binds nil
                     manage-requests 0)
               (clrhash bound)
               (when (boundp '*output-wl-outputs*)
                 (clrhash (symbol-value '*output-wl-outputs*))))
             (make-output (proxy x name)
               (setf (gethash proxy *outputs*)
                     (make-instance 'rhizome-output
                                    :proxy proxy :x x :y 0
                                    :width 1920 :height 1080
                                    :wl-output-name name)))
             (two-heads ()
               ;; HDMI-A-1 (wl_output 41) registers first; the operator
               ;; works on DVI-D-1 (wl_output 42)
               (reset)
               (let ((hdmi (make-output :hdmi 0 41))
                     (dvi (make-output :dvi 1920 42)))
                 (setf *selected-output* dvi)
                 (values hdmi dvi)))
             (output-of (namespace)
               ;; The output passed for the most recent surface NAMESPACE
               (second (find namespace layers :key #'first :test #'string=)))
             (on-head (namespace name)
               ;; True when surface NAMESPACE got the wl_output bound
               ;; for global NAME
               (let ((wl-output (gethash name bound)))
                 (and wl-output (eq (output-of namespace) wl-output))))
             (quiet (thunk)
               (let ((*standard-output* (make-broadcast-stream)))
                 (funcall thunk))))

      ;; The prefix capture and its which-key message
      (two-heads)
      (quiet #'enter-prefix-mode)
      (try "the prefix capture opens on the current output's wl_output"
           (lambda () (on-head "rhizome-prefix" 42))
           t)
      (try "the current output's wl_output is bound from its global name"
           (lambda () (mapcar #'first binds))
           '(42))
      (try "the which-key message opens on the current output's wl_output"
           (lambda () (on-head "rhizome-message" 42))
           t)
      (try "neither surface is left for the compositor to place"
           (lambda () (remove nil (mapcar #'second layers)
                              :test-not #'eq))
           nil)

      ;; A second key sequence reuses the binding
      (quiet #'exit-prefix-mode)
      (quiet #'enter-prefix-mode)
      (try "a wl_output is bound once per output global"
           (lambda () (mapcar #'first binds))
           '(42))

      ;; The operator moves to the other head
      (multiple-value-bind (hdmi dvi) (two-heads)
        (declare (ignore dvi))
        (quiet #'enter-prefix-mode)
        (setf *selected-output* hdmi)
        (quiet #'exit-prefix-mode)
        (quiet #'enter-prefix-mode)
        (try "the capture follows the current output to the other head"
             (lambda () (on-head "rhizome-prefix" 41))
             t)
        (try "the which-key message follows it too"
             (lambda () (on-head "rhizome-message" 41))
             t))

      ;; A message already showing on another head
      (multiple-value-bind (hdmi dvi) (two-heads)
        (declare (ignore dvi))
        (quiet (lambda () (show-message "one")))
        (setf *selected-output* hdmi)
        (quiet (lambda () (show-message "two")))
        (try "a message showing on another head moves to the current output"
             (lambda () (on-head "rhizome-message" 41))
             t)
        (try "the moved message carries the new text"
             (lambda () (and *message-state*
                             (gethash :text *message-state*)))
             "two"))

      ;; The prompt a prefix command opens
      (two-heads)
      (quiet (lambda () (prompt-open "Shell:" :candidates nil)))
      (try "the prompt opens on the current output's wl_output"
           (lambda () (on-head "rhizome-prompt" 42))
           t)

      ;; No usable output
      (reset)
      (make-output :anon 0 nil)
      (quiet #'enter-prefix-mode)
      (try "an output without a wl_output name leaves the choice to river"
           (lambda () (list (output-of "rhizome-prefix") binds))
           '(nil nil))
      (reset)
      (quiet #'enter-prefix-mode)
      (try "with no outputs the capture still opens without one"
           (lambda () (list (and (find "rhizome-prefix" layers
                                       :key #'first :test #'string=)
                                 t)
                            (output-of "rhizome-prefix")))
           '(t nil))

      ;; A removed output's binding is dropped
      (multiple-value-bind (hdmi dvi) (two-heads)
        (declare (ignore hdmi))
        (quiet #'enter-prefix-mode)
        (quiet #'exit-prefix-mode)
        (quiet (lambda () (handle-output-event dvi :removed)))
        (let ((again (make-output :dvi-2 1920 42)))
          (setf *selected-output* again))
        (quiet #'enter-prefix-mode)
        (try "an output that returns is bound again"
             (lambda () (mapcar #'first binds))
             '(42 42)))

      ;; focus_none while the prefix capture is open
      (flet ((focused-seat ()
               (let* ((seat (make-instance 'rhizome-seat :proxy :seat))
                      (win (make-instance 'rhizome-window :proxy :win)))
                 (setf (gethash :seat *seats*) seat
                       (gethash :win *windows*) win
                       (seat-focused-window seat) win
                       (window-focused-p win) t)
                 seat)))
        (two-heads)
        (focused-seat)
        (quiet #'enter-prefix-mode)
        (setf *pending-focus-requests* nil
              manage-requests 0)
        (let ((text (with-output-to-string (*standard-output*)
                      (handle-layer-shell-seat-event :focus-none))))
          (try "focus_none while the capture is open queues no window focus"
               (lambda () (list *pending-focus-requests* manage-requests))
               '(nil 0))
          (try "focus_none while the capture is open is logged as such"
               (lambda () (and (search "prefix capture" text) t))
               t))
        (quiet #'exit-prefix-mode)
        (setf *pending-focus-requests* nil)
        (quiet (lambda () (handle-layer-shell-seat-event :focus-none)))
        (try "focus_none after the capture closes gives the window the keyboard"
             (lambda () (length *pending-focus-requests*))
             1)
        ;; The compositor closed the capture: its handler clears the mode
        (two-heads)
        (focused-seat)
        (quiet #'enter-prefix-mode)
        (setf *prefix-mode-active* nil
              *pending-focus-requests* nil)
        (quiet (lambda () (handle-layer-shell-seat-event :focus-none)))
        (try "focus_none after the compositor closed the capture refocuses"
             (lambda () (length *pending-focus-requests*))
             1))

      ;; A key the capture receives is logged
      (two-heads)
      (quiet #'enter-prefix-mode)
      (let ((text (with-output-to-string (*standard-output*)
                    (prefix-handle-key 65307))))
        (try "a key reaching the prefix capture is logged"
             (lambda () (and (search "prefix key Esc" text) t))
             t)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
