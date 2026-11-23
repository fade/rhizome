;;; Tests for the [focus] log, which writes one stamped line per:
;;;   - keyboard focus change
;;;   - focus request sent, dropped, or queued during a capture
;;;   - layer shell focus event
;;;   - WM keyboard enter or leave
;;;   - pointer enter or leave
;;;   - window map, close, hide, show, or fullscreen change
;;; Also tests the context an event dispatch error names. River
;;; requests are stubbed.
;;;
;;; Event orders come from a recorded WM log of one C-' prefix capture
;;; on a Konsole window: key pressed C-', layer shell exclusive focus,
;;; WM keyboard enter, layer shell focus none, then s-f / s-b key
;;; presses. The recording carries no app-ids, keyboard serials, or
;;; surface ids, so the windows here have no app-id and the serial and
;;; surface values are placeholders. Window titles are the recorded
;;; Konsole titles.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defparameter *konsole-title-1*
  (format nil "(fade) 192.168.1.3 ~C Konsole" (code-char #x2014))
  "Recorded Konsole title, focused when the capture opened.")

(defparameter *konsole-title-2*
  (format nil "~~ : zsh ~C Konsole" (code-char #x2014))
  "Recorded Konsole title of a second window.")

(let ((pass 0) (fail 0) (river '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Record river requests instead of sending them
    (dolist (fn '(message show-message message-hide request-manage-dirty))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    (setf (symbol-function 'rwm:river-seat-v1.set-xcursor-theme)
          (lambda (&rest args)
            (declare (ignore args))
            nil)
          (symbol-function 'rwm:river-seat-v1.get-pointer-binding)
          (lambda (&rest args)
            (declare (ignore args))
            (make-instance 'wl:wl-proxy))
          (symbol-function 'rwm:river-layer-shell-v1.get-seat)
          (lambda (layer-shell seat-proxy)
            (declare (ignore layer-shell seat-proxy))
            (make-instance 'rwm:river-layer-shell-seat-v1))
          (symbol-function 'rwm:river-seat-v1.focus-window)
          (lambda (seat-proxy window-proxy)
            (declare (ignore seat-proxy))
            (push (list :focus window-proxy) river)
            nil)
          (symbol-function 'rwm:river-seat-v1.clear-focus)
          (lambda (seat-proxy)
            (declare (ignore seat-proxy))
            (push (list :clear) river)
            nil))

    (labels ((reset ()
               (configure '(:debug (:focus t)))
               (setf *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *seats-with-pointer-bindings* (make-hash-table)
                     *pending-focus-requests* nil
                     *pending-close-requests* nil
                     *pending-pointer-warps* nil
                     *pointer-bindings* nil
                     *prefix-state* nil
                     *prefix-mode-active* nil
                     *river-layer-shell* nil
                     river nil))
             (new-seat ()
               (let ((proxy (make-instance 'rwm:river-seat-v1)))
                 (with-output-to-string (*standard-output*)
                   (register-seat proxy))
                 (gethash proxy *seats*)))
             (make-win (proxy title)
               (setf (gethash proxy *windows*)
                     (make-instance 'rhizome-window :proxy proxy :title title)))
             (focus-lines (text)
               ;; The [focus] lines of TEXT, in order
               (remove-if-not (lambda (line) (search "[focus]" line))
                              (uiop:split-string text :separator '(#\Newline))))
             (event-of (line)
               ;; The event token after the stamp
               (second (uiop:split-string
                        (subseq line (length "; Rhizome [focus] "))
                        :separator '(#\Space))))
             (events (text)
               (mapcar #'event-of (focus-lines text)))
             (has (line part)
               (and line (search part line) t))
             (layer-event (opcode)
               ;; Deliver a layer shell seat event as wayflan reads it:
               ;; 0 focus_exclusive, 1 focus_non_exclusive, 2 focus_none
               (let* ((ls (make-instance 'rwm:river-layer-shell-seat-v1))
                      (table (slot-value (find-class 'rwm:river-layer-shell-seat-v1)
                                         'wl::%event-table))
                      (event (funcall (aref table opcode) ls nil)))
                 (apply #'handle-layer-shell-seat-event event)))
             (start ()
               ;; One seat focused on the first recorded Konsole window
               (reset)
               (let ((seat (new-seat))
                     (a (make-win :ka *konsole-title-1*)))
                 (with-output-to-string (*standard-output*)
                   (focus-window a seat)
                   (flush-focus-requests))
                 (setf river nil)
                 (values seat a))))

      ;; Stamp: UTC ISO-8601 with milliseconds
      (let ((stamp (focus-log-timestamp)))
        (ok "the stamp is UTC ISO-8601 with milliseconds"
            (and (cl-ppcre:scan "^\\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d\\.\\d{3}Z$"
                                stamp)
                 t)
            t)
        (ok "the stamp names the current UTC month"
            (multiple-value-bind (s m h day month year)
                (decode-universal-time (get-universal-time) 0)
              (declare (ignore s m h day))
              (format nil "~4,'0D-~2,'0D" year month))
            (subseq stamp 0 7)))

      ;; A focus change between two windows
      (let* ((seat (start))
             (b (make-win :kb *konsole-title-2*))
             (text (with-output-to-string (*standard-output*)
                     (let ((*focus-cause* :key))
                       (focus-window b seat))))
             (lines (focus-lines text)))
        (ok "a focus change logs one [focus] line"
            (events text)
            '("focus"))
        (ok "the focus line names the window left"
            (has (first lines) "from=ka:-:\"(fade) 192.168.1.3")
            t)
        (ok "the focus line names the window focused"
            (has (first lines) "to=kb:-:\"~ : zsh")
            t)
        (ok "the focus line names the cause"
            (has (first lines) "cause=key")
            t)
        (ok "the focus line carries a stamp"
            (and (cl-ppcre:scan "^; Rhizome \\[focus\\] \\d{4}-\\d\\d-\\d\\dT\\d\\d:\\d\\d:\\d\\d\\.\\d{3}Z "
                                (first lines))
                 t)
            t)
        (ok "the sent focus_window is logged once, not as a repeat"
            (let ((flush (focus-lines (with-output-to-string (*standard-output*)
                                        (flush-focus-requests)))))
              (list (length flush)
                    (has (first flush) "focus_window")
                    (has (first flush) "REPEAT")))
            '(1 t nil)))

      ;; Focus on the window already focused is no state change
      (multiple-value-bind (seat a) (start)
        (ok "focusing the focused window logs no focus line"
            (events (with-output-to-string (*standard-output*)
                      (focus-window a seat)))
            nil))

      ;; The recorded capture cycle
      (let* ((seat (start))
             (kb (make-instance 'wl:wl-proxy))
             (text (with-output-to-string (*standard-output*)
                     (setup-keyboard-handler seat kb)
                     (layer-event 0)
                     (funcall (first (wl:wl-proxy-hooks kb))
                              :enter 1 :capture-surface #())
                     (layer-event 2)
                     (flush-focus-requests)))
             (lines (focus-lines text)))
        (ok "the capture cycle logs in the recorded order"
            (events text)
            '("layer-exclusive" "wm-keyboard-enter" "layer-none"
              "reassert" "flush"))
        (ok "the layer exclusive line names the focused window"
            (has (first lines) "focused=ka:")
            t)
        (ok "the WM keyboard enter line names the surface"
            (has (second lines) "surface=capture-surface")
            t)
        (ok "the reassert after focus none is sent without REPEAT"
            (list (has (fifth lines) "focus_window")
                  (has (fifth lines) "REPEAT"))
            '(t nil))
        (ok "the reassert reaches river"
            river
            '((:focus :ka))))

      ;; A second focus_window for the same window with no layer event
      ;; between is a repeat
      (start)
      (with-output-to-string (*standard-output*)
        (reassert-seat-focus))
      (ok "a focus_window for the window last sent is flagged REPEAT"
          (let ((flush (focus-lines (with-output-to-string (*standard-output*)
                                      (flush-focus-requests)))))
            (list (length flush)
                  (has (first flush) "focus_window")
                  (has (first flush) "REPEAT")))
          '(1 t t))
      (ok "the repeated request still reaches river"
          river
          '((:focus :ka)))

      ;; WM keyboard leave
      (let* ((seat (start))
             (kb (make-instance 'wl:wl-proxy))
             (text (with-output-to-string (*standard-output*)
                     (setup-keyboard-handler seat kb)
                     (funcall (first (wl:wl-proxy-hooks kb))
                              :leave 2 :capture-surface))))
        (ok "a WM keyboard leave logs one line"
            (events text)
            '("wm-keyboard-leave")))

      ;; Layer non-exclusive: no recording; the event comes from the
      ;; protocol's reader as in the other layer tests
      (start)
      (ok "layer non-exclusive focus logs one line"
          (events (with-output-to-string (*standard-output*)
                    (layer-event 1)))
          '("layer-non-exclusive"))

      ;; A focus request for a window that has gone
      (let* ((seat (start))
             (b (make-win :kb *konsole-title-2*)))
        (with-output-to-string (*standard-output*)
          (focus-window b seat))
        (remhash :kb *windows*)
        (let ((lines (focus-lines (with-output-to-string (*standard-output*)
                                    (flush-focus-requests)))))
          (ok "a request for a closed window is logged as dropped"
              (list (length lines) (has (first lines) "dropped"))
              '(1 t))
          (ok "a dropped request sends nothing"
              river
              nil)))

      ;; A focus clear
      (let* ((seat (start))
             (text (with-output-to-string (*standard-output*)
                     (let ((*focus-cause* :workspace-switch))
                       (clear-focus seat))
                     (flush-focus-requests)))
             (lines (focus-lines text)))
        (ok "a focus clear logs the change and the clear_focus sent"
            (events text)
            '("focus" "flush"))
        (ok "the clear names no window as the target"
            (list (has (first lines) "to=none")
                  (has (first lines) "cause=workspace-switch")
                  (has (second lines) "clear_focus"))
            '(t t t)))

      ;; A request queued while the prefix capture is open. No
      ;; recording holds one; the capture state is set directly.
      (let* ((seat (start))
             (b (make-win :kb *konsole-title-2*)))
        (setf *prefix-mode-active* t)
        (let ((text (with-output-to-string (*standard-output*)
                      (focus-window b seat))))
          (ok "a request queued during the capture is logged"
              (events text)
              '("focus" "queued-during-capture")))
        (setf *prefix-mode-active* nil))

      ;; Pointer enter and leave. No recording holds these events.
      (let* ((seat (start))
             (text (with-output-to-string (*standard-output*)
                     (handle-seat-event seat :pointer-enter :ka)
                     (handle-seat-event seat :pointer-leave)))
             (lines (focus-lines text)))
        (ok "pointer enter and leave log one line each"
            (events text)
            '("pointer-enter" "pointer-leave"))
        (ok "pointer leave names the window the pointer left"
            (has (second lines) "window=ka:")
            t))

      ;; A :focus nil window rule keeps a newly mapped window from
      ;; autofocus. No recording holds this; the rule and the pending
      ;; autofocus are set directly.
      (start)
      (let ((b (make-win :kb *konsole-title-2*)))
        (setf (window-app-id b) "quiet"
              *pending-autofocus* b)
        (configure '(:window-rules ((:match "^quiet$" :focus nil))))
        (let* ((text (with-output-to-string (*standard-output*)
                       (apply-window-rules b)))
               (lines (focus-lines text)))
          (ok "a :focus nil rule logs the autofocus it suppresses"
              (events text)
              '("autofocus-suppressed"))
          (ok "the suppression line names the window and the rule"
              (list (has (first lines) "window=kb:quiet:")
                    (has (first lines) "reason=rule"))
              '(t t))
          (ok "the suppressed window gets no autofocus or focus request"
              (list *pending-autofocus* *pending-focus-requests*)
              '(nil nil)))
        (configure '(:window-rules nil)))

      ;; (:debug (:focus nil)) silences every [focus] line
      (let* ((seat (start))
             (b (make-win :kb *konsole-title-2*))
             (kb (make-instance 'wl:wl-proxy))
             (text (progn
                     (configure '(:debug (:focus nil)))
                     (with-output-to-string (*standard-output*)
                       (setup-keyboard-handler seat kb)
                       (focus-window b seat)
                       (flush-focus-requests)
                       (layer-event 0)
                       (funcall (first (wl:wl-proxy-hooks kb))
                                :enter 1 :capture-surface #())
                       (layer-event 2)
                       (flush-focus-requests)
                       (handle-seat-event seat :pointer-enter :ka)))))
        (ok "(:debug (:focus nil)) silences every [focus] line"
            (focus-lines text)
            nil)
        (ok "focus requests are still sent with the log off"
            (reverse river)
            '((:focus :kb) (:focus :kb)))
        (configure '(:debug (:focus t))))

      ;; Dispatch errors name the object, the opcode, and the step. The
      ;; recorded error is a read on a destroyed proxy.
      (let* ((proxy (make-instance 'rwm:river-window-v1))
             (text (progn
                     (setf (wl::%wl-proxy-id proxy) 21)
                     (change-class proxy 'wl:wl-destroyed-proxy)
                     (with-output-to-string (*standard-output*)
                       (handler-case (dispatch-message proxy 0 nil)
                         (error (e)
                           (report-dispatch-error e)))))))
        (ok "a read error on a destroyed proxy is reported"
            (and (search "error in event dispatch" text) t)
            t)
        (ok "the report names the step, class, id, and opcode"
            (list (has text "step=read")
                  (has text "object=wl-destroyed-proxy")
                  (has text "id=21")
                  (has text "opcode=0"))
            '(t t t t)))

      ;; A handler error names the event read
      (let* ((proxy (make-instance 'rwm:river-layer-shell-seat-v1))
             (text (progn
                     (setf (wl::%wl-proxy-id proxy) 40)
                     (push (lambda (&rest args)
                             (declare (ignore args))
                             (error "handler failed"))
                           (wl:wl-proxy-hooks proxy))
                     (with-output-to-string (*standard-output*)
                       (handler-case (dispatch-message proxy 2 nil)
                         (error (e)
                           (report-dispatch-error e)))))))
        (ok "a handler error names the dispatch step and the event"
            (list (has text "step=dispatch")
                  (has text "event=focus-none")
                  (has text "id=40")
                  (has text "handler failed"))
            '(t t t t))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
