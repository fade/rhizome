;;; Tests for the :focus nil window rule key: a matching window is
;;; placed as its rule says but takes no keyboard focus when it maps,
;;; on the showing workspace, in raw mode, or with :follow. Explicit
;;; focus afterwards still reaches it. Windows, outputs, and a seat are
;;; fakes; river requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (macrolet ((ok (name actual expected)
               ;; An error in ACTUAL is a failure, not a crash, so one
               ;; broken check does not hide the rest
               `(let ((got (handler-case ,actual
                             (error (e) (list :error (princ-to-string e)))))
                      (want ,expected))
                  (if (equal got want)
                      (progn (incf pass)
                             (format t "~&PASS: ~A~%" ,name))
                      (progn (incf fail)
                             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                     ,name got want))))))
    ;; River-touching stubs
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-borders show-window
                  hide-window message show-message request-manage-dirty
                  recommend-dimension-bounds set-window-clip-box
                  set-window-position install-keybindings-if-needed
                  enable-pointer-bindings-if-needed sync-window-decorations
                  flush-fullscreen-requests
                  rwm:river-seat-v1.focus-window rwm:river-seat-v1.clear-focus
                  rwm:river-seat-v1.pointer-warp))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (when (and args (typep (first args) 'rhizome-window))
                  (when (eq fn-sym 'show-window)
                    (setf (window-hidden-p (first args)) nil))
                  (when (eq fn-sym 'hide-window)
                    (setf (window-hidden-p (first args)) t)))
                nil))))
    (setf *config* (default-config))
    (configure '(:bar (:enabled nil)
                 :animations (:enabled nil)
                 :pointer (:warp-on-switch nil)
                 :window-rules ((:match "^quiet$" :focus nil)
                                (:match "^quietraw$" :raw t :focus nil)
                                (:match "^viewer$" :output 1 :workspace 1
                                 :raw t :focus nil)
                                (:match "^quietfollow$" :workspace 4
                                 :follow t :focus nil)
                                (:match "^emptyfollow$" :workspace 5
                                 :follow t :focus nil)
                                (:match "^loudraw$" :raw t)
                                (:match "^loudt$" :raw t :focus t))))
    (setf *windows* (make-hash-table)
          *outputs* (make-hash-table)
          *seats* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *workspace-last-focus* (make-hash-table :test 'equal)
          *pending-focus-requests* nil
          *pending-pointer-warps* nil
          *pending-autofocus* nil
          *last-registered-window* nil
          *prefix-mode-active* nil
          *prefix-state* nil)
    (let ((hdmi (make-instance 'rhizome-output :proxy :nf-hdmi
                               :x 0 :y 0 :width 1920 :height 1080))
          (seat (make-instance 'rhizome-seat :proxy :nf-seat)))
      (setf (gethash :nf-hdmi *outputs*) hdmi
            (gethash :nf-seat *seats*) seat
            (gethash hdmi *output-workspace*) 1)
      (labels ((pass ()
                 ;; One manage sequence, then one render sequence
                 (with-output-to-string (*standard-output*)
                   (on-manage-sequence)
                   (setf *pending-focus-requests* nil)
                   (maphash (lambda (proxy out)
                              (declare (ignore proxy))
                              (render-layout-for-output out))
                            *outputs*)))
               (arrive (proxy app-id)
                 ;; A new window as river announces it: registered,
                 ;; awaiting autofocus, then its app-id
                 (let ((win (make-instance 'rhizome-window :proxy proxy)))
                   (setf (gethash proxy *windows*) win
                         *last-registered-window* win
                         *pending-autofocus* win)
                   (assign-window-number win)
                   (with-output-to-string (*standard-output*)
                     (handle-window-event win :app-id app-id))
                   win))
               (focused ()
                 (seat-focused-window seat))
               (raw-keys-active-p ()
                 (or (not (null (focused-raw-state)))
                     (not (equal (active-top-keymaps) (list *top-map*))))))
        ;; A terminal already open and focused on HDMI workspace 1
        (let ((term (make-instance 'rhizome-window :proxy :nf-term
                                                   :app-id "term")))
          (setf (gethash :nf-term *windows*) term)
          (assign-window-number term)
          (pass)
          (focus-window term seat)
          (pass)
          (ok "the terminal holds focus before any rule window maps"
              (focused) term)

          ;; --- :focus nil on the showing workspace ---
          (let ((quiet (arrive :nf-quiet "quiet")))
            (pass)
            (ok ":focus nil window opens on the showing workspace"
                (list (window-output quiet) (window-workspace quiet))
                (list hdmi 1))
            (ok ":focus nil window is shown"
                (window-hidden-p quiet) nil)
            (ok ":focus nil leaves seat focus on the terminal"
                (focused) term)
            (ok ":focus nil window is not marked focused"
                (window-focused-p quiet) nil)
            (ok ":focus nil leaves no autofocus pending"
                *pending-autofocus* nil)

            ;; --- :raw t with :focus nil ---
            (let ((quietraw (arrive :nf-quietraw "quietraw")))
              (pass)
              (ok ":raw t with :focus nil starts the window raw"
                  (window-raw-state quietraw) :raw)
              (ok ":raw t with :focus nil leaves seat focus on the terminal"
                  (focused) term)
              (ok ":raw t with :focus nil activates no raw key handling"
                  (raw-keys-active-p) nil)

              ;; --- The reported shape: :output :workspace :raw ---
              (let ((viewer (arrive :nf-viewer "viewer")))
                (pass)
                (ok "a viewer rule places the window on output 1 workspace 1"
                    (list (window-output viewer) (window-workspace viewer)
                          (window-hidden-p viewer))
                    (list hdmi 1 nil))
                (ok "a viewer rule on the showing workspace takes no focus"
                    (focused) term)
                (ok "a viewer rule window is raw without raw key handling"
                    (list (window-raw-state viewer) (raw-keys-active-p))
                    (list :raw nil))

                ;; --- Explicit focus afterwards ---
                (focus-window quiet seat)
                (pass)
                (ok "explicit focus reaches a :focus nil window"
                    (list (focused) (window-focused-p quiet))
                    (list quiet t))
                (focus-window viewer seat)
                (pass)
                (ok "explicit focus of a :focus nil raw window enters raw mode"
                    (list (focused) (active-top-keymaps))
                    (list viewer (list *raw-map*)))
                (focus-window term seat)
                (pass)
                (ok "focus returns to the terminal and leaves raw mode"
                    (list (focused) (raw-keys-active-p))
                    (list term nil)))))

          ;; --- Control: rules without :focus nil take focus ---
          (let ((loud (arrive :nf-loud "loudraw")))
            (pass)
            (ok "a rule without :focus gives the new window focus"
                (focused) loud)
            (ok "a raw rule without :focus enters raw mode on map"
                (active-top-keymaps) (list *raw-map*)))
          (focus-window term seat)
          (pass)
          (let ((loudt (arrive :nf-loudt "loudt")))
            (pass)
            (ok "a rule with :focus t gives the new window focus"
                (focused) loudt))
          (focus-window term seat)
          (pass)

          ;; --- :follow with :focus nil ---
          ;; Workspace 4 already holds a window
          (let ((resident (make-instance 'rhizome-window :proxy :nf-resident
                                                         :app-id "resident")))
            (setf (gethash :nf-resident *windows*) resident)
            (assign-window-number resident)
            (setf (window-output resident) hdmi)
            (assign-window-workspace resident 4)
            (frame-adopt-window resident hdmi 4)
            (pass)
            (let ((follow (arrive :nf-follow "quietfollow")))
              (pass)
              (ok ":follow with :focus nil switches the output"
                  (current-workspace hdmi) 4)
              (ok ":follow with :focus nil shows the window there"
                  (list (window-workspace follow) (window-hidden-p follow))
                  (list 4 nil))
              (ok ":follow with :focus nil focuses the workspace's other window"
                  (focused) resident)
              (ok ":follow with :focus nil never focuses the new window"
                  (window-focused-p follow) nil)
              ;; Once focused by hand, the window is what a switch
              ;; back to its workspace focuses
              (focus-window follow seat)
              (pass)
              (switch-workspace 1 hdmi)
              (pass)
              (switch-workspace 4 hdmi)
              (pass)
              (ok "a later switch to the workspace can focus that window"
                  (list (eq (focused) follow) (window-focused-p follow))
                  (list t t))))
          (switch-workspace 1 hdmi)
          (focus-window term seat)
          (pass)
          (let ((empty (arrive :nf-empty "emptyfollow")))
            (pass)
            (ok ":follow with :focus nil to an empty workspace switches"
                (current-workspace hdmi) 5)
            (ok ":follow with :focus nil to an empty workspace clears focus"
                (list (focused) (window-focused-p empty))
                (list nil nil)))))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
