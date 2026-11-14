;;; Window rules regression tests.
;;; Rules match app-id/title regexes and assign workspaces.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run (configure -> apply-config
;; starts the bar process otherwise!)
(configure '(:bar (:enabled nil)))

;; request-manage-dirty is a no-op without a WM proxy, so no stubs are
;; needed for river requests here.

(configure '(:window-rules ((:match "vivaldi" :workspace 2)
                            (:match "emacs" :key :title :workspace 3))))

(defun workspace-rule-placement-checks (check)
  "Drive :workspace rules through the real event and manage paths on two
fake outputs, HDMI (left) and DVI (right), both showing workspace 1.
CHECK is called as (CHECK name actual expected) and compares with EQL.
Resets the window, output, seat, and frame state it uses."
  (let ((calls '()))
    ;; Record river requests instead of sending them
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-borders show-window
                  hide-window message show-message request-manage-dirty
                  recommend-dimension-bounds set-window-clip-box
                  set-window-position install-keybindings-if-needed
                  enable-pointer-bindings-if-needed sync-window-decorations
                  flush-fullscreen-requests))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (when (and args (typep (first args) 'rhizome-window))
                  (push (list fn-sym (first args)) calls)
                  (when (eq fn-sym 'show-window)
                    (setf (window-hidden-p (first args)) nil))
                  (when (eq fn-sym 'hide-window)
                    (setf (window-hidden-p (first args)) t)))
                nil))))
    (dolist (fn '(rwm:river-seat-v1.focus-window rwm:river-seat-v1.clear-focus
                  rwm:river-seat-v1.pointer-warp))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf *config* (default-config))
    (configure '(:bar (:enabled nil)
                 :animations (:enabled nil)
                 :pointer (:warp-on-switch nil)
                 :window-rules ((:match "^rulews$" :workspace 3)
                                (:match "^rulefollow$" :workspace 4 :follow t)
                                (:match "^ruleshow$" :workspace 4)
                                (:match "^ruleout$" :output 2 :workspace 2))))
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
          *last-registered-window* nil)
    (let ((hdmi (make-instance 'rhizome-output :proxy :rule-hdmi
                               :x 0 :y 0 :width 1920 :height 1080))
          (dvi (make-instance 'rhizome-output :proxy :rule-dvi
                              :x 1920 :y 0 :width 1920 :height 1080))
          (seat (make-instance 'rhizome-seat :proxy :rule-seat)))
      (setf (gethash :rule-hdmi *outputs*) hdmi
            (gethash :rule-dvi *outputs*) dvi
            (gethash :rule-seat *seats*) seat
            (gethash hdmi *output-workspace*) 1
            (gethash dvi *output-workspace*) 1)
      (labels ((pass ()
                 ;; One manage sequence, then one render sequence
                 (with-output-to-string (*standard-output*)
                   (on-manage-sequence)
                   (setf *pending-focus-requests* nil)
                   (maphash (lambda (proxy out)
                              (declare (ignore proxy))
                              (render-layout-for-output out))
                            *outputs*)))
               (arrive (proxy app-id &rest titles)
                 ;; A new window as river announces it: app-id, then
                 ;; title, then the title the client sets later.
                 ;; Returns the window and the rule log line count.
                 (let ((win (make-instance 'rhizome-window :proxy proxy)))
                   (setf (gethash proxy *windows*) win
                         *last-registered-window* win
                         *pending-autofocus* win)
                   (assign-window-number win)
                   (let ((log (with-output-to-string (*standard-output*)
                                (handle-window-event win :app-id app-id)
                                (dolist (title titles)
                                  (handle-window-event win :title title)))))
                     (values win
                             (loop with start = 0
                                   for at = (search "window rule" log
                                                    :start2 start)
                                   while at
                                   count t
                                   do (setf start (1+ at)))))))
               (in-tree-p (win out workspace)
                 (window-in-frame-tree-p win out workspace)))
        ;; A terminal already open and focused on HDMI workspace 1
        (let ((term (make-instance 'rhizome-window :proxy :rule-term
                                                   :app-id "term")))
          (setf (gethash :rule-term *windows*) term)
          (assign-window-number term)
          (pass)
          (focus-window term seat)
          (pass)
          (funcall check "terminal sits on HDMI workspace 1"
                   (and (eq (window-output term) hdmi)
                        (eql (window-workspace term) 1)
                        (in-tree-p term hdmi 1))
                   t)

          ;; A rule to a workspace the output is not showing
          (multiple-value-bind (win lines)
              (arrive :rule-ws "rulews" "rulews" "~/src")
            (funcall check "workspace rule logs once per window" lines 1)
            (pass)
            (funcall check "workspace rule keeps the window's workspace"
                     (window-workspace win) 3)
            (funcall check "workspace rule window opens on the current output"
                     (window-output win) hdmi)
            (funcall check "workspace rule window joins workspace 3's tree"
                     (in-tree-p win hdmi 3) t)
            (funcall check "workspace rule window stays out of workspace 1's tree"
                     (in-tree-p win hdmi 1) nil)
            (funcall check "workspace rule window is hidden"
                     (window-hidden-p win) t)
            (funcall check "workspace rule leaves HDMI on workspace 1"
                     (current-workspace hdmi) 1)
            (funcall check "workspace rule leaves focus on the terminal"
                     (seat-focused-window seat) term)
            (funcall check "terminal stays visible in workspace 1"
                     (and (not (window-hidden-p term)) (in-tree-p term hdmi 1))
                     t)
            ;; A later title change does not apply the rule again
            (let ((log (with-output-to-string (*standard-output*)
                         (handle-window-event win :title "rulews again"))))
              (funcall check "a later title change applies no rule"
                       (search "window rule" log) nil))
            ;; Switching there shows the window in its frame
            (switch-workspace 3 hdmi)
            (pass)
            (funcall check "switching to workspace 3 shows the window"
                     (window-hidden-p win) nil)
            (funcall check "the window keeps its frame on workspace 3"
                     (in-tree-p win hdmi 3) t)
            (switch-workspace 1 hdmi)
            (pass)
            (focus-window term seat)
            (pass))

          ;; :follow t switches the output and focuses the window
          (let ((win (arrive :rule-follow "rulefollow" "rulefollow")))
            (pass)
            (funcall check "follow rule switches HDMI to workspace 4"
                     (current-workspace hdmi) 4)
            (funcall check "follow rule focuses the window"
                     (seat-focused-window seat) win)
            (funcall check "follow rule window is shown in workspace 4's tree"
                     (and (not (window-hidden-p win)) (in-tree-p win hdmi 4))
                     t)
            (funcall check "follow rule hides the terminal"
                     (window-hidden-p term) t)

            ;; A rule to the workspace the output is showing
            (let ((shown (arrive :rule-show "ruleshow" "ruleshow")))
              (pass)
              (funcall check "showing-workspace rule opens on workspace 4"
                       (and (eql (window-workspace shown) 4)
                            (eq (window-output shown) hdmi)
                            (in-tree-p shown hdmi 4))
                       t)
              (funcall check "showing-workspace rule window is visible"
                       (window-hidden-p shown) nil)
              (funcall check "showing-workspace rule window takes focus"
                       (seat-focused-window seat) shown)
              (funcall check "showing-workspace rule keeps the follow window"
                       (in-tree-p win hdmi 4) t)))

          ;; :output plus :workspace on the second output
          (let ((before (seat-focused-window seat))
                (win (arrive :rule-out "ruleout" "ruleout")))
            (pass)
            (funcall check "output rule puts the window on DVI"
                     (window-output win) dvi)
            (funcall check "output rule keeps workspace 2"
                     (window-workspace win) 2)
            (funcall check "output rule window joins DVI workspace 2's tree"
                     (in-tree-p win dvi 2) t)
            (funcall check "output rule window stays off DVI workspace 1"
                     (in-tree-p win dvi 1) nil)
            (funcall check "output rule window is hidden"
                     (window-hidden-p win) t)
            (funcall check "output rule leaves DVI on workspace 1"
                     (current-workspace dvi) 1)
            (funcall check "output rule leaves HDMI on workspace 4"
                     (current-workspace hdmi) 4)
            (funcall check "output rule leaves focus alone"
                     (seat-focused-window seat) before)))))))

(let ((pass 0) (fail 0))
  (flet ((check (name actual expected)
           (if (eql actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~A, wanted ~A)~%"
                              name actual expected)))))

    ;; App-id match: "vivaldi-stable" contains "vivaldi"
    (let ((win (make-instance 'rhizome-window :proxy :w1)))
      (setf (gethash :w1 *windows*) win
            (window-app-id win) "vivaldi-stable")
      (apply-window-rules win)
      (check "vivaldi rule assigns workspace 2"
             (window-workspace win) 2))

    ;; Title match: "my GNU Emacs" matches "emacs" case-insensitively
    (let ((win (make-instance 'rhizome-window :proxy :w2)))
      (setf (gethash :w2 *windows*) win
            (window-title win) "my GNU Emacs")
      (apply-window-rules win)
      (check "title rule assigns workspace 3"
             (window-workspace win) 3))

    ;; No rule matches: window keeps its current (nil) workspace
    (let ((win (make-instance 'rhizome-window :proxy :w3)))
      (setf (gethash :w3 *windows*) win
            (window-app-id win) "foot")
      (apply-window-rules win)
      (check "unmatched window left alone" (window-workspace win) nil))

    ;; Layout normalization: string layout names become keywords
    (check "string layout normalized"
           (find-layout-symbol "master-stack") :master-stack)

    ;; --- fullscreen rule + flush direction ---
    ;; The flush was inverted once: fullscreen-p t sent EXIT-fullscreen,
    ;; so rules and the first C-t F11 never fullscreened the window.
    (let ((sent nil))
      (setf (symbol-function 'rwm:river-window-v1.fullscreen)
            (lambda (&rest args) (push (cons :fullscreen args) sent) nil))
      (setf (symbol-function 'rwm:river-window-v1.exit-fullscreen)
            (lambda (&rest args) (push (cons :exit args) sent) nil))
      (configure '(:window-rules ((:match "fs-test" :fullscreen t))))
      ;; river_window_v1.fullscreen names an output: give it one
      (setf (gethash :fs-out *outputs*)
            (make-instance 'rhizome-output :proxy :fs-out))
      (let ((win (make-instance 'rhizome-window :proxy :fs1)))
        (setf (gethash :fs1 *windows*) win
              (window-app-id win) "fs-test")
        (setf *pending-fullscreen-requests* nil)
        (apply-window-rules win)
        (check "fullscreen rule flags the window"
               (window-fullscreen-p win) t)
        (check "fullscreen rule queues the request"
               (and (member win *pending-fullscreen-requests*) t) t)
        (flush-fullscreen-requests)
        (check "flush sends FULLSCREEN for a flagged window"
               (first (first sent)) :fullscreen))
      ;; And the other direction: unfullscreen sends exit
      (setf sent nil)
      (let ((win (make-instance 'rhizome-window :proxy :fs2)))
        (setf (gethash :fs2 *windows*) win)
        (setf (window-fullscreen-p win) nil
              *pending-fullscreen-requests* (list win))
        (flush-fullscreen-requests)
        (check "flush sends EXIT-FULLSCREEN for an unflagged window"
               (first (first sent)) :exit))
      (remhash :fs-out *outputs*)
      (setf *pending-fullscreen-requests* nil
            *config* (default-config))
      ;; Restore the two-rule config the flow tests below expect
      (configure '(:window-rules ((:match "vivaldi" :workspace 2)
                                  (:match "emacs" :key :title :workspace 3)))))

    ;; Rules list survives the config merge heuristic
    (check "rules present in config"
           (length (config-get '(:window-rules))) 2)

    ;; --- identifier rules (river v4 stable IDs) ---

    (configure '(:window-rules ((:match "known-app" :key :identifier
                                        :workspace 5))))

    (let ((win (make-instance 'rhizome-window :proxy :w4)))
      (setf (gethash :w4 *windows*) win
            (window-identifier win) "known-app-123"
            (window-app-id win) "foot")
      (apply-window-rules win)
      (check "identifier rule assigns workspace 5"
             (window-workspace win) 5))

    (let ((win (make-instance 'rhizome-window :proxy :w5)))
      (setf (gethash :w5 *windows*) win
            (window-app-id win) "known-app-similar"
            (window-identifier win) nil)
      (apply-window-rules win)
      (check "identifier rule needs the identifier"
             (window-workspace win) nil))

    ;; --- placement persistence across "restarts" ---

    (let ((*window-state-path* #p"/tmp/rhizome-test-window-state.sexp")
          (*window-state* (make-hash-table :test 'equal)))
      ;; Window with an identifier lives on workspace 4, floats
      (let ((win (make-instance 'rhizome-window :proxy :w6)))
        (setf (gethash :w6 *windows*) win
              (window-identifier win) "persist-me"
              (window-output win) (make-instance 'rhizome-output
                                                  :proxy :out6)
              (window-x win) 10 (window-y win) 10
              (window-width win) 100 (window-height win) 100)
        (assign-window-workspace win 4)
        (float-window win)
        (remember-window-state win)
        ;; "Restart": fresh state hash, fresh window, same identifier
        (let ((*window-state* (make-hash-table :test 'equal)))
          (load-window-state)
          (let ((win2 (make-instance 'rhizome-window :proxy :w7)))
            (setf (gethash :w7 *windows*) win2
                  (window-identifier win2) "persist-me"
                  (window-output win2) (make-instance 'rhizome-output
                                                      :proxy :out7)
                  (window-x win2) 10 (window-y win2) 10
                  (window-width win2) 100 (window-height win2) 100)
            (assign-window-workspace win2 1)
            (restore-window-state win2)
            (check "remembered workspace restored"
                   (window-workspace win2) 4)
            (check "remembered float state restored"
                   (window-floating-p win2) t))))
      ;; The store went through the file
      (check "window state persisted to disk"
             (and (probe-file *window-state-path*) t) t)
      (ignore-errors (delete-file *window-state-path*)))

    ;; A window WITHOUT an identifier remembers nothing
    (let ((*window-state-path* #p"/tmp/rhizome-test-window-state2.sexp")
          (*window-state* (make-hash-table :test 'equal)))
      (let ((win (make-instance 'rhizome-window :proxy :w8)))
        (setf (gethash :w8 *windows*) win
              (window-identifier win) nil)
        (remember-window-state win))
      (check "no identifier, nothing remembered"
             (hash-table-count *window-state*) 0))
    (ignore-errors (delete-file #p"/tmp/rhizome-test-window-state2.sexp"))

    ;; --- :workspace rules on the event and manage paths ---
    (workspace-rule-placement-checks #'check)

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1))))
