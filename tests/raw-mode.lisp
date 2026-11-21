;;; Tests for raw mode: while the focused window is raw, river grabs
;;; only the Right Control escape and the pointer bindings are
;;; disabled, so every other key and MOD+button reaches the client.
;;; River's xkb and pointer binding requests are stubbed to record
;;; against fake objects; windows, an output and a seat are fakes.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (live (make-hash-table))
      (manage-requests 0) (messages '()) (pointer-calls '())
      (borders '()))
  (macrolet ((ok (name actual expected)
               ;; An error in ACTUAL is a failure, not a crash, so the
               ;; suite reports every check before the feature exists
               `(let ((got (handler-case ,actual
                             (error (e) (list :error (princ-to-string e)))))
                      (want (handler-case ,expected
                              (error (e) (list :expected-error
                                               (princ-to-string e))))))
                  (if (equal got want)
                      (progn (incf pass)
                             (format t "~&PASS: ~A~%" ,name))
                      (progn (incf fail)
                             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                     ,name got want))))))
    (labels ((call-if (name &rest args)
               (if (fboundp name) (apply name args) :missing))
             (raw-state (win)
               (call-if 'window-raw-state win))
             (sync ()
               (install-keybindings-if-needed))
             (grab-set ()
               (call-if 'key-grab-set))
             (normal-set ()
               (key-grab-set (list *top-map*)))
             (by-text (list)
               (sort (copy-list list) #'string< :key #'prin1-to-string))
             (same-set-p (a b)
               (equal (by-text a) (by-text b)))
             (grabbed-keys ()
               (by-text (loop for k being the hash-values of live
                              collect k)))
             (last-message ()
               (first messages))
             (make-win (proxy app-id)
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win)
                 (assign-window-number win)
                 (with-output-to-string (*standard-output*)
                   (handle-window-event win :app-id app-id))
                 win))
             (focus (win)
               (setf manage-requests 0)
               (focus-window win (gethash :raw-seat *seats*)))
             (control-r ()
               (make-key 65508)))

      ;; River-touching stubs
      (setf (symbol-function 'message)
            (lambda (fmt &rest args)
              (push (apply #'format nil fmt args) messages)
              nil))
      (dolist (fn '(show-message message-hide propose-window-dimensions
                    set-window-tiled-state make-window-node show-window
                    hide-window recommend-dimension-bounds
                    set-window-clip-box set-window-position
                    sync-window-decorations flush-fullscreen-requests))
        (setf (symbol-function fn)
              (lambda (&rest args) (declare (ignore args)) nil)))
      (setf (symbol-function 'set-window-borders)
            (lambda (win edges width r g b a)
              (declare (ignore edges))
              (push (list win width r g b a) borders)
              nil))
      (setf (symbol-function 'request-manage-dirty)
            (lambda () (incf manage-requests)))
      (dolist (fn '(rwm:river-seat-v1.focus-window
                    rwm:river-seat-v1.clear-focus
                    rwm:river-seat-v1.pointer-warp))
        (setf (symbol-function fn)
              (lambda (&rest args) (declare (ignore args)) nil)))
      (setf (symbol-function 'rwm:river-xkb-bindings-v1.get-xkb-binding)
            (lambda (manager seat keysym modifiers)
              (declare (ignore manager seat))
              (let ((proxy (make-instance 'rwm:river-xkb-binding-v1)))
                (setf (gethash proxy live) (list keysym modifiers))
                proxy)))
      (setf (symbol-function 'rwm:river-xkb-binding-v1.enable)
            (lambda (proxy) (declare (ignore proxy)) nil))
      (setf (symbol-function 'rwm:river-xkb-binding-v1.destroy)
            (lambda (proxy) (remhash proxy live)))
      (setf (symbol-function 'rwm:river-pointer-binding-v1.enable)
            (lambda (binding) (push (list :enable binding) pointer-calls)))
      (setf (symbol-function 'rwm:river-pointer-binding-v1.disable)
            (lambda (binding) (push (list :disable binding) pointer-calls)))

      (setf *config* (default-config))
      (configure '(:bar (:enabled nil)
                   :animations (:enabled nil)
                   :pointer (:warp-on-switch nil)
                   :window-rules ((:match "^rawviewer$" :raw t))))
      (setf *xkb-bindings-proxy* :xkb-bindings
            *windows* (make-hash-table)
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
      (let ((out (make-instance 'rhizome-output :proxy :raw-out
                                :x 0 :y 0 :width 1920 :height 1080))
            (seat (make-instance 'rhizome-seat :proxy :raw-seat)))
        (setf (gethash :raw-out *outputs*) out
              (gethash out *output-workspace*) 1
              (gethash :raw-seat *seats*) seat)
        ;; Fake pointer bindings: the seat already has its set
        (setf *pointer-bindings* (list :pointer-move :pointer-resize)
              *pointer-bindings-enabled* nil
              (gethash seat *seats-with-pointer-bindings*) t)
        (clear-keymap *top-map*)
        (clear-keymap *prefix-map*)
        (setup-default-keybindings)
        (release-key-grabs)

        (let ((term (make-win :raw-term "term"))
              (viewer (make-win :raw-viewer "rawviewer")))

          ;; --- Window rules ---
          (ok "a rule with :raw t starts a matching window raw"
              (raw-state viewer) :raw)
          (ok "a window no :raw rule matches starts normal"
              (raw-state term) nil)

          ;; --- A normal window keeps the normal grabs ---
          (focus term)
          (sync)
          (ok "a normal focused window grabs the normal set"
              (same-set-p (grab-set) (normal-set)) t)
          (ok "a normal focused window does not grab Right Control"
              (and (member (control-r) (grab-set) :test #'equal) t) nil)
          (ok "Right Control alone is not bound outside raw mode"
              (key-binding 65508 nil) nil)

          ;; --- Focusing a raw window ---
          (focus viewer)
          (ok "focusing a raw window asks for a manage sequence"
              (plusp manage-requests) t)
          (ok "a raw focused window grabs exactly Right Control"
              (grab-set) (list (control-r)))
          (sync)
          (ok "the sync leaves only the Right Control grab"
              (grabbed-keys) '((65508 (:none))))
          (ok "the bar snapshot marks a raw focused window"
              (getf (wm-snapshot-plist) :raw) t)

          ;; --- Pointer bindings ---
          (setf pointer-calls '())
          (enable-pointer-bindings-if-needed)
          (ok "pointer bindings stay off while a raw window has focus"
              (list *pointer-bindings-enabled*
                    (count :enable pointer-calls :key #'first))
              '(nil 0))

          ;; --- The escape key ---
          (setf messages '())
          (press-key (control-r))
          (ok "Right Control suspends raw mode"
              (raw-state viewer) :suspended)
          (ok "suspending shows a message"
              (and (last-message) (search "suspended" (last-message)) t) t)
          (ok "a suspended window grabs the normal set plus Right Control"
              (same-set-p (grab-set)
                          (cons (control-r) (normal-set)))
              t)
          (ok "the bar snapshot does not mark a suspended window"
              (getf (wm-snapshot-plist) :raw) nil)
          (sync)
          (setf pointer-calls '())
          (enable-pointer-bindings-if-needed)
          (ok "pointer bindings come back while suspended"
              (list *pointer-bindings-enabled*
                    (count :enable pointer-calls :key #'first))
              '(t 2))
          (setf messages '())
          (press-key (control-r))
          (ok "Right Control on a suspended window resumes raw mode"
              (raw-state viewer) :raw)
          (ok "resuming shows a message"
              (and (last-message) (search "raw: on" (last-message)) t) t)
          (ok "resumed raw mode grabs exactly Right Control"
              (grab-set) (list (control-r)))
          (setf pointer-calls '())
          (enable-pointer-bindings-if-needed)
          (ok "resumed raw mode disables the pointer bindings"
              (list *pointer-bindings-enabled*
                    (count :disable pointer-calls :key #'first))
              '(nil 2))

          ;; --- Indicator: border width and colour, bar marker ---
          (ok "a raw window's border is wider than the focused border"
              (> (window-border-width viewer) (border-width t)) t)
          (ok "a normal focused window keeps the focused border width"
              (progn (focus term)
                     (prog1 (window-border-width term)
                       (focus viewer)))
              (border-width t))
          (ok "the raw border colour comes from the :border-raw element"
              (multiple-value-list (window-border-rgba32 viewer))
              (multiple-value-list
               (multiple-value-call #'border-rgba32 (theme-rgb :border-raw))))
          (ok "the raw border colour differs from the focused one"
              (equal (theme-color :border-raw) (theme-color :border-focused))
              nil)
          (ok "every drawn state still carries a non-hue cue"
              (hue-only-state-pairs) nil)
          (ok "the raw cue pair differs in width"
              (getf (find :window-raw (state-pair-report)
                          :key (lambda (row) (getf row :state)))
                    :non-hue)
              '(:width))
          (setf borders '())
          (with-output-to-string (*standard-output*)
            (on-manage-sequence)
            (setf *pending-focus-requests* nil)
            (render-layout-for-output out))
          (ok "the render pass draws the raw window with the raw width"
              (second (find viewer borders :key #'first))
              (window-border-width viewer))
          (ok "the render pass draws the normal window with its own width"
              (second (find term borders :key #'first))
              (border-width nil))
          (let ((real-snapshot (symbol-function 'bar-snapshot)))
            (setf (symbol-function 'bar-snapshot)
                  (lambda () '(:raw t)))
            (ok "the bar's raw module shows RAW"
                (let ((parts (bar-module-raw)))
                  (and (consp parts) (first (first parts))))
                "RAW")
            (ok "the left segment gains :raw while the marker is shown"
                (first (bar-segment-modules :left)) :raw)
            (setf (symbol-function 'bar-snapshot)
                  (lambda () '(:raw nil)))
            (ok "the bar's raw module is empty otherwise"
                (bar-module-raw) "")
            (ok "the left segment is the configured one otherwise"
                (bar-segment-modules :left)
                (config-getf '(:bar :modules) :left))
            (setf (symbol-function 'bar-snapshot) real-snapshot))

          ;; --- Leaving the raw window ---
          (focus term)
          (ok "focusing a normal window asks for a manage sequence"
              (plusp manage-requests) t)
          (ok "leaving the raw window restores the normal grabs"
              (same-set-p (grab-set) (normal-set)) t)
          (sync)
          (ok "the sync releases Right Control and grabs the normal set"
              (grabbed-keys)
              (by-text (mapcar (lambda (key)
                                 (list (key-keysym key)
                                       (or (river-modifiers
                                            (key-modifiers key))
                                           *mod-none*)))
                               (normal-set))))
          (ok "the bar snapshot is clear on a normal window"
              (getf (wm-snapshot-plist) :raw) nil)
          (setf pointer-calls '())
          (enable-pointer-bindings-if-needed)
          (ok "pointer bindings return on a normal window"
              *pointer-bindings-enabled* t)
          (ok "the unfocused raw window stays raw"
              (raw-state viewer) :raw)

          ;; --- The :raw-mode command ---
          (setf messages '())
          (execute-command out :raw-mode nil)
          (ok ":raw-mode makes the focused window raw"
              (raw-state term) :raw)
          (ok ":raw-mode shows a message"
              (and (last-message) (search "raw: on" (last-message)) t) t)
          (ok ":raw-mode leaves only the escape grabbed"
              (grab-set) (list (control-r)))
          (execute-command out :raw-mode nil)
          (ok ":raw-mode again returns the window to normal"
              (raw-state term) nil)
          (ok "and restores the normal grab set"
              (same-set-p (grab-set) (normal-set)) t)
          (ok "the :raw-mode command is documented"
              (and (gethash :raw-mode *command-docs*) t) t)
          (focus viewer)
          (press-key (control-r))
          (execute-command out :raw-mode nil)
          (ok ":raw-mode on a suspended window turns raw mode off"
              (raw-state viewer) nil)
          (call-if 'set-window-raw-state viewer :raw)

          ;; --- Closing the raw window ---
          (focus viewer)
          (with-output-to-string (*standard-output*)
            (handle-window-event viewer :closed))
          (ok "closing a raw window clears its raw state"
              (raw-state viewer) nil)
          (ok "after the raw window closes the normal set is grabbed"
              (same-set-p (grab-set) (normal-set)) t)))

      (setf *xkb-bindings-proxy* nil))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
