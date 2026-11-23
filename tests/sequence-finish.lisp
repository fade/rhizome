;;; Tests that a manage or render sequence always finishes: a step that
;;; signals an error is logged, the remaining steps run, manage_finish /
;;; render_finish is sent exactly once, and the sequence state leaves
;;; :managing / :rendering. River requests are stubbed; each pass step
;;; is replaced by a recorder so the order of a normal pass is visible.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()) (failing nil))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Each step records its name, and signals when it is the failing one
    (dolist (step '(install-keybindings-if-needed
                    enable-pointer-bindings-if-needed
                    pointer-focus-manage-pass
                    flush-focus-requests
                    sync-window-decorations
                    resolve-pending-window-outputs
                    apply-layout-for-output
                    float-op-manage-pass
                    flush-fullscreen-requests
                    update-wm-snapshot
                    apply-presentation-mode
                    render-layout-for-output
                    decorations-render-pass
                    frame-outline-render-pass
                    titlebars-render-pass
                    fselect-labels-render-pass
                    active-edge-render-pass))
      (let ((step step))
        (setf (symbol-function step)
              (lambda (&rest args)
                (declare (ignore args))
                (push step calls)
                (when (eq step failing)
                  (error "stubbed failure in ~A" step))
                nil))))
    (setf (symbol-function 'rwm:river-window-manager-v1.manage-finish)
          (lambda (proxy)
            (declare (ignore proxy))
            (push 'manage-finish calls)
            nil)
          (symbol-function 'rwm:river-window-manager-v1.render-finish)
          (lambda (proxy)
            (declare (ignore proxy))
            (push 'render-finish calls)
            nil))

    (labels ((reset (&key fail-step pending-manage)
               (setf *outputs* (make-hash-table)
                     *windows* (make-hash-table)
                     *seats* (make-hash-table)
                     *pending-autofocus* nil
                     *pending-manage* pending-manage
                     *sequence-state* :idle
                     *wm-proxy* :wm
                     calls '()
                     failing fail-step)
               (when (boundp '*takeover-state*)
                 (setf *takeover-state* nil))
               (setf (gethash :out-1 *outputs*)
                     (make-instance 'rhizome-output :proxy :out-1
                                    :width 1920 :height 1080)))
             (run-pass (fn)
               ;; Returns (escaped-p log): whether a condition left the
               ;; pass, and everything the pass printed
               (let* ((escaped nil)
                      (log (with-output-to-string (*standard-output*)
                             (handler-case (funcall fn)
                               (error () (setf escaped t))))))
                 (list escaped log)))
             (trace-of ()
               (reverse calls))
             (sent (request)
               (count request calls)))

      ;; --- Block 1: a normal manage pass ---

      (reset)
      (run-pass #'handle-manage-start)
      (ok "a normal manage pass runs its steps in order, then finishes"
          (trace-of)
          '(install-keybindings-if-needed enable-pointer-bindings-if-needed
            pointer-focus-manage-pass
            flush-focus-requests sync-window-decorations
            resolve-pending-window-outputs
            apply-layout-for-output float-op-manage-pass
            flush-fullscreen-requests update-wm-snapshot manage-finish))
      (ok "a normal manage pass waits for render"
          *sequence-state* :waiting-render)

      (reset :pending-manage t)
      (run-pass #'handle-manage-start)
      (ok "a manage pass with a manage pending goes idle"
          *sequence-state* :idle)

      ;; --- Block 2: a manage step signals an error ---

      (reset :fail-step 'flush-focus-requests)
      (destructuring-bind (escaped log) (run-pass #'handle-manage-start)
        (ok "a manage step that signals an error does not escape the pass"
            escaped nil)
        (ok "manage_finish is sent exactly once after a failed step"
            (sent 'manage-finish) 1)
        (ok "the manage pass leaves :managing after a failed step"
            *sequence-state* :waiting-render)
        (ok "the failed manage step is logged by name"
            (and (search "flush-focus-requests" log) t) t)
        (ok "the manage steps after the failed one still run"
            (trace-of)
            '(install-keybindings-if-needed enable-pointer-bindings-if-needed
              pointer-focus-manage-pass
              flush-focus-requests sync-window-decorations
              resolve-pending-window-outputs
              apply-layout-for-output float-op-manage-pass
              flush-fullscreen-requests update-wm-snapshot manage-finish)))

      (reset :fail-step 'resolve-pending-window-outputs)
      (destructuring-bind (escaped log) (run-pass #'handle-manage-start)
        (ok "a failing output resolution does not escape the manage pass"
            escaped nil)
        (ok "manage_finish is sent once after a failing output resolution"
            (sent 'manage-finish) 1)
        (ok "the failing output resolution is logged by name"
            (and (search "resolve-pending-window-outputs" log) t) t))

      (reset :fail-step 'update-wm-snapshot)
      (run-pass #'handle-manage-start)
      (ok "manage_finish is sent when the snapshot refresh signals an error"
          (sent 'manage-finish) 1)

      ;; The per-output debug line runs inside its step: an entry that
      ;; is not an output signals there, and the pass still finishes
      (reset)
      (setf (gethash :bogus *outputs*) :bogus)
      (let ((debug-fn (symbol-function 'debug-events-p)))
        (setf (symbol-function 'debug-events-p) (constantly t))
        (unwind-protect (run-pass #'handle-manage-start)
          (setf (symbol-function 'debug-events-p) debug-fn)))
      (ok "manage_finish is sent when the per-output debug line signals an error"
          (sent 'manage-finish) 1)

      ;; --- Block 3: a normal render pass ---

      (reset)
      (run-pass #'handle-render-start)
      (ok "a normal render pass runs its steps in order, then finishes"
          (trace-of)
          '(apply-presentation-mode render-layout-for-output
            decorations-render-pass frame-outline-render-pass
            titlebars-render-pass fselect-labels-render-pass
            active-edge-render-pass update-wm-snapshot render-finish))
      (ok "a normal render pass goes idle"
          *sequence-state* :idle)

      ;; --- Block 4: a render step signals an error ---

      (reset :fail-step 'render-layout-for-output)
      (destructuring-bind (escaped log) (run-pass #'handle-render-start)
        (ok "a render step that signals an error does not escape the pass"
            escaped nil)
        (ok "render_finish is sent exactly once after a failed step"
            (sent 'render-finish) 1)
        (ok "the render pass leaves :rendering after a failed step"
            *sequence-state* :idle)
        (ok "the failed render step is logged by name"
            (and (search "render-layout-for-output" log) t) t)
        (ok "the render steps after the failed one still run"
            (trace-of)
            '(apply-presentation-mode render-layout-for-output
              decorations-render-pass frame-outline-render-pass
              titlebars-render-pass fselect-labels-render-pass
              active-edge-render-pass update-wm-snapshot render-finish)))

      (reset :fail-step 'frame-outline-render-pass)
      (destructuring-bind (escaped log) (run-pass #'handle-render-start)
        (ok "a failing frame outline does not escape the render pass"
            escaped nil)
        (ok "render_finish is sent once after a failing frame outline"
            (sent 'render-finish) 1)
        (ok "the failing frame outline is logged by name"
            (and (search "frame-outline-render-pass" log) t) t)
        (ok "the render steps after a failing frame outline still run"
            (trace-of)
            '(apply-presentation-mode render-layout-for-output
              decorations-render-pass frame-outline-render-pass
              titlebars-render-pass fselect-labels-render-pass
              active-edge-render-pass update-wm-snapshot render-finish))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
