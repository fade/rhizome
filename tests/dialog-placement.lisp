;;; Tests for dialogs (windows whose client names a parent): they float
;;; on map unless a window rule or the :dialogs group says otherwise,
;;; and the first placement goes either to the active frame or to the
;;; parent's frame, with a notification when that frame is on a hidden
;;; workspace (floating.lisp); goto-last-dialog then shows that
;;; workspace and focuses the dialog. River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run; titlebars off isolates
;; placement from the strip clamp (tests/titlebars.lisp covers that)
(configure '(:bar (:enabled nil) :theme (:titlebar nil)))

(defvar *notes* nil
  "Recorded show-message calls: (TEXT SECONDS COLOR), newest first.")

(let ((pass 0) (fail 0))
  (flet ((check (name thunk expected)
           (let ((actual (handler-case (funcall thunk)
                           (error (e) (list :error (princ-to-string e))))))
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))))

    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position set-window-borders
                  place-node-above message request-manage-dirty
                  recommend-dimension-bounds show-window hide-window
                  sync-window-decorations))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    (setf (symbol-function 'show-message)
          (lambda (text &optional (seconds 3) color)
            (push (list text seconds color) *notes*)
            nil))
    ;; A 36 px bar
    (setf (symbol-function 'bar-reserved-height)
          (lambda (&optional output) (declare (ignore output)) 36))

    (labels ((fresh ()
               (setf *windows* (make-hash-table)
                     *outputs* (make-hash-table)
                     *output-workspace* (make-hash-table)
                     *window-workspace* (make-hash-table)
                     *minimized-windows* (make-hash-table)
                     *frame-trees* (make-hash-table :test 'equal)
                     *current-frames* (make-hash-table :test 'equal)
                     *notes* nil)
               (clrhash *window-parents*)
               (clrhash *float-centre-pending*))
             (mk-out (proxy &optional (x 0))
               ;; A 1280x800 output at X showing workspace 1
               (let ((out (make-instance 'rhizome-output :proxy proxy :x x
                                         :width 1280 :height 800)))
                 (setf (gethash out *output-workspace*) 1
                       (gethash proxy *outputs*) out)
                 out))
             (mk-win (proxy out &key (workspace 1))
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out)
                 (assign-window-workspace win workspace)
                 win))
             (tiled (win out &key (workspace 1))
               (frame-push-window win (frame-tree-for out workspace))
               win)
             (focus (win)
               (dolist (w (all-windows))
                 (setf (window-focused-p w) nil))
               (setf (window-focused-p win) t))
             (new-dialog (proxy out parent-proxy &key (title "Open File")
                                                   (app-id "dlgapp"))
               ;; A window that floats before it is ever shown
               (let ((win (mk-win proxy out)))
                 (setf (window-title win) title
                       (window-app-id win) app-id)
                 (handle-window-event win :parent parent-proxy)
                 (float-window win :animate nil)
                 win))
             (commit-size (win w h)
               (setf (window-width win) w
                     (window-height win) h))
             (render (&rest outs)
               (dolist (out outs)
                 (float-render-decisions out nil)))
             (pos (win)
               (list (window-float-x win) (window-float-y win))))

      ;; --- A1: floating parent: the active frame, on another output ---
      (fresh)
      (let* ((left (mk-out :a1-left))
             (right (mk-out :a1-right 1280))
             (user (tiled (mk-win :a1-u left) left))
             (parent (mk-win :a1-p right)))
        (focus user)
        (split-current-frame left 1 :horizontal)
        (setf (window-node parent) :a1-node)
        (float-window parent :x 1400 :y 100 :width 400 :height 300
                             :animate nil)
        (let ((dlg (new-dialog :a1-d right :a1-p)))
          (check "a pending dialog with no size defers its autofocus"
                 (lambda () (window-placement-pending-p dlg)) t)
          (commit-size dlg 200 100)
          (render left right)
          (check "a dialog of a floating parent goes to the active frame"
                 (lambda () (list (eq (window-output dlg) left)
                                  (window-workspace dlg) (pos dlg)))
                 (list t 1 (list 220 (+ 36 332))))
          (check "once placed the dialog no longer defers autofocus"
                 (lambda () (window-placement-pending-p dlg)) nil)
          (check "a dialog brought to the active frame raises no notification"
                 (lambda () *notes*) nil)))

      ;; --- A2: parent frame narrower than the dialog ---
      (fresh)
      (let* ((left (mk-out :a2-left))
             (right (mk-out :a2-right 1280))
             (user (tiled (mk-win :a2-u left) left))
             (parent (tiled (mk-win :a2-p right) right)))
        (declare (ignore parent))
        (focus user)
        (split-current-frame left 1 :horizontal)
        (split-current-frame right 1 :horizontal)
        (split-current-frame right 1 :horizontal)
        (let ((dlg (new-dialog :a2-d right :a2-p)))
          (commit-size dlg 400 100)
          (render left right)
          (check "a dialog wider than its parent's frame goes to the active frame"
                 (lambda () (list (eq (window-output dlg) left) (pos dlg)))
                 (list t (list 120 (+ 36 332))))))

      ;; --- A3: parent frame shorter than the dialog (either dimension) ---
      (fresh)
      (let* ((left (mk-out :a3-left))
             (right (mk-out :a3-right 1280))
             (user (tiled (mk-win :a3-u left) left))
             (parent (tiled (mk-win :a3-p right) right)))
        (declare (ignore parent))
        (focus user)
        (split-current-frame left 1 :horizontal)
        (split-current-frame right 1 :vertical)
        (let ((dlg (new-dialog :a3-d right :a3-p)))
          (commit-size dlg 300 500)
          (render left right)
          (check "a dialog taller than its parent's frame goes to the active frame"
                 (lambda () (list (eq (window-output dlg) left) (pos dlg)))
                 (list t (list 170 (+ 36 132))))))

      ;; --- A4: minimized parent ---
      (fresh)
      (let* ((left (mk-out :a4-left))
             (right (mk-out :a4-right 1280))
             (user (tiled (mk-win :a4-u left) left))
             (parent (tiled (mk-win :a4-p right) right)))
        (focus user)
        (split-current-frame left 1 :horizontal)
        (setf (window-minimized-p parent) t)
        (let ((dlg (new-dialog :a4-d right :a4-p)))
          (commit-size dlg 200 100)
          (render left right)
          (check "a dialog of a minimized parent goes to the active frame"
                 (lambda () (list (eq (window-output dlg) left) (pos dlg)))
                 (list t (list 220 (+ 36 332))))))

      ;; --- A5: parent named but not managed (no frame) ---
      (fresh)
      (let* ((left (mk-out :a5-left))
             (right (mk-out :a5-right 1280))
             (user (tiled (mk-win :a5-u left) left)))
        (focus user)
        (split-current-frame left 1 :horizontal)
        (let ((dlg (new-dialog :a5-d right :a5-unmanaged)))
          (commit-size dlg 200 100)
          (render left right)
          (check "a dialog whose parent has no frame goes to the active frame"
                 (lambda () (list (eq (window-output dlg) left) (pos dlg)))
                 (list t (list 220 (+ 36 332))))))

      ;; --- A6: a dialog larger than the active frame stays on its output ---
      (fresh)
      (let* ((out (mk-out :a6-out))
             (user (tiled (mk-win :a6-u out) out))
             (parent (mk-win :a6-p out)))
        (focus user)
        (split-current-frame out 1 :horizontal)
        (setf (window-node parent) :a6-node)
        (float-window parent :x 700 :y 100 :width 400 :height 300
                             :animate nil)
        (let ((dlg (new-dialog :a6-d out :a6-p)))
          (commit-size dlg 1000 900)
          (render out)
          (check "a dialog larger than its output starts at the usable area's corner"
                 (lambda () (pos dlg)) (list 0 36))))

      ;; --- A7: centred on a frame, moved the least to fit its output ---
      (fresh)
      (let* ((out (mk-out :a7-out))
             (left (tiled (mk-win :a7-l out) out))
             (parent (mk-win :a7-p out)))
        (focus left)
        (split-current-frame out 1 :horizontal)
        (let ((right (mk-win :a7-r out)))
          (frame-push-window right (second (frame-leaves (frame-tree-for out 1))))
          (focus right))
        (setf (window-node parent) :a7-node)
        (float-window parent :x 100 :y 100 :width 400 :height 300
                             :animate nil)
        (let ((dlg (new-dialog :a7-d out :a7-p)))
          (commit-size dlg 800 300)
          (render out)
          (check "a dialog overhanging its output's edge moves back onto it"
                 (lambda () (pos dlg)) (list 480 (+ 36 232)))))

      ;; --- B1: tiled parent, dialog fits, same workspace ---
      (fresh)
      (let* ((out (mk-out :b1-out))
             (parent (tiled (mk-win :b1-p out) out)))
        (focus parent)
        (split-current-frame out 1 :horizontal)
        (let ((dlg (new-dialog :b1-d out :b1-p)))
          (commit-size dlg 200 100)
          (render out)
          (check "a dialog that fits is centred on its tiled parent's frame"
                 (lambda () (list (eq (window-output dlg) out)
                                  (window-workspace dlg) (pos dlg)))
                 (list t 1 (list 220 (+ 36 332))))
          (check "a parent on the focused workspace raises no notification"
                 (lambda () *notes*) nil)))

      ;; --- B2: tiled parent on a hidden workspace: notification ---
      (fresh)
      (let* ((left (mk-out :b2-left))
             (right (mk-out :b2-right 1280))
             (user (tiled (mk-win :b2-u right) right))
             (parent (mk-win :b2-p right :workspace 2)))
        (declare (ignore left))
        (focus user)
        (let ((root (frame-tree-for right 2)))
          (split-frame root :horizontal 1/2)
          (frame-push-window parent (second (frame-leaves root))))
        (let ((dlg (new-dialog :b2-d right :b2-p)))
          (commit-size dlg 200 100)
          (render right)
          (check "a dialog that fits joins its parent's hidden workspace"
                 (lambda () (list (eq (window-output dlg) right)
                                  (window-workspace dlg) (pos dlg)))
                 (list t 2 (list (+ 1280 640 220) (+ 36 332))))
          (check "the notification names the dialog and group:screen:frame"
                 (lambda ()
                   (let ((text (first (first *notes*))))
                     (list (length *notes*)
                           (and (search "Open File" text) t)
                           (and (search "dlgapp" text) t)
                           (and (search "2:2:1" text) t))))
                 (list 1 t t t))
          (check "the notification uses the :dialogs length and colour"
                 (lambda () (rest (first *notes*)))
                 (list 10 "#ff5fd7"))
          (check "a dialog placed on a hidden workspace is no longer pending"
                 (lambda () (window-placement-pending-p dlg)) nil)))

      ;; --- goto-last-dialog: the last dialog that opened out of view ---
      (check "the default config declares the notice length and colour"
             (lambda ()
               (list (config-getf '(:dialogs) :notify-seconds :default :absent)
                     (config-getf '(:dialogs) :notify-color :default :absent)))
             (list 10 "#ff5fd7"))
      (check "goto-last-dialog is a command, exported, and bound to prefix d"
             (lambda ()
               (setup-default-prefix-bindings)
               (list (and (gethash :goto-last-dialog *command-table*) t)
                     (nth-value 1 (find-symbol "GOTO-LAST-DIALOG" "RHIZOME"))
                     (functionp (keymap-lookup *prefix-map* 100))))
             (list t :external t))
      (let ((focused '())
            (said '())
            (saved-focus (symbol-function 'focus-window))
            (saved-message (symbol-function 'message)))
        (setf (symbol-function 'focus-window)
              (lambda (win seat)
                (declare (ignore seat))
                (push win focused))
              (symbol-function 'message)
              (lambda (control &rest args)
                (push (apply #'format nil control args) said)))
        (unwind-protect
             (progn
               (fresh)
               (setf (gethash :g-seat *seats*) :g-seat)
               (let* ((right (mk-out :g-right 1280))
                      (user (tiled (mk-win :g-u right) right))
                      (parent (mk-win :g-p right :workspace 2)))
                 (focus user)
                 (let ((root (frame-tree-for right 2)))
                   (split-frame root :horizontal 1/2)
                   (frame-push-window parent (second (frame-leaves root))))
                 (check "no dialog out of view: goto-last-dialog stays and says so"
                        (lambda ()
                          (setf *last-dialog-elsewhere* nil said '())
                          (list (goto-last-dialog) (current-workspace right)
                                (and (find-if (lambda (m) (search "dialog" m))
                                              said)
                                     t)))
                        (list nil 1 t))
                 (let ((dlg (new-dialog :g-d right :g-p)))
                   (commit-size dlg 200 100)
                   (render right)
                   (check "a dialog that opens out of view is the last one"
                          (lambda () (eq *last-dialog-elsewhere* dlg)) t)
                   (check "goto-last-dialog shows its workspace and focuses it"
                          (lambda ()
                            (setf focused '())
                            (list (eq (goto-last-dialog) dlg)
                                  (current-workspace right)
                                  (eq (first focused) dlg)))
                          (list t 2 t))
                   ;; The dialog closes; the operator is back on workspace 1
                   (switch-workspace 1 right)
                   (remhash :g-d *windows*)
                   (check "a closed dialog: goto-last-dialog stays and says so"
                          (lambda ()
                            (setf said '() focused '())
                            (list (goto-last-dialog) (current-workspace right)
                                  focused
                                  (and (find-if (lambda (m) (search "dialog" m))
                                                said)
                                       t)))
                          (list nil 1 nil t)))))
          (setf (symbol-function 'focus-window) saved-focus
                (symbol-function 'message) saved-message)
          (clrhash *seats*)))

      ;; --- B3: tiled parent on another visible output: no notification ---
      (fresh)
      (let* ((left (mk-out :b3-left))
             (right (mk-out :b3-right 1280))
             (user (tiled (mk-win :b3-u left) left))
             (parent (tiled (mk-win :b3-p right) right)))
        (declare (ignore parent))
        (focus user)
        (let ((dlg (new-dialog :b3-d left :b3-p)))
          (commit-size dlg 200 100)
          (render left right)
          (check "a dialog that fits goes to its parent's frame on another output"
                 (lambda () (list (eq (window-output dlg) right) (pos dlg)))
                 (list t (list (+ 1280 540) (+ 36 332))))
          (check "a parent on another visible output raises no notification"
                 (lambda () *notes*) nil)))

      ;; --- Floating by default ---
      (flet ((unassigned (proxy &key parent (app-id "dlgapp"))
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-app-id win) app-id)
                 (when parent
                   (handle-window-event win :parent parent))
                 win)))
        (fresh)
        (let* ((out (mk-out :q-out))
               (parent (tiled (mk-win :q-p out) out))
               (dlg (unassigned :q-d :parent :q-p))
               (plain (unassigned :q-plain)))
          (declare (ignore parent))
          (float-new-dialogs)
          (check "a new window with a parent floats on map"
                 (lambda () (list (window-floating-p dlg)
                                  (window-placement-pending-p dlg)))
                 (list t t))
          (check "a new window without a parent stays tiled"
                 (lambda () (window-floating-p plain)) nil)
          (unfloat-window dlg)
          (float-new-dialogs)
          (check "a dialog is floated once: a later tile is kept"
                 (lambda () (window-floating-p dlg)) nil))

        ;; A parent that arrives just after map (a portal's file picker
        ;; names its parent once it is on screen) still floats the window
        (fresh)
        (let* ((out (mk-out :l-out))
               (parent (tiled (mk-win :l-p out) out))
               (late (unassigned :l-d))
               (stale (unassigned :l-s)))
          (declare (ignore parent))
          (float-new-dialogs)
          (setf (window-output late) out (window-output stale) out)
          (assign-window-workspace late 1)
          (assign-window-workspace stale 1)
          (frame-push-window late (frame-tree-for out 1))
          (frame-push-window stale (frame-tree-for out 1))
          (setf (window-node late) :l-d-node (window-node stale) :l-s-node)
          (setf (window-width late) 300 (window-height late) 200)
          (handle-window-event late :parent :l-p)
          (check "a parent that arrives just after map floats the window"
                 (lambda () (list (window-floating-p late)
                                  (window-placement-pending-p late)
                                  (null (window-frame late))))
                 (list t t t))
          (setf (gethash stale *dialog-float-decided*)
                (- (get-internal-real-time)
                   (* 10 internal-time-units-per-second)))
          (handle-window-event stale :parent :l-p)
          (check "a parent that arrives long after map leaves the window tiled"
                 (lambda () (window-floating-p stale)) nil))

        (fresh)
        (configure '(:window-rules ((:match "tiledlg" :float nil))))
        (let* ((out (mk-out :r-out))
               (parent (tiled (mk-win :r-p out) out))
               (dlg (unassigned :r-d :parent :r-p :app-id "tiledlg"))
               (other (unassigned :r-o :parent :r-p :app-id "otherapp")))
          (declare (ignore parent))
          (float-new-dialogs)
          (check "a rule with :float nil tiles that app's dialogs"
                 (lambda () (window-floating-p dlg)) nil)
          (check "a rule for another app leaves this dialog floating"
                 (lambda () (window-floating-p other)) t))
        (configure '(:window-rules ()))

        (fresh)
        (configure '(:dialogs (:float nil)))
        (let* ((out (mk-out :s-out))
               (parent (tiled (mk-win :s-p out) out))
               (dlg (unassigned :s-d :parent :s-p)))
          (declare (ignore parent))
          (float-new-dialogs)
          (check "(:dialogs (:float nil)) leaves dialogs tiled"
                 (lambda () (window-floating-p dlg)) nil))
        (configure '(:dialogs (:float t)))

        (fresh)
        (let* ((out (mk-out :t-out))
               (parent (tiled (mk-win :t-p out) out))
               (dlg (unassigned :t-d :parent :t-p)))
          (declare (ignore parent))
          (apply-layout-for-output out)
          (check "the manage pass floats a new dialog before it is tiled"
                 (lambda () (list (window-floating-p dlg)
                                  (null (window-frame dlg))))
                 (list t t)))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
