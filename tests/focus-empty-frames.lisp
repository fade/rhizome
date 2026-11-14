;;; Tests for focus into empty frames ((:frames (:focus-empty ...))) and
;;; the frame launch verbs (frame-launch.lisp): directional focus and
;;; fselect with the option off and on, toggled at runtime; keyboard
;;; focus cleared on an empty frame; the next new window landing in the
;;; frame focus moved to; focus-frame; launching a program into a frame
;;; on another output, into an occupied frame, and into a hidden
;;; workspace; claim matching by process ancestry and expiry.
;;; River requests are stubbed and recorded.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (focused '()) (cleared 0) (spawned '()))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (stub-protocol ()
             (dolist (fn '(message show-message message-hide
                           request-manage-dirty reassert-seat-focus
                           fselect-labels-render-pass
                           propose-window-dimensions set-window-tiled-state))
               (setf (symbol-function fn)
                     (lambda (&rest args) (declare (ignore args)) nil)))
             (setf (symbol-function 'open-prefix-capture-surface)
                   (lambda () (setf *prefix-mode-active* t)))
             (setf (symbol-function 'prefix-capture-available-p)
                   (lambda () t))
             (setf (symbol-function 'all-seats) (lambda () '(:seat)))
             (setf (symbol-function 'focus-window)
                   (lambda (win seat &rest more)
                     (declare (ignore seat more))
                     (dolist (w (all-windows))
                       (setf (window-focused-p w) (eq w win)))
                     (push win focused)))
             (setf (symbol-function 'clear-focus)
                   (lambda (seat)
                     (declare (ignore seat))
                     (incf cleared)))
             (setf (symbol-function 'launch-spawn)
                   (lambda (command)
                     (push command spawned)
                     4242)))
           (fresh-world ()
             (setf *windows* (make-hash-table)
                   *outputs* (make-hash-table)
                   *output-workspace* (make-hash-table)
                   *window-workspace* (make-hash-table)
                   *minimized-windows* (make-hash-table)
                   *frame-trees* (make-hash-table :test 'equal)
                   *current-frames* (make-hash-table :test 'equal)
                   *scroll-states* (make-hash-table :test 'equal)
                   *launch-claims* '())
             (clrhash *window-launch-targets*)
             (setf focused '() cleared 0 spawned '()))
           (new-window (proxy)
             (let ((w (make-instance 'rhizome-window :proxy proxy)))
               (setf (gethash proxy *windows*) w)
               w))
           (labels-of ()
             (mapcar (lambda (target) (string (first target)))
                     (fselect-targets))))
    (macrolet ((check (name actual expected)
                 `(handler-case (ok ,name ,actual ,expected)
                    (error (e)
                      (incf fail)
                      (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

      (stub-protocol)
      (check "the option defaults to off"
             (frames-focus-empty-p) nil)
      (check "the default config declares the option"
             (config-getf '(:frames) :focus-empty :default :absent) nil)
      (check "launch-into is in the command table"
             (functionp (gethash :launch-into *command-table*)) t)

      ;; Two heads. Left: a | (empty). Right: c, one frame.
      (fresh-world)
      (let* ((left (make-instance 'rhizome-output :proxy :fe-left
                                                  :x 0 :y 0
                                                  :width 1920 :height 1080))
             (right (make-instance 'rhizome-output :proxy :fe-right
                                                   :x 1920 :y 0
                                                   :width 1280 :height 800))
             (a (new-window :fe-a))
             (c (new-window :fe-c)))
        (setf (gethash :fe-left *outputs*) left
              (gethash :fe-right *outputs*) right)
        (setf (window-output a) left (window-output c) right)
        (assign-window-workspace a 1)
        (assign-window-workspace c 1)
        (set-workspace-tree left 1 '((0 0 960 1080) (960 0 960 1080))
                            (list a))
        (set-workspace-tree right 1 '((0 0 1280 800)) (list c))
        (let* ((leaves (frame-leaves (frame-tree-for left 1)))
               (a-frame (first leaves))
               (empty (second leaves)))
          (setf (frame-user-split (frame-tree-for left 1)) t)
          (setf (window-focused-p a) t)

          ;; --- option off: current behaviour ---
          (focus-frame-direction left 1 :right)
          (check "off: the focused window keeps focus"
                 (list (window-focused-p a) cleared) '(t 0))
          (check "off: commands still act on the focused window's frame"
                 (eq (current-frame-for left 1) a-frame) t)
          (check "off: fselect leaves the empty frame unlabelled"
                 (labels-of) '("0" "1"))

          ;; --- option on, set at runtime ---
          (configure '(:frames (:focus-empty t)))
          (check "configure turns the option on" (frames-focus-empty-p) t)
          (setf (gethash (frame-key left 1) *current-frames*) a-frame)
          (focus-frame-direction left 1 :right)
          (check "on: moving into an empty frame clears keyboard focus"
                 (list (window-focused-p a) (plusp cleared)) '(nil t))
          (check "on: the empty frame becomes the current frame"
                 (eq (current-frame-for left 1) empty) t)
          (let ((new (new-window :fe-new)))
            (setf (window-output new) left)
            (assign-window-workspace new 1)
            (frames-manage-decisions left 1 (list new))
            (check "on: the next new window lands in the empty frame"
                   (eq (window-frame new) empty) t)
            (frame-remove-window new)
            (remhash :fe-new *windows*)
            (remhash new *window-workspace*))
          (focus-frame-direction left 1 :left)
          (check "on: moving into an occupied frame focuses its window"
                 (list (first focused) (window-focused-p a)) (list a t))

          ;; --- fselect with the option on ---
          (check "on: fselect labels the empty frame in tree order"
                 (labels-of) '("0" "1" "2"))
          (setf cleared 0)
          (enter-fselect)
          (prefix-handle-key 49)
          (check "on: picking the empty frame's label clears focus"
                 (list (window-focused-p a) (plusp cleared)) '(nil t))
          (check "on: the picked empty frame is current"
                 (eq (current-frame-for left 1) empty) t)
          (check "picking ends the capture"
                 (list *prefix-mode-active* *fselect-active*) '(nil nil))
          (enter-fselect)
          (prefix-handle-key 50)
          (check "on: a label on the other head focuses its window"
                 (first focused) c)

          ;; --- toggled back off ---
          (configure '(:frames (:focus-empty nil)))
          (check "off again: the empty frame loses its label"
                 (labels-of) '("0" "1"))
          (setf cleared 0)
          (dolist (w (all-windows)) (setf (window-focused-p w) nil))
          (setf (window-focused-p a) t)
          (setf (gethash (frame-key left 1) *current-frames*) a-frame)
          (focus-frame-direction left 1 :right)
          (check "off again: directional focus leaves focus alone"
                 (list (window-focused-p a) cleared) '(t 0))

          ;; --- focus-frame, the verb ---
          (check "focus-frame returns the frame it made current"
                 (eq (focus-frame left 1 empty) empty) t)
          (check "focus-frame on an empty frame clears focus regardless of the option"
                 (window-focused-p a) nil)
          (check "focus-frame refuses a workspace its output does not show"
                 (focus-frame left 2 (frame-tree-for left 2)) nil)
          (check "focus-frame refuses a frame outside the tree"
                 (focus-frame left 1 (make-frame)) nil)

          ;; --- frame addressing ---
          (check "workspace-frame-leaf counts leaves in layout order"
                 (list (eq (workspace-frame-leaf left 1 0) a-frame)
                       (eq (workspace-frame-leaf left 1 1) empty)
                       (workspace-frame-leaf left 1 2))
                 '(t t nil))
          (configure '(:frames (:focus-empty t)))
          (check "frame-target-by-label resolves a label to its place"
                 (multiple-value-bind (out ws frame) (frame-target-by-label #\2)
                   (list (eq out right) ws (eq frame (window-frame c))))
                 '(t 1 t))
          (check "an unknown label resolves to nothing"
                 (frame-target-by-label #\z) nil)
          (configure '(:frames (:focus-empty nil)))

          ;; --- launching into a frame on the other head ---
          (dolist (w (all-windows)) (setf (window-focused-p w) nil))
          (setf (window-focused-p a) t)
          (let ((claim (launch-into-frame "foot -a probe" right 1
                                          (window-frame c))))
            (check "launch-into-frame spawns the command"
                   (first spawned) "foot -a probe")
            (check "and returns its claim"
                   (list (launch-claim-p claim) (launch-claim-pid claim))
                   '(t 4242))
            (let ((stray (new-window :fe-stray)))
              (setf (window-unreliable-pid stray) 999)
              (let ((*process-parent-function* (lambda (pid)
                                                 (declare (ignore pid))
                                                 1)))
                (check "a window from another process takes no claim"
                       (launch-claim-match stray) nil))
              (remhash :fe-stray *windows*))
            (let ((new (new-window :fe-launched)))
              (setf (window-unreliable-pid new) 5000)
              (let ((*process-parent-function*
                      (lambda (pid) (if (= pid 5000) 4242 1))))
                (check "a child of the launched process takes the claim"
                       (eq (launch-claim-match new) claim) t))
              (check "the claim names the window it placed"
                     (eq (launch-claim-window claim) new) t)
              (check "a matched claim is used up"
                     *launch-claims* nil)
              (check "the window is stamped onto the target output"
                     (list (eq (window-output new) right)
                           (window-workspace new))
                     '(t 1))
              ;; The left head's pass runs first and must not take it
              (frames-manage-decisions left 1 '())
              (assign-window-workspace new 1)
              (frames-manage-decisions right 1 (list c new))
              (check "the new window lands in the occupied target frame"
                     (let ((frame (window-frame new)))
                       (and frame
                            (eq (frame-parent frame)
                                (frame-tree-for right 1))
                            t))
                     t)
              (check "the source head's frames are untouched"
                     (length (frame-leaves (frame-tree-for left 1))) 2)
              (check "the target is forgotten once the window is placed"
                     (gethash new *window-launch-targets*) nil))))

        ;; --- launching into a hidden workspace ---
        (setf spawned '())
        (let* ((hidden-root (frame-tree-for right 3))
               (claim (launch-into-frame "foot -a hidden" right 3
                                         hidden-root))
               (new (new-window :fe-hidden)))
          (setf (window-unreliable-pid new) 4242)
          (launch-claim-match new)
          (check "a hidden workspace target does not switch workspaces"
                 (current-workspace right) 1)
          (check "the window joins the hidden workspace"
                 (list (eq (window-output new) right) (window-workspace new))
                 '(t 3))
          (frames-manage-decisions right 3 (list new))
          (check "and lands in the target frame there"
                 (eq (window-frame new) hidden-root) t)
          (check "the claim recorded that window"
                 (eq (launch-claim-window claim) new) t))

        ;; --- claim lifetime ---
        (let ((claim (launch-into-frame "late" left 1
                                        (workspace-frame-leaf left 1 0)))
              (old (new-window :fe-old)))
          (check "a claim is pending until a window takes it"
                 (length *launch-claims*) 1)
          (setf (launch-claim-expires claim) 0)
          (setf (window-unreliable-pid old) 4242)
          (check "an expired claim matches nothing"
                 (launch-claim-match old) nil)
          (check "and is dropped"
                 *launch-claims* nil))
        (let ((old (new-window :fe-older)))
          (launch-into-frame "after" left 1 (workspace-frame-leaf left 1 0))
          (setf (window-unreliable-pid old) 4242)
          (check "a window registered before the launch takes no claim"
                 (launch-claim-match old) nil))
        (check "launch-into-frame refuses a frame outside the tree"
               (launch-into-frame "x" left 1 (make-frame)) nil)

        ;; --- process ancestry ---
        (let ((*process-parent-function*
                (lambda (pid) (case pid (30 20) (20 10) (10 1) (t nil)))))
          (check "a grandchild descends from its grandparent"
                 (pid-descends-from-p 30 10) t)
          (check "a process descends from itself"
                 (pid-descends-from-p 10 10) t)
          (check "a parent does not descend from its child"
                 (pid-descends-from-p 10 30) nil)
          (check "an unknown pid descends from nothing"
                 (pid-descends-from-p 77 10) nil))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (uiop:quit 1))))
