;;; Tests for fselect (fselect.lisp): which frames carry a label and
;;; in what order, the key to frame mapping, cancel keys, the focus
;;; move, the C-t f / C-t F bindings, and the label surfaces' lifecycle
;;; across every exit path. River requests are stubbed and recorded;
;;; the shm buffer path runs for real.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()) (focused '()))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (record (op)
             (lambda (&rest args) (push (cons op args) calls) op))
           (calls-of (op)
             (remove-if-not (lambda (c) (eq (car c) op)) calls))
           (stub-protocol ()
             (dolist (spec '((wl:wl-compositor.create-surface :surface)
                             (wl:wl-compositor.create-region :region)
                             (wl:wl-surface.set-input-region :input-region)
                             (wl:wl-region.destroy :region-destroy)
                             (rwm:river-window-manager-v1.get-shell-surface :shell)
                             (rwm:river-shell-surface-v1.get-node :node)
                             (rwm:river-shell-surface-v1.sync-next-commit :sync)
                             (rwm:river-shell-surface-v1.destroy :shell-destroy)
                             (rwm:river-node-v1.set-position :position)
                             (rwm:river-node-v1.place-top :top)
                             (rwm:river-node-v1.place-bottom :bottom)
                             (rwm:river-node-v1.destroy :node-destroy)
                             (wl:wl-surface.attach :attach)
                             (wl:wl-surface.damage :damage)
                             (wl:wl-surface.commit :commit)
                             (wl:wl-surface.destroy :surface-destroy)
                             (wl:wl-shm.create-pool :pool)
                             (wl:wl-shm-pool.create-buffer :buffer)
                             (wl:wl-shm-pool.destroy :pool-destroy)
                             (wl:wl-buffer.destroy :buffer-destroy)))
               (setf (symbol-function (first spec)) (record (second spec))))
             (dolist (fn '(message show-message message-hide
                           request-manage-dirty reassert-seat-focus))
               (setf (symbol-function fn)
                     (lambda (&rest args) (declare (ignore args)) nil)))
             ;; The capture surface: prefix mode opens without river
             (setf (symbol-function 'open-prefix-capture-surface)
                   (lambda () (setf *prefix-mode-active* t)))
             (setf (symbol-function 'prefix-capture-available-p)
                   (lambda () t))
             (setf (symbol-function 'all-seats) (lambda () '(:seat)))
             (setf (symbol-function 'focus-window)
                   (lambda (win seat)
                     (declare (ignore seat))
                     (push win focused)))
             (setf *compositor* :fake-compositor
                   *shm* :fake-shm
                   *wm-proxy* :fake-wm))
           (labels-of ()
             (mapcar (lambda (target) (string (first target)))
                     (fselect-targets)))
           (frame-of (target) (fourth target))
           (fresh-world ()
             (setf *windows* (make-hash-table)
                   *outputs* (make-hash-table)
                   *output-workspace* (make-hash-table)
                   *window-workspace* (make-hash-table)
                   *minimized-windows* (make-hash-table)
                   *frame-trees* (make-hash-table :test 'equal)
                   *current-frames* (make-hash-table :test 'equal)
                   *scroll-states* (make-hash-table :test 'equal))
             (clrhash *fselect-labels*)
             (setf calls '() focused '()))
           (live-labels ()
             (hash-table-count *fselect-labels*)))
    (macrolet ((check (name actual expected)
                 `(handler-case (ok ,name ,actual ,expected)
                    (error (e)
                      (incf fail)
                      (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

      ;; --- bindings and command table ---
      (setup-default-prefix-bindings)
      (check "C-t f is bound to fselect"
             (keymap-binding-doc *default-prefix-map* (make-key 102))
             "select frame by number")
      (check "C-t F is bound to the float/tile toggle"
             (keymap-binding-doc *default-prefix-map* (make-key 70))
             "float/tile window")
      (check "fselect is in the command table"
             (functionp (gethash :fselect *command-table*))
             t)

      (fresh-world)
      (stub-protocol)
      (configure '(:theme (:focused-border "#7aa2f7")))
      ;; out2 registered first and placed left: numbering follows
      ;; screen order, not registration order
      (let* ((right (make-instance 'rhizome-output :proxy :fs-right
                                                   :x 1920 :y 0
                                                   :width 1280 :height 800))
             (left (make-instance 'rhizome-output :proxy :fs-left
                                                  :x 0 :y 0
                                                  :width 1920 :height 1080))
             (t1 (make-instance 'rhizome-window :proxy :fs-t1))
             (t2 (make-instance 'rhizome-window :proxy :fs-t2))
             (t3 (make-instance 'rhizome-window :proxy :fs-t3))
             (t4 (make-instance 'rhizome-window :proxy :fs-t4)))
        (setf (gethash :fs-right *outputs*) right
              (gethash :fs-left *outputs*) left)
        (dolist (w (list t1 t2 t3 t4))
          (setf (gethash (window-proxy w) *windows*) w))
        (dolist (win (list t1 t2))
          (setf (window-output win) left)
          (assign-window-workspace win 1))
        (dolist (win (list t3 t4))
          (setf (window-output win) right)
          (assign-window-workspace win 1))
        (set-workspace-tree left 1 '((0 0 960 1080) (960 0 960 1080))
                            (list t1 t2))
        ;; Right head: three frames, t3 in the first, the middle one
        ;; empty, t4 in the last
        (set-workspace-tree right 1 '((0 0 640 800) (640 0 320 800)
                                      (960 0 320 800))
                            (list t3))
        (let ((leaves (frame-leaves (frame-tree-for right 1))))
          (frame-push-window t4 (third leaves)))
        (setf (window-focused-p t1) t)

        ;; --- numbering ---
        (check "labels: StumpWM's frame number map, every visible head"
               (labels-of) '("0" "1" "2" "3"))
        (check "numbering runs left head first, in tree order"
               (mapcar (lambda (target) (list (eq (second target) left)
                                              (frame-x (frame-of target))))
                       (fselect-targets))
               '((t 0) (t 960) (nil 0) (nil 960)))
        (check "the empty frame carries no label"
               (some (lambda (target) (null (frame-windows (frame-of target))))
                     (fselect-targets))
               nil)
        (setf (window-minimized-p t4) t)
        (check "a frame holding only minimized windows carries no label"
               (labels-of) '("0" "1" "2"))
        (setf (window-minimized-p t4) nil)
        (let ((*frame-number-map* "ab"))
          (check "labels stop at the end of the map"
                 (labels-of) '("a" "b")))
        (setf (gethash (frame-key right 1) *workspace-types*) :floating)
        (check "a floating workspace's head carries no labels"
               (labels-of) '("0" "1"))
        (remhash (frame-key right 1) *workspace-types*)

        ;; --- key mapping ---
        (check "digit 3 maps to t4's frame on the right head"
               (eq (frame-of (fselect-key-target (make-key 51)))
                   (window-frame t4))
               t)
        (check "a digit with Control still held maps the same"
               (eq (frame-of (fselect-key-target (make-key 50 '(:control))))
                   (window-frame t3))
               t)
        (check "a label past the last frame maps to nothing"
               (fselect-key-target (make-key 52)) nil)
        (check "a letter outside the labels maps to nothing"
               (fselect-key-target (make-key 122)) nil)
        (check "C-g cancels" (fselect-cancel-key-p (make-key 103 '(:control))) t)
        (check "Escape cancels" (fselect-cancel-key-p (make-key 65307)) t)
        (check "plain g does not cancel" (fselect-cancel-key-p (make-key 103)) nil)

        ;; --- labels while active ---
        (enter-fselect)
        (check "fselect enters its capture mode"
               (list *prefix-mode-active* *fselect-active*) '(t t))
        (setf calls '())
        (fselect-labels-render-pass)
        (check "one label surface per numbered frame"
               (list (length (calls-of :shell)) (live-labels)) '(4 4))
        (check "labels sit on top of the render list"
               (length (calls-of :top)) 4)
        (check "labels take no input"
               (length (calls-of :input-region)) 4)
        (check "commits are synced to the render sequence"
               (length (calls-of :sync)) (length (calls-of :commit)))
        (check "label 2 sits inside t3's frame on the right head"
               (multiple-value-list
                (fselect-label-position right (window-frame t3)))
               (list (+ 1920 *fselect-label-inset*) *fselect-label-inset*))
        (check "a label lands at its frame's position"
               (not (null (member (list (+ 1920 *fselect-label-inset*)
                                        *fselect-label-inset*)
                                  (mapcar (lambda (c) (rest (rest c)))
                                          (calls-of :position))
                                  :test #'equal)))
               t)
        (setf calls '())
        (fselect-labels-render-pass)
        (check "an unchanged pass redraws nothing"
               (list (length (calls-of :buffer)) (length (calls-of :commit)))
               '(0 0))

        ;; --- label pixels ---
        (let ((state (fselect-label-draw (list) "7" 30 28 255 255 255 0 0 0)))
          (check "label fill in the outline colour"
                 (get-pixel-argb (getf state :ptr) 30 28 0 0) #xffffffff)
          (frame-outline-free state))
        (configure '(:theme (:frame-outline "#00ffff")))
        (check "label colour follows (:theme :frame-outline)"
               (subseq (multiple-value-list (fselect-label-colours)) 0 3)
               '(0 255 255))
        (configure '(:theme (:frame-outline t)))
        (check "label colour defaults to the focused border"
               (subseq (multiple-value-list (fselect-label-colours)) 0 3)
               '(#x7a #xa2 #xf7))

        ;; --- a digit picks a frame on the other head ---
        (setf focused '())
        (prefix-handle-key 51)
        (check "the digit focuses that frame's window"
               (first focused) t4)
        (check "the picked frame becomes that head's current frame"
               (eq (gethash (frame-key right 1) *current-frames*)
                   (window-frame t4))
               t)
        (check "the pick selects the other head"
               (eq *selected-output* right)
               t)
        (check "picking ends the capture"
               (list *prefix-mode-active* *fselect-active*) '(nil nil))
        (setf calls '())
        (fselect-labels-render-pass)
        (check "after a pick every label is destroyed"
               (list (length (calls-of :shell-destroy)) (live-labels))
               '(4 0))

        ;; --- every other exit path tears the labels down ---
        (flet ((exit-path (name thunk)
                 (setf focused '())
                 (enter-fselect)
                 (fselect-labels-render-pass)
                 (funcall thunk)
                 (setf calls '())
                 (fselect-labels-render-pass)
                 (check (format nil "~A: labels gone, nothing focused" name)
                        (list *fselect-active* (live-labels) focused)
                        '(nil 0 nil))))
          (exit-path "C-g" (lambda () (prefix-handle-key 103 '(:ctrl))))
          (exit-path "Escape" (lambda () (prefix-handle-key 65307)))
          (exit-path "an unlabelled key" (lambda () (prefix-handle-key 120)))
          (exit-path "timeout"
                     (lambda ()
                       (setf *prefix-expiry* 1)
                       (prefix-drain))))

        ;; --- output removal while active ---
        (enter-fselect)
        (fselect-labels-render-pass)
        (remhash :fs-right *outputs*)
        (setf calls '())
        (fselect-labels-render-pass)
        (check "a removed head's labels are destroyed"
               (list (length (calls-of :shell-destroy)) (live-labels))
               '(2 2))
        (exit-prefix-mode)
        (fselect-labels-render-pass)
        (check "and the rest on exit" (live-labels) 0)

        ;; --- nothing to select ---
        (setf *outputs* (make-hash-table))
        (enter-fselect)
        (check "no frames: no capture"
               (list *prefix-mode-active* *fselect-active*) '(nil nil))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (uiop:quit 1))))
