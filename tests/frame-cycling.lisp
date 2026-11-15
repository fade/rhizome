;;; Tests for cycling frame focus in tree order (StumpWM's fnext and
;;; fprev): the exported entry points, next and previous order across
;;; three leaves with an empty one between occupied ones, wrapping in
;;; both directions, keyboard focus clearing on the empty frame, the
;;; command-table entries, and the single-frame no-op.
;;; River requests are stubbed and recorded.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (focused '()) (cleared 0))
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
                           propose-window-dimensions set-window-tiled-state))
               (setf (symbol-function fn)
                     (lambda (&rest args) (declare (ignore args)) nil)))
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
                     (incf cleared))))
           (fresh-world ()
             (setf *windows* (make-hash-table)
                   *outputs* (make-hash-table)
                   *output-workspace* (make-hash-table)
                   *window-workspace* (make-hash-table)
                   *minimized-windows* (make-hash-table)
                   *frame-trees* (make-hash-table :test 'equal)
                   *current-frames* (make-hash-table :test 'equal)
                   *scroll-states* (make-hash-table :test 'equal))
             (setf focused '() cleared 0))
           (new-window (proxy output)
             (let ((w (make-instance 'rhizome-window :proxy proxy)))
               (setf (gethash proxy *windows*) w
                     (window-output w) output)
               (assign-window-workspace w 1)
               w))
           (external-p (name)
             (multiple-value-bind (sym status)
                 (find-symbol name '#:rhizome)
               (and sym (eq status :external)))))
    (macrolet ((check (name actual expected)
                 `(handler-case (ok ,name ,actual ,expected)
                    (error (e)
                      (incf fail)
                      (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

      (stub-protocol)
      (check "focus-frame is exported" (external-p "FOCUS-FRAME") t)
      (check "focus-next-frame is exported"
             (external-p "FOCUS-NEXT-FRAME") t)
      (check "focus-previous-frame is exported"
             (external-p "FOCUS-PREVIOUS-FRAME") t)
      (check "fnext is in the command table"
             (functionp (gethash :fnext *command-table*)) t)
      (check "fprev is in the command table"
             (functionp (gethash :fprev *command-table*)) t)

      ;; Three columns: a | (empty) | c.
      (fresh-world)
      (let* ((out (make-instance 'rhizome-output :proxy :fc-out
                                                 :x 0 :y 0
                                                 :width 1920 :height 1080))
             (a (progn (setf (gethash :fc-out *outputs*) out)
                       (new-window :fc-a out)))
             (c (new-window :fc-c out)))
        (set-workspace-tree out 1 '((0 0 640 1080) (640 0 640 1080)
                                    (1280 0 640 1080))
                            (list a c))
        (let* ((leaves (frame-leaves (frame-tree-for out 1)))
               (a-frame (first leaves))
               (empty (second leaves))
               (c-frame (third leaves)))
          (frame-remove-window c)
          (frame-push-window c c-frame)
          (setf (frame-user-split (frame-tree-for out 1)) t)
          (setf (window-focused-p a) t)
          (check "setup: three leaves, the middle one empty"
                 (mapcar (lambda (f) (length (frame-windows f))) leaves)
                 '(1 0 1))
          (check "setup: a's frame is current"
                 (eq (current-frame-for out 1) a-frame) t)

          ;; --- next ---
          (check "next returns the empty frame"
                 (eq (focus-next-frame out) empty) t)
          (check "next: the empty frame is current"
                 (eq (current-frame-for out 1) empty) t)
          (check "next: keyboard focus clears on the empty frame"
                 (list (window-focused-p a) (plusp cleared)) '(nil t))
          (focus-next-frame out)
          (check "next: the frame after the empty one is current"
                 (eq (current-frame-for out 1) c-frame) t)
          (check "next: its window takes keyboard focus"
                 (list (first focused) (window-focused-p c)) (list c t))
          (focus-next-frame out)
          (check "next wraps from the last frame to the first"
                 (list (eq (current-frame-for out 1) a-frame)
                       (window-focused-p a))
                 '(t t))

          ;; --- previous ---
          (setf cleared 0)
          (focus-previous-frame out)
          (check "previous wraps from the first frame to the last"
                 (list (eq (current-frame-for out 1) c-frame)
                       (window-focused-p c))
                 '(t t))
          (focus-previous-frame out)
          (check "previous stops on the empty frame"
                 (eq (current-frame-for out 1) empty) t)
          (check "previous: keyboard focus clears on the empty frame"
                 (list (window-focused-p c) (plusp cleared)) '(nil t))
          (focus-previous-frame out)
          (check "previous reaches the first frame"
                 (list (eq (current-frame-for out 1) a-frame)
                       (window-focused-p a))
                 '(t t))

          ;; --- through the command table ---
          (check "fnext command returns NIL or a string, not a frame"
                 (let ((r (execute-command out :fnext nil)))
                   (or (null r) (stringp r)))
                 t)
          (check "fnext moves to the next frame"
                 (eq (current-frame-for out 1) empty) t)
          (check "fprev command returns NIL or a string, not a frame"
                 (let ((r (execute-command out :fprev nil)))
                   (or (null r) (stringp r)))
                 t)
          (check "fprev moves to the previous frame"
                 (eq (current-frame-for out 1) a-frame) t)

          ;; --- through IPC dispatch: the reply is printed with ~A,
          ;; and a frame's parent links are circular ---
          (let ((saved (symbol-function 'current-output)))
            (setf (symbol-function 'current-output) (lambda () out))
            (unwind-protect
                 (flet ((clean-reply-p (reply)
                          (and (stringp reply)
                               (not (search "error" reply)))))
                   (check "IPC fnext replies with a string"
                          (clean-reply-p (ipc-dispatch-line "fnext")) t)
                   (check "IPC fnext moves to the next frame"
                          (eq (current-frame-for out 1) empty) t)
                   (check "IPC fprev replies with a string"
                          (clean-reply-p (ipc-dispatch-line "fprev")) t)
                   (check "IPC fprev moves to the previous frame"
                          (eq (current-frame-for out 1) a-frame) t))
              (setf (symbol-function 'current-output) saved)))))

      ;; --- one frame: nothing to cycle ---
      (fresh-world)
      (let* ((out (make-instance 'rhizome-output :proxy :fc-one
                                                 :x 0 :y 0
                                                 :width 1280 :height 800))
             (a (progn (setf (gethash :fc-one *outputs*) out)
                       (new-window :fc-solo out))))
        (set-workspace-tree out 1 '((0 0 1280 800)) (list a))
        (setf (window-focused-p a) t)
        (let ((only (current-frame-for out 1)))
          (check "single frame: next returns NIL"
                 (focus-next-frame out) nil)
          (check "single frame: previous returns NIL"
                 (focus-previous-frame out) nil)
          (check "single frame: the frame and focus are unchanged"
                 (list (eq (current-frame-for out 1) only)
                       (window-focused-p a) focused cleared)
                 '(t t nil 0))
          (check "single frame: fnext command returns NIL or a string"
                 (let ((r (execute-command out :fnext nil)))
                   (or (null r) (stringp r)))
                 t)
          (check "single frame: fprev command returns NIL or a string"
                 (let ((r (execute-command out :fprev nil)))
                   (or (null r) (stringp r)))
                 t))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (uiop:quit 1))))
