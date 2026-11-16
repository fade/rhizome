;;; Tests for (:frames (:cycle-skips-empty ...)), which decides whether
;;; fnext and fprev stop on empty frames:
;;;
;;; - the default still stops on an empty frame (control);
;;; - with the option on, three leaves (a | empty | b) cycle a, b, a in
;;;   both directions;
;;; - with the option on, four leaves with two adjacent empty frames
;;;   skip both;
;;; - a frame holding only a minimized window counts as empty;
;;; - from an empty current frame, the next occupied frame takes focus;
;;; - when every other frame is empty, cycling is a no-op;
;;; - the fnext and fprev commands return NIL or a string under both
;;;   settings.
;;;
;;; River requests are stubbed and recorded.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (focused '()) (cleared 0))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (let ((*print-circle* t) (*print-level* 3)
                       (*print-length* 8))
                   (incf fail)
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
           (columns (proxy count)
             ;; An output split into COUNT equal columns. Returns the
             ;; output and its leaves; every column is empty.
             (let* ((width (* 400 count))
                    (out (make-instance 'rhizome-output :proxy proxy
                                                        :x 0 :y 0
                                                        :width width
                                                        :height 800))
                    (seed (progn (setf (gethash proxy *outputs*) out)
                                 (new-window (list proxy :seed) out))))
               (set-workspace-tree out 1
                                   (loop for i below count
                                         collect (list (* 400 i) 0 400 800))
                                   (list seed))
               (frame-remove-window seed)
               (remhash (list proxy :seed) *windows*)
               (setf (frame-user-split (frame-tree-for out 1)) t)
               (values out (frame-leaves (frame-tree-for out 1)))))
           (place (win frame)
             (frame-remove-window win)
             (frame-push-window win frame))
           (command-result-ok-p (out command)
             (let ((r (execute-command out command nil)))
               (or (null r) (stringp r)))))
    (macrolet ((check (name actual expected)
                 `(handler-case (ok ,name ,actual ,expected)
                    (error (e)
                      (incf fail)
                      (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

      (stub-protocol)
      (check "frames-cycle-skips-empty-p is NIL by default"
             (progn (configure '(:frames (:cycle-skips-empty nil)))
                    (frames-cycle-skips-empty-p))
             nil)
      (check "frames-cycle-skips-empty-p is T when the option is on"
             (progn (configure '(:frames (:cycle-skips-empty t)))
                    (frames-cycle-skips-empty-p))
             t)

      ;; --- three columns: a | (empty) | b ---
      (fresh-world)
      (configure '(:frames (:cycle-skips-empty nil)))
      (multiple-value-bind (out leaves) (columns :cs-three 3)
        (let ((a (new-window :cs-a out))
              (b (new-window :cs-b out))
              (a-frame (first leaves))
              (empty (second leaves))
              (b-frame (third leaves)))
          (place a a-frame)
          (place b b-frame)
          (check "three: setup, the middle frame is empty"
                 (mapcar (lambda (f) (length (frame-windows f))) leaves)
                 '(1 0 1))
          (focus-frame out 1 a-frame)

          ;; Control: the default stops on the empty frame.
          (check "default: next lands on the empty frame"
                 (eq (focus-next-frame out) empty) t)
          (check "default: fnext command returns NIL or a string"
                 (command-result-ok-p out :fnext) t)
          (check "default: fnext moves on to b's frame"
                 (eq (current-frame-for out 1) b-frame) t)
          (check "default: fprev command returns NIL or a string"
                 (command-result-ok-p out :fprev) t)
          (check "default: fprev lands back on the empty frame"
                 (eq (current-frame-for out 1) empty) t)
          (focus-frame out 1 a-frame)

          ;; The option on: the empty frame is never a stop.
          (configure '(:frames (:cycle-skips-empty t)))
          (setf cleared 0)
          (check "skip: next goes from a to b"
                 (list (eq (focus-next-frame out) b-frame)
                       (window-focused-p b))
                 '(t t))
          (check "skip: next wraps from b to a"
                 (list (eq (focus-next-frame out) a-frame)
                       (window-focused-p a))
                 '(t t))
          (check "skip: previous wraps from a to b"
                 (list (eq (focus-previous-frame out) b-frame)
                       (window-focused-p b))
                 '(t t))
          (check "skip: previous goes from b to a"
                 (list (eq (focus-previous-frame out) a-frame)
                       (window-focused-p a))
                 '(t t))
          (check "skip: keyboard focus never cleared"
                 cleared 0)
          (check "skip: fnext command returns NIL or a string"
                 (command-result-ok-p out :fnext) t)
          (check "skip: fnext moves to b's frame"
                 (eq (current-frame-for out 1) b-frame) t)
          (check "skip: fprev command returns NIL or a string"
                 (command-result-ok-p out :fprev) t)
          (check "skip: fprev moves to a's frame"
                 (eq (current-frame-for out 1) a-frame) t)

          ;; A frame whose only window is minimized counts as empty.
          (setf (window-minimized-p b) t)
          (check "skip: a minimized-only frame is skipped, next is a no-op"
                 (list (null (focus-next-frame out))
                       (eq (current-frame-for out 1) a-frame))
                 '(t t))
          (setf (window-minimized-p b) nil)

          ;; From an empty current frame, focus reaches an occupied one.
          (configure '(:frames (:cycle-skips-empty nil)))
          (focus-frame out 1 empty)
          (configure '(:frames (:cycle-skips-empty t)))
          (check "skip: next from the empty frame reaches b's frame"
                 (eq (focus-next-frame out) b-frame) t)
          (configure '(:frames (:cycle-skips-empty nil)))
          (focus-frame out 1 empty)
          (configure '(:frames (:cycle-skips-empty t)))
          (check "skip: previous from the empty frame reaches a's frame"
                 (eq (focus-previous-frame out) a-frame) t)))

      ;; --- four columns: a | (empty) | (empty) | b ---
      (fresh-world)
      (configure '(:frames (:cycle-skips-empty t)))
      (multiple-value-bind (out leaves) (columns :cs-four 4)
        (let ((a (new-window :cs4-a out))
              (b (new-window :cs4-b out)))
          (place a (first leaves))
          (place b (fourth leaves))
          (focus-frame out 1 (first leaves))
          (check "four: setup, the middle two frames are empty"
                 (mapcar (lambda (f) (length (frame-windows f))) leaves)
                 '(1 0 0 1))
          (check "four: next skips both empty frames"
                 (eq (focus-next-frame out) (fourth leaves)) t)
          (check "four: next wraps to a's frame"
                 (eq (focus-next-frame out) (first leaves)) t)
          (check "four: previous wraps to b's frame"
                 (eq (focus-previous-frame out) (fourth leaves)) t)
          (check "four: previous skips both empty frames"
                 (eq (focus-previous-frame out) (first leaves)) t)
          (configure '(:frames (:cycle-skips-empty nil)))
          (check "four, default: next lands on the first empty frame"
                 (eq (focus-next-frame out) (second leaves)) t)))

      ;; --- every other frame empty: a | (empty) | (empty) ---
      (fresh-world)
      (configure '(:frames (:cycle-skips-empty t)))
      (multiple-value-bind (out leaves) (columns :cs-alone 3)
        (let ((a (new-window :csx-a out)))
          (place a (first leaves))
          (focus-frame out 1 (first leaves))
          (setf focused '() cleared 0)
          (check "alone: next returns NIL"
                 (null (focus-next-frame out)) t)
          (check "alone: previous returns NIL"
                 (null (focus-previous-frame out)) t)
          (check "alone: the frame and focus are unchanged"
                 (list (eq (current-frame-for out 1) (first leaves))
                       (window-focused-p a) (length focused) cleared)
                 '(t t 0 0))
          (check "alone: fnext command returns NIL or a string"
                 (command-result-ok-p out :fnext) t)
          (check "alone: fprev command returns NIL or a string"
                 (command-result-ok-p out :fprev) t)
          (check "alone: the commands leave the frame unchanged"
                 (eq (current-frame-for out 1) (first leaves)) t)))

      (configure '(:frames (:cycle-skips-empty nil))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (uiop:quit 1))))
