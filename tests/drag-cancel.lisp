;;; Drag cancel: check that a pointer move or resize starting on a
;;; window mid-glide lands the glide where the window is drawn, so the
;;; drag starts from what is under the pointer and nothing interpolates
;;; against it.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (dolist (fn '(message request-manage-dirty propose-window-dimensions
                  rwm:river-seat-v1.op-start-pointer
                  rwm:river-seat-v1.op-end))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (labels ((start-op (type setup)
               ;; Float a window at (300 100), run SETUP on it with the
               ;; animation clock at 5000, move the clock halfway
               ;; through a 1000 ms linear glide, then start a TYPE op
               ;; on it. Return the window, the seat, and whether a
               ;; glide is still active.
               (let* ((now 5000)
                      (*animation-clock* (lambda () now))
                      (*animations* (make-hash-table))
                      (*windows* (make-hash-table))
                      (*seats* (make-hash-table))
                      (*config* '(:animations (:enabled t)))
                      (seat (make-instance 'rhizome-seat :proxy :drag-seat))
                      (win (make-instance 'rhizome-window :proxy :drag-win)))
                 (setf (gethash :drag-win *windows*) win
                       (gethash :drag-seat *seats*) seat
                       (window-floating-p win) t
                       (window-float-x win) 300 (window-float-y win) 100
                       (window-float-width win) 200
                       (window-float-height win) 150)
                 (funcall setup win)
                 (incf now (floor internal-time-units-per-second 2))
                 (setf (seat-op-pending seat) (list :win win :type type))
                 (float-op-manage-pass)
                 (values win seat (animation-active-p win)))))

      ;; An absolute glide from (10 20) toward (300 100), halfway
      ;; through, is drawn at (155 60).
      (multiple-value-bind (win seat active)
          (start-op :move
                    (lambda (win)
                      (animation-start win :from-x 10 :from-y 20
                                           :duration 1000 :easing :linear)))
        (ok "a move started mid-glide ends the glide" active nil)
        (ok "the window stays where it was drawn"
            (list (window-float-x win) (window-float-y win)) '(155 60))
        (let ((op (seat-op-state seat)))
          (ok "the move starts from the drawn position"
              (list (getf op :origin-x) (getf op :origin-y)) '(155 60))
          (setf (getf op :dx) 10 (getf op :dy) -5
                (seat-op-state seat) op)
          (let ((*seats* (make-hash-table))
                (*windows* (make-hash-table)))
            (setf (gethash :drag-seat *seats*) seat
                  (gethash :drag-win *windows*) win)
            (float-op-manage-pass))
          (ok "the drag moves the window from where it was drawn"
              (list (window-float-x win) (window-float-y win)) '(165 55))))

      ;; A workspace slide on a float: offset -1920 decaying to 0,
      ;; halfway through, is drawn 960 px left of its float position.
      (multiple-value-bind (win seat active)
          (start-op :move
                    (lambda (win)
                      (animation-start win :dx -1920
                                           :duration 1000 :easing :linear)))
        (declare (ignore seat))
        (ok "a move started mid-slide ends the slide" active nil)
        (ok "the sliding window stays where it was drawn"
            (list (window-float-x win) (window-float-y win)) '(-660 100)))

      (multiple-value-bind (win seat active)
          (start-op :resize
                    (lambda (win)
                      (animation-start win :from-x 10 :from-y 20
                                           :duration 1000 :easing :linear)))
        (ok "a resize started mid-glide ends the glide" active nil)
        (ok "the resized window stays where it was drawn"
            (list (window-float-x win) (window-float-y win)) '(155 60))
        (ok "the resize starts from the drawn position"
            (list (getf (seat-op-state seat) :origin-x)
                  (getf (seat-op-state seat) :origin-y))
            '(155 60)))

      (multiple-value-bind (win seat active)
          (start-op :move (lambda (win) (declare (ignore win)) nil))
        (declare (ignore active))
        (ok "a move on a still window starts from its float position"
            (list (window-float-x win) (window-float-y win)
                  (getf (seat-op-state seat) :origin-x))
            '(300 100 300)))))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
