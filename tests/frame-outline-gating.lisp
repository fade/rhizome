;;; Tests for the frame outline's two rings: the ring around a focused
;;; frame that shows a window, and the ring around a focused empty
;;; frame, each switched by its own width. River requests are stubbed
;;; and recorded; the shm buffer path runs for real.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()))
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
           (styles ()
             (list (multiple-value-list (frame-outline-style nil))
                   (multiple-value-list (frame-outline-style t))))
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
             (dolist (fn '(message request-manage-dirty))
               (setf (symbol-function fn)
                     (lambda (&rest args) (declare (ignore args)) nil)))
             (setf *compositor* :fake-compositor
                   *shm* :fake-shm
                   *wm-proxy* :fake-wm))
           (fresh-world ()
             (setf *windows* (make-hash-table)
                   *outputs* (make-hash-table)
                   *output-workspace* (make-hash-table)
                   *window-workspace* (make-hash-table)
                   *minimized-windows* (make-hash-table)
                   *frame-trees* (make-hash-table :test 'equal)
                   *current-frames* (make-hash-table :test 'equal)
                   *scroll-states* (make-hash-table :test 'equal)
                   *seats* (make-hash-table)
                   *selected-output* nil)
             (setf (gethash :fg-seat *seats*)
                   (make-instance 'rhizome-seat :proxy :fg-seat))
             (clrhash *frame-outlines*)
             (setf calls '())))
    (macrolet ((check (name actual expected)
                 `(handler-case (ok ,name ,actual ,expected)
                    (error (e)
                      (incf fail)
                      (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

      ;; --- styles ---
      (configure '(:theme (:focused-border "#7aa2f7")))
      (check "defaults: window ring (122 162 247 2), empty ring (255 158 100 1)"
             (styles)
             '((122 162 247 2) (255 158 100 1)))
      (configure '(:theme (:frame-outline-width 1
                           :frame-outline-empty-width 1)))
      (check "width 1, empty width 1: both rings 1 px"
             (styles)
             '((122 162 247 1) (255 158 100 1)))
      (configure '(:theme (:frame-outline-width 2
                           :frame-outline-empty-width 0)))
      (check "width 2, empty width 0: window ring 2 px, no empty ring"
             (styles)
             '((122 162 247 2) (nil)))
      (configure '(:theme (:frame-outline-width 2
                           :frame-outline-empty-width :thin)))
      (check "empty width not an integer: the empty ring takes :frame-outline-width"
             (styles)
             '((122 162 247 2) (255 158 100 2)))
      (configure '(:theme (:frame-outline-width 2
                           :frame-outline-empty-width -1)))
      (check "empty width negative: the empty ring takes :frame-outline-width"
             (styles)
             '((122 162 247 2) (255 158 100 2)))
      (configure '(:theme (:frame-outline-width 0
                           :frame-outline-empty-width 1)))
      (check "width 0, empty width 1: no ring around a frame with a window"
             (multiple-value-list (frame-outline-style nil))
             '(nil))
      (check "width 0, empty width 1: a 1 px ring around an empty frame"
             (multiple-value-list (frame-outline-style t))
             '(255 158 100 1))
      (configure '(:theme (:frame-outline-width 3
                           :frame-outline-empty-width nil)))
      (check "empty width nil: the empty ring takes :frame-outline-width"
             (styles)
             '((122 162 247 3) (255 158 100 3)))
      (configure '(:theme (:frame-outline-width 0
                           :frame-outline-empty-width nil)))
      (check "empty width nil, width 0: both rings off"
             (styles)
             '((nil) (nil)))
      (configure '(:theme (:frame-outline nil
                           :frame-outline-width 2
                           :frame-outline-empty-width 1)))
      (check ":frame-outline nil: both rings off"
             (styles)
             '((nil) (nil)))
      (configure '(:theme (:frame-outline-width 0)))
      (check ":frame-outline nil, width 0, empty width 1: both rings off"
             (styles)
             '((nil) (nil)))
      (configure '(:theme (:frame-outline t)))

      ;; --- render pass: width 0, empty width 1 ---
      (fresh-world)
      (stub-protocol)
      (configure '(:theme (:frame-outline t :focused-border "#7aa2f7"
                           :frame-outline-empty "#ff9e64"
                           :frame-outline-width 0
                           :frame-outline-empty-width 1)))
      (let ((out (make-instance 'rhizome-output :proxy :fg-out
                                                :x 0 :y 0
                                                :width 800 :height 600))
            (win (make-instance 'rhizome-window :proxy :fg-win)))
        (setf (gethash :fg-out *outputs*) out)
        (flet ((frame ()
                 (current-frame-for out (current-workspace out)))
               (state () (gethash out *frame-outlines*)))
          (check "a frame with no window is empty"
                 (frame-outline-empty-p (frame)) t)
          (check "empty focused frame: an outline target"
                 (not (null (frame-outline-target))) t)
          (frame-outline-render-pass)
          (check "empty focused frame: the ring is mapped"
                 (getf (state) :mapped) t)
          (check "empty focused frame: the ring is the empty colour"
                 (get-pixel-argb (getf (state) :ptr) (getf (state) :w)
                                 (getf (state) :h) 0 0)
                 #xffff9e64)
          (check "empty focused frame: the ring is 1 px wide"
                 (list (car (last (getf (state) :drawn)))
                       (get-pixel-argb (getf (state) :ptr) (getf (state) :w)
                                       (getf (state) :h) 1 1))
                 '(1 0))
          ;; A window arrives in the focused frame
          (setf (gethash :fg-win *windows*) win
                (window-output win) out)
          (assign-window-workspace win (current-workspace out))
          (frame-push-window win (frame))
          (check "a frame holding a window is not empty"
                 (frame-outline-empty-p (frame)) nil)
          (check "frame with a window: no outline target"
                 (multiple-value-list (frame-outline-target)) '(nil))
          (setf calls '())
          (frame-outline-render-pass)
          (check "window arrives: the ring unmaps with a null buffer"
                 (mapcar #'third (calls-of :attach)) '(nil))
          (check "window arrives: the ring is not mapped"
                 (getf (state) :mapped) nil)
          ;; The window leaves: the empty ring returns
          (frame-remove-window win)
          (setf calls '())
          (frame-outline-render-pass)
          (check "window leaves: the empty ring maps again"
                 (list (getf (state) :mapped)
                       (count-if #'third (calls-of :attach)))
                 '(t 1))))

      ;; --- render pass: width 2, empty width 0 ---
      (fresh-world)
      (stub-protocol)
      (configure '(:theme (:frame-outline t :focused-border "#7aa2f7"
                           :frame-outline-empty "#ff9e64"
                           :frame-outline-width 2
                           :frame-outline-empty-width 0)))
      (let ((out (make-instance 'rhizome-output :proxy :fg-out
                                                :x 0 :y 0
                                                :width 800 :height 600)))
        (setf (gethash :fg-out *outputs*) out)
        (check "empty width 0, empty focused frame: no outline target"
               (multiple-value-list (frame-outline-target)) '(nil))
        (frame-outline-render-pass)
        (check "empty width 0, empty focused frame: the ring is never mapped"
               (list (getf (gethash out *frame-outlines*) :mapped)
                     (count-if #'third (calls-of :attach)))
               '(nil 0)))
      (configure '(:theme (:frame-outline-width 2
                           :frame-outline-empty-width 1)))

      (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
      (unless (zerop fail)
        (uiop:quit 1)))))
