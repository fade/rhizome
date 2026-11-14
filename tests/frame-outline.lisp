;;; Tests for the focused-frame outline (outline.lisp): which frame
;;; and output carry the outline, when it hides, the theme settings,
;;; the ring pixels, and the render pass's surface lifecycle. River
;;; requests are stubbed and recorded; the shm buffer path runs for
;;; real.
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
           (sync-focus ()
             ;; current-output reads keyboard focus from the seat
             (let ((seat (first (all-seats))))
               (when seat
                 (setf (seat-focused-window seat)
                       (find-if #'window-focused-p (all-windows))))))
           (render-pass ()
             (sync-focus)
             (frame-outline-render-pass))
           (target ()
             (sync-focus)
             (multiple-value-list (frame-outline-target)))
           (rect ()
             (rest (rest (target))))
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
             (setf (gethash :fo-seat *seats*)
                   (make-instance 'rhizome-seat :proxy :fo-seat))
             (clrhash *frame-outlines*)
             (setf calls '())))
    (macrolet ((check (name actual expected)
                 `(handler-case (ok ,name ,actual ,expected)
                    (error (e)
                      (incf fail)
                      (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

      ;; --- theme settings ---
      (configure '(:theme (:focused-border "#7aa2f7")))
      (check "default: on, focused-border colour, width 2"
             (multiple-value-list (frame-outline-style))
             '(#x7a #xa2 #xf7 2))
      (configure '(:theme (:frame-outline "#00ffff" :frame-outline-width 3)))
      (check "configured colour and width"
             (multiple-value-list (frame-outline-style))
             '(0 255 255 3))
      (check "configure keeps the sibling theme keys"
             (config-getf '(:theme) :focused-border)
             "#7aa2f7")
      (configure '(:theme (:frame-outline nil)))
      (check ":frame-outline nil switches it off"
             (multiple-value-list (frame-outline-style))
             '(nil))
      (configure '(:theme (:frame-outline t :frame-outline-width 0)))
      (check "width 0 switches it off"
             (multiple-value-list (frame-outline-style))
             '(nil))
      (configure '(:theme (:frame-outline t :frame-outline-width 2)))
      (check ":frame-outline t uses the focused-border colour"
             (multiple-value-list (frame-outline-style))
             '(#x7a #xa2 #xf7 2))

      ;; --- which frame and output ---
      (fresh-world)
      (stub-protocol)
      (let* ((out1 (make-instance 'rhizome-output :proxy :fo-out1
                                                  :x 0 :y 0
                                                  :width 1920 :height 1080))
             (out2 (make-instance 'rhizome-output :proxy :fo-out2
                                                  :x 1920 :y 0
                                                  :width 1280 :height 800))
             (t1 (make-instance 'rhizome-window :proxy :fo-t1))
             (t2 (make-instance 'rhizome-window :proxy :fo-t2)))
        (setf (gethash :fo-out1 *outputs*) out1
              (gethash :fo-out2 *outputs*) out2
              (gethash :fo-t1 *windows*) t1
              (gethash :fo-t2 *windows*) t2)
        (let ((head (frame-outline-output)))
          (check "outline output is the output frame commands act on"
                 (eq head (current-output))
                 t)
          (check "empty single frame: outline frames the whole content area"
                 (rect)
                 (list (output-x head) (output-y head)
                       (output-width head) (output-height head)))
          (check "target output is the commands' output"
                 (eq (first (target)) head)
                 t))
        ;; Two frames on out1, t1 left, t2 right, t2 focused
        (dolist (win (list t1 t2))
          (setf (window-output win) out1)
          (assign-window-workspace win 1))
        (setf *outputs* (make-hash-table))
        (setf (gethash :fo-out1 *outputs*) out1
              (gethash :fo-out2 *outputs*) out2)
        (set-workspace-tree out1 1 '((0 0 960 1080) (960 0 960 1080))
                            (list t1 t2))
        (setf (window-focused-p t2) t)
        (check "occupied focused frame: the focused window's frame"
               (rect) '(960 0 960 1080))
        (check "target frame is current-frame-for"
               (eq (second (target)) (current-frame-for out1 1))
               t)
        ;; Frame focus moves left (the fnext/fother analogue)
        (focus-frame-direction out1 1 :left)
        (setf (window-focused-p t2) nil
              (window-focused-p t1) t)
        (check "after frame focus moves: the left frame"
               (rect) '(0 0 960 1080))
        ;; Split the current (left) frame: focus stays on the
        ;; original half; frame focus then moves to the new, empty half
        (setf (window-focused-p t1) nil)
        (split-current-frame out1 1 :vertical)
        (check "after split: the original half"
               (rect) '(0 0 960 540))
        (focus-frame-direction out1 1 :down)
        (check "frame focus down: the new empty frame"
               (rect) '(0 540 960 540))
        (check "the new frame is empty"
               (frame-windows (second (target))) nil)
        ;; Remove that split: the sibling takes the parent's rect
        (remove-current-split out1 1)
        (check "after remove: the merged frame"
               (rect) '(0 0 960 1080))
        ;; Output offset: a frame on the second head
        (dolist (win (list t1 t2))
          (setf (window-output win) out2))
        (setf *outputs* (make-hash-table))
        (setf (gethash :fo-out1 *outputs*) out1
              (gethash :fo-out2 *outputs*) out2)
        (set-workspace-tree out2 1 '((0 0 640 800) (640 0 640 800))
                            (list t1 t2))
        (setf (window-focused-p t2) t)
        (check "focused window on the second head: the outline follows it"
               (eq (first (target)) out2) t)
        (check "frame on a head at x 1920: global coordinates"
               (rect) '(2560 0 640 800))
        ;; Bar reservation shifts the frame down
        (configure '(:bar (:enabled t :height 20 :margin 8)))
        (check "bar reservation offsets y"
               (rect) (list 2560 36 640 800))
        (configure '(:bar (:enabled nil)))
        ;; Fullscreen hides it
        (setf (window-fullscreen-p t2) t)
        (check "fullscreen window on the head hides the outline"
               (target) '(nil))
        (setf (window-fullscreen-p t2) nil)
        ;; A minimized fullscreen window does not
        (setf (window-fullscreen-p t1) t
              (window-minimized-p t1) t)
        (check "minimized fullscreen window does not hide it"
               (not (null (first (target)))) t)
        (setf (window-fullscreen-p t1) nil
              (window-minimized-p t1) nil)
        ;; A fullscreen window on another workspace does not
        (assign-window-workspace t1 2)
        (setf (window-fullscreen-p t1) t)
        (check "fullscreen window on another workspace does not hide it"
               (not (null (first (target)))) t)
        (setf (window-fullscreen-p t1) nil)
        (assign-window-workspace t1 1)
        ;; Scroll ribbon: frames do not apply
        (scroll-enter out2 1)
        (check "scroll workspace hides the outline" (target) '(nil))
        (scroll-exit out2 1)
        (set-workspace-tree out2 1 '((0 0 640 800) (640 0 640 800))
                            (list t1 t2))
        (check "back to frames: outline returns"
               (not (null (first (target)))) t)
        ;; Floating workspace: frames do not apply
        (setf (gethash (frame-key out2 1) *workspace-types*) :floating)
        (check "floating workspace hides the outline" (target) '(nil))
        (remhash (frame-key out2 1) *workspace-types*)
        ;; Switched off
        (configure '(:theme (:frame-outline nil)))
        (check "switched off: no target" (target) '(nil))
        (configure '(:theme (:frame-outline t)))

        ;; --- ring pixels ---
        (let ((state (frame-outline-draw (list) 10 8 2 0 255 255)))
          (flet ((px (x y) (get-pixel-argb (getf state :ptr) 10 8 x y)))
            (check "ring: top-left corner in colour" (px 0 0) #xff00ffff)
            (check "ring: inner edge of the band" (px 1 4) #xff00ffff)
            (check "ring: bottom-right corner" (px 9 7) #xff00ffff)
            (check "ring: just inside the band is clear" (px 2 2) 0)
            (check "ring: centre is clear" (px 5 4) 0))
          ;; Same area, new shape: a fresh buffer with the new stride
          (setf calls '())
          (setf state (frame-outline-draw state 8 10 2 0 255 255))
          (check "reshape at equal area: a new buffer"
                 (length (calls-of :buffer)) 1)
          (check "reshape: the buffer has the new width and stride"
                 (rest (rest (rest (first (calls-of :buffer)))))
                 '(8 10 32 :argb8888))
          (check "reshape: bottom-right corner in colour"
                 (get-pixel-argb (getf state :ptr) 8 10 7 9) #xff00ffff)
          (frame-outline-free state))

        ;; --- render pass ---
        (setf calls '())
        (clrhash *frame-outlines*)
        (setf (window-focused-p t2) t)
        (render-pass)
        (check "a shell surface only for the outlined output"
               (length (calls-of :shell)) 1)
        (check "the surface takes an empty input region"
               (length (calls-of :input-region)) 1)
        (check "one mapped outline: one buffer attached"
               (count-if #'third (calls-of :attach)) 1)
        (check "the outline sits at the bottom of the render list"
               (length (calls-of :bottom)) 1)
        (check "positioned at the focused frame"
               (rest (rest (first (calls-of :position))))
               (subseq (rect) 0 2))
        (check "commits are synced to the render sequence"
               (length (calls-of :sync)) (length (calls-of :commit)))
        (check "only the focused head's outline is mapped"
               (list (getf (gethash out2 *frame-outlines*) :mapped)
                     (gethash out1 *frame-outlines*))
               '(t nil))
        ;; An unchanged pass draws nothing
        (setf calls '())
        (render-pass)
        (check "unchanged pass: no new buffer or commit"
               (list (length (calls-of :buffer)) (length (calls-of :commit)))
               '(0 0))
        ;; Same size, new place: moved, not redrawn
        (setf (window-focused-p t2) nil
              (window-focused-p t1) t)
        (setf calls '())
        (render-pass)
        (check "focus to a same-size frame: moved without a redraw"
               (list (length (calls-of :commit))
                     (rest (rest (first (calls-of :position)))))
               (list 0 (subseq (rect) 0 2)))
        (check "the two frames differ in position"
               (equal (subseq (rect) 0 2)
                      (list (frame-x (window-frame t2))
                            (frame-y (window-frame t2))))
               nil)
        ;; Colour change redraws
        (configure '(:theme (:frame-outline "#ff0000")))
        (setf calls '())
        (render-pass)
        (check "colour change: one redraw"
               (length (calls-of :commit)) 1)
        (check "colour change: same size reuses the buffer"
               (length (calls-of :buffer)) 0)
        ;; Fullscreen unmaps (null buffer attach)
        (setf (window-fullscreen-p t1) t)
        (setf calls '())
        (render-pass)
        (check "fullscreen: outline unmapped with a null buffer"
               (mapcar #'third (calls-of :attach)) '(nil))
        (setf (window-fullscreen-p t1) nil)
        (setf calls '())
        (render-pass)
        (check "fullscreen ends: outline mapped again"
               (count-if #'third (calls-of :attach)) 1)
        ;; Output removal destroys that output's outline
        (remhash :fo-out2 *outputs*)
        (setf calls '())
        (render-pass)
        (check "removed output: its shell surface destroyed"
               (length (calls-of :shell-destroy)) 1)
        (check "removed output: state dropped"
               (gethash out2 *frame-outlines*) nil)
        (check "the outline moves to the remaining output"
               (getf (gethash out1 *frame-outlines*) :mapped) t)))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (uiop:quit 1))))
