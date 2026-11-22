;;; The focus cue (focus-cue.lisp): when (:animations :focus-cue
;;; :enabled) is on and keyboard focus moves to another frame or output,
;;; a ring around the newly focused frame appears fully opaque and
;;; fades out over :duration-ms, then unmaps. A workspace switch, a
;;; split, and a merge show no cue, and a workspace switch ends a cue in
;;; flight. Off by default; reduced motion and (:animations :enabled
;;; nil) turn it off. River requests are stubbed and recorded; the shm
;;; buffer path runs for real, and time comes from a stepped
;;; *animation-clock*.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()) (now 0))
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
           (use (animations)
             (setf *config* (merge-plist (default-config)
                                         (list :bar '(:enabled nil)
                                               :theme '(:frame-outline-width 0
                                                        :frame-outline-empty-width 0)
                                               :animations animations))))
           (at-ms (ms)
             (setf now (* ms (/ internal-time-units-per-second 1000))))
           (focus (win)
             ;; Keyboard focus on WIN alone, as focus-window leaves it
             (dolist (w (all-windows))
               (setf (window-focused-p w) (eq w win)))
             (let ((seat (first (all-seats))))
               (setf (seat-focused-window seat) win)))
           (render ()
             (focus-cue-render-pass))
           (alpha (x y)
             ;; Alpha byte of the cue's pixel at (X Y), or NIL unmapped
             (let ((state *focus-cue-surface*))
               (when (and state (getf state :mapped))
                 (ldb (byte 8 24)
                      (get-pixel-argb (getf state :ptr) (getf state :w)
                                      (getf state :h) x y))))))
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
    (dolist (fn '(message request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf *compositor* :fake-compositor
          *shm* :fake-shm
          *wm-proxy* :fake-wm
          *windows* (make-hash-table)
          *outputs* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *scroll-states* (make-hash-table :test 'equal)
          *seats* (make-hash-table)
          *selected-output* nil
          *animation-clock* (lambda () now))
    (setf (gethash :fc-seat *seats*)
          (make-instance 'rhizome-seat :proxy :fc-seat))
    (let ((out (make-instance 'rhizome-output :proxy :fc-out
                                              :x 0 :y 0
                                              :width 1920 :height 1080))
          (t1 (make-instance 'rhizome-window :proxy :fc-t1))
          (t2 (make-instance 'rhizome-window :proxy :fc-t2)))
      (setf (gethash :fc-out *outputs*) out
            (gethash :fc-t1 *windows*) t1
            (gethash :fc-t2 *windows*) t2)
      (dolist (win (list t1 t2))
        (setf (window-output win) out)
        (assign-window-workspace win 1))
      (set-workspace-tree out 1 '((0 0 960 1080) (960 0 960 1080))
                          (list t1 t2))

      ;; --- off by default ---
      (use '())
      (ok "the default config: the cue is off"
          (getf (getf (default-config) :animations) :focus-cue)
          '(:enabled nil :duration-ms 180 :width 3))
      (focus t1) (at-ms 0) (render)
      (focus t2) (render)
      (ok "off: a frame focus move creates no surface"
          (calls-of :surface) nil)
      (ok "off: nothing to pump" (animations-running-p) nil)

      ;; --- on ---
      (use '(:focus-cue (:enabled t)))
      (setf calls '())
      (render)
      (ok "on, focus unmoved: nothing drawn" (calls-of :commit) nil)
      (focus t1) (at-ms 1000) (render)
      (ok "on, focus moves: one surface, mapped above the windows"
          (list (length (calls-of :surface)) (and (calls-of :top) t))
          '(1 t))
      (ok "the cue frames the newly focused frame"
          (list (getf *focus-cue-surface* :position)
                (getf *focus-cue-surface* :w)
                (getf *focus-cue-surface* :h))
          '((0 0) 960 1080))
      (ok "at the start the ring is at full alpha" (alpha 0 0) 255)
      (ok "the ring is 3 px wide, clear inside"
          (list (alpha 2 2) (alpha 3 3)) '(255 0))
      (ok "while fading, the pump runs" (animations-running-p) t)
      (at-ms 1090) (render)
      (let ((mid (alpha 0 0)))
        (ok "halfway, the alpha has decayed to about half"
            (and mid (< 100 mid 155)) t))
      (at-ms 1170) (render)
      (ok "near the end, the alpha is low"
          (let ((a (alpha 0 0))) (and a (< 0 a 30))) t)
      (at-ms 1180) (render)
      (ok "at the duration, the ring unmaps"
          (list (alpha 0 0) (first (last (first (calls-of :attach)) 3)))
          '(nil nil))
      (ok "after the fade, nothing to pump" (animations-running-p) nil)
      (setf calls '())
      (at-ms 1500) (render)
      (ok "after the fade, no redraw" (calls-of :commit) nil)

      ;; --- configured width and duration ---
      (use '(:focus-cue (:enabled t :duration-ms 400 :width 5)))
      (focus t2) (at-ms 2000) (render)
      (ok ":width 5 draws a 5 px ring" (list (alpha 4 4) (alpha 5 5)) '(255 0))
      (at-ms 2300) (render)
      (ok ":duration-ms 400: still showing at 300 ms"
          (and (alpha 0 0) t) t)
      (at-ms 2400) (render)
      (ok ":duration-ms 400: gone at 400 ms" (alpha 0 0) nil)

      ;; --- a split or a merge keeps focus in place: no cue ---
      (use '(:focus-cue (:enabled t)))
      (at-ms 2500) (render)
      (split-current-frame out 1 :vertical)
      (render)
      (ok "a split: no cue" (alpha 0 0) nil)
      (remove-current-split out 1)
      (render)
      (ok "removing the split: no cue" (alpha 0 0) nil)

      ;; --- switched off by the global switches ---
      (use '(:focus-cue (:enabled t) :reduced-motion t))
      (setf calls '())
      (focus t1) (at-ms 3000) (render)
      (ok "reduced motion: no cue" (list (alpha 0 0) (calls-of :commit))
          '(nil nil))
      (use '(:focus-cue (:enabled t) :enabled nil))
      (focus t2) (at-ms 4000) (render)
      (ok ":enabled nil: no cue" (alpha 0 0) nil)

      ;; --- turned off mid-fade ---
      (use '(:focus-cue (:enabled t)))
      (at-ms 4500) (render)
      (ok "switched back on: the first pass only records focus"
          (alpha 0 0) nil)
      (focus t1) (at-ms 5000) (render)
      (ok "a fresh move cues again" (alpha 0 0) 255)
      (use '(:focus-cue (:enabled nil)))
      (at-ms 5050) (render)
      (ok "turned off mid-fade: the ring unmaps at once"
          (list (alpha 0 0) (animations-running-p)) '(nil nil))

      ;; --- a workspace switch shows no cue; frame and output moves do ---
      (let ((out2 (make-instance 'rhizome-output :proxy :fc-out2
                                                 :x 1920 :y 0
                                                 :width 1920 :height 1080))
            (t3 (make-instance 'rhizome-window :proxy :fc-t3))
            (t4 (make-instance 'rhizome-window :proxy :fc-t4))
            (t5 (make-instance 'rhizome-window :proxy :fc-t5)))
        (flet ((cue-at ()
                 ;; Where the ring is mapped and its corner alpha
                 (list (getf *focus-cue-surface* :position) (alpha 0 0))))
          (setf (gethash :fc-out2 *outputs*) out2
                (gethash :fc-t3 *windows*) t3
                (gethash :fc-t4 *windows*) t4
                (gethash :fc-t5 *windows*) t5
                (window-output t3) out2)
          (assign-window-workspace t3 1)
          (set-workspace-tree out2 1 '((0 0 1920 1080)) (list t3))
          (dolist (win (list t4 t5))
            (setf (window-output win) out)
            (assign-window-workspace win 2))
          (set-workspace-tree out 2 '((0 0 960 1080) (960 0 960 1080))
                              (list t4 t5))
          (use '(:focus-cue (:enabled t)))
          (focus t1) (at-ms 6000) (render)
          (switch-workspace 2 out) (at-ms 7000) (render)
          (ok "a workspace switch: no cue"
              (list (alpha 0 0) *focus-cue*) '(nil nil))
          (focus t4) (at-ms 8000) (render)
          (ok "another frame on the same workspace: the cue fires"
              (cue-at) '((0 0) 255))
          (at-ms 8050) (switch-workspace 1 out) (render)
          (ok "a workspace switch mid-fade ends the cue"
              (list (alpha 0 0) *focus-cue*) '(nil nil))
          (focus t3) (at-ms 9000) (render)
          (ok "another output, same workspace number: the cue fires"
              (cue-at) '((1920 0) 255))
          (focus t1) (at-ms 10000) (render)
          (ok "back to the first output: the cue fires"
              (cue-at) '((0 0) 255))
          (at-ms 11000) (render)
          (switch-workspace 2 out) (at-ms 12000) (render)
          (ok "a second workspace switch: no cue"
              (list (alpha 0 0) *focus-cue*) '(nil nil))
          (focus t3) (at-ms 13000) (render)
          (ok "another output on a different workspace number: the cue fires"
              (cue-at) '((1920 0) 255))))))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
