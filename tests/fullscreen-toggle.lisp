;;; Tests for fullscreen toggling: at most one fullscreen window per
;;; output and workspace, a fullscreen window's decoration left
;;; alone while it covers the output, and a decoration redraw cheap
;;; enough for a render sequence. River requests are stubbed and
;;; recorded, so no compositor is needed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun reference-shadow-fill (ptr width height radius alpha-0 cr cg cb)
  "Shadow fill computed independently for every pixel: the
expected buffer for DECORATION-FILL."
  (let* ((left radius) (right (- width radius))
         (top radius) (bottom (- height radius)))
    (dotimes (py height)
      (dotimes (px width)
        (let* ((dx (max 0 (cond ((< px left) (- left px))
                                ((> px right) (- px right))
                                (t 0))))
               (dy (max 0 (cond ((< py top) (- top py))
                                ((> py bottom) (- py bottom))
                                (t 0))))
               (dist (sqrt (+ (* dx dx) (* dy dy))))
               (value
                 (if (>= dist radius)
                     0
                     (let* ((fall (if (zerop radius) 0.0 (- 1.0 (/ dist radius))))
                            (a (round (* alpha-0 (* fall fall))))
                            (sr (min 255 (round (* cr a) 255)))
                            (sg (min 255 (round (* cg a) 255)))
                            (sb (min 255 (round (* cb a) 255))))
                       (logior (ash a 24) (ash sr 16) (ash sg 8) sb)))))
          (setf (cffi:mem-aref ptr :unsigned-int (+ (* width py) px)) value))))))

(let ((pass 0) (fail 0) (sent '()) (draws 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (flush ()
           (setf sent '())
           (flush-fullscreen-requests)
           (reverse sent)))

    ;; Record river requests instead of sending them
    (setf (symbol-function 'request-manage-dirty) (lambda () nil))
    (dolist (spec '((rwm:river-window-v1.fullscreen . :fullscreen)
                    (rwm:river-window-v1.exit-fullscreen . :exit-fullscreen)
                    (rwm:river-window-v1.inform-fullscreen . :inform-fullscreen)
                    (rwm:river-window-v1.inform-not-fullscreen
                     . :inform-not-fullscreen)))
      (let ((tag (cdr spec)))
        (setf (symbol-function (car spec))
              (lambda (&rest args) (push (cons tag args) sent) nil))))

    ;; Two outputs; three windows on the left output's workspace 1,
    ;; one on its workspace 2, one on the right output's workspace 1
    (setf *windows* (make-hash-table)
          *outputs* (make-hash-table)
          *window-workspace* (make-hash-table)
          *pending-fullscreen-requests* nil)
    (let* ((left (make-instance 'rhizome-output :proxy :left-proxy
                                :width 1920 :height 1080))
           (right (make-instance 'rhizome-output :proxy :right-proxy
                                 :x 1920 :width 2560 :height 1440))
           (wins '()))
      (setf (gethash :left-proxy *outputs*) left
            (gethash :right-proxy *outputs*) right)
      (flet ((add (proxy output workspace)
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) output
                       (gethash win *window-workspace*) workspace)
                 (push win wins)
                 win)))
        (let ((emacs (add :emacs-proxy left 1))
              (konsole (add :konsole-proxy left 1))
              (kitty (add :kitty-proxy left 1))
              (hidden (add :hidden-proxy left 2))
              (other (add :other-proxy right 1)))

          ;; --- a new fullscreen request takes over the output ---

          (handle-window-event konsole :fullscreen-requested nil)
          (handle-window-event hidden :fullscreen-requested nil)
          (handle-window-event other :fullscreen-requested nil)
          (flush)
          (handle-window-event emacs :fullscreen-requested nil)
          (let ((reqs (flush)))
            (ok "the requesting window goes fullscreen"
                (and (member '(:fullscreen :emacs-proxy :left-proxy) reqs
                             :test #'equal)
                     (member '(:inform-fullscreen :emacs-proxy) reqs
                             :test #'equal)
                     t)
                t)
            (ok "the window it displaces exits fullscreen and is told"
                (and (member '(:exit-fullscreen :konsole-proxy) reqs
                             :test #'equal)
                     (member '(:inform-not-fullscreen :konsole-proxy) reqs
                             :test #'equal)
                     t)
                t)
            (ok "nothing is sent for windows on another workspace or output"
                (remove-if-not (lambda (r)
                                 (member (second r)
                                         '(:hidden-proxy :other-proxy)))
                               reqs)
                nil))
          (ok "only the new window holds the fullscreen flag on that workspace"
              (list (window-fullscreen-p emacs) (window-fullscreen-p konsole)
                    (window-fullscreen-p kitty))
              '(t nil nil))
          (ok "fullscreen on another workspace and output is kept"
              (list (window-fullscreen-p hidden) (window-fullscreen-p other))
              '(t t))

          ;; --- the same holds for C-t F11 ---

          (toggle-fullscreen kitty)
          (let ((reqs (flush)))
            (ok "C-t F11 takes over from the fullscreen window"
                (list (window-fullscreen-p kitty) (window-fullscreen-p emacs)
                      (and (member '(:exit-fullscreen :emacs-proxy) reqs
                                   :test #'equal)
                           t))
                '(t nil t)))

          ;; --- two requests in one manage pass: the newest wins ---

          (toggle-fullscreen kitty)
          (flush)
          (handle-window-event konsole :fullscreen-requested nil)
          (handle-window-event emacs :fullscreen-requested nil)
          (flush)
          (ok "two requests in one pass leave one fullscreen window"
              (count-if #'window-fullscreen-p
                        (list emacs konsole kitty))
              1)
          (ok "the newest request wins"
              (window-fullscreen-p emacs) t)

          ;; --- repeating a request changes nothing ---

          (handle-window-event emacs :fullscreen-requested nil)
          (flush)
          (ok "a repeated request keeps the window fullscreen"
              (window-fullscreen-p emacs) t)
          (handle-window-event emacs :exit-fullscreen-requested)
          (ok "exit after a repeat sends one exit and one inform"
              (flush)
              '((:exit-fullscreen :emacs-proxy)
                (:inform-not-fullscreen :emacs-proxy)))
          (ok "exit leaves no window fullscreen on the workspace"
              (count-if #'window-fullscreen-p (list emacs konsole kitty))
              0)))

      ;; --- decorations: a fullscreen window keeps its decoration ---

      (clrhash *decorations*)
      (setf (symbol-function 'wl:wl-compositor.create-surface)
            (lambda (&rest a) (declare (ignore a)) :fake-surface)
            (symbol-function 'rwm:river-window-v1.get-decoration-below)
            (lambda (&rest a) (declare (ignore a)) :fake-deco)
            (symbol-function 'rwm:river-decoration-v1.set-offset)
            (lambda (&rest a) (declare (ignore a)) nil)
            (symbol-function 'rwm:river-decoration-v1.sync-next-commit)
            (lambda (&rest a) (declare (ignore a)) nil)
            (symbol-function 'wl:wl-surface.attach)
            (lambda (&rest a) (declare (ignore a)) nil)
            (symbol-function 'wl:wl-surface.damage)
            (lambda (&rest a) (declare (ignore a)) nil)
            (symbol-function 'wl:wl-surface.commit)
            (lambda (&rest a) (declare (ignore a)) nil)
            (symbol-function 'wl:wl-buffer.destroy)
            (lambda (&rest a) (declare (ignore a)) nil)
            (symbol-function 'wl:wl-shm-pool.destroy)
            (lambda (&rest a) (declare (ignore a)) nil)
            (symbol-function 'wl:wl-shm.create-pool)
            (lambda (&rest a) (declare (ignore a)) :fake-pool)
            (symbol-function 'wl:wl-shm-pool.create-buffer)
            (lambda (&rest a) (declare (ignore a)) :fake-buffer)
            *compositor* :fake-compositor
            *shm* :fake-shm)
      (let ((real-draw (symbol-function 'decoration-draw)))
        (setf (symbol-function 'decoration-draw)
              (lambda (&rest args) (incf draws) (apply real-draw args))))
      (configure '(:decorations (:enabled t :radius 12)))
      (setf *windows* (make-hash-table))
      (let ((emacs (make-instance 'rhizome-window :proxy :emacs-proxy)))
        (setf (gethash :emacs-proxy *windows*) emacs
              (window-focused-p emacs) t
              (window-width emacs) 960
              (window-height emacs) 1040)
        (decorations-render-pass)
        (ok "a focused tiled window gets its glow drawn once" draws 1)
        (setf (window-fullscreen-p emacs) t
              (window-width emacs) 1920
              (window-height emacs) 1080)
        (decorations-render-pass)
        (ok "entering fullscreen does not redraw the decoration" draws 1)
        (ok "the decoration survives fullscreen"
            (and (gethash emacs *decorations*) t) t)
        (setf (window-fullscreen-p emacs) nil
              (window-width emacs) 960
              (window-height emacs) 1040)
        (decorations-render-pass)
        (ok "leaving fullscreen at the old size does not redraw it" draws 1))

      ;; --- the shadow fill: same pixels, render-sequence cost ---

      (flet ((fill-matches-reference (width height radius)
               (let ((got (cffi:foreign-alloc :unsigned-int
                                              :count (* width height)))
                     (want (cffi:foreign-alloc :unsigned-int
                                               :count (* width height))))
                 (unwind-protect
                      (progn
                        (decoration-fill got width height radius 153 76 81 123)
                        (reference-shadow-fill want width height radius
                                               153 76 81 123)
                        (loop for i below (* width height)
                              always (= (cffi:mem-aref got :unsigned-int i)
                                        (cffi:mem-aref want :unsigned-int i))))
                   (cffi:foreign-free got)
                   (cffi:foreign-free want)))))
        (dolist (case '((46 36 8) (220 120 10) (31 25 12) (10 10 6) (24 24 12)
                        (17 9 0)))
          (ok (format nil "shadow fill matches every pixel at ~{~D~^x~} radius ~D"
                      (butlast case) (third case))
              (handler-case (apply #'fill-matches-reference case)
                (error (e) (format nil "~A" e)))
              t)))
      (let* ((width (+ 1920 12)) (height (+ 1080 12))
             (state (list :ptr (cffi:foreign-alloc :unsigned-int
                                                   :count (* width height))
                          :focused t))
             (best nil))
        (unwind-protect
             (dotimes (i 3)
               (let ((t0 (get-internal-real-time)))
                 (decoration-draw state width height 6 0.3 0.3 122 162 247)
                 (let ((ms (/ (- (get-internal-real-time) t0)
                              (/ internal-time-units-per-second 1000))))
                   (setf best (if best (min best ms) ms)))))
          (cffi:foreign-free (getf state :ptr)))
        (format t "~&; glow for a 1920x1080 window: ~,1F ms~%" best)
        (ok "a 1920x1080 glow draws in under 50 ms" (< best 50) t))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
