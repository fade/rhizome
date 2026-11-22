;;; Workspace slide duration: check that a workspace switch slides for
;;; (:animations :workspace-duration-ms), 160 ms when unset, and that
;;; :duration-ms, which sets other glides, leaves the slide alone.
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
    (dolist (fn '(message request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (labels ((slide-ms (animations)
               ;; Switch an output from workspace 1 to 2 under the
               ;; :animations section ANIMATIONS; return the slide's
               ;; duration in milliseconds, or NIL when none started.
               (configure (list :animations animations))
               (let* ((*animations* (make-hash-table))
                      (*windows* (make-hash-table))
                      (*output-workspace* (make-hash-table))
                      (*window-workspace* (make-hash-table))
                      (out (make-instance 'rhizome-output :proxy :slide-out
                                          :width 1920 :height 1080))
                      (win (make-instance 'rhizome-window
                                          :proxy :slide-win)))
                 (setf (gethash :slide-win *windows*) win
                       (window-output win) out
                       (gethash out *output-workspace*) 1)
                 (assign-window-workspace win 2)
                 (switch-workspace 2 out)
                 (let ((anim (gethash win *animations*)))
                   (and anim
                        (/ (getf anim :duration)
                           (/ internal-time-units-per-second 1000)))))))
      (ok "an unset slide duration slides for 160 ms"
          (slide-ms '(:enabled t)) 160)
      (ok "the glide duration leaves the slide at 160 ms"
          (slide-ms '(:enabled t :duration-ms 500)) 160)
      (ok ":workspace-duration-ms sets the slide duration"
          (slide-ms '(:enabled t :workspace-duration-ms 300)) 300)
      (ok "a zero slide duration switches without a slide"
          (slide-ms '(:enabled t :workspace-duration-ms 0)) nil)
      (ok "disabled animations switch without a slide"
          (slide-ms '(:enabled nil :workspace-duration-ms 300)) nil)
      (ok "the default config slides for 160 ms"
          (getf (getf (default-config) :animations)
                :workspace-duration-ms :absent)
          160)))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
