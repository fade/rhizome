;;; Per-kind animation switches (animate.lisp): the workspace slide,
;;; expose glides, and floating-window glides each resolve their own
;;; :enabled and :duration-ms under :animations, falling back to the
;;; global keys. A config that sets none of them behaves as before:
;;; every kind on, the slide 160 ms, the other glides 200 ms.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (use (animations)
             ;; The default config with the :animations section
             ;; ANIMATIONS merged in, as configure would merge it
             (setf *config* (merge-plist (default-config)
                                         (list :bar '(:enabled nil)
                                               :animations animations))))
           (ms (anim)
             (and anim
                  (/ (getf anim :duration)
                     (/ internal-time-units-per-second 1000))))
           (resolved ()
             (mapcar (lambda (kind)
                       (list kind (animation-kind-enabled-p kind)
                             (animation-kind-duration kind)))
                     '(:workspace :expose :float))))
    (dolist (fn '(message request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))

    ;; --- resolution ---

    (use '())
    (ok "unset config: every kind on, slide 160 ms, glides 200 ms"
        (resolved)
        '((:workspace t 160) (:expose t 200) (:float t 200)))
    (use '(:duration-ms 300))
    (ok ":duration-ms sets expose and float glides, not the slide"
        (resolved)
        '((:workspace t 160) (:expose t 300) (:float t 300)))
    (use '(:workspace-duration-ms 250))
    (ok ":workspace-duration-ms sets the slide"
        (resolved)
        '((:workspace t 250) (:expose t 200) (:float t 200)))
    (use '(:duration-ms 300 :workspace-duration-ms 250
           :expose (:duration-ms 90) :workspace (:duration-ms 120)))
    (ok "a kind's own :duration-ms wins over the global key"
        (resolved)
        '((:workspace t 120) (:expose t 90) (:float t 300)))
    (use '(:float (:enabled nil)))
    (ok "(:float (:enabled nil)) turns float glides off only"
        (mapcar #'second (resolved))
        '(t t nil))
    (use '(:workspace nil))
    (ok "(:workspace nil) turns the slide off"
        (mapcar #'second (resolved))
        '(nil t t))
    (use '(:enabled nil :expose (:enabled t)))
    (ok ":enabled nil turns every kind off"
        (mapcar #'second (resolved))
        '(nil nil nil))
    (use '(:expose (:duration-ms -5)))
    (ok "a negative duration resolves to 0"
        (animation-kind-duration :expose) 0)
    (ok "the default config names each kind, enabled, with no duration"
        (let ((animations (getf (default-config) :animations)))
          (mapcar (lambda (kind) (getf animations kind)) '(:workspace :expose :float)))
        '((:enabled t) (:enabled t) (:enabled t)))

    ;; --- animation-start ---

    (let ((*animations* (make-hash-table))
          (win (make-instance 'rhizome-window :proxy :asw-start)))
      (setf (window-x win) 0 (window-y win) 0)
      (use '(:float (:enabled nil)))
      (animation-start win :kind :float :from-x 5)
      (ok "animation-start of a kind that is off starts nothing"
          (animation-active-p win) nil)
      (animation-start win :kind :expose :from-x 5)
      (let ((anim (gethash win *animations*)))
        (ok "animation-start records its kind and the kind's duration"
            (list (getf anim :kind) (ms anim))
            '(:expose 200))))

    ;; --- call sites ---

    (labels ((slide (animations)
               ;; Switch an output from workspace 1 to 2; return the
               ;; window's animation plist, or NIL
               (use animations)
               (let ((*animations* (make-hash-table))
                     (*windows* (make-hash-table))
                     (*output-workspace* (make-hash-table))
                     (*window-workspace* (make-hash-table))
                     (out (make-instance 'rhizome-output :proxy :asw-out
                                         :width 1920 :height 1080))
                     (win (make-instance 'rhizome-window
                                         :proxy :asw-win)))
                 (setf (gethash :asw-win *windows*) win
                       (window-output win) out
                       (gethash out *output-workspace*) 1)
                 (assign-window-workspace win 2)
                 (switch-workspace 2 out)
                 (gethash win *animations*)))
             (float-move (animations)
               ;; Move a floating window to an explicit position
               (use animations)
               (let ((*animations* (make-hash-table))
                     (win (make-instance 'rhizome-window :proxy :asw-float)))
                 (setf (window-x win) 0 (window-y win) 0
                       (window-floating-p win) t)
                 (float-window win :x 40 :y 50)
                 (gethash win *animations*)))
             (expose-back (animations)
               ;; Restore one floating window from expose
               (use animations)
               (let* ((*animations* (make-hash-table))
                      (win (make-instance 'rhizome-window :proxy :asw-exp))
                      (*expose-saved* (list (list (list win nil t 10 10 100 100))
                                            nil))
                      (*expose-prev-focus* nil))
                 (setf (window-x win) 0 (window-y win) 0)
                 (expose-restore)
                 (gethash win *animations*))))
      (let ((anim (slide '())))
        (ok "workspace switch: a :workspace slide of 160 ms"
            (list (getf anim :kind) (ms anim)) '(:workspace 160)))
      (ok "workspace switch with the slide off: no slide"
          (slide '(:workspace (:enabled nil))) nil)
      (let ((anim (float-move '())))
        (ok "float move: a :float glide of 200 ms"
            (list (getf anim :kind) (ms anim)) '(:float 200)))
      (ok "float move with float glides off: no glide"
          (float-move '(:float (:enabled nil))) nil)
      (ok "float move with only expose off: still glides"
          (and (float-move '(:expose (:enabled nil))) t) t)
      (let ((anim (expose-back '(:expose (:duration-ms 120)))))
        (ok "expose restore: an :expose glide at its own duration"
            (list (getf anim :kind) (ms anim)) '(:expose 120)))
      (ok "expose restore with expose glides off: no glide"
          (expose-back '(:expose (:enabled nil))) nil))

    ;; --- the run-time toggle ---

    (let ((*desktop-reduced-motion* nil))
      (use '(:duration-ms 90 :float (:enabled nil)))
      (ok "toggle-animations: on to off"
          (list (execute-command nil :toggle-animations nil)
                (animations-enabled-p))
          '("animations off" nil))
      (ok "toggle-animations: off to on"
          (list (execute-command nil :toggle-animations nil)
                (animations-enabled-p))
          '("animations on" t))
      (ok "toggle-animations keeps the other :animations keys"
          (list (animation-duration) (animation-kind-enabled-p :float))
          '(90 nil))
      (use '(:reduced-motion t))
      (execute-command nil :toggle-animations nil)
      (ok "toggle-animations under reduced motion says so"
          (execute-command nil :toggle-animations nil)
          "animations on (reduced motion in effect)")
      (ok "toggle-animations has command help"
          (and (gethash :toggle-animations *command-docs*) t) t)))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
