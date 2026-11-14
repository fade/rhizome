;;; Tests for the mouse focus policy: :click (the default) ignores
;;; pointer_enter, :sloppy gives the entered window keyboard focus
;;; without raising it, leaving a window for empty space keeps focus,
;;; a pointer that has not moved never takes focus back from a key
;;; or command, modal interactions suppress it, and a click still
;;; focuses and raises.
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

    ;; River-touching stubs
    (dolist (name '(message request-manage-dirty propose-window-dimensions
                    set-window-position set-window-borders show-window
                    hide-window))
      (setf (symbol-function name)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'make-window-node)
          (lambda (win)
            (unless (window-node win)
              (setf (window-node win) (list :node (window-proxy win))))))
    (setf (symbol-function 'place-node-above)
          (lambda (node below) (declare (ignore node below)) nil))

    (let* ((seat (make-instance 'rhizome-seat :proxy :mf-seat))
           (out (make-instance 'rhizome-output :proxy :mf-out
                               :width 1920 :height 1080))
           (a (make-instance 'rhizome-window :proxy :mf-a))
           (b (make-instance 'rhizome-window :proxy :mf-b))
           (c (make-instance 'rhizome-window :proxy :mf-c))
           (f1 (make-instance 'rhizome-window :proxy :mf-f1))
           (f2 (make-instance 'rhizome-window :proxy :mf-f2))
           (px 100) (py 100))
      (labels ((reset-world ()
                 (setf *seats* (make-hash-table)
                       (gethash :mf-seat *seats*) seat
                       *windows* (make-hash-table)
                       *output-workspace* (make-hash-table)
                       *window-workspace* (make-hash-table)
                       *minimized-windows* (make-hash-table)
                       *pending-focus-requests* nil
                       (gethash out *output-workspace*) 1)
                 (when (boundp '*pointer-focus-state*)
                   (clrhash (symbol-value '*pointer-focus-state*)))
                 (dolist (win (list a b c f1 f2))
                   (setf (gethash (window-proxy win) *windows*) win
                         (window-output win) out
                         (window-focused-p win) nil
                         (window-hidden-p win) nil
                         (window-width win) 400
                         (window-height win) 300)
                   (assign-window-workspace win 1))
                 (dolist (win (list f1 f2))
                   (setf (window-floating-p win) t
                         (window-float-x win) 200
                         (window-float-y win) 200
                         (window-float-width win) 400
                         (window-float-height win) 300))
                 (setf (seat-focused-window seat) nil
                       (seat-op-state seat) nil
                       (seat-op-pending seat) nil)
                 (focus-window a seat)
                 ;; one quiet sequence settles the bookkeeping
                 (manage))
               (policy (p)
                 (configure (list :pointer (list :focus-policy p))))
               (event (name &rest args)
                 (apply #'handle-seat-event seat name args))
               (manage ()
                 (handler-case (funcall 'pointer-focus-manage-pass)
                   (error (e) (format t "~&; manage pass: ~A~%" e))))
               (move ()
                 (incf px 7)
                 (event :pointer-position px py))
               (enter (win) (event :pointer-enter (window-proxy win)))
               (focused () (seat-focused-window seat))
               (top-float ()
                 (first (last (float-render-decisions out nil))))
               (float-order ()
                 (float-render-decisions out nil)))

        ;; --- Configuration ---

        (ok "the policy defaults to :click"
            (handler-case (funcall 'mouse-focus-policy)
              (error () :undefined))
            :click)

        ;; --- :click ignores enter ---

        (policy :click)
        (reset-world)
        (enter b) (move) (manage)
        (ok ":click: entering a window leaves focus alone"
            (focused) a)

        ;; --- :sloppy enter focuses ---

        (policy :sloppy)
        (reset-world)
        (enter b) (move) (manage)
        (ok ":sloppy: entering a window with motion focuses it"
            (focused) b)
        (ok ":sloppy: the focus request reaches river"
            (car (first *pending-focus-requests*)) b)

        ;; --- No raise, no stacking change ---

        (reset-world)
        (event :window-interaction (window-proxy f1))
        (manage)
        (let ((before (float-order)))
          (enter f2) (move) (manage)
          (ok ":sloppy: entering a float focuses it" (focused) f2)
          (ok ":sloppy: the clicked float stays on top" (top-float) f1)
          (ok ":sloppy: float stacking order is unchanged"
              (float-order) before))

        ;; --- Leaving for empty space keeps focus ---

        (reset-world)
        (enter b) (move) (manage)
        (event :pointer-leave) (move) (manage)
        (ok ":sloppy: leaving for empty space keeps focus" (focused) b)
        (move) (manage)
        (ok ":sloppy: moving over empty space keeps focus" (focused) b)

        ;; --- Key focus is not undone by an enter without motion ---

        (reset-world)
        (enter b) (move) (manage)
        ;; A key moves focus; the same sequence reports motion that
        ;; may predate the key press
        (move)
        (focus-window c seat)
        (manage)
        (ok "key focus survives the sequence that carried it" (focused) c)
        (manage)
        (ok "key focus survives a sequence without motion" (focused) c)
        ;; A layout change slides a under the still pointer
        (enter a) (manage)
        (ok "an enter without motion does not take focus" (focused) c)
        (move) (manage)
        (ok "motion afterwards hands focus to the window under the pointer"
            (focused) a)

        ;; A window opening under a still pointer
        (reset-world)
        (enter b) (move) (manage)
        (enter c) (manage)
        (ok "a window mapping under a still pointer does not take focus"
            (focused) b)

        ;; --- Expose, prompt, prefix and float ops suppress ---

        (dolist (spec '((*expose-active* "the expose overview")
                        (*prompt-active* "a prompt")
                        (*menu-active* "a menu")
                        (*prefix-mode-active* "prefix-key capture")
                        (*iresize-active* "interactive frame resize")))
          (reset-world)
          (setf (symbol-value (first spec)) t)
          (unwind-protect
               (progn (enter b) (move) (manage))
            (setf (symbol-value (first spec)) nil))
          (ok (format nil "~A suppresses pointer focus" (second spec))
              (focused) a))

        (reset-world)
        (setf (seat-op-state seat) (list :win f1 :type :move))
        (enter b) (move) (manage)
        (setf (seat-op-state seat) nil)
        (ok "an interactive float move suppresses pointer focus"
            (focused) a)

        ;; --- A click still focuses and raises ---

        (dolist (p '(:sloppy :click))
          (policy p)
          (reset-world)
          (event :window-interaction (window-proxy f1))
          (manage)
          (event :window-interaction (window-proxy f2))
          (manage)
          (ok (format nil "~(~A~): a click focuses" p) (focused) f2)
          (ok (format nil "~(~A~): a click raises" p) (top-float) f2)
          ;; With no window focused, floats fall back to creation order
          (event :window-interaction (window-proxy f1))
          (manage)
          (setf (window-focused-p f1) nil
                (seat-focused-window seat) nil)
          (ok (format nil "~(~A~): clearing focus drops the raised float" p)
              (top-float) f2))

        ;; --- Multi-head: the entered window's output becomes current ---

        (policy :sloppy)
        (reset-world)
        (let ((saved *outputs*)
              (right (make-instance 'rhizome-output :proxy :mf-out2
                                    :x 1920 :width 1920 :height 1080)))
          (unwind-protect
               (progn
                 (setf *outputs* (make-hash-table)
                       (gethash :mf-out *outputs*) out
                       (gethash :mf-out2 *outputs*) right
                       (gethash right *output-workspace*) 2
                       (window-output c) right)
                 (assign-window-workspace c 2)
                 (ok "multi-head: the focused output is current at first"
                     (current-output) out)
                 (enter c) (move) (manage)
                 (ok "multi-head: entering a window on another output focuses it"
                     (focused) c)
                 (ok "multi-head: that output becomes current"
                     (current-output) right))
            (setf *outputs* saved)))

        (policy :click)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
