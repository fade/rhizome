;;; Tests for the output keyboard commands and new windows act on: the
;;; output holding keyboard focus, including an empty focused frame,
;;; whatever the pointer rests on. Two fake outputs side by side and one
;;; seat; river requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; The default C-' bindings, as at startup
(setup-default-prefix-bindings)

;; Never start the bar from a test run; keep geometry exact
(configure '(:bar (:enabled nil)
             :animations (:enabled nil)
             :pointer (:warp-on-switch nil)
             :frames (:focus-empty t)
             :tiling (:auto-split nil)))

(let ((pass 0) (fail 0))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (try (name thunk expected)
             ;; An error is a failure, not a crash: every case reports
             (ok name
                 (handler-case (funcall thunk)
                   (error (e) (list :error (princ-to-string e))))
                 expected)))

    ;; Record nothing, touch no river object
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-borders show-window
                  hide-window message show-message message-hide
                  request-manage-dirty recommend-dimension-bounds
                  set-window-clip-box set-window-position
                  open-prefix-capture-surface))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    ;; The capture surface opens without river
    (setf (symbol-function 'open-prefix-capture-surface)
          (lambda () (setf *prefix-mode-active* t))
          (symbol-function 'prefix-capture-available-p)
          (lambda () t))

    (let (samsung apple seat)
      (labels ((reset ()
                 (setf *windows* (make-hash-table)
                       *outputs* (make-hash-table)
                       *seats* (make-hash-table)
                       *output-workspace* (make-hash-table)
                       *window-workspace* (make-hash-table)
                       *minimized-windows* (make-hash-table)
                       *frame-trees* (make-hash-table :test 'equal)
                       *current-frames* (make-hash-table :test 'equal)
                       *pending-focus-requests* nil
                       *pending-autofocus* nil
                       *last-registered-window* nil
                       *selected-output* nil
                       *prefix-mode-active* nil
                       *prefix-state* nil)
                 ;; The Samsung registers first, so it is also the
                 ;; first-output fallback; the operator works on the
                 ;; Apple to its right
                 (setf samsung (make-instance 'rhizome-output
                                              :proxy :samsung :x 0 :y 0
                                              :width 1920 :height 1080)
                       apple (make-instance 'rhizome-output
                                            :proxy :apple :x 1920 :y 0
                                            :width 2560 :height 1440)
                       seat (make-instance 'rhizome-seat :proxy :seat)
                       (gethash :samsung *outputs*) samsung
                       (gethash :apple *outputs*) apple
                       (gethash :seat *seats*) seat
                       (gethash samsung *output-workspace*) 1
                       (gethash apple *output-workspace*) 1))
               (manage-pass ()
                 (maphash (lambda (proxy out)
                            (declare (ignore proxy))
                            (apply-layout-for-output out))
                          *outputs*))
               (open-on (proxy output)
                 ;; A window that opens on OUTPUT and takes focus
                 (let ((win (make-instance 'rhizome-window :proxy proxy
                                           :app-id (string proxy))))
                   (setf (gethash proxy *windows*) win
                         (window-output win) output)
                   (assign-window-workspace win 1)
                   (manage-pass)
                   (focus-window win seat)
                   win))
               (pointer-at (output)
                 (setf (seat-pointer-x seat) (+ (output-x output) 100)
                       (seat-pointer-y seat) 100))
               (empty-leaf (output)
                 (find-if-not #'frame-windows
                              (frame-leaves (frame-tree-for output 1))))
               (scene ()
                 ;; The Apple: a user split whose second frame is empty,
                 ;; and two windows in the first, one hidden. The
                 ;; Samsung: a browser over a hidden window. Keyboard
                 ;; focus ends on the Apple's visible window; the
                 ;; pointer rests on the Samsung.
                 (reset)
                 (open-on :hidden-b samsung)
                 (open-on :browser samsung)
                 (let ((term (open-on :term apple)))
                   (split-horizontal apple)
                   (open-on :hidden-a apple)
                   (focus-window term seat)
                   (pointer-at samsung)
                   term))
               (focus-empty-frame ()
                 (focus-frame apple 1 (empty-leaf apple)))
               (name (output)
                 (cond ((eq output apple) :apple)
                       ((eq output samsung) :samsung)
                       (t output))))

        ;; --- The scene itself ---

        (scene)
        (try "the scene has an empty frame on the Apple"
             (lambda () (and (empty-leaf apple) t))
             t)
        (try "with a window focused on the Apple, the Apple is current"
             (lambda () (name (current-output)))
             :apple)

        ;; --- An empty focused frame ---

        (scene)
        (focus-empty-frame)
        (try "an empty focused frame keeps its output current over the pointer"
             (lambda () (list (focused-window) (name (current-output))))
             '(nil :apple))

        (scene)
        ;; A window moved to the Samsung selected it earlier
        (move-window-to-output (gethash :hidden-b *windows*) samsung)
        (focus-window (gethash :term *windows*) seat)
        (focus-empty-frame)
        (try "an earlier move to the Samsung does not outrank the focused frame"
             (lambda () (name (current-output)))
             :apple)

        (scene)
        (focus-output samsung)
        (focus-window (gethash :term *windows*) seat)
        (focus-empty-frame)
        (try "a click back to the Apple replaces an earlier focus-output"
             (lambda () (name (current-output)))
             :apple)

        ;; --- Frame focus by direction into an empty frame ---

        (scene)
        (focus-frame-direction apple 1 :right)
        (try "focus moved by direction into an empty frame stays on its output"
             (lambda () (list (focused-window) (name (current-output))))
             '(nil :apple))

        ;; --- A switch to an empty workspace clears focus there ---

        (scene)
        (move-window-to-output (gethash :hidden-b *windows*) samsung)
        (focus-window (gethash :term *windows*) seat)
        (switch-workspace 2 apple)
        (try "a switch to an empty workspace keeps that output current"
             (lambda () (list (focused-window) (name (current-output))))
             '(nil :apple))

        ;; --- Prefix commands act on the focused frame's output ---

        (scene)
        (focus-empty-frame)
        (try "pull-hidden-next from the prefix map pulls onto the Apple"
             (lambda ()
               (enter-prefix-mode)
               ;; The Apple's hidden window, behind the visible one
               (let ((pulled (find-if-not
                              (lambda (w)
                                (member w (visible-frame-windows apple 1)))
                              (windows-on-workspace apple 1))))
                 (funcall (keymap-lookup *prefix-map*
                                         (list (char-code #\n))))
                 (list (name (window-output pulled))
                       (eq (window-frame pulled) (current-frame-for apple 1))
                       (eq (frame-current-window
                            (current-frame-for samsung 1))
                           (gethash :browser *windows*)))))
             '(:apple t t))

        ;; --- New windows open on the focused frame's output ---

        (scene)
        (focus-empty-frame)
        (let ((new (make-instance 'rhizome-window :proxy :new
                                  :app-id "kitty")))
          (setf (gethash :new *windows*) new)
          (manage-pass)
          (ok "a new window opens in the Apple's empty focused frame"
              (list (name (window-output new))
                    (eq (window-frame new) (current-frame-for apple 1)))
              '(:apple t)))

        ;; --- The output is captured when the prefix key is pressed ---

        (scene)
        (focus-empty-frame)
        (enter-prefix-mode)
        (pointer-at samsung)
        (try "the pointer moving while the prefix is open changes nothing"
             (lambda () (name (current-output)))
             :apple)
        (exit-prefix-mode)

        (reset)
        (pointer-at samsung)
        (enter-prefix-mode)
        (pointer-at apple)
        (try "with nothing ever focused, the output at prefix entry holds"
             (lambda () (name (current-output)))
             :samsung)
        (exit-prefix-mode)

        ;; --- The pointer remains the last resort ---

        (reset)
        (pointer-at apple)
        (try "with nothing ever focused, the pointer's output is current"
             (lambda () (name (current-output)))
             :apple)

        ;; --- Sloppy focus still moves keyboard focus with the pointer ---

        (scene)
        (let ((browser (gethash :browser *windows*))
              (*config* (merge-plist *config*
                                     '(:pointer (:focus-policy :sloppy)))))
          (setf (gethash seat *pointer-focus-state*) nil)
          (pointer-focus-manage-pass)
          (pointer-focus-put seat :entered browser)
          (pointer-focus-put seat :crossed t)
          (setf (seat-pointer-x seat) 400)
          (pointer-focus-manage-pass)
          (try "sloppy focus on the Samsung makes the Samsung current"
               (lambda () (list (eq (focused-window) browser)
                                (name (current-output))))
               '(t :samsung))
          (focus-frame samsung 1 (current-frame-for samsung 1))
          (focus-frame apple 1 (empty-leaf apple))
          (try "sloppy focus leaves an empty focused frame's output current"
               (lambda () (name (current-output)))
               :apple))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
