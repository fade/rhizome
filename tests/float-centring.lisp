;;; Tests for where a new floating window first appears (floating.lisp):
;;; centred on the frame holding its parent window, on the parent itself
;;; when the parent floats, else on its output's usable area; once its
;;; size is known it is placed and left alone. River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run; titlebars off isolates centring
;; from the strip clamp (tests/titlebars.lisp covers that)
(configure '(:bar (:enabled nil) :theme (:titlebar nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position set-window-borders
                  place-node-above message request-manage-dirty
                  recommend-dimension-bounds show-window hide-window))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    ;; A 36 px bar
    (setf (symbol-function 'bar-reserved-height) (lambda () 36))

    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal))
    (clrhash *window-parents*)

    (labels ((mk-out (proxy &optional (x 0))
               ;; A 1280x800 output at X showing workspace 1
               (let ((out (make-instance 'rhizome-output :proxy proxy :x x
                                         :width 1280 :height 800)))
                 (setf (gethash out *output-workspace*) 1)
                 out))
             (mk-win (proxy out &key (workspace 1))
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out)
                 (assign-window-workspace win workspace)
                 win))
             (new-float (proxy out &key parent)
               ;; A window that floats before it is ever shown
               (let ((win (mk-win proxy out)))
                 (when parent
                   (handle-window-event win :parent (window-proxy parent)))
                 (float-window win :animate nil)
                 win))
             (commit-size (win w h)
               (setf (window-width win) w
                     (window-height win) h))
             (render (out)
               (float-render-decisions out nil))
             (pos (win)
               (list (window-float-x win) (window-float-y win))))

      ;; --- A: no parent: the output's usable area ---
      (let* ((out (mk-out :fc-a-out))
             (win (new-float :fc-a out)))
        (commit-size win 400 300)
        (render out)
        (ok "a new float with no parent is centred on its output"
            (pos win) (list 440 (+ 36 232))))

      ;; --- B: the parent is tiled: the frame holding it ---
      (let* ((out (mk-out :fc-b-out))
             (parent (mk-win :fc-b-p out)))
        (frame-push-window parent (frame-tree-for out 1))
        (setf (window-focused-p parent) t)
        (split-current-frame out 1 :horizontal)
        (setf (window-focused-p parent) nil)
        (let ((win (new-float :fc-b out :parent parent)))
          (commit-size win 200 100)
          (render out)
          (ok "a new float is centred on its parent's frame"
              (pos win) (list 220 (+ 36 332)))))

      ;; --- C: the parent floats: the parent's own rectangle ---
      (let* ((out (mk-out :fc-c-out))
             (parent (mk-win :fc-c-p out)))
        (setf (window-node parent) :parent-node)
        (float-window parent :x 700 :y 100 :width 400 :height 300
                             :animate nil)
        (let ((win (new-float :fc-c out :parent parent)))
          (commit-size win 200 100)
          (render out)
          (ok "a new float over a floating parent is centred on it"
              (pos win) '(800 200))))

      ;; --- D: placed once its size is known, then left alone ---
      (let* ((out (mk-out :fc-d-out))
             (win (new-float :fc-d out)))
        (render out)
        (ok "with no size yet the float is not centred"
            (equal (pos win) (list 440 (+ 36 232))) nil)
        (commit-size win 400 300)
        (render out)
        (ok "the first render with a size centres it"
            (pos win) (list 440 (+ 36 232)))
        (setf (window-float-x win) 10
              (window-float-y win) 50)
        (render out)
        (ok "a later move is kept"
            (pos win) '(10 50)))

      ;; --- E: floating a shown window keeps its position ---
      (let* ((out (mk-out :fc-e-out))
             (win (mk-win :fc-e out)))
        (setf (window-node win) :fc-e-node
              (window-x win) 30 (window-y win) 80)
        (commit-size win 400 300)
        (float-window win :animate nil)
        (render out)
        (ok "a window already on screen floats where it is"
            (pos win) '(30 80)))

      ;; --- F: the parent is on another output ---
      (let* ((left (mk-out :fc-f-left))
             (right (mk-out :fc-f-right 1280))
             (parent (mk-win :fc-f-p right)))
        (frame-push-window parent (frame-tree-for right 1))
        (let ((win (new-float :fc-f left :parent parent)))
          (commit-size win 200 100)
          (float-manage-decisions left)
          (render left)
          (render right)
          (ok "a new float follows its parent to the parent's output"
              (list (eq (window-output win) right) (pos win))
              (list t (list (+ 1280 540) (+ 36 332))))))

      ;; --- G: the parent is on a hidden workspace ---
      (let* ((out (mk-out :fc-g-out))
             (parent (mk-win :fc-g-p out :workspace 2)))
        (setf (window-node parent) :parent-node)
        (float-window parent :x 700 :y 100 :width 400 :height 300
                             :animate nil)
        (let ((win (new-float :fc-g out :parent parent)))
          (commit-size win 400 300)
          (render out)
          (ok "a parent on a hidden workspace leaves the float on its output"
              (pos win) (list 440 (+ 36 232))))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
