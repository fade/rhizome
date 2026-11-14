;;; Tests for keeping every tiled window in a leaf of its workspace's
;;; frame tree: the pass that splits a frame for a new window sizes both
;;; halves, and a window whose frame left the tree (or whose frame no
;;; longer lists it) is adopted again on the next manage pass, with a log
;;; line. One fake output; river requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run; keep geometry exact
(configure '(:bar (:enabled nil)
             :animations (:enabled nil)
             :pointer (:warp-on-switch nil)))

(let ((pass 0) (fail 0) (proposals '()))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected)))))

    ;; Record proposals; every other river request is a no-op that
    ;; keeps the hidden flag honest
    (dolist (fn '(set-window-tiled-state make-window-node
                  set-window-borders show-window hide-window message
                  show-message request-manage-dirty
                  recommend-dimension-bounds set-window-clip-box
                  set-window-position))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (when (and args (typep (first args) 'rhizome-window))
                  (when (eq fn-sym 'show-window)
                    (setf (window-hidden-p (first args)) nil))
                  (when (eq fn-sym 'hide-window)
                    (setf (window-hidden-p (first args)) t)))
                nil))))
    (setf (symbol-function 'propose-window-dimensions)
          (lambda (win width height)
            (push (list (window-app-id win) width height) proposals)
            nil))

    (let (dvi seat)
      (labels ((reset ()
                 (setf *windows* (make-hash-table)
                       *outputs* (make-hash-table)
                       *seats* (make-hash-table)
                       *output-workspace* (make-hash-table)
                       *window-workspace* (make-hash-table)
                       *minimized-windows* (make-hash-table)
                       *frame-trees* (make-hash-table :test 'equal)
                       *current-frames* (make-hash-table :test 'equal)
                       *workspace-layouts* (make-hash-table :test 'equal)
                       *pending-focus-requests* nil
                       *pending-autofocus* nil
                       *last-registered-window* nil
                       *focus-history* nil
                       *expose-active* nil
                       *expose-saved* nil
                       proposals '())
                 ;; DVI-D-1: 1920x1200, no bar, so the content area is
                 ;; the whole output and a half frame is 960 wide
                 (setf dvi (make-instance 'rhizome-output :proxy :dvi-proxy
                                          :x 0 :y 0 :width 1920 :height 1200)
                       seat (make-instance 'rhizome-seat :proxy :seat-proxy)
                       (gethash :dvi-proxy *outputs*) dvi
                       (gethash :seat-proxy *seats*) seat
                       (gethash dvi *output-workspace*) 1))
               (new-window (app-id)
                 (let* ((proxy (intern (string-upcase app-id) :keyword))
                        (win (make-instance 'rhizome-window :proxy proxy
                                            :app-id app-id)))
                   (setf (gethash proxy *windows*) win)
                   (assign-window-number win)
                   win))
               (manage-pass ()
                 ;; The frame part of on-manage-sequence; returns what
                 ;; the pass logged
                 (with-output-to-string (*standard-output*)
                   (let ((*arrival-output* dvi))
                     (apply-layout-for-output dvi))
                   (setf *pending-focus-requests* nil)))
               (render-pass ()
                 (render-layout-for-output dvi))
               (in-leaf-p (win)
                 ;; WIN's frame is a leaf of its tree and lists WIN
                 (let ((frame (window-frame win)))
                   (and frame
                        (member frame (frame-leaves (frame-tree-for dvi 1))
                                :test #'eq)
                        (member win (frame-windows frame) :test #'eq)
                        t)))
               (proposed (app-id)
                 (remove-if-not (lambda (p) (equal (first p) app-id))
                                proposals))
               (readopted-p (log app-id)
                 (and (search (format nil "re-adopted frameless window ~A"
                                      app-id)
                              log)
                      t)))

        ;; --- The pass that splits a frame sizes both halves ---
        (reset)
        (let ((kitty (new-window "kitty")))
          (manage-pass)
          (render-pass)
          (focus-window kitty seat)
          (setf proposals '())
          (let* ((firefox (new-window "firefox"))
                 (log (manage-pass)))
            (ok "the split pass proposes the new window its half"
                (proposed "firefox") '(("firefox" 948 1188)))
            (ok "the split pass proposes the split-off window its half"
                (proposed "kitty") '(("kitty" 948 1188)))
            (ok "both windows sit in leaves after the split"
                (list (in-leaf-p kitty) (in-leaf-p firefox)) '(t t))
            (ok "an ordinary arrival logs no re-adoption"
                (readopted-p log "firefox") nil)))

        ;; --- A layout switch while a window is minimized ---
        (reset)
        (let ((kitty (new-window "kitty")))
          (manage-pass)
          (focus-window kitty seat)
          (setf (window-minimized-p kitty) t)
          (switch-layout dvi :master-stack)
          (manage-pass)
          (setf (window-minimized-p kitty) nil)
          (let ((log (manage-pass)))
            (ok "a window minimized across a layout switch returns to a leaf"
                (in-leaf-p kitty) t)
            (ok "returning it to a leaf logs the re-adoption"
                (readopted-p log "kitty") t)))

        ;; --- A new window never follows a focused window's stale frame ---
        (reset)
        (let ((kitty (new-window "kitty")))
          (manage-pass)
          (focus-window kitty seat)
          (setf (window-minimized-p kitty) t)
          (switch-layout dvi :monocle)
          (manage-pass)
          (let ((firefox (new-window "firefox")))
            (manage-pass)
            (ok "a new window opens in a leaf of the tree"
                (in-leaf-p firefox) t)
            (ok "the current frame is a leaf of the tree"
                (and (member (current-frame-for dvi 1)
                             (frame-leaves (frame-tree-for dvi 1))
                             :test #'eq)
                     t)
                t)))

        ;; --- Removing a split whose sibling is itself split ---
        (reset)
        (let ((a (new-window "a")) (b (new-window "b")) (c (new-window "c")))
          (manage-pass)
          (dolist (win (list a b c))
            (unless (in-leaf-p win)
              (manage-pass)))
          (focus-window a seat)
          (setf (gethash (frame-key dvi 1) *current-frames*)
                (window-frame a))
          (remove-current-split dvi 1)
          (manage-pass)
          (ok "every window is in a leaf after removing a split"
              (mapcar #'in-leaf-p (list a b c)) '(t t t)))

        ;; --- A window that arrives during expose ---
        (reset)
        (let ((a (new-window "a")) (b (new-window "b")))
          (manage-pass)
          (manage-pass)
          (when (expose-begin dvi 1)
            (setf *expose-active* t))
          (let ((c (new-window "c")))
            (manage-pass)
            (expose-restore)
            (let ((log (manage-pass)))
              (ok "a window that arrived during expose is in a leaf after it"
                  (in-leaf-p c) t)
              (ok "the windows exposed stay in leaves"
                  (list (in-leaf-p a) (in-leaf-p b)) '(t t))
              (ok "putting it back logs the re-adoption"
                  (readopted-p log "c") t))))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (sb-ext:exit :code 1))))
