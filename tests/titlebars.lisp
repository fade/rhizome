;;; Tests for float titlebars (titlebar.lisp): which windows qualify
;;; (parent, fixed size, CSD-only, window rule, fullscreen, maximized,
;;; tiled, switched off), the strip geometry and colours, title
;;; fitting, the render pass (decoration lifecycle, redraw only on
;;; change, buffer reallocation on any size change, teardown), and a
;;; press on the strip starting a move. River requests are stubbed and
;;; recorded; the shm buffer path runs for real.
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
           (ops ()
             (mapcar #'car (reverse calls)))
           (stub-protocol ()
             (dolist (spec '((wl:wl-compositor.create-surface :surface)
                             (rwm:river-window-v1.get-decoration-above :deco)
                             (rwm:river-decoration-v1.set-offset :offset)
                             (rwm:river-decoration-v1.sync-next-commit :sync)
                             (rwm:river-decoration-v1.destroy :deco-destroy)
                             (wl:wl-surface.attach :attach)
                             (wl:wl-surface.damage :damage)
                             (wl:wl-surface.commit :commit)
                             (wl:wl-surface.destroy :surface-destroy)
                             (wl:wl-shm.create-pool :pool)
                             (wl:wl-shm-pool.create-buffer :buffer)
                             (wl:wl-shm-pool.destroy :pool-destroy)
                             (wl:wl-buffer.destroy :buffer-destroy)))
               (setf (symbol-function (first spec)) (record (second spec))))
             (dolist (fn '(message request-manage-dirty expose-clicked))
               (setf (symbol-function fn)
                     (lambda (&rest args) (declare (ignore args)) nil)))
             (setf *compositor* :fake-compositor
                   *shm* :fake-shm))
           (fresh-world ()
             (setf *windows* (make-hash-table)
                   *seats* (make-hash-table))
             (clrhash *titlebars*)
             (clrhash *window-parents*)
             (setf calls '()))
           (make-float (proxy &key (x 100) (y 200) (w 400) (h 300)
                                   (focused t) (title "term"))
             (let ((win (make-instance 'rhizome-window :proxy proxy
                                                       :title title)))
               (setf (gethash proxy *windows*) win
                     (window-floating-p win) t
                     (window-focused-p win) focused
                     (window-float-x win) x
                     (window-float-y win) y
                     (window-float-width win) w
                     (window-float-height win) h)
               win))
           (render-pass ()
             (setf calls '())
             (titlebars-render-pass)))
    (macrolet ((check (name actual expected)
                 `(handler-case (ok ,name ,actual ,expected)
                    (error (e)
                      (incf fail)
                      (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

      (fresh-world)
      (stub-protocol)
      (configure '(:theme (:titlebar t :titlebar-height 24
                           :focused-border "#7aa2f7"
                           :unfocused-border "#404040")
                   :windows (:decorations :server)
                   :window-rules ()))

      ;; --- which windows qualify ---
      (let ((win (make-float :tb-a)))
        (check "plain float gets a titlebar" (window-titlebar-p win) t)
        (setf (window-floating-p win) nil)
        (check "tiled window gets none" (window-titlebar-p win) nil)
        (setf (window-floating-p win) t)

        (handle-window-event win :parent :tb-parent)
        (check "parent event marks a child window" (window-has-parent-p win) t)
        (check "window with a parent gets none" (window-titlebar-p win) nil)
        (handle-window-event win :parent nil)
        (check "null parent event clears it" (window-titlebar-p win) t)

        (handle-window-event win :dimensions-hint 400 300 400 300)
        (check "fixed size (min = max) gets none" (window-titlebar-p win) nil)
        (handle-window-event win :dimensions-hint 200 100 800 600)
        (check "resizable window qualifies" (window-titlebar-p win) t)
        (handle-window-event win :dimensions-hint 0 0 0 0)
        (check "no size preference qualifies" (window-titlebar-p win) t)

        (handle-window-event win :decoration-hint :only-supports-csd)
        (check "CSD-only window gets none" (window-titlebar-p win) nil)
        (handle-window-event win :decoration-hint :prefers-csd)
        (check "window that also supports SSD qualifies"
               (window-titlebar-p win) t)

        (setf (window-app-id win) "pavucontrol")
        (configure '(:window-rules ((:match "pavu" :titlebar nil))))
        (check "rule :titlebar nil gets none" (window-titlebar-p win) nil)
        (configure '(:window-rules ((:match "pavu" :workspace 2)
                                    (:match "pavu" :titlebar nil))))
        (check "a later rule carrying :titlebar still applies"
               (window-titlebar-p win) nil)
        (configure '(:window-rules ((:match "other" :titlebar nil))))
        (check "a rule for another window does not apply"
               (window-titlebar-p win) t)
        (configure '(:window-rules ()))

        (setf (window-fullscreen-p win) t)
        (check "fullscreen float gets none" (window-titlebar-p win) nil)
        (setf (window-fullscreen-p win) nil
              (window-maximized-p win) t)
        (check "maximized float gets none" (window-titlebar-p win) nil)
        (setf (window-maximized-p win) nil
              (window-minimized-p win) t)
        (check "minimized float gets none" (window-titlebar-p win) nil)
        (setf (window-minimized-p win) nil)

        (configure '(:theme (:titlebar nil)))
        (check ":titlebar nil switches every titlebar off"
               (window-titlebar-p win) nil)
        (configure '(:theme (:titlebar t)))
        (configure '(:windows (:decorations :client)))
        (check "client decorations mode draws no titlebar"
               (window-titlebar-p win) nil)
        (configure '(:windows (:decorations :server)))
        (check "back on: float qualifies" (window-titlebar-p win) t))

      ;; --- height and geometry ---
      (check "configured height" (titlebar-height) 24)
      (configure '(:theme (:titlebar-height nil)))
      (check "default height follows the font"
             (titlebar-height) (+ (font-line-height) 8))
      (configure '(:theme (:titlebar-height 24)))
      (let ((win (make-float :tb-g :w 400 :focused t)))
        (check "focused strip spans the 2 px border, sits above it"
               (multiple-value-list (titlebar-geometry win))
               '(-2 -26 404 24))
        (setf (window-focused-p win) nil)
        (check "unfocused strip spans the 1 px border"
               (multiple-value-list (titlebar-geometry win))
               '(-1 -25 402 24))
        (setf (window-focused-p win) t)
        (check "hit: inside the strip" (titlebar-hit-p win 150 190) t)
        (check "hit: the strip's left edge over the border"
               (titlebar-hit-p win 98 180) t)
        (check "miss: window content" (titlebar-hit-p win 150 200) nil)
        (check "miss: above the strip" (titlebar-hit-p win 150 173) nil)
        (check "miss: right of the strip" (titlebar-hit-p win 502 190) nil))

      ;; --- colours and title ---
      (check "focused: focused-border fill, dark text"
             (multiple-value-list (titlebar-colours t))
             '(122 162 247 26 27 38))
      (check "unfocused: unfocused-border fill, light text"
             (multiple-value-list (titlebar-colours nil))
             '(64 64 64 192 202 245))
      (flet ((eight (s) (* 8 (length s))))
        (check "title that fits stays whole"
               (titlebar-fit-title "hello" 48 #'eight) "hello")
        (check "long title is cut with an ellipsis"
               (titlebar-fit-title "hello world" 48 #'eight) "hel...")
        (check "no room even for the ellipsis: empty"
               (titlebar-fit-title "hello world" 16 #'eight) "")
        (check "no title: empty" (titlebar-fit-title nil 48 #'eight) ""))

      ;; --- render pass ---
      (fresh-world)
      (let ((win (make-float :tb-r :w 400 :focused t)))
        (render-pass)
        (check "first pass: one decoration above the window"
               (length (calls-of :deco)) 1)
        (check "first pass: offset above the window"
               (mapcar #'cddr (calls-of :offset)) '((-2 -26)))
        (check "first pass: sync, attach, commit in that order"
               (remove-if-not (lambda (op) (member op '(:sync :attach :commit)))
                              (ops))
               '(:sync :attach :commit))
        (check "first pass: buffer sized to the strip"
               (mapcar (lambda (c) (subseq c 3 5)) (calls-of :buffer))
               '((404 24)))
        (render-pass)
        (check "unchanged: no redraw, no offset"
               (list (length (calls-of :commit)) (length (calls-of :offset))
                     (length (calls-of :deco)))
               '(0 0 0))
        (setf (window-title win) "vim")
        (render-pass)
        (check "title change: one redraw, same buffer"
               (list (length (calls-of :commit)) (length (calls-of :buffer)))
               '(1 0))
        (setf (window-focused-p win) nil)
        (render-pass)
        (check "focus change: redraw and new offset"
               (list (length (calls-of :commit))
                     (mapcar #'cddr (calls-of :offset)))
               '(1 ((-1 -25))))
        (setf (window-focused-p win) t
              (window-float-width win) 500)
        (render-pass)
        (check "width change: buffer reallocated at the new width"
               (mapcar (lambda (c) (subseq c 3 5)) (calls-of :buffer))
               '((504 24)))
        (configure '(:theme (:focused-border "#ff0000")))
        (render-pass)
        (check "colour change: redraw"
               (length (calls-of :commit)) 1)
        (configure '(:theme (:focused-border "#7aa2f7")))
        (render-pass)
        ;; 504 x 24 and 432 x 28 hold the same number of bytes
        (setf (window-float-width win) 428)
        (configure '(:theme (:titlebar-height 28)))
        (render-pass)
        (check "same byte size, new shape: buffer reallocated"
               (mapcar (lambda (c) (subseq c 3 5)) (calls-of :buffer))
               '((432 28)))
        (configure '(:theme (:titlebar-height 24)))
        (render-pass)

        ;; --- teardown ---
        (setf (window-floating-p win) nil)
        (render-pass)
        (check "tiled: decoration destroyed"
               (list (length (calls-of :deco-destroy)) (gethash win *titlebars*))
               '(1 nil))
        (setf (window-floating-p win) t)
        (render-pass)
        (check "floated again: a fresh decoration"
               (length (calls-of :deco)) 1)
        (setf (window-fullscreen-p win) t)
        (render-pass)
        (check "fullscreen: decoration destroyed"
               (list (length (calls-of :deco-destroy)) (gethash win *titlebars*))
               '(1 nil))
        (setf (window-fullscreen-p win) nil)
        (render-pass)
        (handle-window-event win :parent :tb-parent)
        (setf calls '())
        (run-hook :window-closed win)
        (check "closed: decoration destroyed, parent forgotten"
               (list (length (calls-of :deco-destroy))
                     (gethash win *titlebars*)
                     (nth-value 1 (gethash win *window-parents*)))
               '(1 nil nil))
        (configure '(:theme (:titlebar nil)))
        (setf (gethash :tb-r *windows*) win)
        (clrhash *window-parents*)
        (render-pass)
        (check "switched off: no decoration created"
               (length (calls-of :deco)) 0)
        (configure '(:theme (:titlebar t))))

      ;; --- a press on the strip starts a move ---
      (fresh-world)
      (let* ((seat (make-instance 'rhizome-seat :proxy :tb-seat))
             (win (make-float :tb-p :x 100 :y 200 :w 400 :focused t))
             (dialog (make-float :tb-d :x 700 :y 200 :w 300 :focused nil)))
        (setf (gethash :tb-seat *seats*) seat)
        (handle-window-event dialog :parent :tb-p)
        (setf (seat-pointer-x seat) 150 (seat-pointer-y seat) 190)
        (handle-seat-event seat :window-interaction :tb-p)
        (check "press on the strip: move op pending for the window"
               (seat-op-pending seat)
               (list :win win :type :move :edges nil))
        (setf (seat-op-pending seat) nil
              (seat-pointer-x seat) 150 (seat-pointer-y seat) 300)
        (handle-seat-event seat :window-interaction :tb-p)
        (check "press on the content: no move" (seat-op-pending seat) nil)
        (setf (seat-pointer-x seat) 750 (seat-pointer-y seat) 190)
        (handle-seat-event seat :window-interaction :tb-d)
        (check "press above a dialog: no move" (seat-op-pending seat) nil))

      (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
      (unless (zerop fail)
        (uiop:quit 1)))))
