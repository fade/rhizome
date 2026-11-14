;;; Tests for WM hot-swap takeover state (takeover.lisp): the
;;; serialize/restore round-trip, per-window placement restore,
;;; number translation for the frame trees, settle detection, and
;;; consumption. River-touching functions are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls nil))
  (labels ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (stub (fn &optional (record t))
           (setf (symbol-function fn)
                 (lambda (&rest args)
                   (when record (push (cons fn args) calls))
                   (case fn
                     (show-window (setf (window-hidden-p (first args)) nil))
                     (hide-window (setf (window-hidden-p (first args)) t))
                     (float-window (setf (window-floating-p (first args)) t))
                     (otherwise nil)))))
         (reset ()
           (setf calls nil
                 *windows* (make-hash-table)
                 *output-workspace* (make-hash-table)
                 *window-workspace* (make-hash-table)
                 *minimized-windows* (make-hash-table)
                 *window-number-map* (make-hash-table)
                 *takeover-state* nil
                 *takeover-manage-passes* 0)
           (stub 'float-window)
           (stub 'request-manage-dirty)
           (stub 'focus-window)
           (stub 'assign-window-workspace nil)
           ;; assign-window-workspace must actually stamp
           (setf (symbol-function 'assign-window-workspace)
                 (lambda (win ws)
                   (setf (gethash win *window-workspace*) ws)))))

    ;; --- round-trip: write then read ---
    (let* ((*takeover-state-path* #P"/tmp/rhizome-test-takeover.sexp")
           (*workspace-count* 4)
           (out (make-instance 'rhizome-output :proxy :tk-out
                               :width 800 :height 600)))
      (setf (output-wl-output-name out) "DP-1")
      (reset)
      (setf (gethash :tk-out *outputs*) out
            (gethash out *output-workspace*) 3)
      ;; two windows with identifiers, numbers, geometry
      (let ((w1 (make-instance 'rhizome-window :proxy :tk-w1))
            (w2 (make-instance 'rhizome-window :proxy :tk-w2)))
        (setf (gethash :tk-w1 *windows*) w1
              (gethash :tk-w2 *windows*) w2
              (window-identifier w1) "app-one"
              (window-identifier w2) "app-two"
              (gethash w1 *window-number-map*) 1
              (gethash w2 *window-number-map*) 2
              (gethash w1 *window-workspace*) 2
              (gethash w2 *window-workspace*) 3
              (window-floating-p w2) t
              (window-float-x w2) 40 (window-float-y w2) 50
              (window-float-width w2) 200 (window-float-height w2) 100)
        (ignore-errors (delete-file *takeover-state-path*))
        ;; trees: write-takeover-state needs frame trees — with no
        ;; frame trees it serializes none (tree-for may return nil)
        (write-takeover-state)
        (ok "takeover file written"
            (and (probe-file *takeover-state-path*) t) t)
        (load-takeover-state)
        (ok "state loaded from disk"
            (and *takeover-state* t) t)
        (ok "output workspace round-trips"
            (cdr (assoc "DP-1" (getf *takeover-state* :outputs)
                        :test #'equal))
            3)
        (ok "window entries round-trip (identifier-keyed)"
            (sort (mapcar #'car (getf *takeover-state* :windows))
                  #'string<)
            '("app-one" "app-two"))
        (ok "float geometry round-trips"
            (getf (cdr (assoc "app-two"
                              (getf *takeover-state* :windows)
                              :test #'equal))
                  :float-x)
            40))

      ;; --- per-window placement restore (the successor WM) ---
      (let ((win (make-instance 'rhizome-window :proxy :tk-w3)))
        (setf (gethash :tk-w3 *windows*) win
              (window-identifier win) "app-two")
        (restore-takeover-placement win)
        (ok "takeover placement restores workspace"
            (window-workspace win) 3)
        (ok "takeover placement floats with geometry"
            (list (window-floating-p win)
                  (window-float-x win) (window-float-width win))
            '(t 40 200))
        (ok "placement stamped the output"
            (and (window-output win) t) t))

      ;; --- number translation for the trees ---
      (let ((number-map (make-hash-table)))
        (setf (gethash 1 number-map) 7   ; old 1 → new 7
              (gethash 2 number-map) 9)  ; old 2 → new 9
        (let ((tree '(:split :horizontal (:rect 0 0 800 300)
                       (:frame (:rect 0 0 400 300) 1 5)
                       (:frame (:rect 400 0 400 300) 2))))
          (let ((translated (translate-tree-numbers tree number-map)))
            (ok "tree leaves translate through the map"
                (fourth translated) '(:frame (:rect 0 0 400 300) 7))
            (ok "mapped numbers translate on the right too"
                (fifth translated) '(:frame (:rect 400 0 400 300) 9))
            (ok "unknown numbers vanish from frames"
                (translate-tree-numbers
                  '(:frame (:rect 0 0 100 100) 1 42) number-map)
                '(:frame (:rect 0 0 100 100) 7)))))

      ;; --- settle detection ---
      (setf *takeover-manage-passes* 0
            *takeover-state* (list :windows '((("app-one") . nil))))
      (ok "not settled while the window is missing"
          (takeover-settled-p) nil)
      (ok "pass counter advances"
          *takeover-manage-passes* 1)

      ;; --- consumption ---
      (consume-takeover-state)
      (ok "consume clears the state and deletes the file"
          (list *takeover-state* (probe-file *takeover-state-path*))
          '(nil nil)))

    ;; --- two heads: each window returns to its own output ---
    (let ((*takeover-state-path* #P"/tmp/rhizome-test-takeover-heads.sexp")
          (*workspace-count* 4)
          (*selected-output* nil))
      (labels ((head (proxy name x)
                 (let ((out (make-instance 'rhizome-output :proxy proxy
                                           :x x :y 0
                                           :width 1920 :height 1080)))
                   (setf (output-wl-output-name out) name
                         (gethash proxy *outputs*) out
                         (gethash out *output-workspace*) 1)
                   out))
               (fresh-wm ()
                 (reset)
                 (setf *outputs* (make-hash-table)
                       *frame-trees* (make-hash-table :test 'equal)
                       *current-frames* (make-hash-table :test 'equal)
                       *seats* (make-hash-table)
                       *selected-output* nil))
               (new-window (proxy id number)
                 (let ((win (make-instance 'rhizome-window :proxy proxy)))
                   (setf (gethash proxy *windows*) win
                         (window-identifier win) id
                         (gethash win *window-number-map*) number)
                   win))
               (tree-windows (out ws)
                 (loop for leaf in (frame-leaves (frame-tree-for out ws))
                       append (frame-windows leaf)))
               (entry (id)
                 (cdr (assoc id (getf *takeover-state* :windows)
                             :test #'equal))))
        ;; The outgoing WM: HDMI registered first, firefox on HDMI,
        ;; kitty on DVI, both on workspace 1 in their head's tree
        (fresh-wm)
        (let* ((hdmi (head :old-hdmi "HDMI-A-1" 0))
               (dvi (head :old-dvi "DVI-D-1" 1920))
               (firefox (new-window :old-ff "firefox-id" 1))
               (kitty (new-window :old-kitty "kitty-id" 2)))
          (setf (window-output firefox) hdmi
                (window-output kitty) dvi
                (gethash firefox *window-workspace*) 1
                (gethash kitty *window-workspace*) 1)
          (frame-push-window firefox (frame-tree-for hdmi 1))
          (frame-push-window kitty (frame-tree-for dvi 1))
          (ignore-errors (delete-file *takeover-state-path*))
          (let ((*standard-output* (make-broadcast-stream)))
            (write-takeover-state)))
        ;; The successor: DVI registers first this time, so the
        ;; first output is the wrong head for firefox
        (fresh-wm)
        (let* ((dvi (head :new-dvi "DVI-D-1" 1920))
               (hdmi (head :new-hdmi "HDMI-A-1" 0))
               (firefox (new-window :new-ff "firefox-id" 5))
               (kitty (new-window :new-kitty "kitty-id" 6)))
          (let ((*standard-output* (make-broadcast-stream)))
            (load-takeover-state))
          (ok "each window entry records its output by wl_output name"
              (list (getf (entry "firefox-id") :output)
                    (getf (entry "kitty-id") :output))
              '("HDMI-A-1" "DVI-D-1"))
          (ok "the first output is DVI in the successor"
              (eq (first (all-outputs)) dvi) t)
          ;; firefox arrived on DVI (an arrival output, or a stamp
          ;; from window-state): the recorded output still wins
          (setf (window-output firefox) dvi)
          (let ((*standard-output* (make-broadcast-stream)))
            (restore-takeover-placement firefox)
            (restore-takeover-placement kitty))
          (ok "firefox returns to HDMI"
              (eq (window-output firefox) hdmi) t)
          (ok "kitty returns to DVI"
              (eq (window-output kitty) dvi) t)
          (ok "both keep workspace 1"
              (list (window-workspace firefox) (window-workspace kitty))
              '(1 1))
          ;; The settled moment: each tree takes its own head's window
          (let ((*standard-output* (make-broadcast-stream)))
            (apply-takeover-trees))
          (ok "HDMI's workspace 1 tree holds firefox only"
              (tree-windows hdmi 1) (list firefox))
          (ok "DVI's workspace 1 tree holds kitty only"
              (tree-windows dvi 1) (list kitty)))

        ;; A window that re-registered before its output was known
        ;; moves to that output when the trees are applied
        (fresh-wm)
        (let* ((dvi (head :late-dvi "DVI-D-1" 1920))
               (hdmi (head :late-hdmi "HDMI-A-1" 0))
               (firefox (new-window :late-ff "firefox-id" 3)))
          (setf *takeover-state*
                (list :windows '(("firefox-id" :workspace 1 :number 1
                                  :output "HDMI-A-1"))
                      :trees '((("HDMI-A-1" . 1)
                                . (:frame (:rect 0 0 1920 1080) 1))))
                (window-output firefox) dvi
                (gethash firefox *window-workspace*) 1)
          (frame-push-window firefox (frame-tree-for dvi 1))
          (let ((*standard-output* (make-broadcast-stream)))
            (apply-takeover-trees))
          (ok "the tree restore moves a window onto its recorded output"
              (list (eq (window-output firefox) hdmi)
                    (tree-windows hdmi 1)
                    (tree-windows dvi 1))
              (list t (list firefox) nil)))

        ;; An old-format file (no :output key) and a recorded output
        ;; that is gone both fall back to the current output
        (fresh-wm)
        (let* ((dvi (head :fb-dvi "DVI-D-1" 1920))
               (hdmi (head :fb-hdmi "HDMI-A-1" 0))
               (old (new-window :fb-old "old-format-id" 1))
               (gone (new-window :fb-gone "gone-output-id" 2)))
          (declare (ignorable dvi))
          (setf *selected-output* hdmi
                *takeover-state*
                (list :windows '(("old-format-id" :workspace 2 :number 1)
                                 ("gone-output-id" :workspace 3 :number 2
                                  :output "DP-9"))))
          (let ((*standard-output* (make-broadcast-stream)))
            (restore-takeover-placement old)
            (restore-takeover-placement gone))
          (ok "an old-format entry lands on the current output"
              (list (eq (window-output old) hdmi) (window-workspace old))
              '(t 2))
          (ok "an absent recorded output falls back to the current output"
              (list (eq (window-output gone) hdmi) (window-workspace gone))
              '(t 3)))

        ;; window-state.sexp: the remembered output comes back too
        (fresh-wm)
        (let* ((*window-state-path* #P"/tmp/rhizome-test-window-state.sexp")
               (*window-state* (make-hash-table :test 'equal))
               (dvi (head :ws-dvi "DVI-D-1" 1920))
               (hdmi (head :ws-hdmi "HDMI-A-1" 0))
               (closing (new-window :ws-closing "remembered-id" 1)))
          (setf (window-output closing) hdmi
                (gethash closing *window-workspace*) 2)
          (let ((*standard-output* (make-broadcast-stream)))
            (remember-window-state closing))
          (ok "window-state records the output by wl_output name"
              (getf (gethash "remembered-id" *window-state*) :output)
              "HDMI-A-1")
          (let ((again (new-window :ws-again "remembered-id" 2))
                (legacy (new-window :ws-legacy "legacy-id" 3)))
            (setf (gethash "legacy-id" *window-state*) '(:workspace 3)
                  *selected-output* hdmi)
            (let ((*standard-output* (make-broadcast-stream)))
              (restore-window-state again)
              (restore-window-state legacy))
            (ok "window-state restores the remembered output"
                (list (eq (window-output again) hdmi)
                      (window-workspace again))
                '(t 2))
            (ok "a window-state entry without an output takes the current output"
                (list (eq (window-output legacy) hdmi)
                      (eq (window-output legacy) dvi))
                '(t nil))))
        (ignore-errors (delete-file *takeover-state-path*))
        (ignore-errors
          (delete-file #P"/tmp/rhizome-test-window-state.sexp"))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
