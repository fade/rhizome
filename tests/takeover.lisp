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
      (setf (output-wl-output-name out) 61
            *display-modes* (make-hash-table :test 'equal)
            *display-heads* (make-hash-table :test 'equal)
            (gethash :tk-mode *display-modes*)
            (list :width 800 :height 600 :refresh 60000)
            (gethash :tk-head *display-heads*)
            (list :head :tk-head :name "DP-1" :enabled t
                  :position (cons 0 0) :current-mode :tk-mode :scale 1))
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
                '(:frame (:rect 0 0 100 100) 7))
            (ok "translation keeps the user mark of a split"
                (nthcdr 5 (translate-tree-numbers
                            (append tree '(:user t)) number-map))
                '(:user t))
            (ok "translation adds no mark to an unmarked split"
                (nthcdr 5 translated) nil))))

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
      (labels ((head (proxy name x &optional (wl-name (+ 59 (floor x 1920))))
                 ;; A river output named by its numeric wl_output
                 ;; global, and the output-manager head carrying its
                 ;; connector name at the same position and size
                 (let ((out (make-instance 'rhizome-output :proxy proxy
                                           :x x :y 0
                                           :width 1920 :height 1080))
                       (mode (list proxy :mode))
                       (display-head (list proxy :head)))
                   (setf (output-wl-output-name out) wl-name
                         (gethash proxy *outputs*) out
                         (gethash out *output-workspace*) 1
                         (gethash mode *display-modes*)
                         (list :width 1920 :height 1080 :refresh 60000)
                         (gethash display-head *display-heads*)
                         (list :head display-head :name name :enabled t
                               :position (cons x 0)
                               :current-mode mode :scale 1))
                   out))
               (fresh-wm ()
                 (reset)
                 (setf *outputs* (make-hash-table)
                       *display-heads* (make-hash-table :test 'equal)
                       *display-modes* (make-hash-table :test 'equal)
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
          (ok "each window entry records its output by connector name"
              (list (getf (entry "firefox-id") :output)
                    (getf (entry "kitty-id") :output))
              '("HDMI-A-1" "DVI-D-1"))
          (ok "output workspaces and trees are keyed by connector name"
              (list (mapcar #'car (getf *takeover-state* :outputs))
                    (mapcar #'caar (getf *takeover-state* :trees)))
              '(("HDMI-A-1" "DVI-D-1") ("HDMI-A-1" "DVI-D-1")))
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

        ;; A full river restart between write and read: river hands
        ;; out new wl_output global names, here swapped between the
        ;; heads, and each window still lands on its own head
        (let ((*takeover-state-path*
                #P"/tmp/rhizome-test-takeover-restart.sexp"))
          (fresh-wm)
          (let* ((hdmi (head :pre-hdmi "HDMI-A-1" 0 59))
                 (dvi (head :pre-dvi "DVI-D-1" 1920 60))
                 (firefox (new-window :pre-ff "firefox-id" 1))
                 (kitty (new-window :pre-kitty "kitty-id" 2)))
            (setf (window-output firefox) hdmi
                  (window-output kitty) dvi
                  (gethash firefox *window-workspace*) 1
                  (gethash kitty *window-workspace*) 1
                  (gethash dvi *output-workspace*) 3)
            (frame-push-window firefox (frame-tree-for hdmi 1))
            (frame-push-window kitty (frame-tree-for dvi 1))
            (ignore-errors (delete-file *takeover-state-path*))
            (let ((*standard-output* (make-broadcast-stream)))
              (write-takeover-state)))
          (fresh-wm)
          (let* ((hdmi (head :post-hdmi "HDMI-A-1" 0 60))
                 (dvi (head :post-dvi "DVI-D-1" 1920 59))
                 (firefox (new-window :post-ff "firefox-id" 7))
                 (kitty (new-window :post-kitty "kitty-id" 8)))
            (let ((*standard-output* (make-broadcast-stream)))
              (load-takeover-state)
              (takeover-restore-output-workspaces)
              (restore-takeover-placement firefox)
              (restore-takeover-placement kitty))
            (ok "after a river restart firefox lands on HDMI and kitty on DVI"
                (list (eq (window-output firefox) hdmi)
                      (eq (window-output kitty) dvi))
                '(t t))
            (ok "after a river restart DVI shows its own workspace"
                (list (gethash hdmi *output-workspace*)
                      (gethash dvi *output-workspace*))
                '(1 3))
            (let ((*standard-output* (make-broadcast-stream)))
              (apply-takeover-trees))
            (ok "after a river restart each head's tree holds its own window"
                (list (tree-windows hdmi 1) (tree-windows dvi 1))
                (list (list firefox) (list kitty))))
          (ignore-errors (delete-file *takeover-state-path*)))

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

        ;; A user split with one empty frame survives the hot swap:
        ;; the restored split keeps its mark, so pruning leaves the
        ;; empty frame and kitty keeps half the width
        (let ((*takeover-state-path*
                #P"/tmp/rhizome-test-takeover-usersplit.sexp"))
          (fresh-wm)
          (let* ((hdmi (head :us-old-hdmi "HDMI-A-1" 0))
                 (dvi (head :us-old-dvi "DVI-D-1" 1920))
                 (firefox (new-window :us-old-ff "firefox-id" 1))
                 (kitty (new-window :us-old-kitty "kitty-id" 2))
                 (root (frame-tree-for dvi 1)))
            (setf (window-output firefox) hdmi
                  (window-output kitty) dvi
                  (gethash firefox *window-workspace*) 1
                  (gethash kitty *window-workspace*) 1)
            (frame-push-window firefox (frame-tree-for hdmi 1))
            (split-frame root :horizontal 1/2)
            (setf (frame-user-split root) t)
            (frame-push-window kitty (frame-first-child root))
            (ignore-errors (delete-file *takeover-state-path*))
            (let ((*standard-output* (make-broadcast-stream)))
              (write-takeover-state)))
          (fresh-wm)
          (let* ((dvi (head :us-new-dvi "DVI-D-1" 1920))
                 (hdmi (head :us-new-hdmi "HDMI-A-1" 0))
                 (firefox (new-window :us-new-ff "firefox-id" 4))
                 (kitty (new-window :us-new-kitty "kitty-id" 5)))
            (let ((*standard-output* (make-broadcast-stream)))
              (load-takeover-state)
              (restore-takeover-placement firefox)
              (restore-takeover-placement kitty)
              (apply-takeover-trees))
            (prune-empty-frames dvi 1)
            (prune-empty-frames hdmi 1)
            (ok "a user split keeps its empty frame across a takeover"
                (mapcar (lambda (leaf) (length (frame-windows leaf)))
                        (frame-leaves (frame-tree-for dvi 1)))
                '(1 0))
            (ok "the window in a restored user split keeps half the width"
                (frame-width (window-frame kitty)) 960)
            (ok "the other head keeps its single frame"
                (tree-windows hdmi 1) (list firefox)))
          (ignore-errors (delete-file *takeover-state-path*)))

        ;; A takeover file from a build that wrote no user mark keeps
        ;; today's behaviour: the empty frame prunes
        (fresh-wm)
        (let* ((dvi (head :um-dvi "DVI-D-1" 1920))
               (kitty (new-window :um-kitty "kitty-id" 3)))
          (setf *takeover-state*
                (list :windows '(("kitty-id" :workspace 1 :number 2
                                  :output "DVI-D-1"))
                      :trees '((("DVI-D-1" . 1)
                                . (:split :horizontal (:rect 0 0 1920 1080)
                                   (:frame (:rect 0 0 960 1080) 2)
                                   (:frame (:rect 960 0 960 1080))))))
                (window-output kitty) dvi
                (gethash kitty *window-workspace*) 1)
          (let ((*standard-output* (make-broadcast-stream)))
            (apply-takeover-trees))
          (prune-empty-frames dvi 1)
          (ok "an unmarked takeover tree prunes its empty frame"
              (list (length (frame-leaves (frame-tree-for dvi 1)))
                    (frame-width (window-frame kitty)))
              '(1 1920)))

        ;; An old-format file (no :output key) and a recorded output
        ;; that is gone both fall back to the current output
        (fresh-wm)
        (let* ((dvi (head :fb-dvi "DVI-D-1" 1920))
               (hdmi (head :fb-hdmi "HDMI-A-1" 0))
               (old (new-window :fb-old "old-format-id" 1))
               (gone (new-window :fb-gone "gone-output-id" 2))
               (numeric (new-window :fb-numeric "numeric-id" 3)))
          (setf *selected-output* hdmi
                *takeover-state*
                (list :windows '(("old-format-id" :workspace 2 :number 1)
                                 ("gone-output-id" :workspace 3 :number 2
                                  :output "DP-9")
                                 ("numeric-id" :workspace 4 :number 3
                                  :output 60))))
          (let ((*standard-output* (make-broadcast-stream)))
            (restore-takeover-placement old)
            (restore-takeover-placement gone)
            (restore-takeover-placement numeric))
          (ok "a numeric output key from an older file is treated as absent"
              (list (eq (window-output numeric) hdmi)
                    (eq (window-output numeric) dvi)
                    (window-workspace numeric))
              '(t nil 4))
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
          (ok "window-state records the output by connector name"
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

        ;; --- boot: restore waits until connector names are known ---
        ;; The successor learns its windows and outputs from river
        ;; before the output manager names any head: in the VM log the
        ;; first done with named heads follows the first manage pass.
        ;; Events go through the real registration functions and proxy
        ;; hooks, in that order.
        (labels ((proxy () (make-instance 'wl:wl-proxy))
                 (deliver (proxy event &rest args)
                   (dolist (hook (wl:wl-proxy-hooks proxy))
                     (apply hook event args)))
                 (arrive-window (id)
                   (let ((proxy (proxy)))
                     (register-window proxy)
                     (deliver proxy :identifier id)
                     (gethash proxy *windows*)))
                 (arrive-output (x width height)
                   (let ((proxy (proxy)))
                     (register-output proxy)
                     (deliver proxy :dimensions width height)
                     (deliver proxy :position x 0)
                     (gethash proxy *outputs*)))
                 (arrive-heads ()
                   ;; Virtual-1 1280x800 at 0,0 and Virtual-2 1024x768
                   ;; at 1280,0, as in the VM, then the done
                   (loop for (name x width height)
                           in '(("Virtual-1" 0 1280 800)
                                ("Virtual-2" 1280 1024 768))
                         do (let ((head (proxy)) (mode (proxy)))
                              (display-manager-event :head head)
                              (deliver head :name name)
                              (deliver head :mode mode)
                              (deliver mode :size width height)
                              (deliver mode :refresh 60000)
                              (deliver head :enabled 1)
                              (deliver head :current-mode mode)
                              (deliver head :position x 0)
                              (deliver head :scale 1)))
                   (display-manager-event :done 1))
                 (manage-pass ()
                   ;; The restore steps of on-manage-sequence, in order
                   (resolve-pending-window-outputs)
                   (when (and *takeover-state* (takeover-settled-p))
                     (apply-takeover-trees)))
                 (connector (win)
                   (output-connector-name (window-output win)))
                 (boot-wm ()
                   (fresh-wm)
                   (setf *pending-window-outputs* (make-hash-table)
                         *window-state* (make-hash-table :test 'equal)))
                 (two-window-takeover (&optional (second-output "Virtual-2"))
                   (setf *takeover-state*
                         (list :outputs (list (cons "Virtual-1" 1)
                                              (cons second-output 1))
                               :windows (list (list "term-a" :workspace 1
                                                    :number 1
                                                    :output "Virtual-1")
                                              (list "term-b" :workspace 1
                                                    :number 2
                                                    :output second-output))
                               :trees (list (cons (cons "Virtual-1" 1)
                                                  '(:frame (:rect 0 0 1280 800) 1))
                                            (cons (cons second-output 1)
                                                  '(:frame (:rect 0 0 1024 768) 2))))))
                 (takeover-boot (outputs-first)
                   ;; Returns (term-a term-b virtual-1 virtual-2 log)
                   (boot-wm)
                   (two-window-takeover)
                   (let (a b v1 v2)
                     (let ((log (with-output-to-string (*standard-output*)
                                  (when outputs-first
                                    (setf v1 (arrive-output 0 1280 800)
                                          v2 (arrive-output 1280 1024 768)))
                                  (setf a (arrive-window "term-a")
                                        b (arrive-window "term-b"))
                                  (manage-pass)
                                  (unless outputs-first
                                    (setf v1 (arrive-output 0 1280 800)
                                          v2 (arrive-output 1280 1024 768))
                                    (manage-pass))
                                  (arrive-heads)
                                  (manage-pass))))
                       (list a b v1 v2 log)))))
          (let ((*window-state-path*
                  #P"/tmp/rhizome-test-window-state-boot.sexp")
                (*window-state* (make-hash-table :test 'equal))
                (*pending-window-outputs* (make-hash-table)))
            ;; Windows, then outputs, then heads and done
            (destructuring-bind (a b v1 v2 log) (takeover-boot nil)
              (ok "windows before outputs before heads: each window returns to its own output"
                  (list (connector a) (connector b))
                  '("Virtual-1" "Virtual-2"))
              (ok "windows before outputs before heads: each tree is restored on its own output"
                  (list (tree-windows v1 1) (tree-windows v2 1)
                        (and (search "tree restored Virtual-2" log) t))
                  (list (list a) (list b) t))
              (ok "windows before outputs before heads: the takeover is consumed once settled"
                  *takeover-state* nil))
            ;; River's own order: outputs with geometry, windows, heads
            (destructuring-bind (a b v1 v2 log) (takeover-boot t)
              (declare (ignore log))
              (ok "outputs before windows before heads: each window returns to its own output"
                  (list (connector a) (connector b)
                        (tree-windows v1 1) (tree-windows v2 1))
                  (list "Virtual-1" "Virtual-2" (list a) (list b))))

            ;; Heads never arrive: the wait is bounded and names the
            ;; connectors it gave up on
            (boot-wm)
            (two-window-takeover)
            (let* ((passes 0)
                   (log (with-output-to-string (*standard-output*)
                          (arrive-output 0 1280 800)
                          (arrive-output 1280 1024 768)
                          (arrive-window "term-a")
                          (arrive-window "term-b")
                          (loop while (and *takeover-state* (< passes 100))
                                do (incf passes) (manage-pass)))))
              (ok "with no heads the takeover settles after the pass limit"
                  passes (1+ *output-name-pass-limit*))
              (ok "the timeout logs the unresolved connectors"
                  (and (search "outputs unresolved after" log)
                       (search "Virtual-1, Virtual-2" log)
                       t)
                  t))

            ;; Heads known, but the saved head is gone: settle at once
            ;; and log the output and the skipped tree
            (boot-wm)
            (two-window-takeover "Virtual-9")
            (let* ((passes 0)
                   (log (with-output-to-string (*standard-output*)
                          (arrive-output 0 1280 800)
                          (arrive-output 1280 1024 768)
                          (arrive-heads)
                          (arrive-window "term-a")
                          (arrive-window "term-b")
                          (loop while (and *takeover-state* (< passes 100))
                                do (incf passes) (manage-pass)))))
              (ok "a gone head does not hold the takeover once heads are named"
                  passes 1)
              (ok "a gone head is logged, and so is its skipped tree"
                  (list (and (search "takeover: no output Virtual-9" log) t)
                        (and (search "takeover tree skipped: no output Virtual-9"
                                     log)
                             t))
                  '(t t)))

            ;; An output whose geometry has not arrived matches no head;
            ;; its default 0,0 must not borrow Virtual-1's name
            (boot-wm)
            (let ((*standard-output* (make-broadcast-stream)))
              (arrive-heads))
            (let ((proxy (proxy)))
              (let ((*standard-output* (make-broadcast-stream)))
                (register-output proxy))
              (let ((out (gethash proxy *outputs*)))
                (ok "an output without position or dimensions has no connector name"
                    (output-connector-name out) nil)
                (deliver proxy :dimensions 1024 768)
                (ok "an output without its position has no connector name"
                    (output-connector-name out) nil)
                (deliver proxy :position 1280 0)
                (ok "an output with its geometry takes its head's name"
                    (output-connector-name out) "Virtual-2")))

            ;; window-state.sexp (a crash relaunch): the remembered
            ;; output waits for the heads the same way
            (boot-wm)
            (setf (gethash "remembered-b" *window-state*)
                  '(:workspace 2 :output "Virtual-2"))
            (let ((*standard-output* (make-broadcast-stream)))
              (arrive-output 0 1280 800)
              (arrive-output 1280 1024 768))
            (let ((win (let ((*standard-output* (make-broadcast-stream)))
                         (arrive-window "remembered-b"))))
              (let ((*standard-output* (make-broadcast-stream)))
                (manage-pass)
                (arrive-heads)
                (manage-pass))
              (ok "window-state: the remembered output is restored once heads are named"
                  (list (connector win) (window-workspace win)
                        (hash-table-count *pending-window-outputs*))
                  '("Virtual-2" 2 0)))
            ;; ... and gives up, logged, when heads never come
            (boot-wm)
            (setf (gethash "remembered-b" *window-state*)
                  '(:workspace 2 :output "Virtual-2"))
            (let ((log (with-output-to-string (*standard-output*)
                         (arrive-output 0 1280 800)
                         (arrive-output 1280 1024 768)
                         (arrive-window "remembered-b")
                         (loop repeat (+ 5 *output-name-pass-limit*)
                               do (manage-pass)))))
              (ok "window-state: an output that never resolves is dropped and logged"
                  (list (hash-table-count *pending-window-outputs*)
                        (and (search "no output Virtual-2 for remembered-b" log)
                             t))
                  '(0 t)))))
        (ignore-errors (delete-file *takeover-state-path*))
        (ignore-errors
          (delete-file #P"/tmp/rhizome-test-window-state.sexp"))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
