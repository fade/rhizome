;;; frame-addresses.lisp: W:S:F frame addresses. Covers parsing an
;;; address string; resolving it to a screen, a workspace, and a frame,
;;; with omitted parts, workspace names, and connector names; the
;;; address every leaf frame carries, read through the shared index
;;; base; that address staying current across each change to frame
;;; trees, screens, and workspace counts, each with a control run where
;;; the refresh is switched off and the address goes stale; and the
;;; frame-address and frame-addresses commands. No compositor is
;;; needed: river requests and on-screen messages are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))
(configure '(:animations (:enabled nil)))

(defmacro check (form)
  "FORM's value, or :ERROR when evaluating it signals an error."
  `(handler-case ,form
     (error (e)
       (format t "~&;   (error: ~A)~%" e)
       :error)))

(setf (symbol-function 'message)
      (lambda (fmt &rest args) (declare (ignore fmt args)) nil)
      (symbol-function 'request-manage-dirty)
      (lambda (&rest args) (declare (ignore args)) nil))

(let ((pass 0) (fail 0))
  (flet ((report (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (macrolet ((ok (name actual expected)
                 `(report ,name (check ,actual) ,expected)))
      (labels ((reset (&rest config)
                 (setf *config* (default-config))
                 (configure '(:bar (:enabled nil)))
                 (configure '(:animations (:enabled nil)))
                 (when config
                   (configure config))
                 (clrhash *workspace-count-overrides*)
                 (setf *outputs* (make-hash-table)
                       *windows* (make-hash-table)
                       *seats* (make-hash-table)
                       *display-heads* (make-hash-table)
                       *output-workspace* (make-hash-table)
                       *window-workspace* (make-hash-table)
                       *frame-trees* (make-hash-table :test 'equal)
                       *current-frames* (make-hash-table :test 'equal)
                       *selected-output* nil))
               (make-output (proxy connector x)
                 (let ((out (make-instance 'rhizome-output :proxy proxy
                                           :x x :y 0
                                           :width 1920 :height 1080)))
                   (setf (gethash proxy *outputs*) out
                         (gethash out *output-workspace*) 1)
                   (when connector
                     (setf (gethash proxy *display-heads*)
                           (list :head proxy :name connector :enabled t
                                 :position (cons x 0))))
                   out))
               (two-screens ()
                 ;; DVI-D-1 at x=1920 is primary (screen 1); HDMI-A-1
                 ;; at x=0 is screen 2
                 (values (make-output :dvi "DVI-D-1" 1920)
                         (make-output :hdmi "HDMI-A-1" 0)))
               (addr (frame)
                 (frame-address frame))
               (addrs (output workspace)
                 (mapcar #'frame-address
                         (frame-leaves (frame-tree-for output workspace))))
               (split3 (output workspace)
                 ;; Three side-by-side leaves; returns them
                 (let ((root (frame-tree-for output workspace)))
                   (split-frame root :horizontal 1/2)
                   (split-frame (frame-second-child root) :horizontal 1/2)
                   (frame-leaves root)))
               (target (string)
                 (let ((target (resolve-frame-address string)))
                   (and target
                        (list (screen-number (getf target :output))
                              (getf target :workspace)
                              (let ((frame (getf target :frame)))
                                (and frame (frame-address frame)))
                              (getf target :ordinal))))))

        ;; --- Parsing ---

        (reset)
        (ok "W:S:F parses into its three parts"
            (parse-frame-address "2:1:3")
            '(:workspace "2" :screen "1" :frame "3"))
        (ok "empty parts parse as omitted"
            (parse-frame-address "::2")
            '(:workspace nil :screen nil :frame "2"))
        (ok "W:S leaves the frame out"
            (parse-frame-address "irc:DVI-D-1")
            '(:workspace "irc" :screen "DVI-D-1" :frame nil))
        (ok "a string without a colon is no address"
            (parse-frame-address "3") nil)
        (ok "more than three parts is no address"
            (parse-frame-address "1:2:3:4") nil)

        ;; --- The address a frame carries ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (ok "a new tree's frame is 1:1:1 on the primary"
              (addr (frame-tree-for dvi 1)) "1:1:1")
          (ok "workspace 3 on screen 2"
              (addr (frame-tree-for hdmi 3)) "3:2:1")
          (ok "the :index-base keyword reads the address from 0"
              (frame-address (frame-tree-for hdmi 3) :index-base 0) "2:1:0")
          (ok "frame-address-parts gives the three numbers"
              (multiple-value-list (frame-address-parts (frame-tree-for hdmi 3)))
              '(3 2 1)))

        ;; --- Resolving ---

        (reset :screens '(:primary "DVI-D-1")
               :workspaces '(:screens (("DVI-D-1" :names ("mail" "irc"))
                                       ("HDMI-A-1" :names ("web" "irc")))))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (split3 hdmi 2)
          (setf *selected-output* dvi)
          (ok "a full address names screen, workspace, and frame"
              (target "2:2:3") '(2 2 "2:2:3" 2))
          (ok "a connector name stands for the screen"
              (target "2:hdmi-a-1:2") '(2 2 "2:2:2" 1))
          (ok "an omitted screen is the focused one"
              (target "1::1") '(1 1 "1:1:1" 0))
          (ok "an omitted workspace is the screen's current one"
              (target ":2:1") '(2 1 "1:2:1" 0))
          (ok "an omitted frame resolves to no frame"
              (target "2:2:") '(2 2 nil nil))
          (ok "a frame past the count resolves to its ordinal with no frame"
              (target "2:2:5") '(2 2 nil 4))
          (ok "a workspace name resolves on the given screen"
              (target "web:2:1") '(2 1 "1:2:1" 0))
          (ok "a name on two screens without a screen takes the focused screen's"
              (target "irc::1") '(1 2 "2:1:1" 0))
          (setf *selected-output* hdmi)
          (ok "and from the other screen, that screen's"
              (target "irc::1") '(2 2 "2:2:1" 0))
          (ok "a name only another screen holds is found in screen order"
              (target "mail::1") '(1 1 "1:1:1" 0))
          (ok "no screen 3"
              (nth-value 1 (resolve-frame-address "1:3:1"))
              "no screen 3")
          (ok "no workspace 12"
              (nth-value 1 (resolve-frame-address "12:1:1"))
              "no workspace 12: DVI-D-1 has 1-9")
          (ok "frame 0 is below the base"
              (nth-value 1 (resolve-frame-address "1:1:0"))
              "no frame 0: frames count from 1"))
        (reset :screens '(:primary "DVI-D-1") :workspaces '(:index-base 0))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore dvi))
          (setf *selected-output* hdmi)
          (ok "under base 0 the address counts from 0"
              (target "0:1:0") '(1 1 "0:1:0" 0)))

        ;; --- Kept current: split ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (let ((root (frame-tree-for dvi 1)))
            (let ((*frame-addresses-refresh* nil))
              (split-frame root :horizontal 1/2))
            (ok "control: without the refresh a split's new frame has no address"
                (addr (frame-second-child root)) nil))
          (let ((root (frame-tree-for dvi 2)))
            (split-frame root :horizontal 1/2)
            (ok "a split gives both halves their addresses"
                (addrs dvi 2) '("2:1:1" "2:1:2"))
            (ok "and the split node carries none"
                (addr root) nil)))

        ;; --- Kept current: remove split ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (destructuring-bind (a b c) (split3 dvi 1)
            (declare (ignore a))
            (setf (gethash (frame-key dvi 1) *current-frames*) b)
            (let ((*frame-addresses-refresh* nil))
              (remove-current-split dvi 1))
            (ok "control: without the refresh the last frame keeps frame 3"
                (addr c) "1:1:3"))
          (destructuring-bind (a b c) (split3 dvi 2)
            (declare (ignore a))
            (setf (gethash (frame-key dvi 2) *current-frames*) b)
            (remove-current-split dvi 2)
            (ok "removing a split renumbers the frames after it"
                (addr c) "2:1:2")))

        ;; --- Kept current: prune ---

        (reset :screens '(:primary "DVI-D-1") :frames '(:prune-empty :always))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (destructuring-bind (a b c) (split3 dvi 2)
            (declare (ignore a b))
            (let ((*frame-addresses-refresh* nil))
              (prune-empty-frames dvi 2))
            (ok "control: without the refresh the surviving last frame still reads frame 3"
                (addr c) "2:1:3"))
          (destructuring-bind (a b c) (split3 dvi 3)
            (declare (ignore a b))
            (prune-empty-frames dvi 3)
            (ok "the survivor is the frame that was third"
                (eq c (first (frame-leaves (frame-tree-for dvi 3)))) t)
            (ok "pruning renumbers the survivor"
                (addr c) "3:1:1")))

        ;; --- Kept current: only ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (destructuring-bind (a b c) (split3 dvi 1)
            (declare (ignore a b))
            (setf (gethash (frame-key dvi 1) *current-frames*) c)
            (let ((*frame-addresses-refresh* nil))
              (frame-only dvi 1))
            (ok "control: without the refresh the only frame keeps frame 3"
                (addr c) "1:1:3"))
          (destructuring-bind (a b c) (split3 dvi 2)
            (declare (ignore a b))
            (setf (gethash (frame-key dvi 2) *current-frames*) c)
            (frame-only dvi 2)
            (ok "collapsing to one frame makes it frame 1"
                (addr c) "2:1:1")))

        ;; --- Kept current: tree rebuilt (reorder) ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (frame-tree-for dvi 1)
          (let ((*frame-addresses-refresh* nil))
            (set-workspace-tree dvi 1 '((0 0 960 1080) (960 0 960 1080)) nil))
          (ok "control: without the refresh a rebuilt tree's frames have none"
              (addrs dvi 1) '(nil nil))
          (set-workspace-tree dvi 2 '((0 0 960 1080) (960 0 960 1080)) nil)
          (ok "a rebuilt tree numbers its frames in layout order"
              (addrs dvi 2) '("2:1:1" "2:1:2")))

        ;; --- Kept current: screen order from config ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (let ((d (frame-tree-for dvi 1))
                (h (frame-tree-for hdmi 1)))
            (let ((*frame-addresses-refresh* nil))
              (configure '(:screens (:order ("HDMI-A-1" "DVI-D-1")))))
            (ok "control: without the refresh a reorder leaves screen numbers stale"
                (list (addr d) (addr h)) '("1:1:1" "1:2:1"))
            (configure '(:screens (:order ("HDMI-A-1" "DVI-D-1"))))
            (ok "a new screen order renumbers every frame"
                (list (addr d) (addr h)) '("1:2:1" "1:1:1"))))

        ;; --- Kept current: screen added and removed ---

        (reset)
        (let* ((first (make-output :first "DVI-D-1" 1920))
               (f (frame-tree-for first 1)))
          (ok "a lone screen is screen 1" (addr f) "1:1:1")
          (configure '(:screens (:primary "HDMI-A-1")))
          (let ((proxy (make-instance 'wl:wl-proxy))
                (*river-layer-shell* nil))
            (setf (gethash proxy *display-heads*)
                  (list :head proxy :name "HDMI-A-1" :enabled t
                        :position (cons 0 0)))
            (register-output proxy)
            (let ((new (gethash proxy *outputs*)))
              (let ((*frame-addresses-refresh* nil))
                (handle-output-event new :position 0 0)
                (handle-output-event new :dimensions 1920 1080))
              (ok "control: without the refresh an added primary leaves frames stale"
                  (addr f) "1:1:1")
              (handle-output-event new :dimensions 1920 1080)
              (ok "an added primary moves the other screen to 2"
                  (addr f) "1:2:1")
              (let ((*frame-addresses-refresh* nil))
                (handle-output-event new :removed))
              (ok "control: without the refresh a removed screen leaves frames stale"
                  (addr f) "1:2:1")
              (setf (gethash proxy *outputs*) new)
              (handle-output-event new :removed)
              (ok "removing it makes the remaining screen 1 again"
                  (addr f) "1:1:1"))))

        ;; --- Kept current: workspace count ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (let ((nine (frame-tree-for dvi 9)))
            (let ((*frame-addresses-refresh* nil))
              (remove-workspace dvi))
            (ok "control: without the refresh a removed workspace's frame keeps its address"
                (addr nine) "9:1:1")
            (add-workspace dvi)
            (ok "adding the workspace back keeps it addressed"
                (addr nine) "9:1:1")
            (remove-workspace dvi)
            (ok "a removed workspace's frames lose their address"
                (addr nine) nil)))

        ;; --- Base change ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore dvi))
          (let ((h (frame-tree-for hdmi 2)))
            (ok "base 1" (addr h) "2:2:1")
            (let ((*frame-addresses-refresh* nil))
              (configure '(:workspaces (:index-base 0))))
            (ok "a base change reads through at once, with no refresh needed"
                (addr h) "1:1:0")))

        ;; --- Commands ---

        (reset :screens '(:primary "DVI-D-1"))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (setf *selected-output* hdmi)
          (let ((leaves (split3 hdmi 1))
                (win (make-instance 'rhizome-window :proxy :w1)))
            (setf (gethash :w1 *windows*) win
                  (window-output win) hdmi
                  (window-title win) "notes")
            (assign-window-workspace win 1)
            (frame-push-window win (second leaves))
            (setf (gethash (frame-key hdmi 1) *current-frames*) (second leaves))
            (ok "frame-address replies with the current frame's address"
                (execute-command hdmi :frame-address nil) "1:2:2")
            (ok "frame-address with an address replies with what it names"
                (execute-command hdmi :frame-address '("1:2:3")) "1:2:3")
            (ok "frame-address with a bad address replies with the error"
                (execute-command hdmi :frame-address '("1:7:1")) "no screen 7")
            (let ((lines (uiop:split-string
                          (execute-command dvi :frame-addresses nil)
                          :separator '(#\Newline))))
              (ok "frame-addresses lists every frame of every workspace"
                  (length lines) 20)
              (ok "in workspace, screen, frame order"
                  (subseq lines 0 5)
                  '("1:1:1 empty" "1:2:1 empty" "1:2:2 notes"
                    "1:2:3 empty" "2:1:1 empty")))
            (ok "the commands are in the command table"
                (list (functionp (gethash :frame-address *command-table*))
                      (functionp (gethash :frame-addresses *command-table*)))
                '(t t))))

        (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
        (sb-ext:exit :code (if (zerop fail) 0 1))))))
