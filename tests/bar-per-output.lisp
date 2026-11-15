;;; One bar per output: the WM snapshot carries each output's
;;; workspace, layout and windows; the bar process binds every
;;; wl_output and keeps one layer surface per output the snapshot
;;; marks; by default every bar shows the current output, and under
;;; (:bar :per-output-content t) each bar shows its own; (:bar
;;; :tray-output :primary) and (:bar :global-modules :primary) keep the
;;; tray, or every global module, on the primary bar; and the reserved
;;; strip follows the outputs that carry a bar.
;;;
;;; No compositor: wl_registry binds, layer surfaces and the snapshot
;;; file are stubbed, and no bar process is ever launched.
(ql:quickload :rhizome)

(in-package #:rhizome)

(defvar *po-pass* 0)
(defvar *po-fail* 0)

(defun po-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *po-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *po-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro po-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *po-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

;;; --- Effect stubs ---

(defvar *po-snapshot* nil "What bar-snapshot returns in the bar-side cases.")
(defvar *po-commands* nil)
(defvar *po-opened* nil)
(defvar *po-closed* nil)

(setf (symbol-function 'bar-write-snapshot) (lambda (snapshot) snapshot))
(setf (symbol-function 'request-manage-dirty) (lambda () nil))
(setf (symbol-function 'message) (lambda (&rest args) (declare (ignore args)) nil))
(setf (symbol-function 'uiop:run-program)
      (lambda (&rest args) (declare (ignore args)) nil))
(setf (symbol-function 'uiop:launch-program)
      (lambda (&rest args) (error "no process may start here: ~S" args)))
(setf (symbol-function 'bar-run-command)
      (lambda (command) (push command *po-commands*)))
(setf (symbol-function 'wl:find-interface-named)
      (lambda (name) (intern (string-upcase name) :keyword)))
(setf (symbol-function 'wl:wl-registry.bind)
      (lambda (registry name iface version)
        (declare (ignore registry version))
        (list :bound iface name)))

(defun po-config (&rest bar)
  (setf *config* (list :bar (append bar (list :enabled t :height 28 :margin 8))
                       :animations '(:enabled nil)
                       :pointer '(:warp-on-switch nil))))

;;; --- WM side: two heads, one window on each ---

(setf *wm-process-mode* nil
      *bar-process-mode* nil
      *wm-proxy* nil
      *windows* (make-hash-table)
      *outputs* (make-hash-table)
      *seats* (make-hash-table)
      *output-workspace* (make-hash-table)
      *window-workspace* (make-hash-table)
      *minimized-windows* (make-hash-table)
      *frame-trees* (make-hash-table :test 'equal)
      *current-frames* (make-hash-table :test 'equal))
(po-config)

;; HDMI-A-1 at x=0 (wl_output 10), DVI-D-1 at x=1920 (wl_output 11)
(defvar *hdmi* (make-instance 'rhizome-output :proxy :hdmi-proxy :wl-output-name 10
                              :x 0 :y 0 :width 1920 :height 1080))
(defvar *dvi* (make-instance 'rhizome-output :proxy :dvi-proxy :wl-output-name 11
                             :x 1920 :y 0 :width 1920 :height 1200))
(defvar *seat* (make-instance 'rhizome-seat :proxy :seat-proxy))
(setf (gethash :hdmi-proxy *outputs*) *hdmi*
      (gethash :dvi-proxy *outputs*) *dvi*
      (gethash :seat-proxy *seats*) *seat*
      (gethash *hdmi* *output-workspace*) 1
      (gethash *dvi* *output-workspace*) 3
      (output-current-layout *dvi*) :monocle)

(defun po-window (proxy output workspace)
  (let ((win (make-instance 'rhizome-window :proxy proxy)))
    (setf (gethash proxy *windows*) win
          (window-output win) output)
    (assign-window-workspace win workspace)
    win))

(defvar *w-hdmi* (po-window :w1 *hdmi* 1))
(defvar *w-dvi* (po-window :w2 *dvi* 3))
(setf (seat-focused-window *seat*) *w-dvi*)

(defun po-entries (snapshot &rest keys)
  (mapcar (lambda (entry)
            (mapcar (lambda (key) (getf entry key)) keys))
          (getf snapshot :outputs)))

(po-case "snapshot per output"
  (setf *wm-snapshot* nil)
  (update-wm-snapshot)
  (let ((snap *wm-snapshot*))
    (po-ok "one entry per output, left to right, keyed by wl_output"
           (po-entries snap :wl-output :x) '((10 0) (11 1920)))
    (po-ok "each output reports its own workspace and layout"
           (po-entries snap :workspace :layout)
           '((1 :master-stack) (3 :monocle)))
    (po-ok "each output counts only its own windows"
           (po-entries snap :windows) '((1) (1)))
    (po-ok "each output reports its own occupied workspaces"
           (po-entries snap :occupied) '(((1)) ((3))))
    (po-ok "the current output is marked"
           (po-entries snap :current) '((nil) (t)))
    (po-ok "every output carries a bar by default; the leftmost is primary"
           (po-entries snap :bar :primary) '((t t) (t nil)))
    (po-ok "the global workspace and layout follow the current output"
           (list (getf snap :workspace) (getf snap :layout)) '(3 :monocle))))

(po-case "restricting the bar to some outputs"
  (po-config :outputs :primary)
  (update-wm-snapshot)
  (po-ok ":outputs :primary: only the leftmost output carries a bar"
         (po-entries *wm-snapshot* :bar :primary) '((t t) (nil nil)))
  (po-config :outputs '(1))
  (update-wm-snapshot)
  (po-ok ":outputs (1): only the second output carries a bar, and is primary"
         (po-entries *wm-snapshot* :bar :primary) '((nil nil) (t t)))
  (po-config :enabled nil)
  (po-ok "bar disabled: no output is selected"
         (list (bar-output-selected-p *hdmi*) (bar-output-selected-p *dvi*))
         '(nil nil)))

;;; --- Reserved strip per output ---

(po-case "reserved strip per output"
  (po-config)
  (clrhash *bar-non-exclusive-heights*)
  (po-ok "no area reported: both outputs reserve the strip"
         (list (bar-reserved-height *hdmi*) (bar-reserved-height *dvi*))
         '(44 44))
  (bar-note-non-exclusive-area *hdmi* (- 1080 44))
  (bar-note-non-exclusive-area *dvi* 1200)
  (po-ok "an output whose bar river shows no zone for reserves nothing"
         (list (bar-reserved-height *hdmi*) (bar-reserved-height *dvi*))
         '(44 0))
  (clrhash *bar-non-exclusive-heights*)
  (po-config :outputs :primary)
  (po-ok "an output without a bar reserves nothing, even before river reports"
         (list (bar-reserved-height *hdmi*) (bar-reserved-height *dvi*))
         '(44 0))
  (clrhash *frame-trees*)
  (po-ok "frame trees fill the head without a bar"
         (list (frame-height (frame-tree-for *hdmi* 1))
               (frame-height (frame-tree-for *dvi* 3)))
         '(1036 1200))
  (po-config))

(po-case "workspace switch from a bar"
  (bar-switch-workspace 5 10)
  (po-ok "the bar's own output switches, the other stays"
         (list (current-workspace *hdmi*) (current-workspace *dvi*)) '(5 3))
  (bar-switch-workspace 1 10))

;;; --- Bar side: one surface per wl_output ---

(setf (symbol-function 'bar-snapshot) (lambda () *po-snapshot*))
(setf (symbol-function 'bar-open-surface)
      (lambda (state output)
        (let ((ostate (make-hash-table))
              (name (bar-output-global-name state output)))
          (push name *po-opened*)
          (setf (gethash :output ostate) output
                (gethash :global-name ostate) name
                (gethash :surface ostate) (list :surface name)
                (gethash :mapped ostate) t
                (gethash output *bar-output-states*) ostate))))
(setf (symbol-function 'bar-close-surface)
      (lambda (ostate)
        (push (gethash :global-name ostate) *po-closed*)
        (remhash (gethash :output ostate) *bar-output-states*)))

(defun po-root ()
  "A fresh bar root state bound to wl_outputs 10 and 11, with no bar
open."
  (clrhash *bar-output-states*)
  (clrhash *bar-surface-states*)
  (clrhash *bar-closed-at*)
  (setf *bar-pointer-ostate* nil)
  (let ((root (make-hash-table)))
    (dolist (global '((1 "wl_compositor") (2 "wl_shm") (3 "zwlr_layer_shell_v1")
                      (10 "wl_output") (11 "wl_output")))
      (bar-bind-global root :registry (first global) (second global) 4))
    root))

(defun po-ostate (root name)
  "The open ostate of the output ROOT bound as wl_output NAME, or NIL."
  (values (gethash (bar-output-named root name) *bar-output-states*)))

(defun po-snap (&key hidden (bars '(t t)))
  (list :bar-hidden hidden :workspace 3 :layout :monocle :windows 2
        :workspace-count 9
        :outputs (list (list :wl-output 10 :x 0 :workspace 1 :layout :master-stack
                             :windows 1 :occupied '(1) :bar (first bars)
                             :primary (first bars))
                       (list :wl-output 11 :x 1920 :workspace 3 :layout :monocle
                             :windows 1 :occupied '(3) :bar (second bars)
                             :primary (and (second bars) (not (first bars)))))))

(po-case "bar binds every output"
  (let ((root (po-root)))
    (po-ok "two wl_outputs are bound"
           (sort (mapcar (lambda (output) (bar-output-global-name root output))
                         (gethash :outputs root))
                 #'<)
           '(10 11))
    (po-ok "each output is found by its global name"
           (bar-output-named root 11) '(:bound :wl_output 11))
    (bar-bind-global root :registry 11 "wl_output" 4)
    (po-ok "a repeated global binds nothing more"
           (length (gethash :outputs root)) 2)))

(po-case "one surface per output"
  (let ((root (po-root)))
    (setf *po-opened* nil *po-closed* nil *po-snapshot* (po-snap))
    (bar-sync-visibility root)
    (po-ok "both outputs get a surface" (sort (copy-list *po-opened*) #'<) '(10 11))
    (po-ok "each bar knows its output's global name"
           (gethash :global-name (po-ostate root 10)) 10)
    (bar-sync-visibility root)
    (po-ok "a second sync opens nothing more" (length *po-opened*) 2)
    (setf *po-snapshot* (po-snap :bars '(t nil)))
    (bar-sync-visibility root)
    (po-ok "an output the snapshot drops loses its surface" *po-closed* '(11))
    (setf *po-snapshot* (po-snap :hidden t))
    (bar-sync-visibility root)
    (po-ok "hidden: every surface closes" (sort (copy-list *po-closed*) #'<) '(10 11))
    (setf *po-opened* nil *po-closed* nil *po-snapshot* (po-snap))
    (bar-sync-visibility root)
    (bar-forget-output root 11)
    (po-ok "a removed output closes its bar and leaves the bound outputs"
           (list *po-closed* (mapcar (lambda (output)
                                       (bar-output-global-name root output))
                                     (gethash :outputs root)))
           '((11) (10)))
    (bar-bind-global root :registry 12 "wl_output" 4)
    (bar-sync-visibility root)
    (po-ok "an added output that the WM has not published yet waits"
           (po-ostate root 12) nil)
    (setf *po-opened* nil *po-snapshot* (list :bar-hidden nil))
    (bar-sync-visibility root)
    (po-ok "a snapshot that lists no outputs: a bar on every output"
           (sort (copy-list *po-opened*) #'<) '(12))))

(po-case "compositor closes one bar"
  (let ((root (po-root))
        (*bar-running* t))
    (setf *po-opened* nil *po-closed* nil *po-snapshot* (po-snap))
    (bar-sync-visibility root)
    (bar-layer-surface-handler (po-ostate root 11) :layer-surface :closed nil)
    (po-ok "the bar keeps running" *bar-running* t)
    (po-ok "only that output's bar closes"
           (list *po-closed* (and (po-ostate root 10) t))
           '((11) t))
    (bar-sync-visibility root)
    (po-ok "the closed bar is not reopened at once" (length *po-opened*) 2)
    (setf (gethash (bar-output-named root 11) *bar-closed-at*)
          (- (get-universal-time) *bar-reopen-delay*))
    (bar-sync-visibility root)
    (po-ok "the closed bar reopens after the delay" (first *po-opened*) 11)))

;;; --- Per-output rendering ---

(po-case "modules report their own output"
  (let ((root (po-root)))
    (setf *po-snapshot* (po-snap))
    (bar-sync-visibility root)
    (flet ((view (name)
             (with-bar-ostate ((po-ostate root name))
               (list (bar-module-windows) (bar-module-layout)
                     (first (find-if (lambda (part) (getf (cddr part) :pill))
                                     (bar-module-workspaces)))))))
      (po-config)
      (po-ok "unset: the HDMI bar shows the current output"
             (view 10) '("2 wins" "monocle" "3"))
      (po-ok "unset: the DVI bar shows the same"
             (view 11) '("2 wins" "monocle" "3"))
      (po-config :per-output-content t)
      (po-ok ":per-output-content t: the HDMI bar shows HDMI"
             (view 10) '("1 win" "master-stack" "1"))
      (po-ok ":per-output-content t: the DVI bar shows DVI"
             (view 11) '("1 win" "monocle" "3"))
      (po-config))))

(po-case "tray and global modules"
  (let ((root (po-root)))
    (setf *po-snapshot* (po-snap))
    (bar-sync-visibility root)
    (flet ((segments (name)
             (with-bar-ostate ((po-ostate root name))
               (list (bar-ostate-segment-modules :left)
                     (bar-ostate-segment-modules :right)))))
      (po-config :modules '(:left (:workspaces :layout) :right (:tray :clock)))
      (po-ok "unset: the primary bar holds every module"
             (segments 10) '((:workspaces :layout) (:tray :clock)))
      (po-ok "unset: another bar holds every module, the tray included"
             (segments 11) '((:workspaces :layout) (:tray :clock)))
      (po-config :tray-output :primary
                 :modules '(:left (:workspaces :layout) :right (:tray :clock)))
      (po-ok ":tray-output :primary: another bar drops only the tray"
             (segments 11) '((:workspaces :layout) (:clock)))
      (po-ok ":tray-output :primary: the primary bar keeps the tray"
             (segments 10) '((:workspaces :layout) (:tray :clock)))
      (po-config :global-modules :primary
                 :modules '(:left (:workspaces :layout) :right (:tray :clock)))
      (po-ok ":global-modules :primary: other bars keep only per-output modules"
             (segments 11) '((:workspaces :layout) nil))
      (po-ok ":global-modules :primary: the primary bar keeps everything"
             (segments 10) '((:workspaces :layout) (:tray :clock))))
    (po-config)))

(po-case "clicks route to their own output"
  (let ((root (po-root)))
    (setf *po-snapshot* (po-snap))
    (bar-sync-visibility root)
    (let ((dvi (po-ostate root 11))
          (hdmi (po-ostate root 10)))
      (setf *po-commands* nil
            (gethash :dvi-surface *bar-surface-states*) dvi
            (gethash :hdmi-surface *bar-surface-states*) hdmi
            ;; the workspaces module, and the cell of workspace 3 in it
            (gethash :hitboxes dvi) (list (list :workspaces 0 72)
                                          (list (list :workspace-cell
                                                      (list 3 nil))
                                                16 24))
            (gethash :hitboxes hdmi) nil)
      (flet ((click (surface)
               (setf *po-commands* nil)
               (bar-pointer-event :enter (list 7 surface 20 5))
               (bar-pointer-event :button (list 8 9 272 :pressed))
               (first *po-commands*)))
        (po-config)
        (po-ok "unset: a workspace click switches the current output"
               (click :dvi-surface) "rhizome-cmd switch-workspace 3")
        (po-config :per-output-content t)
        (po-ok ":per-output-content t: a workspace click switches the clicked bar's output"
               (and (search "bar-switch-workspace 3 11" (click :dvi-surface)) t)
               t)
        (po-ok "the other bar's hitboxes are its own" (click :hdmi-surface) nil)
        (bar-pointer-event :enter (list 7 :dvi-surface 20.75 5))
        (po-ok "the pointer x is rounded at the door" *bar-pointer-x* 21)
        (bar-pointer-event :enter (list :dvi-surface 30 5))
        (po-ok ":enter without a serial still finds the bar"
               (list (eq *bar-pointer-ostate* dvi) *bar-pointer-x*) '(t 30))
        (bar-pointer-event :leave (list :dvi-surface))
        (po-config)))))

(format t "~&=== ~A passed, ~A failed ===~%" *po-pass* *po-fail*)
(uiop:quit (if (zerop *po-fail*) 0 1))
