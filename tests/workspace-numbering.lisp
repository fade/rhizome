;;; workspace-numbering.lisp: how workspaces are numbered, counted, and
;;; named per screen. Covers the display base (0 or 1) at the keys, the
;;; IPC, the picker, window rules, and :types; per-screen counts and
;;; names keyed by connector name; shrinking a count by config;
;;; add-workspace and remove-workspace at runtime; reload-config
;;; restoring the configured count; and the bar's per-screen cells and
;;; clicks. River requests and on-screen messages are stubbed, so
;;; nothing here needs a compositor.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))
;; No slide-in animation in a test run
(configure '(:animations (:enabled nil)))

(defvar *test-messages* nil
  "Formatted on-screen messages, most recent first.")

(defvar *test-commands* nil
  "Shell commands the bar asked to run, most recent first.")

(defmacro check (form)
  "FORM's value, or :ERROR when evaluating it signals an error."
  `(handler-case ,form
     (error (e)
       (format t "~&;   (error: ~A)~%" e)
       :error)))

(let ((pass 0) (fail 0))
  (flet ((report (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
   (macrolet ((ok (name actual expected)
                ;; An error in ACTUAL fails this check, not the run
                `(report ,name (check ,actual) ,expected)))

    (setf (symbol-function 'message)
          (lambda (fmt &rest args)
            (push (apply #'format nil fmt args) *test-messages*)
            nil)
          (symbol-function 'show-message) (lambda (&rest args)
                                            (declare (ignore args))
                                            nil)
          (symbol-function 'show-command-reply)
          (lambda (result &rest args)
            (declare (ignore args))
            result)
          (symbol-function 'request-manage-dirty) (lambda (&rest args)
                                                    (declare (ignore args))
                                                    nil)
          (symbol-function 'save-window-state)
          (lambda () (error "save-window-state must not run"))
          (symbol-function 'bar-run-command)
          (lambda (command) (push command *test-commands*) nil)
          (symbol-function 'rwm:river-seat-v1.focus-window)
          (lambda (&rest args) (declare (ignore args)) nil)
          (symbol-function 'rwm:river-seat-v1.clear-focus)
          (lambda (&rest args) (declare (ignore args)) nil)
          (symbol-function 'rwm:river-seat-v1.pointer-warp)
          (lambda (&rest args) (declare (ignore args)) nil))

    (labels ((reset (&rest workspaces-config)
               (setf *config* (default-config))
               (configure '(:bar (:enabled nil)))
               (configure '(:animations (:enabled nil)))
               (when workspaces-config
                 (configure (list :workspaces workspaces-config)))
               (when (boundp '*workspace-count-overrides*)
                 (clrhash (symbol-value '*workspace-count-overrides*)))
               (setf *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *outputs* (make-hash-table)
                     *display-heads* (make-hash-table)
                     *output-workspace* (make-hash-table)
                     *window-workspace* (make-hash-table)
                     *minimized-windows* (make-hash-table)
                     *marked-windows* (make-hash-table)
                     *frame-trees* (make-hash-table :test 'equal)
                     *current-frames* (make-hash-table :test 'equal)
                     *scroll-states* (make-hash-table :test 'equal)
                     *workspace-last-focus* (make-hash-table :test 'equal)
                     *window-state* (make-hash-table :test 'equal)
                     *pending-focus-requests* nil
                     *pending-pointer-warps* nil
                     *pending-autofocus* nil
                     *test-messages* nil
                     *test-commands* nil))
             (make-output (proxy connector x wl-name)
               ;; An output at X with an enabled output-management head
               ;; named CONNECTOR at the same position
               (let ((out (make-instance 'rhizome-output :proxy proxy
                                         :x x :y 0
                                         :width 1920 :height 1080
                                         :wl-output-name wl-name)))
                 (setf (gethash proxy *outputs*) out
                       (gethash out *output-workspace*) 1)
                 (when connector
                   (setf (gethash proxy *display-heads*)
                         (list :head proxy :name connector :enabled t
                               :position (cons x 0))))
                 out))
             (make-win (proxy output workspace)
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) output)
                 (assign-window-workspace win workspace)
                 win))
             (two-screens ()
               (values (make-output :out-a "DVI-D-1" 0 41)
                       (make-output :out-b "HDMI-A-1" 1920 42))))

      ;; --- The display base ---

      (reset)
      (ok "base 1 is the default" (check (workspace-index-base)) 1)
      (ok "base 1: workspace 3 shows as 3" (check (workspace-to-display 3)) 3)
      (ok "base 1: 3 names workspace 3" (check (workspace-from-display 3)) 3)
      (reset :index-base 0)
      (ok "base 0: workspace 1 shows as 0" (check (workspace-to-display 1)) 0)
      (ok "base 0: workspace 9 shows as 8" (check (workspace-to-display 9)) 8)
      (ok "base 0: 0 names workspace 1" (check (workspace-from-display 0)) 1)
      (ok "base 0: the string \"0\" names workspace 1"
          (check (workspace-from-display "0")) 1)
      (ok "a word is no workspace number"
          (check (workspace-from-display "web")) nil)

      ;; --- Per-screen counts keyed by connector name ---

      (reset :count 7
             :screens '(("DVI-D-1" :count 6 :names ("mail" "web"))
                        ("HDMI-A-1" :count 4)))
      (multiple-value-bind (a b) (two-screens)
        (let ((other (make-output :out-c "DP-1" 3840 43))
              (bare (make-output :out-d nil 5760 44)))
          (ok "connector name from the head at the output's position"
              (check (output-connector-name b)) "HDMI-A-1")
          (ok "DVI-D-1 has its own count" (check (workspace-count a)) 6)
          (ok "HDMI-A-1 has its own count" (check (workspace-count b)) 4)
          (ok "a screen without an entry takes the global :count"
              (check (workspace-count other)) 7)
          (ok "an output with no known connector takes the global :count"
              (check (workspace-count bare)) 7))
        (ok "a named workspace is labelled by its name"
            (check (workspace-label a 2)) "web")
        (ok "an unnamed workspace is labelled by its number"
            (check (workspace-label a 3)) "3")
        (ok "switch-workspace past the screen's count does nothing"
            (progn (check (switch-workspace 5 b)) (current-workspace b)) 1)
        (ok "switch-workspace within the screen's count switches"
            (progn (check (switch-workspace 5 a)) (current-workspace a)) 5))
      (reset)
      (ok "no config: *workspace-count* is the count"
          (check (workspace-count (make-output :out-a "DVI-D-1" 0 41)))
          *workspace-count*)

      ;; --- Keys reach display numbers 0..9 ---

      (reset :index-base 0 :screens '(("DVI-D-1" :count 6)))
      (multiple-value-bind (a b) (two-screens)
        (declare (ignore b))
        (check (switch-to-workspace-number 0 a))
        (ok "base 0: key 0 shows the first workspace" (current-workspace a) 1)
        (check (switch-to-workspace-number 5 a))
        (ok "base 0: key 5 shows the sixth workspace" (current-workspace a) 6)
        (setf *test-messages* nil)
        (check (switch-to-workspace-number 6 a))
        (ok "base 0: key 6 past a count of 6 leaves the view"
            (current-workspace a) 6)
        (ok "base 0: key 6 past the count names the range"
            (first *test-messages*) "no workspace 6: DVI-D-1 has 0-5"))
      (reset)
      (multiple-value-bind (a b) (two-screens)
        (declare (ignore b))
        (check (switch-to-workspace-number 0 a))
        (ok "base 1: key 0 reaches nothing" (current-workspace a) 1)
        (check (switch-to-workspace-number 9 a))
        (ok "base 1: key 9 shows workspace 9" (current-workspace a) 9))

      ;; --- C-t N / C-t P wrap within the screen ---

      (reset :screens '(("DVI-D-1" :count 3)))
      (multiple-value-bind (a b) (two-screens)
        (declare (ignore b))
        (switch-workspace 3 a)
        (check (switch-workspace-relative 1))
        (ok "next from the last workspace wraps to the first"
            (current-workspace a) 1)
        (check (switch-workspace-relative -1))
        (ok "previous from the first wraps to the last"
            (current-workspace a) 3))

      ;; --- IPC speaks display numbers, integer or string ---

      (reset :index-base 0 :screens '(("DVI-D-1" :count 6)
                                      ("HDMI-A-1" :count 4)))
      (multiple-value-bind (a b) (two-screens)
        ;; IPC commands act on the first output unless one is named
        (let ((first-out (first (all-outputs))))
          (check (ipc-dispatch-line "switch-workspace 0"))
          (ok "ipc: switch-workspace 0 under base 0 shows workspace 1"
              (current-workspace first-out) 1)
          (check (execute-command first-out :switch-workspace '("2")))
          (ok "a string argument works as an integer one"
              (current-workspace first-out) 3)
          (ok "ipc: current-workspace answers in display numbers"
              (check (ipc-dispatch-line "current-workspace")) "2"))
        (check (ipc-dispatch-line "switch-workspace 3 HDMI-A-1"))
        (ok "ipc: switch-workspace N CONNECTOR switches that screen"
            (current-workspace b) 4)
        (ok "ipc: current-workspace CONNECTOR answers for that screen"
            (check (ipc-dispatch-line "current-workspace HDMI-A-1")) "3")
        (ok "ipc: a number past the screen's count is refused"
            (check (ipc-dispatch-line "switch-workspace 4 HDMI-A-1"))
            "no workspace 4: HDMI-A-1 has 0-3")
        (ok "ipc: an unknown connector is named back"
            (check (ipc-dispatch-line "switch-workspace 1 VGA-9"))
            "no output VGA-9")
        (ok "ipc: the first screen keeps its own view" (current-workspace a)
            (if (eq a (first (all-outputs))) 3 1)))

      ;; --- Names: picker and IPC take a name as well as a number ---

      (reset :index-base 0 :screens '(("DVI-D-1" :count 4
                                       :names ("mail" "web"))))
      (multiple-value-bind (a b) (two-screens)
        (declare (ignore b))
        (let ((first-out (first (all-outputs))))
          (when (eq first-out a)
            (check (ipc-dispatch-line "switch-workspace web"))
            (ok "ipc: a workspace name switches to it"
                (current-workspace a) 2)))
        (ok "names resolve on their screen"
            (check (workspace-from-designator "MAIL" a)) 1)
        (ok "numbers resolve in display numbering"
            (check (workspace-from-designator "3" a)) 4)
        (let ((candidates nil) (complete nil))
          (setf (symbol-function 'prompt-open)
                (lambda (title &key ((:candidates offered)) on-complete
                         &allow-other-keys)
                  (declare (ignore title))
                  (setf candidates offered
                        complete on-complete)))
          (switch-workspace 1 (first (all-outputs)))
          (check (workspace-list))
          (when (eq (first (all-outputs)) a)
            (ok "picker: display numbers with names, current starred"
                candidates '("0 mail *" "1 web" "2" "3"))
            (when complete
              (funcall complete "web")
              (ok "picker: choosing a name switches" (current-workspace a) 2)
              (funcall complete "3 ")
              (ok "picker: typing a number switches" (current-workspace a) 4)))))

      ;; --- Window rules and :types take display numbers ---

      (reset :index-base 0 :count 6 :types '(0 :floating))
      (multiple-value-bind (a b) (two-screens)
        (declare (ignore b))
        (ok ":types keys are display numbers"
            (check (workspace-type a 1)) :floating)
        (configure '(:window-rules ((:match "mail" :workspace 2)
                                    (:match "far" :workspace 8))))
        (let ((w (make-instance 'rhizome-window :proxy :w-rule)))
          (setf (gethash :w-rule *windows*) w
                (window-output w) a
                (window-app-id w) "mail")
          (check (apply-window-rules w))
          (ok "rule :workspace 2 under base 0 is workspace 3"
              (window-workspace w) 3))
        (let ((w (make-instance 'rhizome-window :proxy :w-far)))
          (setf (gethash :w-far *windows*) w
                (window-output w) a
                (window-app-id w) "far")
          (check (apply-window-rules w))
          (ok "a rule past the screen's count places nothing"
              (window-workspace w) nil)))

      ;; --- Shrinking a count by config ---

      (reset :screens '(("DVI-D-1" :count 6)))
      (multiple-value-bind (a b) (two-screens)
        (let ((w5 (make-win :w5 a 5))
              (w6 (make-win :w6 a 6))
              (w2 (make-win :w2 a 2))
              (wb (make-win :wb b 6)))
          (switch-workspace 6 a)
          (let ((log (with-output-to-string (*standard-output*)
                       (check (configure '(:workspaces
                                           (:screens (("DVI-D-1" :count 4)))))))))
            (ok "shrink: windows past the count go to the last workspace"
                (mapcar #'window-workspace (list w5 w6 w2)) '(4 4 2))
            (ok "shrink: the screen shows its last workspace"
                (current-workspace a) 4)
            (ok "shrink: the other screen is untouched"
                (window-workspace wb) 6)
            (ok "shrink: one log line"
                (count-if (lambda (line) (search "workspaces:" line))
                          (uiop:split-string log :separator '(#\newline)))
                1))))

      ;; --- A saved placement past the count lands on the last workspace ---

      (reset :screens '(("DVI-D-1" :count 4)))
      (multiple-value-bind (a b) (two-screens)
        (declare (ignore b))
        (setf (gethash "saved-id" *window-state*) '(:workspace 8 :float nil))
        (let ((w (make-instance 'rhizome-window :proxy :w-saved)))
          (setf (gethash :w-saved *windows*) w
                (window-output w) a
                (window-identifier w) "saved-id")
          (check (restore-window-state w))
          (ok "restore: a saved workspace past the count is the last one"
              (window-workspace w) 4)
          (ok "restore: the saved placement is left as it was"
              (getf (gethash "saved-id" *window-state*) :workspace) 8)))

      ;; --- add-workspace and remove-workspace at runtime ---

      (reset :screens '(("DVI-D-1" :count 6) ("HDMI-A-1" :count 4)))
      (multiple-value-bind (a b) (two-screens)
        (check (add-workspace b))
        (ok "add-workspace: one more workspace on that screen"
            (workspace-count b) 5)
        (ok "add-workspace: the other screen keeps its count"
            (workspace-count a) 6)
        (let ((w (make-win :w-last b 5)))
          (switch-workspace 5 b)
          (check (remove-workspace b))
          (ok "remove-workspace: one fewer workspace" (workspace-count b) 4)
          (ok "remove-workspace: its windows go to the workspace before it"
              (window-workspace w) 4)
          (ok "remove-workspace: the screen shows that workspace"
              (current-workspace b) 4))
        (check (ipc-dispatch-line "add-workspace HDMI-A-1"))
        (ok "ipc: add-workspace CONNECTOR" (workspace-count b) 5)
        (check (ipc-dispatch-line "remove-workspace HDMI-A-1"))
        (ok "ipc: remove-workspace CONNECTOR" (workspace-count b) 4)
        (dotimes (i 6) (check (remove-workspace a)))
        (ok "remove-workspace keeps one workspace" (workspace-count a) 1)
        (ok "a configure call keeps a runtime count"
            (progn (configure '(:gaps (:inner 4))) (workspace-count a)) 1)

        ;; --- reload-config gives the config count back ---

        (let ((saved (symbol-function 'load-user-config)))
          (setf (symbol-function 'load-user-config)
                (lambda ()
                  (configure '(:bar (:enabled nil)
                               :animations (:enabled nil)
                               :workspaces (:screens (("DVI-D-1" :count 6)
                                                      ("HDMI-A-1" :count 4)))))))
          (check (add-workspace b))
          (check (reload-config))
          (setf (symbol-function 'load-user-config) saved))
        (ok "reload-config restores DVI-D-1's config count"
            (workspace-count a) 6)
        (ok "reload-config restores HDMI-A-1's config count"
            (workspace-count b) 4))

      ;; --- The snapshot and the bar, per screen ---

      (reset :index-base 0 :screens '(("DVI-D-1" :count 3
                                       :names ("mail" "web"))
                                      ("HDMI-A-1" :count 2)))
      (multiple-value-bind (a b) (two-screens)
        (make-win :wb b 2)
        (switch-workspace 2 a)
        (let* ((*wm-snapshot* nil)
               (snapshot (progn (setf (symbol-function 'bar-write-snapshot)
                                      (lambda (s) (declare (ignore s)) nil))
                                (check (update-wm-snapshot))
                                *wm-snapshot*))
               (screens (and (listp snapshot) (getf snapshot :screens)))
               (hdmi (find "HDMI-A-1" screens
                           :key (lambda (s) (getf s :connector))
                           :test #'equal)))
          (ok "snapshot: one entry per screen" (length screens) 2)
          (ok "snapshot: a screen's labels use its names and the base"
              (getf (find "DVI-D-1" screens
                          :key (lambda (s) (getf s :connector))
                          :test #'equal)
                    :labels)
              '("mail" "web" "2"))
          (ok "snapshot: occupancy is per screen" (getf hdmi :occupied) '(2))
          (ok "snapshot: the bar finds its screen by wl_output name"
              (getf hdmi :wl-name) 42)
          ;; The bar on HDMI-A-1: one cell per workspace of that screen
          (setf (symbol-function 'bar-snapshot) (lambda () snapshot))
          (let ((*bar-state* (make-hash-table)))
            (setf (gethash :output-global-name *bar-state*) 42)
            (ok "bar: one cell per workspace of its own screen"
                (mapcar #'first (check (bar-module-workspaces)))
                '("0" "1"))
            (setf (gethash :output-global-name *bar-state*) 41)
            (let ((parts (check (bar-module-workspaces))))
              (ok "bar: a name replaces the number in its cell"
                  (mapcar #'first parts) '("mail" "web" "2"))
              ;; Draw with fixed-width text and read back the cells
              (setf (symbol-function 'bar-draw-text)
                    (lambda (&rest args) (declare (ignore args)) nil)
                    (symbol-function 'bar-text-width)
                    (lambda (text) (* 8 (length text)))
                    (symbol-function 'font-line-height) (lambda () 16)
                    (symbol-function 'blend-pixel)
                    (lambda (&rest args) (declare (ignore args)) nil)
                    *bar-hitboxes* nil)
              (check (bar-draw-module (cffi:null-pointer) 0 0 100 0
                                      parts 0 0 0 :workspaces))
              ;; "mail" spans 100..132, gap 8: "web" starts at 140
              (check (bar-handle-click 150))
              (ok "bar: a click on a named cell switches to it on its screen"
                  (first *test-commands*)
                  "rhizome-cmd switch-workspace 1 DVI-D-1")
              (check (bar-handle-click 101))
              (ok "bar: a click on the first cell"
                  (first *test-commands*)
                  "rhizome-cmd switch-workspace 0 DVI-D-1")
              (check (bar-handle-click 172))
              (ok "bar: cells follow the drawn widths"
                  (first *test-commands*)
                  "rhizome-cmd switch-workspace 2 DVI-D-1")))))

      ;; --- Messages show display labels ---

      (reset :index-base 0 :screens '(("DVI-D-1" :count 3 :names ("mail"))))
      (multiple-value-bind (a b) (two-screens)
        (declare (ignore b))
        (switch-workspace 2 a)
        (setf *test-messages* nil)
        (switch-workspace 1 a)
        (ok "switch message names the workspace by its label"
            (first *test-messages*) "workspace mail")
        (switch-workspace 2 a)
        (ok "switch message shows the display number"
            (first *test-messages*) "workspace 1")))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1)))))
