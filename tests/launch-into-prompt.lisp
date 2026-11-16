;;; launch-into-prompt.lisp: the interactive launch: a prompt listing
;;; every frame address with what the frame shows, then a prompt for the
;;; command. Covers the candidates (every frame in workspace, screen,
;;; frame order, plus one new frame per workspace), the current frame
;;; selected first, launching into a picked occupied frame, an empty
;;; frame, and a new frame, cancelling at either prompt, that no default
;;; key opens it, and the prompt's scrolling arithmetic that keeps the
;;; selected row in view. The prompt surface is stubbed: prompt-open
;;; records its arguments.
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

(defvar *spawned* nil "Commands launch-spawn was called with, newest first.")
(defvar *prompts* nil "prompt-open calls as plists, newest first.")

(setf (symbol-function 'message)
      (lambda (fmt &rest args) (declare (ignore fmt args)) nil)
      (symbol-function 'show-message)
      (lambda (&rest args) (declare (ignore args)) nil)
      (symbol-function 'show-command-reply)
      (lambda (result &rest args) (declare (ignore args)) result)
      (symbol-function 'request-manage-dirty)
      (lambda (&rest args) (declare (ignore args)) nil)
      (symbol-function 'launch-spawn)
      (lambda (command) (push command *spawned*) 4242)
      (symbol-function 'prompt-open)
      (lambda (title &key candidates on-complete (initial "") complete-line)
        (push (list :title title :candidates candidates
                    :on-complete on-complete :initial initial
                    :complete-line complete-line)
              *prompts*)
        (setf *prompt-selection* 0)
        nil))

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
      (labels ((reset ()
                 (setf *config* (default-config))
                 (configure '(:bar (:enabled nil)))
                 (configure '(:animations (:enabled nil)))
                 (configure '(:screens (:primary "DVI-D-1")
                              :workspaces (:count 2)))
                 (setf *outputs* (make-hash-table)
                       *windows* (make-hash-table)
                       *seats* (make-hash-table)
                       *display-heads* (make-hash-table)
                       *output-workspace* (make-hash-table)
                       *window-workspace* (make-hash-table)
                       *frame-trees* (make-hash-table :test 'equal)
                       *current-frames* (make-hash-table :test 'equal)
                       *scroll-states* (make-hash-table :test 'equal)
                       *selected-output* nil
                       *launch-claims* '()
                       *spawned* nil
                       *prompts* nil)
                 (clrhash *window-launch-targets*))
               (make-output (proxy connector x)
                 (let ((out (make-instance 'rhizome-output :proxy proxy
                                           :x x :y 0
                                           :width 1920 :height 1080)))
                   (setf (gethash proxy *outputs*) out
                         (gethash out *output-workspace*) 1
                         (gethash proxy *display-heads*)
                         (list :head proxy :name connector :enabled t
                               :position (cons x 0)))
                   out))
               (window-in (proxy output workspace frame title)
                 (let ((win (make-instance 'rhizome-window :proxy proxy)))
                   (setf (gethash proxy *windows*) win
                         (window-output win) output
                         (window-title win) title)
                   (assign-window-workspace win workspace)
                   (frame-push-window win frame)
                   win))
               (prompt (n)
                 (nth n *prompts*))
               (answer (choice)
                 ;; Complete the newest prompt with CHOICE
                 (funcall (getf (prompt 0) :on-complete) choice))
               (claim-address ()
                 (let* ((claim (first (last *launch-claims*)))
                        (frame (and claim (launch-claim-frame claim))))
                   (and frame (frame-address frame)))))

        (reset)
        (let* ((dvi (make-output :dvi "DVI-D-1" 1920))
               (hdmi (make-output :hdmi "HDMI-A-1" 0))
               (root (frame-tree-for hdmi 1)))
          (declare (ignore dvi))
          (split-frame root :horizontal 1/2)
          (window-in :w1 hdmi 1 (first (frame-leaves root)) "editor")
          (setf *selected-output* hdmi
                (gethash (frame-key hdmi 1) *current-frames*)
                (second (frame-leaves root)))

          ;; --- The address prompt ---

          (ok "the command opens a prompt and replies nothing"
              (execute-command hdmi :launch-into-prompt nil) nil)
          (ok "the first prompt asks where"
              (getf (prompt 0) :title) "Launch into:")
          (ok "candidates list every frame and a new frame per workspace"
              (getf (prompt 0) :candidates)
              '("1:1:1 empty" "1:1:2 new frame"
                "1:2:1 editor" "1:2:2 empty" "1:2:3 new frame"
                "2:1:1 empty" "2:1:2 new frame"
                "2:2:1 empty" "2:2:2 new frame"))
          (ok "the current frame is selected first"
              *prompt-selection* 3)

          ;; --- Then the command ---

          (answer "1:2:1 editor")
          (ok "picking a frame asks for the command, naming the frame"
              (list (getf (prompt 0) :title) (getf (prompt 0) :candidates))
              '("Launch into 1:2:1:" nil))
          (answer "foot -a probe")
          (ok "the command starts"
              *spawned* '("foot -a probe"))
          (ok "into the picked occupied frame"
              (claim-address) "1:2:1")

          (setf *prompts* nil)
          (execute-command hdmi :launch-into-prompt nil)
          (answer "2:1:1 empty")
          (answer "kitty")
          (ok "an empty frame on another screen's hidden workspace"
              (list (claim-address) (current-workspace (screen-by-number 1)))
              '("2:1:1" 1))

          (setf *prompts* nil)
          (execute-command hdmi :launch-into-prompt nil)
          (answer "1:2:3 new frame")
          (answer "kitty")
          (ok "a new frame is made for the launch"
              (list (claim-address)
                    (length (frame-leaves (frame-tree-for hdmi 1))))
              '("1:2:3" 3))

          ;; --- Cancelling ---

          (setf *prompts* nil *spawned* nil)
          (execute-command hdmi :launch-into-prompt nil)
          (ok "opening the prompt starts nothing"
              (list (length *prompts*) *spawned*) '(1 nil))
          (answer "1:1:1 empty")
          (answer "   ")
          (ok "a command of only blanks starts nothing"
              *spawned* nil))

        ;; --- No default key ---

        (reset)
        (setup-default-prefix-bindings)
        (let ((found '()))
          (labels ((walk (map)
                     (map-keymap
                      (lambda (key binding)
                        (let ((doc (keymap-binding-doc map key))
                              (sub (binding-keymap binding)))
                          (when (and doc (search "launch into a frame" doc))
                            (push doc found))
                          (when sub (walk sub))))
                      map)))
            (walk *prefix-map*))
          (ok "no default prefix key opens the prompt" found nil))
        (ok "the command is in the command table"
            (functionp (gethash :launch-into-prompt *command-table*)) t)

        ;; --- Scrolling the candidate list ---

        (ok "eight whole 20 px rows fit a 200 px prompt below its input line"
            (prompt-visible-rows 200 20) 8)
        (ok "a selection in view scrolls nothing"
            (prompt-scroll-offset 4 9) 0)
        (ok "a selection below the last row scrolls it to the bottom row"
            (prompt-scroll-offset 12 9) 4)
        (ok "at least one row always shows"
            (prompt-visible-rows 20 20) 1)

        (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
        (sb-ext:exit :code (if (zerop fail) 0 1))))))
