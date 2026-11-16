;;; transpose-frame.lisp: moving the focused frame's window stack to a
;;; frame named by a W:S:F address. Covers a swap between two occupied
;;; frames (stacks exchanged, order kept, focus follows the focused
;;; window); a swap with an empty target on another screen; a swap from
;;; an empty source; an overlay onto an occupied target (source stack on
;;; top, the emptied source kept under a user split and pruned under an
;;; automatic one, with :always pruning a user-made frame too); an
;;; overlay from an empty source, refused; the source as its own target,
;;; a no-op; a target on a hidden workspace, which that output then
;;; shows; a frame number past the target workspace's count; the
;;; candidate lists with and without --all; the popup and its toggle
;;; row; and that every frame's address stays current. The prompt
;;; surface is stubbed: prompt-open records its arguments.
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

(defvar *said* nil "On-screen messages, newest first.")
(defvar *prompts* nil "prompt-open calls as plists, newest first.")

(setf (symbol-function 'message)
      (lambda (fmt &rest args) (push (apply #'format nil fmt args) *said*) nil)
      (symbol-function 'show-message)
      (lambda (&rest args) (declare (ignore args)) nil)
      (symbol-function 'request-manage-dirty)
      (lambda (&rest args) (declare (ignore args)) nil)
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
      (let (dvi hdmi)
        (labels ((reset (&rest config)
                   (setf *config* (default-config))
                   (configure '(:bar (:enabled nil)))
                   (configure '(:animations (:enabled nil)))
                   (configure '(:screens (:primary "DVI-D-1")
                                :workspaces (:count 2)))
                   (when config
                     (configure config))
                   (setf *outputs* (make-hash-table)
                         *windows* (make-hash-table)
                         *seats* (make-hash-table)
                         *display-heads* (make-hash-table)
                         *output-workspace* (make-hash-table)
                         *window-workspace* (make-hash-table)
                         *frame-trees* (make-hash-table :test 'equal)
                         *current-frames* (make-hash-table :test 'equal)
                         *scroll-states* (make-hash-table :test 'equal)
                         *workspace-last-focus* (make-hash-table :test 'equal)
                         *selected-output* nil
                         *launch-claims* '()
                         *said* nil
                         *prompts* nil)
                   (setf (gethash :seat *seats*)
                         (make-instance 'rhizome-seat :proxy :seat))
                   (setf dvi (make-output :dvi "DVI-D-1" 1920)
                         hdmi (make-output :hdmi "HDMI-A-1" 0)
                         *selected-output* dvi))
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
                 (win (title output workspace frame)
                   ;; A window pushed on top of FRAME's stack
                   (let* ((proxy (intern (string-upcase title) :keyword))
                          (w (make-instance 'rhizome-window :proxy proxy)))
                     (setf (gethash proxy *windows*) w
                           (window-output w) output
                           (window-title w) title)
                     (assign-window-workspace w workspace)
                     (frame-push-window w frame)
                     w))
                 (named (title)
                   (gethash (intern (string-upcase title) :keyword) *windows*))
                 (focus (title)
                   (focus-window (named title) (gethash :seat *seats*)))
                 (leaf (output workspace n)
                   (nth n (frame-leaves (frame-tree-for output workspace))))
                 (split-two (output workspace &key user)
                   ;; Two side-by-side leaves; USER marks the split as one
                   ;; the user made, which :auto pruning keeps
                   (let ((root (frame-tree-for output workspace)))
                     (split-frame root :horizontal 1/2)
                     (setf (frame-user-split root) user)
                     (frame-leaves root)))
                 (stack (frame)
                   (mapcar #'window-title (frame-windows frame)))
                 (place (title)
                   ;; Where the window sits: its frame's address, and
                   ;; whether that frame lists it
                   (let* ((w (named title))
                          (frame (window-frame w)))
                     (list (and frame (frame-address frame))
                           (and frame (member w (frame-windows frame)) t))))
                 (focused ()
                   (let ((w (focused-window)))
                     (and w (window-title w))))
                 (addresses-current-p ()
                   ;; Every leaf's stored address matches its place
                   (every (lambda (row)
                            (destructuring-bind (address frame output workspace)
                                row
                              (let ((n (position frame (frame-leaves
                                                        (frame-tree-for
                                                         output workspace)))))
                                (string= address
                                         (format nil "~D:~D:~D" workspace
                                                 (screen-number output)
                                                 (1+ n))))))
                          (all-frame-addresses)))
                 (transpose (&rest words)
                   (execute-command (current-output) :transpose-frame words))
                 (prompt (n)
                   (nth n *prompts*))
                 (answer (choice)
                   (funcall (getf (prompt 0) :on-complete) choice)))

          ;; --- Swap: two occupied frames ---

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "a2" dvi 1 a) (win "a1" dvi 1 a)
            (win "b2" dvi 1 b) (win "b1" dvi 1 b)
            (focus "a1")
            (ok "control: before, each frame holds its own stack"
                (list (stack a) (stack b)) '(("a1" "a2") ("b1" "b2")))
            (ok "a swap replies with both addresses"
                (transpose "1:1:2") "swapped 1:1:1 with 1:1:2")
            (ok "the stacks are exchanged, order kept"
                (list (stack a) (stack b)) '(("b1" "b2") ("a1" "a2")))
            (ok "each window's frame lists it"
                (mapcar #'place '("a1" "a2" "b1" "b2"))
                '(("1:1:2" t) ("1:1:2" t) ("1:1:1" t) ("1:1:1" t)))
            (ok "focus follows the focused window"
                (list (focused) (frame-address (current-frame-for dvi 1)))
                '("a1" "1:1:2"))
            (ok "addresses stay current after a swap" (addresses-current-p) t))

          ;; --- Swap: an empty target on another screen ---

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "a1" dvi 1 a) (win "b1" dvi 1 b)
            (focus "a1")
            (ok "a swap with an empty frame on screen 2"
                (transpose "1:2:1") "swapped 1:1:1 with 1:2:1")
            (ok "moves the stack there, onto that screen"
                (list (place "a1") (eq (window-output (named "a1")) hdmi)
                      (window-workspace (named "a1")))
                '(("1:2:1" t) t 1))
            (ok "the emptied source under a user split stays"
                (list (length (frame-leaves (frame-tree-for dvi 1)))
                      (stack (leaf dvi 1 0)))
                '(2 nil))
            (ok "focus follows to the other screen"
                (list (focused) (eq (current-output) hdmi)) '("a1" t))
            (ok "addresses stay current after a cross-screen swap"
                (addresses-current-p) t))

          ;; --- Swap: an empty source ---

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "b2" dvi 1 b) (win "b1" dvi 1 b)
            (setf (gethash (frame-key dvi 1) *current-frames*) a)
            (ok "a swap from an empty frame"
                (transpose "1:1:2") "swapped 1:1:1 with 1:1:2")
            (ok "brings the target's stack into the source"
                (list (stack a) (stack b)) '(("b1" "b2") nil))
            (ok "and focus stays in the source frame, on the arrival"
                (list (focused) (frame-address (current-frame-for dvi 1)))
                '("b1" "1:1:1")))

          ;; --- Overlay onto an occupied target ---

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "a2" dvi 1 a) (win "a1" dvi 1 a)
            (win "b2" dvi 1 b) (win "b1" dvi 1 b)
            (focus "a1")
            (ok "an overlay replies with both addresses"
                (transpose "--overlay" "1:1:2") "overlaid 1:1:1 onto 1:1:2")
            (ok "the source stack goes on top, the target's underneath"
                (list (stack a) (stack b)) '(nil ("a1" "a2" "b1" "b2")))
            (ok "the emptied source under a user split stays"
                (length (frame-leaves (frame-tree-for dvi 1))) 2)
            (ok "focus follows the focused window"
                (list (focused) (place "a1")) '("a1" ("1:1:2" t)))
            (ok "addresses stay current after an overlay"
                (addresses-current-p) t))

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user nil)
            (win "a1" dvi 1 a) (win "b1" dvi 1 b)
            (focus "a1")
            (transpose "--overlay" "1:1:2")
            (ok "an emptied source a new window split open is pruned"
                (length (frame-leaves (frame-tree-for dvi 1))) 1)
            (ok "and the target takes its place and its address"
                (list (stack (leaf dvi 1 0)) (place "b1"))
                '(("a1" "b1") ("1:1:1" t)))
            (ok "addresses stay current after pruning"
                (addresses-current-p) t))

          (reset :frames '(:prune-empty :always))
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "a1" dvi 1 a) (win "b1" dvi 1 b)
            (focus "a1")
            (transpose "--overlay" "1:1:2")
            (ok "under :always even a user-made source frame is pruned"
                (length (frame-leaves (frame-tree-for dvi 1))) 1))

          ;; --- Refusals ---

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "b1" dvi 1 b)
            (setf (gethash (frame-key dvi 1) *current-frames*) a)
            (ok "an overlay from an empty frame replies nothing"
                (transpose "--overlay" "1:1:2") nil)
            (ok "and says why"
                (first *said*)
                "transpose-frame: nothing to overlay: the focused frame is empty")
            (ok "and nothing moves"
                (list (stack a) (stack b)) '(nil ("b1"))))

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "a1" dvi 1 a) (win "b1" dvi 1 b)
            (focus "a1")
            (setf *said* nil)
            (ok "the focused frame as its own target replies nothing"
                (transpose "1:1:1") nil)
            (ok "and says so"
                (first *said*) "transpose-frame: the target is the focused frame")
            (ok "\".\" is the focused frame too"
                (transpose "--overlay" ".") nil)
            (ok "and nothing moves"
                (list (stack a) (stack b) (focused)) '(("a1") ("b1") "a1"))
            (setf *said* nil)
            (ok "an unknown flag replies nothing"
                (transpose "--move" "1:1:2") nil)
            (ok "and gives the usage"
                (first *said*)
                "transpose-frame: usage: transpose-frame [--overlay] [--all] [TARGET]")
            (ok "a word that is no target replies nothing"
                (transpose "firefox") nil)
            (ok "and says so"
                (first *said*) "transpose-frame: not a frame address: firefox"))

          ;; --- A target on a hidden workspace ---

          (reset)
          (let ((a (frame-tree-for dvi 1))
                (c (frame-tree-for dvi 2)))
            (win "a1" dvi 1 a)
            (win "c1" dvi 2 c)
            (focus "a1")
            (ok "control: workspace 2 is hidden"
                (current-workspace dvi) 1)
            (ok "a swap with a frame of a hidden workspace"
                (transpose "2:1:1") "swapped 1:1:1 with 2:1:1")
            (ok "the screen shows the target workspace, focus following"
                (list (current-workspace dvi) (focused)) '(2 "a1"))
            (ok "the moved stack belongs to the shown workspace"
                (list (window-workspace (named "a1")) (place "a1"))
                '(2 ("2:1:1" t)))
            (ok "the target's stack now sits on the hidden workspace"
                (list (window-workspace (named "c1")) (place "c1")
                      (eql (window-workspace (named "c1"))
                           (current-workspace dvi)))
                '(1 ("1:1:1" t) nil))
            (ok "addresses stay current across workspaces"
                (addresses-current-p) t))

          ;; --- A frame past the target workspace's count ---

          (reset)
          (let ((a (frame-tree-for dvi 1)))
            (win "a1" dvi 1 a)
            (focus "a1")
            (ok "a frame one past the count is made, as launch-into makes it"
                (transpose "1:2:2") "swapped 1:1:1 with 1:2:2")
            (ok "the target screen now has two frames, the stack in the second"
                (list (length (frame-leaves (frame-tree-for hdmi 1)))
                      (place "a1"))
                '(2 ("1:2:2" t)))
            (setf *said* nil)
            (ok "five past the count is refused"
                (transpose "1:2:8") nil)
            (ok "with launch-into's message"
                (first *said*)
                "transpose-frame: no frame 8: 1:2 has 2 frames; at most 4 more can be made")
            (ok "addresses stay current after frames are made"
                (addresses-current-p) t))

          ;; --- Candidates and the popup ---

          (reset)
          (destructuring-bind (a b) (split-two dvi 1 :user t)
            (win "a1" dvi 1 a) (win "b1" dvi 1 b)
            (win "h1" hdmi 2 (frame-tree-for hdmi 2))
            (focus "a1")
            (ok "default candidates: the shown workspaces, not the source"
                (mapcar #'first (transpose-frame-candidates))
                '("1:1:2 b1" "1:1:3 new frame" "1:2:1 empty" "1:2:2 new frame"))
            (ok "with all, hidden workspaces too"
                (mapcar #'first (transpose-frame-candidates :all t))
                '("1:1:2 b1" "1:1:3 new frame" "1:2:1 empty" "1:2:2 new frame"
                  "2:1:1 empty" "2:1:2 new frame" "2:2:1 h1" "2:2:2 new frame"))
            (ok "with no target the command opens the popup and replies nothing"
                (transpose) nil)
            (ok "titled for a swap, listing shown workspaces and the toggle row"
                (list (getf (prompt 0) :title) (getf (prompt 0) :candidates))
                '("Swap frame with:"
                  ("1:1:2 b1" "1:1:3 new frame" "1:2:1 empty" "1:2:2 new frame"
                   "[all workspaces]")))
            (answer "[all workspaces]")
            (ok "the toggle row reopens it with every workspace"
                (list (getf (prompt 0) :title)
                      (car (last (getf (prompt 0) :candidates)))
                      (length (getf (prompt 0) :candidates)))
                '("Swap frame with (all workspaces):" "[shown workspaces]" 9))
            (answer "[shown workspaces]")
            (ok "and toggles back"
                (length (getf (prompt 0) :candidates)) 5)
            (setf *prompts* nil)
            (transpose "--all" "--overlay")
            (ok "--all and --overlay open the overlay popup with every workspace"
                (list (getf (prompt 0) :title)
                      (length (getf (prompt 0) :candidates)))
                '("Overlay frame onto (all workspaces):" 9))
            (answer "2:2:1 h1")
            (ok "picking a row overlays the stack onto that frame"
                (list (stack (frame-tree-for hdmi 2)) (focused)
                      (current-workspace hdmi))
                '(("a1" "h1") "a1" 2))
            (ok "addresses stay current after a popup pick"
                (addresses-current-p) t))

          ;; --- Command table, no default key ---

          (reset)
          (setup-default-prefix-bindings)
          (let ((found '()))
            (labels ((walk (map)
                       (map-keymap
                        (lambda (key binding)
                          (let ((sub (binding-keymap binding)))
                            (when (search "transpose"
                                          (string-downcase
                                           (princ-to-string binding)))
                              (push key found))
                            (when sub (walk sub))))
                        map)))
              (walk *prefix-map*))
            (ok "no default prefix key runs it" found nil))
          (ok "the command is in the command table"
              (functionp (gethash :transpose-frame *command-table*)) t)

          (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
          (sb-ext:exit :code (if (zerop fail) 0 1)))))))
