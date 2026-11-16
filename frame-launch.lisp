;;; Launching a program into a chosen frame
;;;
;;; launch-into-frame starts a shell command and records a claim on one
;;; frame of a workspace on an output, or on the workspace alone. The
;;; first window registered afterwards whose process is the launched
;;; one, or a descendant of it, takes the claim: it opens on that output
;;; and workspace, and frame adoption places it in that frame, split
;;; first when occupied as (:tiling :auto-split) says, or, with no frame
;;; claimed, where the placement policy says. The workspace need not be
;;; shown, and no workspace switches unless the caller asks. A claim
;;; that no window takes within *launch-claim-seconds* lapses.
;;;
;;; Windows match by river's unreliable pid. A client that hands its
;;; window to a process already running (a terminal server, a browser
;;; with an open session) takes no claim, and its window opens where
;;; any new window does.
;;;
;;;   rhizome-cmd launch-into 2:1:3 COMMAND...   frame 3, workspace 2, screen 1
;;;   rhizome-cmd launch-into 2:1: COMMAND...    the placement policy's frame
;;;   rhizome-cmd launch-into LABEL COMMAND...   the frame C-t f labels LABEL
;;;   rhizome-cmd launch-into . COMMAND...       the focused frame
;;;   rhizome-cmd launch-into COMMAND...         the focused frame
;;;
;;; A frame number past the workspace's frame count makes the missing
;;; frames by splitting the largest frame along its longer axis, at most
;;; *launch-frames-max-new* of them. A frame a pending claim waits for
;;; stays through pruning until the claim is taken or lapses.
;;;
;;; launch-into-prompt asks for a frame address, then for the command.

(in-package #:rhizome)

(defparameter *launch-claim-seconds* 30
  "Seconds a launch claim waits for its window before it lapses.")

(defstruct launch-claim
  "A pending launch: the first window of process PID, or of a
descendant, registered after window registration number AFTER, opens
in FRAME of WORKSPACE on OUTPUT. WINDOW is that window once matched."
  (command "" :type string)
  (pid nil)
  (output nil)
  (workspace 1)
  (frame nil)
  (after 0 :type integer)
  (expires 0 :type integer)
  (window nil))

(defvar *launch-claims* '()
  "Pending launch claims, oldest first.")

(defvar *window-launch-targets* (make-hash-table :test #'eq)
  "WINDOW -> the launch claim it took, until the window enters a frame.")

(defvar *process-parent-function* 'process-parent-pid
  "Function of a pid returning its parent pid, or NIL when unknown.")

;;; --- Process ancestry ---

(defun process-parent-pid (pid)
  "The parent pid of PID read from /proc, or NIL when unreadable."
  (handler-case
      (with-open-file (in (format nil "/proc/~D/stat" pid))
        (let* ((line (read-line in))
               (end (position #\) line :from-end t)))
          ;; After the parenthesized command name: state, then ppid
          (and end
               (parse-integer
                (second (uiop:split-string
                         (string-trim " " (subseq line (1+ end)))
                         :separator " "))
                :junk-allowed t))))
    (error () nil)))

(defun pid-descends-from-p (pid ancestor)
  "True when PID is ANCESTOR or one of its descendants, following
*process-parent-function* at most 64 steps up."
  (loop for step below 64
        for p = pid then (funcall *process-parent-function* p)
        while (and (integerp p) (> p 1))
        when (eql p ancestor)
          return t))

;;; --- Claims ---

(defun launch-spawn (command)
  "Start COMMAND, a shell command line, through bash -c with no
connection to rhizome's streams. Returns its pid."
  (uiop:process-info-pid
   (uiop:launch-program (list "bash" "-c" command)
                        :input nil :output nil :error-output nil)))

(defun frame-launch-pending-p (frame)
  "True when a launch claim that has not lapsed waits for FRAME."
  (let ((now (get-universal-time)))
    (and frame
         (some (lambda (claim)
                 (and (eq (launch-claim-frame claim) frame)
                      (>= (launch-claim-expires claim) now)))
               *launch-claims*)
         t)))

(defun frame-pending-launch-command (frame)
  "The command of the oldest launch claim that has not lapsed and waits
for FRAME, or NIL."
  (let ((now (get-universal-time)))
    (and frame
         (let ((claim (find-if (lambda (claim)
                                 (and (eq (launch-claim-frame claim) frame)
                                      (>= (launch-claim-expires claim) now)))
                               *launch-claims*)))
           (and claim (launch-claim-command claim))))))

(defun launch-into-frame (command output workspace frame &key switch)
  "Start COMMAND, a shell command line, so that its first window opens
on WORKSPACE of OUTPUT: in FRAME, a leaf of that workspace's frame
tree, or, when FRAME is NIL, in the frame the placement policy picks.
With SWITCH, OUTPUT shows WORKSPACE first and FRAME, when given, takes
focus (focus-frame); without it no workspace or focus changes. Returns
the launch-claim, or NIL with nothing started when FRAME is not a leaf
of that tree or the command cannot start."
  (when (or (null frame)
            (member frame (frame-leaves (frame-tree-for output workspace))
                    :test #'eq))
    (when switch
      (unless (eql (current-workspace output) workspace)
        (switch-workspace workspace output))
      (setf *selected-output* output)
      (when frame
        (focus-frame output workspace frame)))
    (let ((claim (make-launch-claim
                  :command command :output output :workspace workspace
                  :frame frame :after *window-registration-count*
                  :expires (+ (get-universal-time) *launch-claim-seconds*))))
      (handler-case
          (progn
            (setf (launch-claim-pid claim) (launch-spawn command))
            (setf *launch-claims* (append *launch-claims* (list claim)))
            claim)
        (error (e)
          (format t "~&; Rhizome: launch-into-frame ~S failed: ~A~%"
                  command e)
          nil)))))

(defun launch-target-assert (win claim)
  "Put WIN on CLAIM's output and workspace."
  (setf (window-output win) (launch-claim-output claim)
        (window-minimized-p win) nil)
  (assign-window-workspace win (launch-claim-workspace claim)))

(defun launch-claim-match (win)
  "Give WIN the oldest live claim whose process WIN's pid descends from,
when WIN registered after the claim was made. WIN moves to the claim's
output and workspace and keeps the claim for frame adoption. Lapsed
claims are dropped. Returns the claim, or NIL."
  (let ((now (get-universal-time))
        (pid (window-unreliable-pid win)))
    (setf *launch-claims*
          (remove-if (lambda (claim) (< (launch-claim-expires claim) now))
                     *launch-claims*))
    (let ((claim (and (integerp pid)
                      (find-if (lambda (claim)
                                 (and (> (window-registration win)
                                         (launch-claim-after claim))
                                      (integerp (launch-claim-pid claim))
                                      (pid-descends-from-p
                                       pid (launch-claim-pid claim))))
                               *launch-claims*))))
      (when claim
        (setf *launch-claims* (remove claim *launch-claims*)
              (launch-claim-window claim) win
              (gethash win *window-launch-targets*) claim)
        (launch-target-assert win claim))
      claim)))

(defun launch-targets-assert ()
  "Apply each pending launch target again while its window sits in no
frame, so a window rule or remembered placement applied after the
match leaves the window on its launch output and workspace. Floating
windows are applied once and forgotten; closed windows are forgotten."
  (loop for win being the hash-keys of *window-launch-targets*
          using (hash-value claim)
        do (cond ((not (window-alive-p win))
                  (remhash win *window-launch-targets*))
                 ((window-floating-p win)
                  (launch-target-assert win claim)
                  (remhash win *window-launch-targets*))
                 ((null (window-frame win))
                  (launch-target-assert win claim)))))

(defun launch-target-frame (win output workspace)
  "The frame WIN was launched into when it is still a leaf of
WORKSPACE's tree on OUTPUT, else NIL. Forgets WIN's launch target."
  (let ((claim (gethash win *window-launch-targets*)))
    (when claim
      (remhash win *window-launch-targets*)
      (let ((frame (launch-claim-frame claim)))
        (and (eq output (launch-claim-output claim))
             (eql workspace (launch-claim-workspace claim))
             (member frame (frame-leaves (frame-tree-for output workspace))
                     :test #'eq)
             frame)))))

;;; --- Addressing by label ---

(defun frame-target-by-label (char)
  "The frame C-t f labels CHAR, as (VALUES OUTPUT WORKSPACE FRAME), or
NIL when no frame carries that label."
  (let ((target (find char (fselect-targets) :key #'first)))
    (when target
      (values (second target) (third target) (fourth target)))))

(defparameter *launch-frames-max-new* 4
  "Most frames launch-into makes to reach a frame number past a
workspace's frame count.")

(defun split-largest-frame (output workspace)
  "Split the largest leaf of WORKSPACE's tree on OUTPUT in half along
its longer axis, the first in layout order among equals. Its windows
move to the first half, and the split counts as user-made, so its
empty half survives the :auto prune policy. Returns the second half, or
NIL with nothing changed when that leaf is too small to split."
  (let* ((leaves (frame-leaves (frame-tree-for output workspace)))
         (largest (reduce (lambda (a b)
                            (if (> (* (frame-width b) (frame-height b))
                                   (* (frame-width a) (frame-height a)))
                                b
                                a))
                          leaves)))
    (when (frame-splittable-p largest)
      (let ((second (split-frame largest
                                 (if (>= (frame-width largest)
                                         (frame-height largest))
                                     :horizontal
                                     :vertical)
                                 1/2))
            (first (frame-first-child largest)))
        (dolist (win (frame-windows largest))
          (setf (window-frame win) first))
        (setf (frame-windows first) (frame-windows largest)
              (frame-windows largest) nil
              (frame-user-split largest) t)
        second))))

(defun launch-frame-at (output workspace ordinal)
  "The leaf at zero-based ORDINAL of WORKSPACE's tree on OUTPUT. When
ORDINAL is past the leaf count, split-largest-frame makes frames until
it exists. Returns the frame, or NIL and the reply text when that would
take more than *launch-frames-max-new* frames or a frame is too small
to split."
  (let* ((count (length (frame-leaves (frame-tree-for output workspace))))
         (needed (- (1+ ordinal) count)))
    (cond ((<= needed 0)
           (nth ordinal (frame-leaves (frame-tree-for output workspace))))
          ((> needed *launch-frames-max-new*)
           (values nil
                   (format nil "~A; at most ~D more can be made"
                           (frame-range-text (display-index ordinal)
                                             output workspace)
                           *launch-frames-max-new*)))
          (t
           (loop repeat needed
                 unless (split-largest-frame output workspace)
                   do (return-from launch-frame-at
                        (values nil
                                (format nil "no room to split for frame ~D"
                                        (display-index ordinal)))))
           (nth ordinal (frame-leaves (frame-tree-for output workspace)))))))

(defun launch-target-kind (word)
  "What the first word of a launch-into line names: :focused for \".\",
:address for a word holding a colon, :label for one character, and NIL
for any other word, which starts the command."
  (cond ((null word) nil)
        ((string= word ".") :focused)
        ((find #\: word) :address)
        ((= (length word) 1) :label)
        (t nil)))

(define-command :launch-into (output args)
  "Start a program so that its first window opens in a chosen frame,
without switching workspaces. Usage: launch-into [TARGET] COMMAND...
TARGET is one of:
- a frame address W:S:F, any part empty for the focused screen, its
  current workspace, or a frame the placement policy picks;
- a frame number past the workspace's frame count, which splits the
  largest frame along its longer axis until that frame exists, at
  most four more frames;
- a one-character C-t f label;
- \".\" for the focused frame.
The first word is a TARGET only when it is \".\", holds a colon, or is
one character long. Any other first word starts COMMAND, and the
window opens in the focused frame. A command whose name is one
character therefore needs \".\" before it. On a scroll or floating
workspace the frame part is ignored and the window joins the
workspace by its own rule. A refused launch shows its reason and
replies with it."
  (let* ((words (mapcar #'princ-to-string args))
         (kind (launch-target-kind (first words)))
         (command (format nil "~{~A~^ ~}" (if kind (rest words) words))))
    (flet ((fail (control &rest arguments)
             (let ((reason (format nil "launch-into: ~?" control arguments)))
               (message "~A" reason)
               reason))
           (start (out ws frame where)
             (if (launch-into-frame command out ws frame)
                 (format nil "launched ~A into ~A" command where)
                 (let ((reason (format nil "launch-into: ~A did not start"
                                       command)))
                   (message "~A" reason)
                   reason))))
      (cond
        ((null words)
         (fail "usage: launch-into [TARGET] COMMAND..."))
        ((eq kind :label)
         (multiple-value-bind (out ws frame)
             (frame-target-by-label (char-downcase (char (first words) 0)))
           (if (or (null frame) (zerop (length command)))
               (fail "no frame labelled ~A, or no command" (first words))
               (start out ws frame (format nil "frame ~A" (first words))))))
        ((zerop (length command))
         (fail "no command"))
        ((eq kind :address)
         (multiple-value-bind (target error)
             (resolve-frame-address (first words))
           (if error
               (fail "~A" error)
               (destructuring-bind (&key output workspace ordinal
                                    &allow-other-keys)
                   target
                 (if (or (null ordinal)
                         (scroll-workspace-p output workspace)
                         (floating-workspace-p output workspace))
                     (start output workspace nil
                            (format nil "~D:~D"
                                    (workspace-to-display workspace)
                                    (screen-number output)))
                     (multiple-value-bind (frame why)
                         (launch-frame-at output workspace ordinal)
                       (if frame
                           (start output workspace frame
                                  (frame-address frame))
                           (fail "~A" why))))))))
        (t
         (let ((out (or output (current-output))))
           (if (null out)
               (fail "no screen")
               (let* ((ws (current-workspace out))
                      (frame (current-frame-for out ws)))
                 (start out ws frame (frame-address frame))))))))))

(defun launch-into-candidates ()
  "The address prompt's rows as a list of (LABEL ADDRESS FRAME): every
frame from all-frame-addresses labelled \"W:S:F CONTEXT\", and after
each tiled workspace's last frame a \"W:S:F new frame\" row naming the
next frame number, which launch-into makes. FRAME is NIL on a new frame
row."
  (let ((rows '())
        (previous nil))
    (flet ((close-workspace ()
             (when previous
               (destructuring-bind (output workspace count) previous
                 (unless (or (scroll-workspace-p output workspace)
                             (floating-workspace-p output workspace))
                   (let ((address (format nil "~D:~D:~D"
                                          (workspace-to-display workspace)
                                          (screen-number output)
                                          (display-index count))))
                     (push (list (format nil "~A new frame" address)
                                 address nil)
                           rows)))))))
      (dolist (row (all-frame-addresses))
        (destructuring-bind (address frame output workspace) row
          (unless (and previous
                       (eq (first previous) output)
                       (eql (second previous) workspace))
            (close-workspace)
            (setf previous (list output workspace 0)))
          (incf (third previous))
          (push (list (format nil "~A ~A" address (frame-address-context frame))
                      address frame)
                rows)))
      (close-workspace))
    (nreverse rows)))

(defun launch-into-prompt ()
  "Ask where, then what, and launch it there: the first prompt lists
every frame address with the frame's window or \"empty\", and a new
frame per workspace, with the current frame selected; picking one opens
a second prompt for the shell command, which runs as launch-into
ADDRESS COMMAND. Escape at either prompt, or a blank command, starts
nothing. Returns NIL."
  (let* ((rows (launch-into-candidates))
         (names (mapcar #'first rows))
         (output (current-output))
         (current (and output
                       (current-frame-for output (current-workspace output)))))
    (prompt-open "Launch into:"
                 :candidates names
                 :on-complete
                 (lambda (choice)
                   (let ((row (find choice rows :key #'first :test #'string=)))
                     (when row
                       (let ((address (second row)))
                         (prompt-open (format nil "Launch into ~A:" address)
                                      :on-complete
                                      (lambda (command)
                                        (let ((line (string-trim " " command)))
                                          (when (plusp (length line))
                                            (ipc-dispatch-line
                                             (format nil "launch-into ~A ~A"
                                                     address line)))))))))))
    (let ((selected (position current rows :key #'third)))
      (when (and selected current)
        (setf *prompt-selection* selected)))
    nil))

(define-command :launch-into-prompt (output args)
  "Open the launch prompt: launch into a frame picked from every frame
address, then the command to run there. No key opens it by default.
Usage: launch-into-prompt"
  (declare (ignore output args))
  (launch-into-prompt))
