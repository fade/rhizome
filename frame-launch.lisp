;;; Launching a program into a chosen frame
;;;
;;; launch-into-frame starts a shell command and records a claim on one
;;; frame of a workspace on an output. The first window registered
;;; afterwards whose process is the launched one, or a descendant of
;;; it, takes the claim: it opens on that output and workspace, and
;;; frame adoption places it in that frame, split first when occupied
;;; as (:tiling :auto-split) says. The workspace need not be shown, and
;;; no workspace switches unless the caller asks. A claim that no
;;; window takes within *launch-claim-seconds* lapses.
;;;
;;; Windows match by river's unreliable pid. A client that hands its
;;; window to a process already running (a terminal server, a browser
;;; with an open session) takes no claim, and its window opens where
;;; any new window does.
;;;
;;;   rhizome-cmd launch-into LABEL COMMAND...
;;;
;;; starts COMMAND in the frame C-t f labels LABEL.

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
          ;; After the parenthesised command name: state, then ppid
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

(defun launch-into-frame (command output workspace frame &key switch)
  "Start COMMAND, a shell command line, so that its first window opens
in FRAME, a leaf of WORKSPACE's frame tree on OUTPUT. With SWITCH,
OUTPUT shows WORKSPACE first and FRAME takes focus (focus-frame);
without it no workspace or focus changes. Returns the launch-claim, or
NIL with nothing started when FRAME is not a leaf of that tree or the
command cannot start."
  (when (member frame (frame-leaves (frame-tree-for output workspace))
                :test #'eq)
    (when switch
      (unless (eql (current-workspace output) workspace)
        (switch-workspace workspace output))
      (focus-frame output workspace frame))
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

(define-command :launch-into (output args)
  "Start a program in the frame C-t f labels LABEL, without switching
workspaces. Usage: launch-into LABEL COMMAND..."
  (declare (ignore output))
  (let* ((label (and args (princ-to-string (first args))))
         (command (format nil "~{~A~^ ~}" (rest args))))
    (multiple-value-bind (out ws frame)
        (and label (= (length label) 1)
             (frame-target-by-label (char-downcase (char label 0))))
      (cond ((or (null frame) (zerop (length command)))
             (message "launch-into: no frame labelled ~A, or no command"
                      label)
             nil)
            ((launch-into-frame command out ws frame)
             (format nil "launched ~A into frame ~A" command label))
            (t
             (message "launch-into: ~A did not start" command)
             nil)))))
