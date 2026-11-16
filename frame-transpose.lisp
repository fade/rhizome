;;; Transposing a frame's window stack to another frame
;;;
;;; transpose-frame moves the focused frame's whole window stack to a
;;; target frame named by a W:S:F address (frame-addresses.lisp), a
;;; C-t f label, or "." for the focused frame itself.
;;;
;;; - Swap, the default: the two frames exchange their stacks, each
;;;   keeping its order.
;;; - Overlay: the source stack goes on top of the target's stack and
;;;   the source frame is left empty. An emptied source frame then
;;;   prunes as (:frames (:prune-empty ...)) says: under :auto it stays
;;;   when the user split it and closes when a new window split it open.
;;;
;;; Focus follows the focused window: the target's output shows the
;;; target workspace, and the window keeps keyboard focus in the target
;;; frame. A swap from an empty source focuses the source frame, which
;;; now shows the arrived window. An overlay from an empty source is
;;; refused, and so is any target that is the source itself.
;;;
;;; A target frame number past its workspace's frame count makes the
;;; missing frames as launch-into does (launch-frame-at). An address
;;; without a frame part names that workspace's current frame. Both
;;; workspaces must be tiled.
;;;
;;;   rhizome-cmd transpose-frame 2:1:3            swap with frame 3, ws 2
;;;   rhizome-cmd transpose-frame --overlay 1:2:1  overlay onto screen 2
;;;   rhizome-cmd transpose-frame [--all]          pick from a popup

(in-package #:rhizome)

(defun transpose-tiled-p (output workspace)
  "True when WORKSPACE on OUTPUT tiles its windows in frames."
  (not (or (scroll-workspace-p output workspace)
           (floating-workspace-p output workspace))))

(defun transpose-target (word output)
  "The frame the target WORD names, with OUTPUT the focused screen, as
(VALUES OUTPUT WORKSPACE FRAME). A frame number past the workspace's
count makes the missing frames (launch-frame-at). On failure, returns
NIL and the reply text."
  (case (launch-target-kind word)
    (:focused
     (let ((ws (current-workspace output)))
       (values output ws (current-frame-for output ws))))
    (:label
     (multiple-value-bind (out ws frame)
         (frame-target-by-label (char-downcase (char word 0)))
       (if frame
           (values out ws frame)
           (values nil (format nil "no frame labelled ~A" word)))))
    (:address
     (multiple-value-bind (target error) (resolve-frame-address word)
       (if error
           (values nil error)
           (destructuring-bind (&key output workspace ordinal
                                &allow-other-keys)
               target
             (cond ((not (transpose-tiled-p output workspace))
                    (values nil (format nil "~D:~D is not a tiled workspace"
                                        (workspace-to-display workspace)
                                        (screen-number output))))
                   ((null ordinal)
                    (values output workspace
                            (current-frame-for output workspace)))
                   (t
                    (multiple-value-bind (frame why)
                        (launch-frame-at output workspace ordinal)
                      (if frame
                          (values output workspace frame)
                          (values nil why)))))))))
    (t (values nil (format nil "not a frame address: ~A" word)))))

(defun transpose-relocate (windows frame output workspace)
  "Make WINDOWS belong to FRAME of WORKSPACE on OUTPUT."
  (dolist (win windows)
    (setf (window-output win) output
          (window-frame win) frame)
    (assign-window-workspace win workspace)))

(defun transpose-focus (output workspace frame win)
  "Show WORKSPACE on OUTPUT, make FRAME its current frame, and give
keyboard focus to WIN, or to FRAME's own window when WIN is NIL."
  (unless (eql (current-workspace output) workspace)
    (switch-workspace workspace output))
  (setf *selected-output* output)
  (focus-frame output workspace frame)
  (when (and win (not (window-minimized-p win)))
    (dolist (seat (all-seats))
      (focus-window win seat))))

(defun transpose-frame (target &key overlay (output (current-output)))
  "Move the window stack of OUTPUT's focused frame to the frame the
string TARGET names: an address, a C-t f label, or \".\".
Without OVERLAY the two frames swap stacks, each keeping its order.
With OVERLAY the source stack goes on top of the target's and the
emptied source frame prunes as (:frames (:prune-empty ...)) allows.
Focus follows the focused window to the target, whose output then shows
the target workspace. Returns the reply text. Returns NIL after a
message, with nothing moved, when:
- OUTPUT is NIL or either workspace is not tiled;
- TARGET names no frame, or names the source frame;
- OVERLAY is given and the source frame is empty."
  (flet ((fail (control &rest arguments)
           (message "transpose-frame: ~?" control arguments)
           (return-from transpose-frame nil)))
    (unless output
      (fail "no screen"))
    (let* ((src-ws (current-workspace output))
           (source (current-frame-for output src-ws)))
      (unless (transpose-tiled-p output src-ws)
        (fail "~D:~D is not a tiled workspace"
              (workspace-to-display src-ws) (screen-number output)))
      (when (and overlay (null (frame-windows source)))
        (fail "nothing to overlay: the focused frame is empty"))
      (let ((mover (and (frame-windows source)
                        (let ((focused (focused-window)))
                          (if (member focused (frame-windows source))
                              focused
                              (frame-focus-window source))))))
        (multiple-value-bind (tgt-out tgt-ws target-frame)
            (transpose-target target output)
          (unless tgt-out
            (fail "~A" tgt-ws))
          ;; Making frames may have split the source: its windows are
          ;; in its first half
          (loop while (frame-first-child source)
                do (setf source (frame-first-child source)))
          (when (eq source target-frame)
            (fail "the target is the focused frame"))
          (let ((from (frame-address source))
                (to (frame-address target-frame))
                (src-wins (frame-windows source))
                (tgt-wins (frame-windows target-frame)))
            (cond (overlay
                   (setf (frame-windows target-frame) (append src-wins tgt-wins)
                         (frame-windows source) nil)
                   (transpose-relocate src-wins target-frame tgt-out tgt-ws))
                  (t
                   (setf (frame-windows target-frame) src-wins
                         (frame-windows source) tgt-wins)
                   (transpose-relocate src-wins target-frame tgt-out tgt-ws)
                   (transpose-relocate tgt-wins source output src-ws)))
            (unless (frames-preserve-empty-p)
              (prune-empty-frames output src-ws))
            (if src-wins
                (transpose-focus tgt-out tgt-ws target-frame mover)
                (transpose-focus output src-ws source nil))
            (note-frame-trees-changed)
            (request-manage-dirty)
            (format nil "~:[swapped~;overlaid~] ~A ~:[with~;onto~] ~A"
                    overlay from overlay to)))))))

(defun transpose-frame-candidates (&key all (output (current-output)))
  "The transpose popup's rows as a list of (LABEL ADDRESS): the rows of
launch-into-candidates for the workspaces each screen shows, or for
every workspace when ALL, without the focused frame of OUTPUT. A new
frame row names the next frame number, which transpose-frame makes."
  (let ((source (and output
                     (current-frame-for output (current-workspace output)))))
    (loop for (label address frame) in (launch-into-candidates)
          for target = (resolve-frame-address address)
          when (and target
                    (not (and frame (eq frame source)))
                    (or all
                        (eql (getf target :workspace)
                             (current-workspace (getf target :output)))))
            collect (list label address))))

(defun transpose-frame-prompt (&key overlay all (output (current-output)))
  "Open the transpose popup: pick a target frame from
transpose-frame-candidates, then swap with it, or overlay onto it with
OVERLAY. The last row toggles between the shown workspaces and all
workspaces, ALL selecting the second. The reply shows as a message.
Returns NIL."
  (let ((rows (transpose-frame-candidates :all all :output output))
        (toggle (if all "[shown workspaces]" "[all workspaces]")))
    (prompt-open (format nil "~:[Swap frame with~;Overlay frame onto~]~:[~; (all workspaces)~]:"
                         overlay all)
                 :candidates (append (mapcar #'first rows) (list toggle))
                 :on-complete
                 (lambda (choice)
                   (if (string= choice toggle)
                       (transpose-frame-prompt :overlay overlay :all (not all)
                                               :output output)
                       (let ((row (find choice rows :key #'first
                                                    :test #'string=)))
                         (when row
                           (let ((reply (transpose-frame (second row)
                                                         :overlay overlay
                                                         :output output)))
                             (when reply
                               (message "~A" reply))))))))
    nil))

(define-command :transpose-frame (output args)
  "Move the focused frame's window stack to another frame.
Usage: transpose-frame [--overlay] [--all] [TARGET]
TARGET is a frame address W:S:F, a C-t f label, or \".\". A frame
number past the workspace's count makes the missing frames, as
launch-into does; an address without a frame part names that
workspace's current frame.
- By default the two frames swap stacks, each keeping its order.
- --overlay puts the source stack on top of the target's; the emptied
  source frame prunes as (:frames (:prune-empty ...)) allows. An empty
  source is refused.
Focus follows the focused window, and the target's screen shows the
target workspace. The focused frame as its own target does nothing.
With no TARGET a popup lists the frames of the workspaces shown on
every screen, or of all workspaces with --all; its last row switches
between the two. No key runs it by default."
  (let* ((words (mapcar #'princ-to-string args))
         (flags (remove-if-not (lambda (w) (and (> (length w) 2)
                                                (string= "--" w :end2 2)))
                               words))
         (targets (set-difference words flags :test #'string=))
         (out (or output (current-output))))
    (cond ((or (set-difference flags '("--overlay" "--all") :test #'string=)
               (rest targets))
           (message "transpose-frame: usage: transpose-frame [--overlay] [--all] [TARGET]")
           nil)
          (targets
           (transpose-frame (first targets)
                            :overlay (member "--overlay" flags :test #'string=)
                            :output out))
          (t
           (transpose-frame-prompt :overlay (and (member "--overlay" flags
                                                         :test #'string=)
                                                 t)
                                   :all (and (member "--all" flags
                                                     :test #'string=)
                                             t)
                                   :output out)))))
