;;; Workspace numbers, counts, and names for Rhizome
;;;
;;; Workspaces are numbered from 1 inside rhizome: window placements,
;;; frame trees, saved window state, and the exported Lisp API all use
;;; that numbering. The numbers a user sees and types start at the
;;; display base instead, in:
;;;
;;;   - the digit keys (MOD+N, C-t N);
;;;   - the bar;
;;;   - IPC commands;
;;;   - the workspace picker and the window menu;
;;;   - messages;
;;;   - window rule :workspace values and (:workspaces :types) keys.
;;;
;;;   (rhizome:configure
;;;     '(:workspaces (:index-base 0
;;;                    :count 9
;;;                    :screens (("DVI-D-1" :count 6
;;;                                         :names ("mail" "web"))
;;;                              ("HDMI-A-1" :count 4)))))
;;;
;;; :index-base is 0 or 1 (default 1), and is the one base for every
;;; user-facing index: workspace, screen (screens.lisp), and frame
;;; (frame addresses). Each function that reads or takes such an index
;;; accepts an :index-base keyword defaulting to it. :count is the
;;; number of workspaces on a screen without its own entry (default
;;; *workspace-count*). A :screens entry is keyed by connector name and
;;; may set that screen's :count and :names; a missing name shows the
;;; number. add-workspace and remove-workspace change a screen's count
;;; until the next restart or reload-config.

(in-package #:rhizome)

(defvar *workspace-count-overrides* (make-hash-table)
  "Output -> workspace count set by add-workspace or remove-workspace.
Cleared by reload-config, so the configured count applies again.")

;;; --- The display base ---

(defun index-base ()
  "The number every first workspace, screen, and frame shows as: 0 or
1, from (:workspaces :index-base). Any value other than 0 reads as 1.
Functions that read or take a user-facing index accept an :index-base
keyword that defaults to this value."
  (if (eql (config-getf '(:workspaces) :index-base :default 1) 0) 0 1))

(defun workspace-index-base ()
  "The number the first workspace shows as: the shared index-base."
  (index-base))

(defun display-index (ordinal &key (index-base (index-base)))
  "The user-facing number of the zero-based ORDINAL under INDEX-BASE."
  (+ ordinal index-base))

(defun index-ordinal (number &key (index-base (index-base)))
  "The zero-based ordinal the user-facing NUMBER names under
INDEX-BASE, or NIL when NUMBER is not a number. NUMBER is an integer
or a string of digits, blanks around it ignored. No range check."
  (let ((n (typecase number
             (integer number)
             (string (let ((text (string-trim " " number)))
                       (and (plusp (length text))
                            (every #'digit-char-p text)
                            (parse-integer text)))))))
    (and n (- n index-base))))

(defun workspace-to-display (workspace &key (index-base (index-base)))
  "The number WORKSPACE (1-based) shows as under INDEX-BASE."
  (display-index (1- workspace) :index-base index-base))

(defun workspace-from-display (number &key (index-base (index-base)))
  "The workspace (1-based) the user-facing NUMBER names under
INDEX-BASE, or NIL when NUMBER is not a number. NUMBER is an integer
or a string holding one: IPC hands handlers either form. No range
check."
  (let ((ordinal (index-ordinal number :index-base index-base)))
    (and ordinal (1+ ordinal))))

;;; --- Screens ---

(defun workspace-screen-config (output)
  "The (:workspaces :screens) plist for OUTPUT's connector, or NIL."
  (let ((name (output-connector-name output)))
    (when name
      (cdr (find-if (lambda (entry)
                      (and (consp entry)
                           (stringp (car entry))
                           (string-equal (car entry) name)))
                    (config-getf '(:workspaces) :screens))))))

(defun configured-workspace-count (output)
  "OUTPUT's workspace count from the config: its :screens entry, else
(:workspaces :count), else *workspace-count*."
  (let ((n (or (getf (workspace-screen-config output) :count)
               (config-getf '(:workspaces) :count)
               *workspace-count*)))
    (if (and (integerp n) (plusp n)) n *workspace-count*)))

(defun workspace-count (&optional (output (current-output)))
  "The number of workspaces on OUTPUT: a count set by add-workspace or
remove-workspace this session, else the configured one."
  (or (and output (gethash output *workspace-count-overrides*))
      (configured-workspace-count output)))

(defun valid-workspace-p (output workspace)
  "Whether WORKSPACE (1-based) is one of OUTPUT's workspaces."
  (and (integerp workspace)
       (<= 1 workspace (workspace-count output))))

(defun workspace-name (output workspace)
  "The configured name of WORKSPACE (1-based) on OUTPUT, or NIL."
  (let ((name (nth (1- workspace)
                   (getf (workspace-screen-config output) :names))))
    (and (stringp name) (plusp (length name)) name)))

(defun workspace-label (output workspace)
  "What WORKSPACE (1-based) on OUTPUT shows as: its name, else its
display number."
  (or (workspace-name output workspace)
      (princ-to-string (workspace-to-display workspace))))

(defun workspace-from-designator (designator output)
  "The workspace (1-based) DESIGNATOR names on OUTPUT: a display
number (integer or string) or a workspace name, case ignored. NIL
when it names neither. No range check for numbers."
  (or (workspace-from-display designator)
      (when (stringp designator)
        (let* ((text (string-trim " " designator))
               (pos (loop for ws from 1 to (workspace-count output)
                          when (let ((name (workspace-name output ws)))
                                 (and name (string-equal name text)))
                            return ws)))
          pos))))

(defun workspace-range-text (output number)
  "\"no workspace NUMBER: SCREEN has FIRST-LAST\", in display numbers."
  (format nil "no workspace ~A: ~A has ~D-~D"
          number
          (or (output-connector-name output) "this screen")
          (workspace-to-display 1)
          (workspace-to-display (workspace-count output))))

;;; --- User-facing entry points (display numbers) ---

(defun switch-to-workspace-number (number &optional (output (current-output)))
  "Show the workspace NUMBER names in display numbering on OUTPUT.
A number outside OUTPUT's workspaces shows a message naming the
range instead."
  (when output
    (let ((workspace (workspace-from-display number)))
      (if (valid-workspace-p output workspace)
          (switch-workspace workspace output)
          (message "~A" (workspace-range-text output number))))))

(defun move-focused-window-to-workspace-number (number)
  "Move the focused window to the workspace NUMBER names in display
numbering on its output, and follow it. A number outside that
output's workspaces shows a message naming the range instead."
  (let* ((seats (all-seats))
         (focused (and seats (seat-focused-window (first seats))))
         (output (and focused (window-output focused)))
         (workspace (workspace-from-display number)))
    (cond ((null output))
          ((valid-workspace-p output workspace)
           (move-focused-window-to-workspace workspace))
          (t (message "~A" (workspace-range-text output number))))))

(defun cycle-workspace (output delta &key with-window)
  "Show the workspace DELTA steps from OUTPUT's current one, wrapping
at either end, as StumpWM's gnext and gprev do. With WITH-WINDOW, a
focused window on OUTPUT's current workspace moves to that workspace
and keeps focus; with none, only the view moves. Return the reply
text: \"workspace LABEL\", \"no other workspace\" when OUTPUT has one
workspace, or \"no output\" when OUTPUT is NIL."
  (cond
    ((null output) "no output")
    ((< (workspace-count output) 2) "no other workspace")
    (t
     (let* ((current (current-workspace output))
            (target (1+ (mod (+ (1- current) delta)
                             (workspace-count output))))
            (win (and with-window (focused-window))))
       (if (and win
                (eq (window-output win) output)
                (eql (window-workspace win) current))
           (move-window-to-workspace win target output)
           (switch-workspace target output))
       (format nil "workspace ~A" (workspace-label output target))))))

(defun command-target-output (output name)
  "The output a command acts on: the one with connector NAME when
NAME is given (NIL when there is none), else OUTPUT."
  (if name
      (find-output-by-connector (princ-to-string name))
      output))

(defun window-rule-workspace (win designator)
  "The workspace (1-based) a window rule's :workspace DESIGNATOR (a
display number or a name) names for WIN, on WIN's output, else the
output it opens on. Outside that output's workspaces: one log line
and NIL."
  (let* ((output (or (window-output win) (find-output-for-window win)))
         (workspace (workspace-from-designator designator output)))
    (if (valid-workspace-p output workspace)
        workspace
        (progn
          (format t "~&; Rhizome: window rule names workspace ~S, outside ~A's ~D workspaces~%"
                  designator (or (output-connector-name output) "the output")
                  (workspace-count output))
          nil))))

;;; --- Fitting windows to a count ---

(defun fit-output-to-workspace-count (output &key quiet)
  "Move OUTPUT's windows on a workspace past its count to its last
workspace, and show that workspace when OUTPUT shows one past the
count. Logs one line when anything moved, unless QUIET. Returns the
number of windows moved. Saved state files are not rewritten."
  (let* ((count (workspace-count output))
         (stray (remove-if-not (lambda (win)
                                 (let ((ws (window-workspace win)))
                                   (and (eq (window-output win) output)
                                        ws (> ws count))))
                               (all-windows)))
         (showing (gethash output *output-workspace*)))
    (dolist (win stray)
      (frame-remove-window win)
      (assign-window-workspace win count))
    (when (and showing (> showing count))
      (switch-workspace count output))
    (when (and (not quiet)
               (or stray (and showing (> showing count))))
      (format t "~&; Rhizome: ~A has ~D workspaces: ~D window~:P moved to workspace ~A~%"
              (or (output-connector-name output) "output") count
              (length stray) (workspace-label output count)))
    (when stray
      (request-manage-dirty))
    (length stray)))

(defun reconcile-workspace-counts ()
  "Fit every output to its workspace count (see
fit-output-to-workspace-count). Runs from apply-config."
  (dolist (output (all-outputs))
    (fit-output-to-workspace-count output)))

(defun fit-saved-workspace (workspace output)
  "WORKSPACE (1-based) from saved state, or OUTPUT's last workspace
when WORKSPACE is past OUTPUT's count (logged in one line). The saved
state itself is left as it is."
  (let ((count (workspace-count output)))
    (if (and (integerp workspace) (> workspace count))
        (progn
          (format t "~&; Rhizome: saved workspace ~D is past ~A's ~D workspaces: using workspace ~A~%"
                  workspace (or (output-connector-name output) "the output")
                  count (workspace-label output count))
          count)
        workspace)))

(defun clear-workspace-count-overrides ()
  "Forget every count set by add-workspace or remove-workspace."
  (clrhash *workspace-count-overrides*))

;;; --- Runtime add and remove ---

(defun add-workspace (&optional (output (current-output)))
  "Give OUTPUT one more workspace until the next restart or
reload-config. Returns the new count, or NIL without an output."
  (when output
    (let ((count (1+ (workspace-count output))))
      (setf (gethash output *workspace-count-overrides*) count)
      (note-frame-trees-changed)
      (format t "~&; Rhizome: added workspace ~A to ~A (~D workspaces)~%"
              (workspace-label output count)
              (or (output-connector-name output) "output") count)
      (message "workspace ~A added: ~D workspaces"
               (workspace-label output count) count)
      (request-manage-dirty)
      count)))

(defun remove-workspace (&optional (output (current-output)))
  "Remove OUTPUT's last workspace until the next restart or
reload-config. Its windows move to the workspace before it, which
OUTPUT then shows if it showed the removed one. An output keeps at
least one workspace. Returns the new count, or NIL when nothing was
removed."
  (when output
    (let* ((count (workspace-count output))
           (label (workspace-label output count)))
      (cond ((<= count 1)
             (message "~A has only one workspace"
                      (or (output-connector-name output) "this screen"))
             nil)
            (t
             (setf (gethash output *workspace-count-overrides*) (1- count))
             (note-frame-trees-changed)
             (let ((moved (fit-output-to-workspace-count output :quiet t)))
               (format t "~&; Rhizome: removed workspace ~A from ~A: ~D window~:P moved to workspace ~A~%"
                       label (or (output-connector-name output) "output")
                       moved (workspace-label output (1- count)))
               (message "workspace ~A removed: ~D workspaces"
                        label (1- count)))
             (1- count))))))
