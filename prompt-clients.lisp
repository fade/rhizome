;;; Interactive prompt clients: windowlist, workspace picker,
;;; command palette. Bound to Alt+W, Alt+G, Alt+X.

(in-package #:rhizome)

(defun windowlist ()
  "Interactive window list: pick by title, jump to its workspace and
focus it."
  (let* ((wins (all-windows))
         (candidates
           (mapcar
             (lambda (win)
               (format nil "~D: [~D] ~A~A"
                       (or (window-number win) ?-)
                       (or (window-workspace win) 0)
                       (or (window-title win)
                           (window-app-id win)
                           "?")
                       (if (window-marked-p win) " *" "")))
             wins)))
    (if (null wins)
        (message "No windows")
        (prompt-open "Window:"
                     :candidates candidates
                     :on-complete
                     (lambda (choice)
                       (let ((pos (position choice candidates :test #'string=)))
                         (when pos
                           (let ((win (nth pos wins)))
                             (let ((out (window-output win)))
                               (when (and out (window-workspace win))
                                 (switch-workspace (window-workspace win) out)))
                             (dolist (seat (all-seats))
                               (focus-window win seat))
                             (request-manage-dirty)))))))))

(defun workspace-list ()
  "Interactive workspace picker for the current output. Candidates show
display numbers and names, the current workspace starred; a number or
a name, typed or picked, switches."
  (let ((out (current-output)))
    (when out
      (let* ((current (current-workspace out))
             (candidates
               (loop for ws from 1 to (workspace-count out)
                     collect (format nil "~D~@[ ~A~]~:[~; *~]"
                                     (workspace-to-display ws)
                                     (workspace-name out ws)
                                     (= ws current)))))
        (prompt-open "Workspace:"
                     :candidates candidates
                     :on-complete
                     (lambda (choice)
                       (let* ((text (string-trim " *" choice))
                              (ws (or (workspace-from-designator text out)
                                      (workspace-from-display
                                        (subseq text 0 (position #\Space text))))))
                         (if (valid-workspace-p out ws)
                             (switch-workspace ws out)
                             (message "~A" (workspace-range-text out text))))))))))

(defun command-palette (&optional (initial ""))
  "Read a command line and run it through ipc-dispatch-line, as
StumpWM's colon does. INITIAL is the starting text of the line.
Candidates complete the command name; aliases run but are not
candidates. Enter runs the typed line when it has arguments or names
a command or an alias exactly, runs the selected command
for a partial name, and runs nothing for a blank line."
  (let ((commands
          (mapcar #'string-downcase (command-names))))
    (prompt-open "Command:"
                 :candidates commands
                 :complete-line t
                 :initial initial
                 :on-complete
                 (lambda (choice)
                   ;; Replies show for 3 s, never over the command's
                   ;; own message; an unknown-command reply, which the
                   ;; dispatcher does not show, is shown here
                   (let ((serial *message-serial*))
                     (show-command-reply
                      (let ((*command-reply-seconds* 3))
                        (ipc-dispatch-line choice))
                      serial))))
    ;; Set after prompt-open, which clears it for every other prompt
    (setf *prompt-exact-names*
          (mapcar #'string-downcase (command-alias-names)))
    nil))

(define-command :colon (output args)
  "Open the command palette, with ARGS as the start of the command
line. Usage: colon [text]"
  (declare (ignore output))
  (command-palette (format nil "~{~A~^ ~}" args))
  nil)

(defun run-shell-command-prompt ()
  "Prompt for a shell command and run it in the background
(StumpWM's C-t ! equivalent)."
  (prompt-open "Shell:"
               :candidates nil
               :on-complete
               (lambda (command)
                 (message "running: ~A" command)
                 (uiop:launch-program (list "bash" "-c" command)))))

(defun select-from-menu (title items &key on-select)
  "Pop a type-to-filter selection menu and act on the choice —
StumpWM's scripting menu (select-from-menu).

ITEMS is a list of labels or (LABEL . ACTION) entries:
- a cons with a FUNCTION cdr runs the action on selection
- any other entry (including data payloads) is passed to ON-SELECT
  if given, otherwise the label is echoed

  (rhizome:select-from-menu \"Workspace?\"
    (list (cons \"Web\"  (lambda () (switch-workspace 2)))
          (cons \"Code\" (lambda () (switch-workspace 3)))))"
  (let* ((labels (mapcar (lambda (item)
                          (if (consp item)
                              (string (car item))
                              (string item)))
                        items)))
    (prompt-open title
                 :candidates labels
                 :on-complete
                 (lambda (choice)
                   (let ((pos (position choice labels :test #'string=)))
                     (when pos
                       (let* ((item (nth pos items))
                              (action (and (consp item) (cdr item))))
                         (cond ((functionp action) (funcall action))
                               (on-select (funcall on-select item))
                               (t (message "~A" choice))))))))))

(defun menu (title items)
  "Alias for select-from-menu (StumpWM's menu macro flavor)."
  (select-from-menu title items))
