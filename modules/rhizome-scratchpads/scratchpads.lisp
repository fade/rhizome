;;; rhizome-scratchpads — the scratchpad module.
;;;
;;; C-t ; toggles the default scratchpad (a floating foot
;;; terminal); rhizome-cmd scratchpad toggle via IPC; multiple
;;; scratchpads via :scratchpads :entries config. Loaded with
;;; (rhizome:load-module "rhizome-scratchpads") — the binding and
;;; the command install at module load (the keymap layer keeps
;;; them across default re-builds).

;;; Scratchpads — the StumpWM C-t ; toggle
;;;
;;; A scratchpad is a hidden floating window (typically a terminal)
;;; that toggles on a keybinding: show it centered and focused, or
;;; hide it (the process stays alive — your session persists).
;;; Multiple scratchpads can exist; each is named and has its own
;;; launch command. The default is a terminal ("foot").
;;;
;;; Keybinding: C-t ; toggles the default scratchpad.
;;; IPC: rhizome-cmd scratchpad toggle
;;; Config: (:scratchpads ((:name "term" :command "foot")
;;;                        (:name "music" :command "ncmpcpp")))

(in-package #:rhizome)

(defvar *scratchpads* (make-hash-table :test #'equal)
  "Name → plist (:window WIN :command CMD :visible T/NIL
:launched-at UNIVERSAL-TIME).")

(defun scratchpad-config ()
  "The configured scratchpads, as ((:name N :command C) ...)."
  (config-getf '(:scratchpads) :entries
               :default '((:name "term" :command "foot"))))

(defun scratchpad-find (name)
  "The scratchpad's state plist, or NIL."
  (gethash name *scratchpads*))

(defun scratchpad-window-alive-p (state)
  "Whether the scratchpad's window still exists in *windows*."
  (and state
       (getf state :window)
       (gethash (window-proxy (getf state :window)) *windows*)))

(defun scratchpad-hide (name state)
  "Hide the scratchpad: minimize it (the process stays alive). The
next render pass sends river's hide, as for any minimized window."
  (let ((win (getf state :window)))
    (when (and win (gethash (window-proxy win) *windows*))
      (setf (window-minimized-p win) t)
      (setf (getf state :visible) nil)
      (setf (gethash name *scratchpads*) state)
      (request-manage-dirty)
      (format t "~&; Rhizome: scratchpad ~A hidden~%" name))))

(defun scratchpad-centre-position (win)
  "Where WIN floats when a scratchpad first shows it, as two values
X and Y: centred horizontally on the usable area of WIN's output, 50
pixels below the bar. A window not yet on an output uses
CURRENT-OUTPUT; with no output known, a 1920 pixel wide screen."
  (let ((out (or (window-output win) (current-output)))
        (w (find-if (lambda (v) (and v (plusp v)))
                    (list (window-float-width win) (window-width win) 800))))
    (if (and out (plusp (output-width out)))
        (multiple-value-bind (ux uy uw) (output-usable-area out)
          (values (+ ux (floor (- uw w) 2)) (+ uy 50)))
        (values (floor (- 1920 w) 2) (+ (bar-reserved-height) 50)))))

(defun scratchpad-show (name state)
  "Show the scratchpad: un-minimize, float, centre, focus."
  (let ((win (getf state :window)))
    (when (and win (gethash (window-proxy win) *windows*))
      ;; Un-minimize; the next render pass sends river's show
      (setf (window-minimized-p win) nil)
      ;; float centred
      (unless (window-floating-p win)
        (multiple-value-bind (x y) (scratchpad-centre-position win)
          (float-window win :x x :y y)))
      ;; focus it (queued through the manage pass)
      (dolist (seat (all-seats))
        (focus-window win seat))
      (setf (getf state :visible) t)
      (setf (gethash name *scratchpads*) state)
      (request-manage-dirty)
      (format t "~&; Rhizome: scratchpad ~A shown~%" name))))

(defun scratchpad-create (name command)
  "Launch the scratchpad's process. SCRATCHPAD-CLAIM-WINDOW gives it
the first window that appears within 10 seconds."
  (setf (gethash name *scratchpads*)
        (list :window nil :command command :visible nil
              :launched-at (get-universal-time)))
  (uiop:launch-program (list "bash" "-c" command)))

(defun scratchpad-claim-window (win)
  "The :window-created handler: give WIN to the first scratchpad
launched within the last 10 seconds that has no window yet, and show
it. Each window goes to at most one scratchpad."
  (when (gethash (window-proxy win) *windows*)
    (loop for name being the hash-keys of *scratchpads*
          for state = (gethash name *scratchpads*)
          when (and (null (getf state :window))
                    (getf state :launched-at)
                    (< (- (get-universal-time) (getf state :launched-at))
                       10))
            do (setf (getf state :window) win)
               (setf (gethash name *scratchpads*) state)
               (format t "~&; Rhizome: scratchpad ~A claimed ~A~%"
                       name (window-app-id win))
               (scratchpad-show name state)
               (return))))

(defun toggle-scratchpad (&optional (name "term"))
  "Toggle the scratchpad NAME (default: the terminal). Creates it
on first use; hides it if visible; shows it if hidden or dead."
  (let ((state (scratchpad-find name)))
    (cond
      ;; doesn't exist — create it
      ((null state)
       (let ((entry (find name (scratchpad-config)
                          :key (lambda (e) (getf e :name))
                          :test #'equal)))
         (let ((command (or (and entry (getf entry :command)) name)))
           (scratchpad-create name command)
           (format t "~&; Rhizome: scratchpad ~A launching~%" name))))
      ;; window is dead — relaunch
      ((not (scratchpad-window-alive-p state))
       (let ((entry (find name (scratchpad-config)
                          :key (lambda (e) (getf e :name))
                          :test #'equal)))
         (let ((command (or (and entry (getf entry :command))
                           (getf state :command) name)))
           (remhash name *scratchpads*)
           (scratchpad-create name command)
           (format t "~&; Rhizome: scratchpad ~A relaunching~%" name))))
      ;; visible — hide
      ((getf state :visible)
       (scratchpad-hide name state))
      ;; hidden — show
      (t
       (scratchpad-show name state)))))

(defun list-scratchpads ()
  "The scratchpad states, for IPC/debug."
  (loop for name being the hash-keys of *scratchpads*
        for state = (gethash name *scratchpads*)
        collect (list :name name
                      :visible (getf state :visible)
                      :alive (scratchpad-window-alive-p state)
                      :command (getf state :command))))

(defun scratchpad-forget-window (win)
  "The :window-closed handler: a scratchpad whose window WIN closed
has no window and is not visible."
  (loop for name being the hash-keys of *scratchpads*
        for state = (gethash name *scratchpads*)
        when (eq (getf state :window) win)
          do (setf (getf state :window) nil
                   (getf state :visible) nil)
             (setf (gethash name *scratchpads*) state)))

(defun scratchpad-debug ()
  "Debug the scratchpad state (IPC: rhizome-cmd scratchpad list)."
  (loop for name being the hash-keys of *scratchpads*
        for state = (gethash name *scratchpads*)
        collect (list :name name
                     :visible (getf state :visible)
                     :window (and (getf state :window)
                                 (gethash (window-proxy (getf state :window))
                                          *windows*)
                                 t)
                     :minimized (and (getf state :window)
                                    (window-minimized-p (getf state :window)))
                     :floating (and (getf state :window)
                                   (window-floating-p (getf state :window))))))

;;; --- install: binding + command, at module load ---

;; Handlers are registered by symbol: a reload of this file finds the
;; same handler already on the hook and adds no second copy.
(add-hook :window-created 'scratchpad-claim-window)

(add-hook :window-closed 'scratchpad-forget-window)

(define-prefix-key 59 (lambda () (toggle-scratchpad))
  "toggle the scratchpad")

(define-command :scratchpad (output args)
  "Toggle a scratchpad (default: term). Use arg LIST to see all."
  (declare (ignore output))
  (cond ((null args) (toggle-scratchpad))
        ((string-equal (princ-to-string (first args)) "list")
         (list-scratchpads))
        (t (toggle-scratchpad (princ-to-string (first args))))))
