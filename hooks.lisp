;;; Hooks for Rhizome
;;;
;;; StumpWM-style hooks: user-defined functions triggered on WM
;;; events. Handlers are added with ADD-HOOK and run (in unspecified
;;; order) with the event's arguments; an error in one handler never
;;; affects others or the WM itself.
;;;
;;;   (rhizome:add-hook :window-created
;;;     (lambda (win) (format t "new window: ~A~%" (window-title win))))
;;;
;;; Available hooks:
;;;   :window-created      (win)
;;;   :window-closed       (win)
;;;   :window-resized      (win width height)  ; the client committed a size
;;;   :window-minimized    (win)  ; minimize-window: a hide, a client's
;;;                               ; request, or the menu's Minimize
;;;   :window-restored     (win)  ; restore-window: the menu's Restore
;;;   :focus-changed       (win)
;;;   :layout-changed      (output layout-name)
;;;   :workspace-changed  (workspace output)  ; 1-based, as the Lisp API;
;;;                                          ; workspace-to-display gives
;;;                                          ; the number shown
;;;   :output-created      (output)

(in-package #:rhizome)

(defvar *hooks* (make-hash-table)
  "Maps hook names (keywords) to lists of handler functions.")

(defun add-hook (name function)
  "Add FUNCTION as a handler for the hook NAME (a keyword)."
  (pushnew function (gethash name *hooks*) :test #'eq)
  function)

(defun remove-hook (name function)
  "Remove FUNCTION from the handlers for the hook NAME."
  (setf (gethash name *hooks*)
        (remove function (gethash name *hooks*) :test #'eq))
  function)

(defun clear-hook (name)
  "Remove all handlers for the hook NAME."
  (remhash name *hooks*))

(defun list-hooks ()
  "Return an alist of (name . handlers) for all hooks with handlers."
  (loop for name being the hash-keys of *hooks*
        collect (cons name (gethash name *hooks*))))

(defun run-hook (name &rest args)
  "Run all handlers for the hook NAME with ARGS. Handler errors are
logged and isolated: they never affect other handlers or the WM."
  (dolist (handler (gethash name *hooks*))
    (handler-case
        (apply handler args)
      (error (e)
        (format t "~&; Rhizome: error in ~A hook handler: ~A~%" name e)))))
