;;; Tests for StumpWM command names: each alias runs its rhizome
;;; command through execute-command and the IPC dispatcher, with its
;;; arguments; the commands an alias runs reply with NIL or a string;
;;; the palette lists rhizome names only, and Enter runs a typed alias
;;; as an exact command name; a configuration can add an alias; an
;;; unmapped StumpWM name answers "unknown command".
;;; Command handlers and Wayland requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defstruct alias-test-thing
  "A non-string value for stubbed back ends to return.")

(defparameter *alias-test-cases*
  '((:gselect :switch-workspace (3))
    (:hsplit :split-horizontal nil)
    (:vsplit :split-vertical nil)
    (:remove :remove-split nil)
    (:fullscreen :toggle-fullscreen nil)
    (:pull-hidden-next :pull-next nil)
    (:pull-hidden-previous :pull-prev nil)
    (:pull-window-by-number :pull (2))
    (:select-window-by-number :select-window (4))
    (:next-in-frame :focus-next nil)
    (:prev-in-frame :focus-previous nil)
    (:snext :focus-next-output nil)
    (:sprev :focus-previous-output nil)
    (:mode-line :toggle-bar nil)
    (:balance-frames :balance nil)
    (:delete-window :delete nil)
    (:kill-window :kill nil))
  "Each StumpWM name, the rhizome command it runs, and test arguments.")

(defun run-stumpwm-alias-tests ()
  "Run the checks; return the pass and fail counts."
  (let ((pass 0) (fail 0) (aliases *alias-test-cases*))
    (labels ((ok (name actual expected)
               (if (equal actual expected)
                   (progn (incf pass)
                          (format t "~&PASS: ~A~%" name))
                   (progn (incf fail)
                          (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                  name actual expected))))
             (stub (fn value)
               (setf (symbol-function fn)
                     (lambda (&rest a) (declare (ignore a)) value)))
             (external-p (name)
               (multiple-value-bind (sym status)
                   (find-symbol name '#:rhizome)
                 (and sym (eq status :external))))
             (line-for (alias args)
               (format nil "~(~A~)~{ ~A~}" alias args))
             (type-text (text)
               (loop for c across text
                     do (prompt-handle-key (char-code c))))
             (enter () (prompt-handle-key 65293)))
      (macrolet ((check (name actual expected)
                   `(handler-case (ok ,name ,actual ,expected)
                      (error (e)
                        (incf fail)
                        (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))

        (stub 'current-output nil)
        (setf (symbol-function 'show-message)
              (lambda (text &optional seconds)
                (declare (ignore seconds))
                text))

        (check "define-command-alias is exported"
               (external-p "DEFINE-COMMAND-ALIAS") t)
        (check "command-alias-names is exported"
               (external-p "COMMAND-ALIAS-NAMES") t)

        ;; Each alias runs its command, through both entry points, with
        ;; its arguments. The target handler is replaced by a recorder.
        (dolist (entry aliases)
          (destructuring-bind (alias target args) entry
            (let ((saved (gethash target *command-table*))
                  (calls '()))
              (setf (gethash target *command-table*)
                    (lambda (output a)
                      (declare (ignore output))
                      (push (list target a) calls)
                      nil))
              (unwind-protect
                   (progn
                     (check (format nil "~(~A~) runs ~(~A~) from execute-command"
                                    alias target)
                            (progn (execute-command nil alias args)
                                   (first calls))
                            (list target args))
                     (setf calls '())
                     (check (format nil "~(~A~) runs ~(~A~) from the IPC line"
                                    alias target)
                            (progn (ipc-dispatch-line (line-for alias args))
                                   (first calls))
                            (list target args)))
                (if saved
                    (setf (gethash target *command-table*) saved)
                    (remhash target *command-table*))))))

        ;; A name argument passes through as text
        (let ((saved (gethash :switch-workspace *command-table*))
              (got :none))
          (setf (gethash :switch-workspace *command-table*)
                (lambda (output a) (declare (ignore output)) (setf got a) nil))
          (unwind-protect
               (progn
                 (ipc-dispatch-line "gselect web")
                 (check "gselect passes a group name through" got '("web")))
            (setf (gethash :switch-workspace *command-table*) saved)))

        ;; A command name is never shadowed by an alias
        (check "an alias cannot take a command's name"
               (handler-case (progn (define-command-alias :only :fnext)
                                    :defined)
                 (undefined-function () :undefined)
                 (error () :refused))
               :refused)
        (check "only still runs the only command"
               (eq (find-command :only) (gethash :only *command-table*))
               t)

        ;; A configuration adds its own alias
        (check "a configuration alias runs its command"
               (progn (define-command-alias :alias-test-frame-next :fnext)
                      (eq (find-command :alias-test-frame-next)
                          (gethash :fnext *command-table*)))
               t)
        (check "the configuration alias is listed"
               (and (member :alias-test-frame-next (command-alias-names)) t)
               t)
        (ignore-errors
         (remhash :alias-test-frame-next (symbol-value '*command-aliases*)))

        ;; Controls: unmapped StumpWM names stay unknown
        (dolist (name '("gravity" "gmove" "float-this"))
          (check (format nil "unmapped ~A answers unknown command" name)
                 (let ((reply (ipc-dispatch-line name)))
                   (and (stringp reply)
                        (eql 0 (search "unknown command" reply))))
                 t)
          (check (format nil "unmapped ~A has no handler" name)
                 (find-command (intern (string-upcase name) :keyword))
                 nil))

        ;; The command each alias runs replies with NIL or a string
        ;; even when its back end returns a structure
        (let ((thing (make-alias-test-thing))
              (win (make-instance 'rhizome-window :proxy :alias-test-win)))
          (setf (window-focused-p win) t)
          (stub 'all-windows (list win))
          (stub 'current-workspace 1)
          (dolist (fn '(split-horizontal split-vertical remove-split
                        toggle-fullscreen pull-hidden-next
                        pull-window-by-number select-window-by-number
                        focus-next focus-previous focus-next-output
                        focus-previous-output toggle-bar close-window
                        kill-client request-manage-dirty message))
            (stub fn thing))
          (dolist (entry aliases)
            (destructuring-bind (alias target args) entry
              (declare (ignore alias))
              (when (gethash target *command-table*)
                (check (format nil "~(~A~) replies with NIL or a string"
                               target)
                       (let ((r (execute-command nil target args)))
                         (or (null r) (stringp r)))
                       t)))))

        ;; The palette: rhizome names only as candidates; a typed alias
        ;; counts as an exact command name
        (setf *compositor* :fake-compositor
              *shm* :fake-shm
              *layer-shell* :fake-layer-shell)
        (stub 'wl:wl-compositor.create-surface :fake-surface)
        (setf (symbol-function 'rwm:zwlr-layer-shell-v1.get-layer-surface)
              (lambda (&rest a)
                (declare (ignore a))
                (make-instance 'wl:wl-proxy)))
        (dolist (fn '(rwm:zwlr-layer-surface-v1.set-anchor
                      rwm:zwlr-layer-surface-v1.set-size
                      rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
                      rwm:zwlr-layer-surface-v1.set-margin
                      rwm:zwlr-layer-surface-v1.destroy
                      wl:wl-surface.commit
                      wl:wl-surface.destroy
                      prompt-redraw))
          (stub fn nil))
        (let ((lines '()))
          (setf (symbol-function 'ipc-dispatch-line)
                (lambda (line) (push line lines) ""))
          (flet ((palette-line (text)
                   (setf lines '())
                   (command-palette)
                   (type-text text)
                   (enter)
                   (reverse lines)))
            (command-palette)
            (check "palette candidates hold no alias names"
                   (loop for (alias) in aliases
                         when (member (string-downcase alias)
                                      *prompt-candidates*
                                      :test #'string=)
                           collect alias)
                   '())
            (check "palette candidates hold the rhizome command names"
                   (and (member "split-horizontal" *prompt-candidates*
                                :test #'string=)
                        t)
                   t)
            (prompt-close)
            (check "a bare alias runs from the palette"
                   (palette-line "hsplit") '("hsplit"))
            (check "an alias no candidate matches runs from the palette"
                   (palette-line "pull-hidden-next") '("pull-hidden-next"))
            (check "an alias with arguments runs from the palette"
                   (palette-line "gselect 2") '("gselect 2"))
            (check "an unmapped name with no match runs nothing"
                   (palette-line "gravity") '())))))
    (values pass fail)))

(multiple-value-bind (pass fail) (run-stumpwm-alias-tests)
  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
