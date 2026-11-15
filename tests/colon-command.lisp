;;; Tests for the command palette as a command line (StumpWM's colon):
;;; Enter runs the typed line, arguments included, when it has
;;; arguments or names a command exactly, and runs the selected
;;; command for a bare partial name. Other candidate prompts still
;;; complete only with a candidate.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (type-text (text)
           (loop for c across text
                 do (prompt-handle-key (char-code c))))
         (enter () (prompt-handle-key 65293)))

    ;; Stub the Wayland requests so the real prompt-open and
    ;; prompt-close run without a compositor.
    (setf *compositor* :fake-compositor
          *shm* :fake-shm
          *layer-shell* :fake-layer-shell)
    (setf (symbol-function 'wl:wl-compositor.create-surface)
          (lambda (&rest a) (declare (ignore a)) :fake-surface))
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
      (setf (symbol-function fn)
            (lambda (&rest a) (declare (ignore a)) nil)))
    (setf (symbol-function 'show-message)
          (lambda (text &optional seconds)
            (declare (ignore seconds))
            text))

    ;; Commands with names chosen so that substring ranking selects
    ;; a different candidate from the one typed exactly
    (let ((received :none))
      (define-command :colon-test-gselect (output args)
        (declare (ignore output))
        (setf received args)
        nil)
      (define-command :colon-test-run (output args)
        (declare (ignore output args))
        nil)
      (define-command :a-colon-test-run (output args)
        (declare (ignore output args))
        nil)

      ;; Record each line the palette hands the dispatcher
      (let* ((real-dispatch (symbol-function 'ipc-dispatch-line))
             (lines '()))
        (setf (symbol-function 'ipc-dispatch-line)
              (lambda (line)
                (push line lines)
                (funcall real-dispatch line)))
        (labels ((palette-line (text &key (initial nil initial-p))
                   (setf lines '() received :none)
                   (if initial-p
                       (handler-case (command-palette initial)
                         (error (e)
                           (format t "~&; palette with initial input: ~A~%" e)))
                       (command-palette))
                   (type-text text)
                   (enter)
                   (reverse lines)))

          (ok "a line with arguments reaches the dispatcher verbatim"
              (palette-line "colon-test-gselect 3")
              '("colon-test-gselect 3"))
          (ok "the command receives the typed arguments"
              received '(3))
          (ok "the palette closes after a line with arguments"
              *prompt-active* nil)

          (ok "a bare partial name runs the selected command"
              (palette-line "colon-test-gs")
              '("colon-test-gselect"))

          (ok "an exact command name runs that command, not the selection"
              (palette-line "colon-test-run")
              '("colon-test-run"))

          (ok "an empty query runs nothing"
              (palette-line "")
              '())
          (ok "a blank query runs nothing"
              (palette-line "   ")
              '())
          (ok "a partial name matching no command runs nothing"
              (palette-line "zzqqx")
              '())

          ;; Arguments typed after a name filter on the name alone
          (command-palette)
          (type-text "colon-test-gs 3")
          (ok "the typed arguments do not hide the command candidate"
              (prompt-selected) "colon-test-gselect")
          (prompt-close)

          (ok "initial input is the start of the command line"
              (palette-line "3" :initial "colon-test-gselect ")
              '("colon-test-gselect 3"))

          ;; The colon command opens the palette with its arguments
          (ok "the colon command is registered" (and (find-command :colon) t) t)
          (setf lines '() received :none)
          (when (find-command :colon)
            (execute-command nil :colon '("colon-test-gselect" 7))
            (setf lines '())
            (enter))
          (ok "colon with arguments opens the palette with them as input"
              (reverse lines) '("colon-test-gselect 7"))
          (ok "colon's initial input reaches the command"
              received '(7)))))

    ;; Other candidate prompts complete only with a candidate
    (let ((got :none))
      (prompt-open "Window:" :candidates '("1: [1] firefox" "2: [1] emacs")
                             :on-complete (lambda (c) (setf got c)))
      (type-text "fire")
      (enter)
      (ok "a window prompt completes with the selected window"
          got "1: [1] firefox"))
    (let ((got :none))
      (prompt-open "Window:" :candidates '("1: [1] firefox" "2: [1] emacs")
                             :on-complete (lambda (c) (setf got c)))
      (type-text "firefox 3")
      (enter)
      (ok "a window prompt ignores typed text that names no window"
          got :none))
    (let ((got :none))
      (prompt-open "Window:" :candidates '("1: [1] firefox" "2: [1] emacs")
                             :on-complete (lambda (c) (setf got c)))
      (enter)
      (ok "a window prompt with an empty query completes with the first window"
          got "1: [1] firefox")))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
