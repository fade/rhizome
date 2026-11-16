;;; ipc-reply-frame-tree.lisp: a command whose handler returns a split
;;; frame tree and a circular list, dispatched as an IPC command line,
;;; gets a bounded reply, and the image still answers the next command.
;;; The dispatch runs in a thread with a deadline, so a reply path that
;;; loops or exhausts the stack fails its case instead of the run.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun run-with-deadline (thunk &optional (seconds 5))
  "Call THUNK in a fresh thread with output discarded. Return :OK and
its value, :CRASHED and the condition when it signals any serious
condition, or :TIMEOUT when it is still running after SECONDS."
  (let* ((status :timeout)
         (value nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (let ((*standard-output* (make-broadcast-stream)))
                      (handler-case
                          (setf value (funcall thunk)
                                status :ok)
                        (serious-condition (c)
                          (setf value c
                                status :crashed)))))
                  :name "ipc-reply-frame-tree-case")))
    (handler-case
        (sb-thread:join-thread thread :timeout seconds :default nil)
      (sb-thread:join-thread-error () nil))
    (when (sb-thread:thread-alive-p thread)
      (sb-thread:terminate-thread thread)
      (setf status :timeout))
    (values status value)))

(defun frame-tree (depth)
  "Return a root frame split DEPTH levels deep with SPLIT-FRAME. Every
child points back at its parent."
  (let ((root (make-frame :width 1920 :height 1080)))
    (labels ((split (frame level)
               (when (plusp level)
                 (let ((second (split-frame frame
                                            (if (evenp level) :horizontal :vertical)
                                            0.5)))
                   (split (frame-first-child frame) (1- level))
                   (split second (1- level))))))
      (split root depth))
    root))

(defun reply-length-cap ()
  "Return the longest reply the dispatch may send: *REPLY-MAX-LENGTH*
plus the 3-character ellipsis, or 4099 in a tree without it."
  (+ 3 (if (boundp '*reply-max-length*)
           (symbol-value '*reply-max-length*)
           4096)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (setf (gethash :reply-printing-frame-tree *command-table*)
          (lambda (output args)
            (declare (ignore output args))
            (let ((cycle (list 1 2 3)))
              (setf (cdr (last cycle)) cycle)
              (list (frame-tree 4) cycle))))
    (multiple-value-bind (status reply)
        (run-with-deadline
         (lambda () (ipc-dispatch-line "reply-printing-frame-tree"))
         20)
      (ok "command returning a frame tree and a cycle returns" status :ok)
      (ok "command returning a frame tree and a cycle replies bounded"
          (and (stringp reply)
               (<= 9 (length reply) (reply-length-cap))
               (string= "(#S(FRAME" reply :end2 9))
          t))
    (remhash :reply-printing-frame-tree *command-table*)
    (multiple-value-bind (status reply)
        (run-with-deadline (lambda () (ipc-dispatch-line "(+ 1 2)")))
      (ok "dispatch still answers after the frame tree reply"
          (if (eq status :ok) reply status)
          "3")))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
