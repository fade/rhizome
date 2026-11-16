;;; launch-into-address.lisp: launch-into with a W:S:F address, the
;;; focused-frame shorthand, and frame creation past the existing count.
;;; Covers launching into an empty frame and an occupied one; an address
;;; that omits the frame, left to the placement policy; a frame number
;;; one, two, and four past the count, which splits the largest frame
;;; along its longer axis, and five past it, which is refused; a frame
;;; made for a launch surviving pruning while its claim is pending, with
;;; a control run where the hold is switched off; the shorthand with no
;;; target, "." for the focused frame, and an fselect label. Programs
;;; are not started: launch-spawn is stubbed and records its command.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))
(configure '(:animations (:enabled nil)))

(defmacro check (form)
  "FORM's value, or :ERROR when evaluating it signals an error."
  `(handler-case ,form
     (error (e)
       (format t "~&;   (error: ~A)~%" e)
       :error)))

(defvar *spawned* nil "Commands launch-spawn was called with, newest first.")
(defvar *said* nil "On-screen messages, newest first.")

(setf (symbol-function 'message)
      (lambda (fmt &rest args) (push (apply #'format nil fmt args) *said*) nil)
      (symbol-function 'request-manage-dirty)
      (lambda (&rest args) (declare (ignore args)) nil)
      (symbol-function 'launch-spawn)
      (lambda (command) (push command *spawned*) 4242))

(let ((pass 0) (fail 0))
  (flet ((report (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (macrolet ((ok (name actual expected)
                 `(report ,name (check ,actual) ,expected)))
      (labels ((reset (&rest config)
                 (setf *config* (default-config))
                 (configure '(:bar (:enabled nil)))
                 (configure '(:animations (:enabled nil)))
                 (configure '(:screens (:primary "DVI-D-1")))
                 (when config
                   (configure config))
                 (setf *outputs* (make-hash-table)
                       *windows* (make-hash-table)
                       *seats* (make-hash-table)
                       *display-heads* (make-hash-table)
                       *output-workspace* (make-hash-table)
                       *window-workspace* (make-hash-table)
                       *frame-trees* (make-hash-table :test 'equal)
                       *current-frames* (make-hash-table :test 'equal)
                       *scroll-states* (make-hash-table :test 'equal)
                       *selected-output* nil
                       *launch-claims* '()
                       *spawned* nil
                       *said* nil)
                 (clrhash *window-launch-targets*))
               (make-output (proxy connector x)
                 (let ((out (make-instance 'rhizome-output :proxy proxy
                                           :x x :y 0
                                           :width 1920 :height 1080)))
                   (setf (gethash proxy *outputs*) out
                         (gethash out *output-workspace*) 1
                         (gethash proxy *display-heads*)
                         (list :head proxy :name connector :enabled t
                               :position (cons x 0)))
                   out))
               (two-screens ()
                 (values (make-output :dvi "DVI-D-1" 1920)
                         (make-output :hdmi "HDMI-A-1" 0)))
               (window-in (proxy output workspace frame &optional title)
                 (let ((win (make-instance 'rhizome-window :proxy proxy)))
                   (setf (gethash proxy *windows*) win
                         (window-output win) output
                         (window-title win) title)
                   (assign-window-workspace win workspace)
                   (frame-push-window win frame)
                   win))
               (launch (&rest words)
                 (execute-command (current-output) :launch-into words))
               (claim ()
                 (first (last *launch-claims*)))
               (claim-address ()
                 (let ((frame (launch-claim-frame (claim))))
                   (and frame (frame-address frame))))
               (sizes (output workspace)
                 (mapcar (lambda (f) (list (frame-width f) (frame-height f)))
                         (frame-leaves (frame-tree-for output workspace)))))

        ;; --- An address naming an existing frame ---

        (reset)
        (multiple-value-bind (dvi hdmi) (two-screens)
          (setf *selected-output* dvi)
          (let ((root (frame-tree-for hdmi 2)))
            (split-frame root :horizontal 1/2)
            (window-in :w1 hdmi 2 (first (frame-leaves root)) "editor"))
          (ok "an address launches into that frame and names it"
              (launch "2:2:2" "foot" "-a" "probe")
              "launched foot -a probe into 2:2:2")
          (ok "the command is started"
              *spawned* '("foot -a probe"))
          (ok "the claim targets that frame on that hidden workspace"
              (list (eq (launch-claim-output (claim)) hdmi)
                    (launch-claim-workspace (claim))
                    (claim-address))
              '(t 2 "2:2:2"))
          (ok "no workspace switches"
              (current-workspace hdmi) 1)
          (launch "2:2:1" "foot")
          (ok "an occupied frame is a target too"
              (claim-address) "2:2:1")
          (let ((win (make-instance 'rhizome-window :proxy :w2)))
            (setf (gethash :w2 *windows*) win
                  (gethash win *window-launch-targets*) (claim))
            (ok "the placement policy hands the launched window its frame"
                (frame-address (placement-frame-for-new-window win hdmi 2))
                "2:2:1")))

        ;; --- An address with no frame ---

        (reset)
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore dvi))
          (ok "an address without a frame names the workspace only"
              (launch "3:2:" "foot") "launched foot into 3:2")
          (ok "and leaves the frame to the placement policy"
              (list (eq (launch-claim-output (claim)) hdmi)
                    (launch-claim-workspace (claim))
                    (launch-claim-frame (claim)))
              '(t 3 nil)))

        ;; --- A frame past the count ---

        (reset)
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (ok "one past the count splits the only frame"
              (launch "1:1:2" "foot") "launched foot into 1:1:2")
          (ok "along its longer axis, 50/50"
              (sizes dvi 1) '((960 1080) (960 1080)))
          (ok "two past the count from one frame makes three"
              (launch "2:1:3" "foot") "launched foot into 2:1:3")
          (ok "each time splitting the largest frame along its longer axis"
              (sizes dvi 2) '((960 540) (960 540) (960 1080)))
          (ok "the claim holds the new frame"
              (claim-address) "2:1:3")
          (ok "four past the count is allowed"
              (launch "3:1:5" "foot") "launched foot into 3:1:5")
          (ok "and makes five frames"
              (length (frame-leaves (frame-tree-for dvi 3))) 5)
          (setf *spawned* nil *said* nil)
          (ok "five past the count is refused"
              (launch "4:1:6" "foot") nil)
          (ok "with a message naming the existing count"
              (first *said*)
              "launch-into: no frame 6: 4:1 has 1 frame; at most 4 more can be made")
          (ok "and nothing started or split"
              (list *spawned* (length (frame-leaves (frame-tree-for dvi 4))))
              '(nil 1)))

        ;; --- A frame made for a launch survives pruning ---

        (reset :frames '(:prune-empty :always))
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore hdmi))
          (window-in :p1 dvi 1 (frame-tree-for dvi 1))
          (window-in :p2 dvi 2 (frame-tree-for dvi 2))
          (launch "1:1:2" "foot")
          (let ((*frames-held-function* nil))
            (prune-empty-frames dvi 1))
          (ok "control: without the hold pruning removes the waiting frame"
              (length (frame-leaves (frame-tree-for dvi 1))) 1)
          (launch "2:1:2" "foot")
          (ok "the window already there moves to the first half"
              (frame-windows (first (frame-leaves (frame-tree-for dvi 2))))
              (list (gethash :p2 *windows*)))
          (prune-empty-frames dvi 2)
          (ok "a frame with a pending claim is kept"
              (length (frame-leaves (frame-tree-for dvi 2))) 2)
          (setf (launch-claim-expires (claim)) 0
                *launch-claims* (remove-if (lambda (c)
                                             (< (launch-claim-expires c)
                                                (get-universal-time)))
                                           *launch-claims*))
          (prune-empty-frames dvi 2)
          (ok "once the claim lapses the frame prunes as usual"
              (length (frame-leaves (frame-tree-for dvi 2))) 1))

        ;; --- The focused-frame shorthand ---

        (reset)
        (multiple-value-bind (dvi hdmi) (two-screens)
          (declare (ignore dvi))
          (setf *selected-output* hdmi)
          (let ((root (frame-tree-for hdmi 1)))
            (split-frame root :horizontal 1/2)
            (setf (gethash (frame-key hdmi 1) *current-frames*)
                  (second (frame-leaves root))))
          (ok "with no target the whole line launches into the focused frame"
              (launch "foot" "-a" "x") "launched foot -a x into 1:2:2")
          (ok "the claim holds the focused frame"
              (claim-address) "1:2:2")
          (ok "\".\" names the focused frame for a one-character command"
              (launch "." "w") "launched w into 1:2:2")
          (ok "and runs it"
              (first *spawned*) "w"))

        ;; --- Labels and errors ---

        (reset)
        (multiple-value-bind (dvi hdmi) (two-screens)
          (window-in :w3 dvi 1 (frame-tree-for dvi 1) "mail")
          (window-in :w4 hdmi 1 (frame-tree-for hdmi 1) "web")
          (setf *selected-output* dvi)
          (ok "a one-character target is still an fselect label"
              (launch "1" "foot") "launched foot into frame 1")
          (setf *said* nil)
          (ok "a label with no command replies nothing"
              (launch "1") nil)
          (ok "and says why"
              (first *said*) "launch-into: no frame labelled 1, or no command")
          (setf *said* nil)
          (ok "an address naming no screen replies nothing"
              (launch "1:5:1" "foot") nil)
          (ok "and the message names the missing screen"
              (first *said*) "launch-into: no screen 5")
          (ok "nothing at all is a usage message"
              (launch) nil))

        (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
        (sb-ext:exit :code (if (zerop fail) 0 1))))))
