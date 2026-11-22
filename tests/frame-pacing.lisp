;;; Frame pacing: check the deadline arithmetic that spaces animation
;;; frames one interval apart from each frame's start, and the main
;;; loop's choice between starting a frame and waiting on the display.
(ql:quickload :rhizome)

(in-package #:rhizome)

(let ((pass 0) (fail 0)
      (units internal-time-units-per-second))
  (labels ((check (name ok detail)
             (if ok
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (~A)~%" name detail))))
           (near (a b)
             (and (realp a) (< (abs (- a b)) 1/10000)))
           (call (function &rest args)
             (if (fboundp function)
                 (apply function args)
                 :undefined)))

    ;; --- the deadline: previous frame start plus one interval ---

    (let ((got (call 'frame-wait-seconds nil 1000)))
      (check "no previous frame: no wait" (and (realp got) (zerop got))
             (format nil "got ~S" got)))
    (let ((got (call 'frame-wait-seconds 1000 1000)))
      (check "just after a frame start: wait one interval"
             (near got 1/60) (format nil "got ~S" got)))
    (let ((got (call 'frame-wait-seconds 1000 (+ 1000 (floor units 120)))))
      (check "half an interval elapsed waits the other half"
             (near got 1/120) (format nil "got ~S" got)))
    (let ((got (call 'frame-wait-seconds 1000 (+ 1000 (ceiling units 60)))))
      (check "at the deadline: no wait" (and (realp got) (zerop got))
             (format nil "got ~S" got)))
    (let ((got (call 'frame-wait-seconds 1000 (+ 1000 units))))
      (check "past the deadline: no wait, never negative"
             (and (realp got) (zerop got)) (format nil "got ~S" got)))
    (let ((got (call 'frame-wait-seconds 1000 1000 1/30)))
      (check "the interval argument sets the spacing"
             (near got 1/30) (format nil "got ~S" got)))
    (let ((got (call 'frame-wait-seconds 1000 (+ 1000 (floor units 120)))))
      (check "the wait is a float, usable as a poll timeout"
             (floatp got) (format nil "got ~S" got)))

    ;; --- the loop's choice: start a frame, or wait on the display ---

    (let ((got (call 'animation-frame-wait nil 1000 nil)))
      (check "first frame of a burst starts at once" (null got)
             (format nil "got ~S" got)))
    (let ((got (call 'animation-frame-wait 1000 1000 nil)))
      (check "before the deadline the loop waits for the rest"
             (near got 1/60) (format nil "got ~S" got)))
    (let ((got (call 'animation-frame-wait 1000 (+ 1000 units) nil)))
      (check "past the deadline with no manage out: start a frame"
             (null got) (format nil "got ~S" got)))
    (let ((got (call 'animation-frame-wait 1000 (+ 1000 units) t)))
      (check "past the deadline with a manage still out: wait for river"
             (and (realp got) (plusp got)) (format nil "got ~S" got)))
    (let ((got (call 'animation-frame-wait 1000 (+ 1000 units) t)))
      (check "the wait for river is at most one interval"
             (and (realp got) (<= got (+ 1/60 1/10000)))
             (format nil "got ~S" got))))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
