;;; Active-screen indicator: with more than one output, the current
;;; output's bar takes (:bar :background-active) and every other bar
;;; (:bar :background-inactive); a current output without a bar shows
;;; a line along one edge, set under (:screens :active-edge
;;; :active-edge-width :active-edge-color).
;;;
;;; No compositor: the snapshot is stubbed and no surface is created.
(ql:quickload :rhizome)

(in-package #:rhizome)

(defvar *as-pass* 0)
(defvar *as-fail* 0)

(defun as-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *as-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *as-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro as-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *as-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

(defun as-config (&optional overrides)
  "Set *config* to the defaults with the plist OVERRIDES merged over
them."
  (setf *config* (merge-plist (default-config) overrides)))

;;; --- Bar background ---

(as-case "bar background defaults"
  (as-config)
  (as-ok "the active bar background defaults to #392e7a"
         (bar-background-color t) "#392e7a")
  (as-ok "the inactive bar background defaults to #1a1b26"
         (bar-background-color nil) "#1a1b26"))

(as-case "bar background settings"
  (as-config '(:bar (:background-active "#444444"
                     :background-inactive "#111111")))
  (as-ok "(:bar :background-active) sets the active background"
         (bar-background-color t) "#444444")
  (as-ok "(:bar :background-inactive) sets the inactive background"
         (bar-background-color nil) "#111111")
  (as-config '(:bar (:background "#222222")))
  (as-ok "without :background-inactive, :background is the inactive background"
         (bar-background-color nil) "#222222"))

(defparameter *as-two-heads*
  '(:outputs ((:wl-output 10 :x 0 :current t :bar t)
              (:wl-output 11 :x 1920 :current nil :bar t))))

(defparameter *as-one-head*
  '(:outputs ((:wl-output 10 :x 0 :current t :bar t))))

(as-case "which bar is active"
  (as-ok "the current output's bar is active with two outputs"
         (bar-active-screen-p (first (getf *as-two-heads* :outputs))
                              *as-two-heads*)
         t)
  (as-ok "another output's bar is inactive"
         (bar-active-screen-p (second (getf *as-two-heads* :outputs))
                              *as-two-heads*)
         nil)
  (as-ok "a single output's bar is inactive: one screen is always current"
         (bar-active-screen-p (first (getf *as-one-head* :outputs))
                              *as-one-head*)
         nil))

;;; A focus change between outputs must redraw both bars even when
;;; no module's text changes.
(defvar *as-snapshot* nil)
(setf (symbol-function 'bar-snapshot) (lambda () *as-snapshot*))

(as-case "a focus change redraws the bars"
  (as-config '(:bar (:modules (:left (:layout) :center () :right ()))))
  (let ((head (make-hash-table)))
    (setf (gethash :global-name head) 11)
    (setf *as-snapshot* *as-two-heads*)
    (bar-content-changed-p head)
    (as-ok "an unchanged snapshot draws nothing new"
           (bar-content-changed-p head) nil)
    (setf *as-snapshot*
          '(:outputs ((:wl-output 10 :x 0 :current nil :bar t)
                      (:wl-output 11 :x 1920 :current t :bar t))))
    (as-ok "the bar whose output became current redraws"
           (and (bar-content-changed-p head) t) t)))

;;; --- Edge line settings ---

(as-case "edge style defaults"
  (as-config)
  (as-ok "the line defaults to the top edge, 3 px, in the focused border colour"
         (multiple-value-list (active-edge-style))
         (list :top 3 #x7a #xa2 #xf7)))

(as-case "edge style settings"
  (dolist (edge '(:top :left :bottom :right))
    (as-config (list :screens (list :active-edge edge)))
    (as-ok (format nil "(:screens :active-edge ~S) puts the line on that edge" edge)
           (nth-value 0 (active-edge-style)) edge))
  (as-config '(:screens (:active-edge nil)))
  (as-ok "(:screens :active-edge nil) turns the line off"
         (active-edge-style) nil)
  (as-config '(:screens (:active-edge :middle)))
  (as-ok "an edge outside :top :left :bottom :right turns the line off"
         (active-edge-style) nil)
  (loop for (width wanted) in '((1 1) (5 5) (0 1) (9 5) (-2 1) ("3" 3) (nil 3))
        do (as-config (list :screens (list :active-edge-width width)))
           (as-ok (format nil "width ~S gives ~D px" width wanted)
                  (nth-value 1 (active-edge-style)) wanted))
  (as-config '(:screens (:active-edge-color "#ffffff")))
  (as-ok "(:screens :active-edge-color) sets the colour"
         (cddr (multiple-value-list (active-edge-style)))
         (list 255 255 255)))

;;; --- Edge geometry ---

(as-case "edge geometry"
  ;; an output at x=1908, 1024 x 768
  (loop for (edge width wanted)
          in '((:top 3 (1908 0 1024 3))
               (:bottom 3 (1908 765 1024 3))
               (:left 3 (1908 0 3 768))
               (:right 3 (2929 0 3 768))
               (:top 1 (1908 0 1024 1))
               (:right 5 (2927 0 5 768))
               (:bottom 5 (1908 763 1024 5)))
        do (as-ok (format nil "~S ~D px" edge width)
                  (multiple-value-list
                   (active-edge-geometry edge width 1908 0 1024 768))
                  wanted)))

;;; --- When the line shows ---

(as-case "edge hidden rules"
  (as-ok "shown on the current output with no bar, two outputs"
         (active-edge-wanted-p :b :b 2 nil) t)
  (as-ok "hidden on an output that is not current"
         (active-edge-wanted-p :a :b 2 nil) nil)
  (as-ok "hidden where a bar shows"
         (active-edge-wanted-p :b :b 2 t) nil)
  (as-ok "hidden with a single output"
         (active-edge-wanted-p :b :b 1 nil) nil))

;;; --- Target on real outputs ---

(setf *wm-process-mode* nil
      *bar-process-mode* nil
      *outputs* (make-hash-table))

(defvar *as-a* (make-instance 'rhizome-output :proxy :a-proxy :wl-output-name 10
                              :x 0 :y 0 :width 1908 :height 989))
(defvar *as-b* (make-instance 'rhizome-output :proxy :b-proxy :wl-output-name 11
                              :x 1908 :y 0 :width 1024 :height 768))
(setf (gethash :a-proxy *outputs*) *as-a*
      (gethash :b-proxy *outputs*) *as-b*)

(as-case "edge target"
  (as-config '(:bar (:outputs (0))))
  (as-ok "the bar-less current output gets the line"
         (multiple-value-list (active-edge-target *as-b* *as-b*))
         (list 1908 0 1024 3))
  (as-ok "the current output with a bar gets none"
         (active-edge-target *as-a* *as-a*) nil)
  (as-ok "an output that is not current gets none"
         (active-edge-target *as-b* *as-a*) nil)
  (as-config '(:bar (:enabled nil)))
  (as-ok "with the bar disabled, the current output gets the line"
         (multiple-value-list (active-edge-target *as-a* *as-a*))
         (list 0 0 1908 3))
  (as-config '(:bar (:outputs (0)) :screens (:active-edge :left :active-edge-width 2)))
  (as-ok "the edge and width settings place the line"
         (multiple-value-list (active-edge-target *as-b* *as-b*))
         (list 1908 0 2 768)))

(as-case "default config"
  (let ((defaults (default-config)))
    (as-ok "the default config declares the active bar background"
           (getf (getf defaults :bar) :background-active) "#392e7a")
    (as-ok "the default config declares the edge line"
           (list (getf (getf defaults :screens) :active-edge)
                 (getf (getf defaults :screens) :active-edge-width))
           (list :top 3))))

(format t "~&=== ~A passed, ~A failed ===~%" *as-pass* *as-fail*)
(uiop:quit (if (zerop *as-fail*) 0 1))
