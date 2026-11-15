;;; Active bar underline: under (:bar :active-screen t), with more than
;;; one output, the current output's bar carries a line along its
;;; window-facing edge in the :bar-active-underline theme element, so
;;; the active bar differs from the inactive ones by shape as well as
;;; by background colour. (:bar :active-underline-width) sets the
;;; line's width, 1-5 px.
;;;
;;; No compositor: the snapshot is stubbed, the bar renders into a
;;; foreign buffer, and the surface calls are stubbed out.
(ql:quickload :rhizome)

(in-package #:rhizome)

(defvar *ul-pass* 0)
(defvar *ul-fail* 0)

(defun ul-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *ul-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *ul-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro ul-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *ul-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

(defun ul-config (&optional overrides)
  "Set *config* to the defaults with the plist OVERRIDES merged over
them, and forget loaded themes."
  (setf *config* (merge-plist (default-config) overrides))
  (clear-theme-cache))

;;; --- The cue table and the hue-only check ---

(defparameter *ul-pair* '(:bar-active :bar-inactive))

(defun ul-table-without-cue ()
  "*state-cues* with the bar pair's cues set equal, so the pair differs
in colour alone."
  (mapcar (lambda (entry)
            (if (eq (first entry) :bar-active)
                (list :bar-active :against :bar-inactive
                      :elements (getf (rest entry) :elements)
                      :cues '((:shape :none :none)))
                entry))
          *state-cues*))

(ul-case "the cue table"
  (ul-config)
  (ul-ok "the bar pair is in the table with its elements and cue"
         (rest (assoc :bar-active *state-cues*))
         '(:against :bar-inactive
           :elements (:bar-background-active :bar-background-inactive)
           :cues ((:shape :underline :none))))
  (ul-ok "the active bar draws underlined"
         (state-cue-flags :bar-active) '(:underline t))
  (ul-ok "an inactive bar draws plain"
         (state-cue-flags :bar-inactive) nil)
  (ul-ok "no state takes two values for one attribute"
         (state-cue-conflicts) nil))

(ul-case "the hue-only check"
  (dolist (name '("tokyo-night-emacs" "ef-summer" "kusanagi" "tokyo-night"))
    (set-theme name)
    (ul-ok (format nil "~A: the bar pair carries a non-hue cue" name)
           (member *ul-pair* (hue-only-state-pairs) :test #'equal)
           nil)
    (ul-ok (format nil "~A: without the cue, the bar pair is hue-only" name)
           (and (member *ul-pair* (hue-only-state-pairs
                                   :table (ul-table-without-cue))
                        :test #'equal)
                t)
           t))
  (ul-config))

;;; --- Width ---

(ul-case "underline width"
  (ul-config)
  (ul-ok "the width defaults to 2 px" (bar-active-underline-width) 2)
  (loop for (width wanted) in '((1 1) (5 5) (3 3) (0 1) (9 5) (-2 1)
                                ("4" 2) (nil 2) (2.5 2))
        do (ul-config (list :bar (list :active-underline-width width)))
           (ul-ok (format nil "width ~S gives ~D px" width wanted)
                  (bar-active-underline-width) wanted))
  (ul-ok "the rows sit on the bottom edge of a top-anchored bar"
         (multiple-value-list (bar-active-underline-rows 20 3))
         '(17 3))
  (ul-ok "the rows never exceed the bar height"
         (multiple-value-list (bar-active-underline-rows 2 5))
         '(0 2))
  (ul-ok "the default config declares the width"
         (getf (getf (default-config) :bar) :active-underline-width) 2))

;;; --- Colour ---

(ul-case "underline colour"
  (ul-config)
  (ul-ok "the underline follows the bar foreground by default"
         (theme-color :bar-active-underline) (theme-color :bar-foreground))
  (ul-config '(:bar (:foreground "#abcdef")))
  (ul-ok "a changed bar foreground carries the underline with it"
         (theme-color :bar-active-underline) "#abcdef")
  (ul-config '(:theme (:elements (:bar-active-underline "#ff0000"))))
  (ul-ok "an element setting gives the underline its own colour"
         (list (theme-color :bar-active-underline)
               (theme-color :bar-foreground))
         '("#ff0000" "#c0caf5"))
  (dolist (name '("tokyo-night-emacs" "ef-summer" "kusanagi"))
    (ul-config)
    (set-theme name)
    (ul-ok (format nil "~A: the underline takes the theme's bar foreground" name)
           (theme-color :bar-active-underline)
           (theme-color :bar-foreground)))
  (ul-config))

;;; --- Rendering ---

(defparameter *ul-two-heads*
  '(:outputs ((:wl-output 10 :x 0 :current t :bar t)
              (:wl-output 11 :x 1920 :current nil :bar t))))

(defparameter *ul-one-head*
  '(:outputs ((:wl-output 10 :x 0 :current t :bar t))))

(defvar *ul-snapshot* nil)
(setf (symbol-function 'bar-snapshot) (lambda () *ul-snapshot*))
(dolist (name '(wl:wl-surface.attach wl:wl-surface.damage wl:wl-surface.commit))
  (setf (fdefinition name) (lambda (&rest args) (declare (ignore args)) nil)))

(defparameter *ul-width* 200)
(defparameter *ul-height* 20)

(defun ul-render (snapshot entry-index overrides)
  "Render the bar of SNAPSHOT's output ENTRY-INDEX under the defaults
merged with OVERRIDES, with no modules. Returns (ROWS PIXEL): ROWS
lists, top to bottom, the number of pixels in each row that equal
PIXEL, the underline colour at the bar's alpha."
  (ul-config (merge-plist
              '(:bar (:modules (:left () :center () :right ())))
              overrides))
  (setf *ul-snapshot* snapshot)
  (let* ((w *ul-width*) (h *ul-height*)
         (ptr (cffi:foreign-alloc :unsigned-int :count (* w h)
                                                :initial-element 0))
         (state (make-hash-table)))
    (setf (gethash :shm-ptr state) ptr (gethash :width state) w
          (gethash :height state) h (gethash :surface state) :surface
          (gethash :wl-buffer state) :buffer)
    (unwind-protect
         (let ((*bar-output-entry* (nth entry-index (getf snapshot :outputs)))
               (pixel (multiple-value-bind (r g b)
                          (theme-rgb :bar-active-underline)
                        (make-argb r g b :alpha (bar-surface-alpha)))))
           (%bar-render state)
           (list (loop for y below h
                       collect (loop for x below w
                                     count (= pixel (cffi:mem-aref
                                                     ptr :unsigned-int
                                                     (+ (* y w) x)))))
                 pixel))
      (cffi:foreign-free ptr))))

(defun ul-rows (snapshot entry-index overrides)
  "The per-row underline pixel counts of ul-render."
  (first (ul-render snapshot entry-index overrides)))

(defun ul-marked-rows (rows)
  "The indices of ROWS that hold any underline pixel."
  (loop for count in rows for y from 0 when (plusp count) collect y))

(ul-case "render"
  (let ((rows (ul-rows *ul-two-heads* 0 '(:bar (:active-screen t)))))
    (ul-ok "the active bar's bottom two rows carry the underline"
           (ul-marked-rows rows) '(18 19))
    (ul-ok "the underline spans the bar's width inside its rounded corners"
           (every (lambda (n) (> n (- *ul-width* 30))) (last rows 2))
           t))
  (ul-ok "with no rounded corners the underline fills both rows edge to edge"
         (last (ul-rows *ul-two-heads* 0
                        '(:bar (:active-screen t :corner-radius 0)))
               2)
         (list *ul-width* *ul-width*))
  (ul-ok "(:bar :active-underline-width 4) draws four rows"
         (ul-marked-rows (ul-rows *ul-two-heads* 0
                                  '(:bar (:active-screen t
                                          :active-underline-width 4))))
         '(16 17 18 19))
  (ul-ok "an inactive bar carries no underline"
         (ul-marked-rows (ul-rows *ul-two-heads* 1 '(:bar (:active-screen t))))
         nil)
  (ul-ok "with :active-screen unset, the current bar carries no underline"
         (ul-marked-rows (ul-rows *ul-two-heads* 0 nil))
         nil)
  (ul-ok "with :active-screen nil, the current bar carries no underline"
         (ul-marked-rows (ul-rows *ul-two-heads* 0 '(:bar (:active-screen nil))))
         nil)
  (ul-ok "with one output, its bar carries no underline"
         (ul-marked-rows (ul-rows *ul-one-head* 0 '(:bar (:active-screen t))))
         nil)
  (ul-ok "a theme's own underline colour is the colour drawn"
         (let ((result (ul-render *ul-two-heads* 0
                                  '(:bar (:active-screen t)
                                    :theme (:elements
                                            (:bar-active-underline "#ff0000"))))))
           (list (ul-marked-rows (first result))
                 (logand #xffffff (second result))))
         (list '(18 19)
               (logand #xffffff (make-argb 255 0 0 :alpha (bar-surface-alpha))))))

(format t "~&=== ~A passed, ~A failed ===~%" *ul-pass* *ul-fail*)
(uiop:quit (if (zerop *ul-fail*) 0 1))
