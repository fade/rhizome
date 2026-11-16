;;; prompt-popup.lisp: how the candidate prompt draws. Covers the popup
;;; height, which holds whole rows and no more rows than there are
;;; candidates; the selected row, drawn as a band the full height of
;;; the row and inside the popup at every scroll position; and the
;;; background, which is opaque whatever the bar's opacity. The prompt
;;; runs for real into a foreign buffer; only the Wayland requests are
;;; stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defmacro check (form)
  "FORM's value, or :ERROR when evaluating it signals an error."
  `(handler-case ,form
     (error (e)
       (format t "~&;   (error: ~A)~%" e)
       :error)))

(defvar *sizes* nil "Sizes the prompt asked its layer surface for, newest first.")

(setf *compositor* :fake-compositor
      *shm* :fake-shm
      *layer-shell* :fake-layer-shell)
(setf (symbol-function 'wl:wl-compositor.create-surface)
      (lambda (&rest a) (declare (ignore a)) :fake-surface)
      (symbol-function 'rwm:zwlr-layer-shell-v1.get-layer-surface)
      (lambda (&rest a) (declare (ignore a)) (make-instance 'wl:wl-proxy))
      (symbol-function 'rwm:zwlr-layer-surface-v1.set-size)
      (lambda (surface width height)
        (declare (ignore surface))
        (push (list width height) *sizes*)))
(dolist (fn '(rwm:zwlr-layer-surface-v1.set-anchor
              rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
              rwm:zwlr-layer-surface-v1.set-margin
              rwm:zwlr-layer-surface-v1.destroy
              wl:wl-surface.attach
              wl:wl-surface.damage
              wl:wl-surface.commit
              wl:wl-surface.destroy
              reassert-seat-focus))
  (setf (symbol-function fn)
        (lambda (&rest a) (declare (ignore a)) nil)))

(defun open-sized (title candidates)
  "Open a prompt with CANDIDATES, give it a buffer the size it asked
for, and return (VALUES WIDTH HEIGHT)."
  (font-ensure-initialized)
  (prompt-open title :candidates candidates :on-complete #'identity)
  (destructuring-bind (width height) (first *sizes*)
    (let ((state *prompt-state*))
      (setf (gethash :width state) width
            (gethash :height state) height
            (gethash :shm-ptr state)
            (cffi:foreign-alloc :uint32 :count (* width height)
                                        :initial-element 0)))
    (values width height)))

(defun free-buffer ()
  "Free the test buffer and close the prompt."
  (let ((ptr (gethash :shm-ptr *prompt-state*)))
    (remhash :shm-ptr *prompt-state*)
    (prompt-close)
    (cffi:foreign-free ptr)))

(defun pixel (x y)
  "The premultiplied ARGB pixel at (X Y) of the open prompt."
  (get-pixel-argb (gethash :shm-ptr *prompt-state*)
                  (gethash :width *prompt-state*)
                  (gethash :height *prompt-state*)
                  x y))

(defun selection-band (x)
  "The pixel rows between the border lines where column X holds the
selection colour, as (FIRST LAST COUNT), or NIL when none does."
  (let ((want (multiple-value-bind (r g b) (theme-rgb :prompt-selection)
                (make-argb r g b)))
        (rows '()))
    (loop for y from 1 below (1- (gethash :height *prompt-state*))
          when (= (pixel x y) want)
            do (push y rows))
    (and rows
         (list (reduce #'min rows) (reduce #'max rows) (length rows)))))

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
      (let ((many (loop for i from 1 to 30
                        collect (format nil "1:1:~D row" i))))

        ;; --- Height ---

        (setf *sizes* nil)
        (multiple-value-bind (width height) (open-sized "Pick:" '("a" "b" "c"))
          (declare (ignore width))
          (ok "three candidates: the popup holds exactly three rows"
              (prompt-visible-rows height (font-line-height)) 3)
          (free-buffer))
        (multiple-value-bind (width height) (open-sized "Free:" nil)
          (declare (ignore width))
          (ok "no candidates: the popup is the input line alone"
              (< height (* 2 (font-line-height))) t)
          (free-buffer))

        ;; --- The selected row at every scroll position ---

        (setf *sizes* nil)
        (multiple-value-bind (width height) (open-sized "Launch into:" many)
          (let* ((row-height (font-line-height))
                 (visible (prompt-visible-rows height row-height))
                 (x (- width 12))
                 (bad '()))
            (ok "thirty candidates show more than one row and fewer than all"
                (< 1 visible 30) t)
            (dotimes (selection 30)
              (setf *prompt-selection* selection)
              (prompt-redraw)
              (let ((band (selection-band x)))
                (unless (and band
                             (= (third band) row-height)
                             (= (- (second band) (first band) -1) row-height)
                             (<= (second band) (- height 2)))
                  (push (list selection band) bad))))
            (ok "the selection is a solid band the height of its row"
                (length bad) 0)
            (when bad
              (format t "~&;   first bad selections: ~S~%"
                      (subseq (reverse bad) 0 (min 3 (length bad)))))
            (setf *prompt-selection* 29)
            (prompt-redraw)
            (ok "scrolled to the end, the last visible row ends inside the border"
                (<= (+ (prompt-row-top (1- visible) row-height) row-height)
                    (1- height))
                t)
            (ok "and the selection band is that row"
                (let ((last-top (prompt-row-top (1- visible) row-height)))
                  (equal (selection-band x)
                         (list last-top (+ last-top row-height -1)
                               row-height)))
                t))
          (free-buffer))

        ;; --- An opaque background ---

        (configure '(:bar (:opacity 0.5)))
        (setf *sizes* nil)
        (multiple-value-bind (width height) (open-sized "Launch into:" many)
          (setf *prompt-selection* 0)
          (prompt-redraw)
          (let ((row-height (font-line-height)))
            (ok "the background is opaque with a translucent bar"
                (ldb (byte 8 24) (pixel (- width 12)
                                        (prompt-row-top 2 row-height)))
                255)
            (ok "the selection is opaque too"
                (ldb (byte 8 24) (pixel (- width 12)
                                        (prompt-row-top 0 row-height)))
                255)
            (ok "the corners stay clear"
                (ldb (byte 8 24) (pixel 0 (1- height))) 0))
          (free-buffer))
        (configure '(:bar (:opacity 0.9))))

      (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
      (sb-ext:exit :code (if (zerop fail) 0 1)))))
