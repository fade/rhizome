;;; Render timing: milliseconds per full static bar render (1920x34
;;; and 5120x34), per tick redraw (a module change over cached static
;;; layers), and per shadow redraw for an 800x600 float. Prints the
;;; median of 25 runs each. Not a test: it asserts nothing. It runs
;;; against any revision of rhizome, so the same script times the
;;; code before and after a change:
;;;
;;;   CL_SOURCE_REGISTRY=<checkout>//: sbcl --non-interactive \
;;;     --load tests/bench/render-timing.lisp
;;;
;;; River and the snapshot file are stubbed; pixels go to private
;;; buffers.
(ql:quickload :rhizome :silent t)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defparameter *timing-runs* 25)

(defun timing-now-ms ()
  "A monotonic clock reading in ms, at nanosecond resolution on SBCL."
  #+sbcl (multiple-value-bind (s ns)
             (sb-unix:clock-gettime sb-unix:clock-monotonic)
           (+ (* s 1000d0) (/ ns 1d6)))
  #-sbcl (/ (get-internal-real-time)
            (/ internal-time-units-per-second 1000d0)))

(defun timing-median (thunk)
  "The median wall time in ms of *timing-runs* calls of THUNK, after
one call to warm up."
  (funcall thunk)
  (let ((samples (sort (loop repeat *timing-runs*
                             collect (let ((t0 (timing-now-ms)))
                                       (funcall thunk)
                                       (- (timing-now-ms) t0)))
                       #'<)))
    (nth (floor (length samples) 2) samples)))

(dolist (name '(wl:wl-surface.attach wl:wl-surface.damage wl:wl-surface.commit))
  (setf (symbol-function name) (lambda (&rest args) (declare (ignore args)) nil)))
(setf (symbol-function 'bar-snapshot)
      (lambda () '(:workspace 1 :workspace-count 9 :occupied (1 2 3))))
(setf (symbol-function 'bar-ostate-active-p) (lambda () t))
(setf *config*
      (merge-plist (default-config)
                   '(:bar (:enabled nil :height 34 :corner-radius 10
                           :active-screen t
                           :modules (:left (:workspaces) :center ()
                                     :right (:clock))))))
(font-ensure-initialized)

(defun timing-bar (width height)
  "(FULL TICK) median ms for a WIDTH x HEIGHT bar."
  (let ((state (make-hash-table))
        (ptr (cffi:foreign-alloc :uint32 :count (* width height)
                                         :initial-element 0)))
    (setf (gethash :shm-ptr state) ptr
          (gethash :width state) width
          (gethash :height state) height
          (gethash :pad state) 0)
    (unwind-protect
         (list (timing-median (lambda ()
                                ;; drop the cached static layers when
                                ;; the revision keeps any
                                (remhash :static-spec state)
                                (%bar-render state)))
               (timing-median (lambda () (%bar-render state))))
      (when (fboundp 'bar-free-static)
        (funcall 'bar-free-static state))
      (cffi:foreign-free ptr))))

(defun timing-shadow ()
  "(MS WIDTH HEIGHT) for one shadow redraw of an 800x600 float at the
revision's default band."
  (if (fboundp 'decoration-paint)
      (let* ((band 16) (w (+ 800 (* 2 band))) (h (+ 600 (* 2 band)))
             (ptr (cffi:foreign-alloc :uint32 :count (* w h))))
        (unwind-protect
             (list (timing-median
                    (lambda ()
                      (funcall 'decoration-paint ptr w h band band (- band 4)
                               800 600 (* 255 0.4) 30 30 36)))
                   w h)
          (cffi:foreign-free ptr)))
      (let* ((band 12) (w (+ 800 (* 2 band))) (h (+ 600 (* 2 band)))
             (state (list :ptr (cffi:foreign-alloc :uint32 :count (* w h))
                          :focused t)))
        (unwind-protect
             (list (timing-median
                    (lambda ()
                      (funcall 'decoration-draw state w h band 0.4 0.6
                               122 162 247)))
                   w h)
          (cffi:foreign-free (getf state :ptr))))))

(let ((narrow (timing-bar 1920 34))
      (wide (timing-bar 5120 34))
      (shadow (timing-shadow)))
  (format t "~&rhizome at ~A~%" (asdf:system-source-directory :rhizome))
  (format t "median of ~D runs, ms~%" *timing-runs*)
  (format t "full static bar 1920x34   ~8,2F~%" (first narrow))
  (format t "full static bar 5120x34   ~8,2F~%" (first wide))
  (format t "tick redraw     1920x34   ~8,2F~%" (second narrow))
  (format t "tick redraw     5120x34   ~8,2F~%" (second wide))
  (format t "shadow 800x600 (~Dx~D) ~8,2F~%" (second shadow) (third shadow)
          (first shadow)))
