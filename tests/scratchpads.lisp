;;; scratchpads.lisp: the scratchpad module and the minimize paths
;;; that share its hide and show. Hiding a scratchpad, a client's
;;; minimize request, and the context menu's Minimize each reach river
;;; as one hide request in the next render pass; showing a scratchpad
;;; and the menu's Restore each reach it as one show request. River's
;;; show and hide requests are replaced by recorders, so nothing here
;;; needs a compositor.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; The scratchpad module loads from the repo's modules/ directory, the
;; way a user loads it
(let ((*module-directory*
        (make-pathname
          :directory (append (pathname-directory
                               (asdf:system-source-directory :rhizome))
                             '("modules")))))
  (load-module "rhizome-scratchpads"))

;; Never start the bar from a test run
(configure '(:bar (:enabled nil) :theme (:titlebar nil)))

(defvar *river-requests* nil
  "River show and hide requests as (:show PROXY) or (:hide PROXY),
most recent first.")

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; River's show and hide: record, never send
    (setf (symbol-function 'rwm:river-window-v1.hide)
          (lambda (proxy) (push (list :hide proxy) *river-requests*) nil)
          (symbol-function 'rwm:river-window-v1.show)
          (lambda (proxy) (push (list :show proxy) *river-requests*) nil))
    ;; Every other river request: a no-op
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position set-window-borders
                  set-window-clip-box place-node-above message
                  request-manage-dirty recommend-dimension-bounds
                  focus-window))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))

    (labels ((fresh ()
               (setf *windows* (make-hash-table)
                     *outputs* (make-hash-table)
                     *seats* (make-hash-table)
                     *output-workspace* (make-hash-table)
                     *window-workspace* (make-hash-table)
                     *minimized-windows* (make-hash-table)
                     *frame-trees* (make-hash-table :test 'equal)
                     *current-frames* (make-hash-table :test 'equal)
                     *river-requests* nil)
               (clrhash *scratchpads*))
             (mk-out (proxy &key (width 1920) (height 1080))
               (let ((out (make-instance 'rhizome-output :proxy proxy
                                         :width width :height height)))
                 (setf (gethash proxy *outputs*) out
                       (gethash out *output-workspace*) 1)
                 out))
             (mk-float (proxy out)
               ;; A floating window already on screen on workspace 1
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out
                       (window-floating-p win) t
                       (window-float-x win) 100
                       (window-float-y win) 100
                       (window-float-width win) 800
                       (window-float-height win) 500
                       (window-width win) 800
                       (window-height win) 500)
                 (assign-window-workspace win 1)
                 win))
             (render (out)
               ;; One render pass; the river requests it sent
               (setf *river-requests* nil)
               (render-layout-for-output out)
               (reverse *river-requests*))
             (scratchpad-on-screen (name out proxy)
               ;; A shown scratchpad NAME whose window has rendered
               (let ((win (mk-float proxy out)))
                 (setf (gethash name *scratchpads*)
                       (list :window win :command "foot" :visible t))
                 (render out)
                 win)))

      ;; --- scratchpad hide and show ---
      (fresh)
      (let* ((out (mk-out :sp-out))
             (win (scratchpad-on-screen "term" out :sp-term)))
        (toggle-scratchpad "term")
        (ok "hiding a scratchpad sends one river hide"
            (render out) '((:hide :sp-term)))
        (ok "a hidden scratchpad sends nothing further"
            (render out) '())
        (toggle-scratchpad "term")
        (ok "showing a scratchpad sends one river show"
            (render out) '((:show :sp-term)))
        (ok "a shown scratchpad sends nothing further"
            (render out) '())
        (ok "a shown scratchpad is not minimized"
            (window-minimized-p win) nil))

      ;; --- hide and show inside one manage cycle ---
      (fresh)
      (let ((out (mk-out :sp-out)))
        (scratchpad-on-screen "term" out :sp-quick)
        (toggle-scratchpad "term")
        (toggle-scratchpad "term")
        (ok "hide then show before a render sends nothing"
            (render out) '()))

      ;; --- a client's minimize request ---
      (fresh)
      (let* ((out (mk-out :sp-out))
             (win (mk-float :sp-min out)))
        (render out)
        (handle-window-event win :minimize-requested)
        (ok "a minimize request sends one river hide"
            (render out) '((:hide :sp-min)))
        (ok "a minimized window sends nothing further"
            (render out) '()))

      ;; --- the context menu's Minimize and Restore ---
      (fresh)
      (let* ((out (mk-out :sp-out))
             (win (mk-float :sp-menu out)))
        (flet ((menu-action (label)
                 (cdr (assoc label (menu-item-list win) :test #'string=))))
          (render out)
          (funcall (menu-action "Minimize") win)
          (ok "menu Minimize sends one river hide"
              (render out) '((:hide :sp-menu)))
          (funcall (menu-action "Restore") win)
          (ok "menu Restore sends one river show"
              (render out) '((:show :sp-menu)))
          (ok "a restored window sends nothing further"
              (render out) '()))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
