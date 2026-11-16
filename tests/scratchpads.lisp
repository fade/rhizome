;;; scratchpads.lisp: the scratchpad module and the minimize paths
;;; that share its hide and show. Hiding a scratchpad, a client's
;;; minimize request, and the context menu's Minimize each reach river
;;; as one hide request in the next render pass; showing a scratchpad
;;; and the menu's Restore each reach it as one show request. The
;;; module's hook handlers stay one each across reloads and claims.
;;; River requests are replaced by recorders or no-ops, so nothing
;;; here needs a compositor.
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

(defvar *sp-pass* 0)
(defvar *sp-fail* 0)

(defvar *river-requests* nil
  "River show and hide requests as (:show PROXY) or (:hide PROXY),
most recent first.")

(defvar *launched* nil
  "Argument lists passed to the launch-program recorder, most recent
first.")

(defvar *real-focus-window* (symbol-function 'focus-window)
  "FOCUS-WINDOW before the stubs below replace it, for the focus checks.")

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

;; Scratchpad commands: record, never run
(setf (symbol-function 'uiop:launch-program)
      (lambda (argv &rest args)
        (declare (ignore args))
        (push argv *launched*)
        nil))

(defun ok (name actual expected)
  "Count and print one check: ACTUAL is EQUAL to EXPECTED."
  (if (equal actual expected)
      (progn (incf *sp-pass*)
             (format t "~&PASS: ~A~%" name))
      (progn (incf *sp-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                     name actual expected))))

(defun fresh ()
  "Empty every table the checks touch."
  (setf *windows* (make-hash-table)
        *outputs* (make-hash-table)
        *seats* (make-hash-table)
        *output-workspace* (make-hash-table)
        *window-workspace* (make-hash-table)
        *minimized-windows* (make-hash-table)
        *frame-trees* (make-hash-table :test 'equal)
        *current-frames* (make-hash-table :test 'equal)
        *river-requests* nil
        *launched* nil)
  (clrhash *scratchpads*))

(defun mk-out (proxy &key (x 0) (y 0) (width 1920) (height 1080))
  "A registered output showing workspace 1."
  (let ((out (make-instance 'rhizome-output :proxy proxy :x x :y y
                            :width width :height height)))
    (setf (gethash proxy *outputs*) out
          (gethash out *output-workspace*) 1)
    out))

(defun mk-win (proxy out)
  "A tiled 800x500 window on OUT's workspace 1."
  (let ((win (make-instance 'rhizome-window :proxy proxy)))
    (setf (gethash proxy *windows*) win
          (window-output win) out
          (window-width win) 800
          (window-height win) 500)
    (assign-window-workspace win 1)
    win))

(defun mk-float (proxy out)
  "A floating window already on screen on OUT's workspace 1."
  (let ((win (mk-win proxy out)))
    (setf (window-floating-p win) t
          (window-float-x win) 100
          (window-float-y win) 100
          (window-float-width win) 800
          (window-float-height win) 500)
    win))

(defun render (out)
  "Run one render pass for OUT; the river show and hide requests it
sent, in order."
  (setf *river-requests* nil)
  (render-layout-for-output out)
  (reverse *river-requests*))

(defun scratchpad-on-screen (name out proxy)
  "A shown scratchpad NAME whose floating window has rendered."
  (let ((win (mk-float proxy out)))
    (setf (gethash name *scratchpads*)
          (list :window win :command "foot" :visible t))
    (render out)
    win))

(defun check-scratchpad-hide-and-show ()
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
  ;; Hide and show inside one manage cycle
  (fresh)
  (let ((out (mk-out :sp-out)))
    (scratchpad-on-screen "term" out :sp-quick)
    (toggle-scratchpad "term")
    (toggle-scratchpad "term")
    (ok "hide then show before a render sends nothing"
        (render out) '())))

(defun check-minimize-request ()
  (fresh)
  (let* ((out (mk-out :sp-out))
         (win (mk-float :sp-min out)))
    (render out)
    (handle-window-event win :minimize-requested)
    (ok "a minimize request sends one river hide"
        (render out) '((:hide :sp-min)))
    (ok "a minimized window sends nothing further"
        (render out) '())))

(defun check-menu-minimize-and-restore ()
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

(defun hook-count (name)
  "How many handlers the hook NAME holds."
  (length (gethash name *hooks*)))

(defun check-module-reload-hooks ()
  (let ((source (merge-pathnames
                  "modules/rhizome-scratchpads/scratchpads.lisp"
                  (asdf:system-source-directory :rhizome)))
        (closed (hook-count :window-closed))
        (created (hook-count :window-created))
        (resized (hook-count :window-resized))
        (minimized (hook-count :window-minimized))
        (restored (hook-count :window-restored)))
    (load source)
    (load source)
    (ok "reloading the module adds no :window-minimized handler"
        (hook-count :window-minimized) minimized)
    (ok "reloading the module adds no :window-restored handler"
        (hook-count :window-restored) restored)
    (ok "reloading the module adds no :window-closed handler"
        (hook-count :window-closed) closed)
    (ok "reloading the module adds no :window-created handler"
        (hook-count :window-created) created)
    (ok "reloading the module adds no :window-resized handler"
        (hook-count :window-resized) resized)))

(defun check-claim-hooks ()
  (fresh)
  (let ((out (mk-out :sp-out))
        (baseline (hook-count :window-created)))
    (toggle-scratchpad "term")
    (ok "launching a scratchpad starts its command"
        *launched* '(("bash" "-c" "foot")))
    (ok "launching a scratchpad adds no :window-created handler"
        (hook-count :window-created) baseline)
    (let ((win (mk-win :sp-claimed out)))
      (run-hook :window-created win)
      (ok "the next new window becomes the scratchpad"
          (getf (scratchpad-find "term") :window) win)
      (ok "a claimed scratchpad is visible"
          (getf (scratchpad-find "term") :visible) t)
      (ok "after a claim the :window-created handlers are at baseline"
          (hook-count :window-created) baseline)
      (run-hook :window-created (mk-win :sp-other out))
      (ok "a later window leaves the claimed scratchpad alone"
          (getf (scratchpad-find "term") :window) win)
      (run-hook :window-closed win)
      (ok "closing the scratchpad's window forgets it"
          (list (getf (scratchpad-find "term") :window)
                (getf (scratchpad-find "term") :visible))
          '(nil nil)))))

(defun centred-at (out proxy &key (on-output t))
  "Show a hidden scratchpad whose tiled 800 pixel window is on OUT, or
on no output yet when ON-OUTPUT is NIL; its float position as (X Y)."
  (let ((win (mk-win proxy out)))
    (unless on-output
      (setf (window-output win) nil))
    (setf (window-minimized-p win) t
          (gethash "term" *scratchpads*)
          (list :window win :command "foot" :visible nil))
    (toggle-scratchpad "term")
    (list (window-float-x win) (window-float-y win))))

(defun check-centring ()
  (let ((real-bar-height (symbol-function 'bar-reserved-height))
        (barred nil))
    ;; A 36 pixel bar on the output BARRED only
    (setf (symbol-function 'bar-reserved-height)
          (lambda (&optional output)
            (if (and output (eq output barred)) 36 0)))
    (unwind-protect
         (progn
           (fresh)
           (let ((out (mk-out :sp-qhd :width 2560 :height 1440)))
             (ok "a 2560x1440 output centres the scratchpad"
                 (centred-at out :sp-qhd-win) '(880 50)))
           (fresh)
           (let ((out (mk-out :sp-fhd)))
             (ok "a 1920x1080 output centres the scratchpad"
                 (centred-at out :sp-fhd-win) '(560 50)))
           (fresh)
           (let ((out (mk-out :sp-barred :width 2560 :height 1440)))
             (setf barred out)
             (ok "the scratchpad sits 50 pixels below its output's bar"
                 (centred-at out :sp-barred-win) '(880 86)))
           (fresh)
           (mk-out :sp-left)
           (let ((right (mk-out :sp-right :x 1920 :width 2560
                                          :height 1440)))
             (setf *selected-output* right
                   barred nil)
             (ok "a window on no output yet centres on the current output"
                 (centred-at right :sp-new-win :on-output nil)
                 '(2800 50))))
      (setf (symbol-function 'bar-reserved-height) real-bar-height))))

(defun check-centre-on-first-size ()
  (fresh)
  (let ((out (mk-out :sp-narrow :width 1234 :height 800)))
    (setf *selected-output* out)
    (toggle-scratchpad "term")
    ;; River announces a window with no output and no size
    (let ((win (make-instance 'rhizome-window :proxy :sp-sized))
          (top (+ (nth-value 1 (output-usable-area out)) 50)))
      (setf (gethash :sp-sized *windows*) win)
      (run-hook :window-created win)
      (handle-window-event win :dimensions 1222 631)
      (ok "a scratchpad claimed before its size centres on the first size"
          (list (window-float-x win) (window-float-y win)) (list 6 top))
      (handle-window-event win :dimensions 600 400)
      (ok "a later size change leaves the scratchpad where it is"
          (list (window-float-x win) (window-float-y win)) (list 6 top)))))

(defun shown-after-hidden-at (out proxy x y width height)
  "Show a hidden floating scratchpad placed at X Y with size WIDTH by
HEIGHT on OUT; its float position as (X Y)."
  (let ((win (mk-float proxy out)))
    (setf (window-float-x win) x
          (window-float-y win) y
          (window-float-width win) width
          (window-float-height win) height
          (window-minimized-p win) t
          (gethash "term" *scratchpads*)
          (list :window win :command "foot" :visible nil))
    (toggle-scratchpad "term")
    (list (window-float-x win) (window-float-y win))))

(defun check-show-keeps-scratchpad-on-output ()
  (fresh)
  (let ((out (mk-out :sp-out))
        (bw (border-width t)))
    (multiple-value-bind (ux uy uw uh) (output-usable-area out)
      (ok "a scratchpad past the bottom-right edge moves back inside"
          (shown-after-hidden-at out :sp-past (+ ux uw -300) (+ uy uh -100)
                                 800 500)
          (list (- (+ ux uw) 800 bw) (- (+ uy uh) 500 bw)))
      (fresh)
      (setf out (mk-out :sp-out))
      (ok "a scratchpad wider than the usable area starts inside its left edge"
          (shown-after-hidden-at out :sp-wide 300 (+ uy 100) (+ uw 80) 500)
          (list (+ ux bw) (+ uy 100)))
      (ok "a scratchpad wider than the usable area shrinks to fit"
          (window-float-width (gethash :sp-wide *windows*))
          (- uw bw bw))
      (fresh)
      (setf out (mk-out :sp-out))
      (ok "a scratchpad already inside keeps its position"
          (shown-after-hidden-at out :sp-inside (+ ux 100) (+ uy 100)
                                 800 500)
          (list (+ ux 100) (+ uy 100))))))

(defun with-decorations (thunk)
  "Call THUNK with a 36 pixel bar on every output and 25 pixel
titlebars on floats; restore both afterwards."
  (let ((real-bar-height (symbol-function 'bar-reserved-height)))
    (setf (symbol-function 'bar-reserved-height)
          (lambda (&optional output)
            (declare (ignore output))
            36))
    (configure '(:theme (:titlebar t :titlebar-height 25)))
    (unwind-protect (funcall thunk)
      (configure '(:theme (:titlebar nil :titlebar-height nil)))
      (setf (symbol-function 'bar-reserved-height) real-bar-height))))

(defun frame-box (win)
  "WIN's decorated frame as (LEFT TOP RIGHT BOTTOM): its float box, the
focused border on every side, and a 25 pixel titlebar above."
  (let ((bw (border-width t))
        (x (window-float-x win))
        (y (window-float-y win)))
    (list (- x bw) (- y bw 25)
          (+ x (window-float-width win) bw)
          (+ y (window-float-height win) bw))))

(defun frame-inside-p (win out)
  "True when WIN's decorated frame lies inside OUT's usable area."
  (multiple-value-bind (ux uy uw uh) (output-usable-area out)
    (destructuring-bind (left top right bottom) (frame-box win)
      (and (<= ux left) (<= uy top)
           (<= right (+ ux uw)) (<= bottom (+ uy uh))))))

(defun count-matches (needle text)
  "How many times NEEDLE occurs in TEXT."
  (loop with start = 0
        for hit = (search needle text :start2 start)
        while hit
        count t
        do (setf start (1+ hit))))

(defun check-decorated-frame-fits ()
  (with-decorations
    (lambda ()
      (let ((bw (border-width t)))
        ;; A 1222x631 terminal at 6,63 on a 1234x679 output whose
        ;; usable area is 0 36 1234 643: the frame is too tall
        (fresh)
        (let* ((out (mk-out :sp-short :width 1234 :height 679))
               (log (with-output-to-string (*standard-output*)
                      (shown-after-hidden-at out :sp-tall 6 63 1222 631)))
               (win (gethash :sp-tall *windows*))
               (fit-h (- 643 25 bw bw)))
          (render out)
          (ok "a decorated frame taller than the usable area shrinks to fit"
              (list (window-float-width win) (window-float-height win))
              (list 1222 fit-h))
          (ok "the shrunk frame lies inside the usable area"
              (frame-inside-p win out) t)
          (ok "shrinking a scratchpad logs one line naming its new size"
              (count-matches
                (format nil "; Rhizome: scratchpad term shrunk to 1222x~D to fit output"
                        fit-h)
                log)
              1))
        ;; The same terminal claimed before its first size
        (fresh)
        (let ((out (mk-out :sp-short :width 1234 :height 679)))
          (setf *selected-output* out)
          (toggle-scratchpad "term")
          (let ((win (make-instance 'rhizome-window :proxy :sp-first)))
            (setf (gethash :sp-first *windows*) win)
            (run-hook :window-created win)
            (let ((log (with-output-to-string (*standard-output*)
                         (handle-window-event win :dimensions 1222 631))))
              (setf (window-output win) out)
              (render out)
              (ok "a first size too tall for the usable area shrinks to fit"
                  (window-float-height win) (- 643 25 bw bw))
              (ok "a frame shrunk on its first size lies inside"
                  (frame-inside-p win out) t)
              (ok "shrinking on the first size logs one line"
                  (count-matches "shrunk to" log) 1))))
        ;; A frame that fits keeps its size and position
        (fresh)
        (let* ((out (mk-out :sp-short :width 1234 :height 679))
               (log (with-output-to-string (*standard-output*)
                      (ok "a decorated frame that fits keeps its position"
                          (shown-after-hidden-at out :sp-fits 100 100 800 500)
                          '(100 100))))
               (win (gethash :sp-fits *windows*)))
          (render out)
          (ok "a decorated frame that fits keeps its size"
              (list (window-float-width win) (window-float-height win))
              '(800 500))
          (ok "a frame that fits lies inside the usable area"
              (frame-inside-p win out) t)
          (ok "a frame that fits logs no shrink"
              (count-matches "shrunk to" log) 0))
        ;; A frame that fits but hangs past the bottom moves up only
        (fresh)
        (let* ((out (mk-out :sp-short :width 1234 :height 679))
               (pos (shown-after-hidden-at out :sp-low 6 100 1222 600))
               (win (gethash :sp-low *windows*)))
          (ok "a frame that fits moves the least distance back inside"
              pos (list 6 (- 679 bw 600)))
          (ok "a frame that moves back inside keeps its size"
              (list (window-float-width win) (window-float-height win))
              '(1222 600)))))))

(defun check-hide-moves-focus ()
  (let ((stub (symbol-function 'focus-window)))
    (setf (symbol-function 'focus-window) *real-focus-window*)
    (unwind-protect
         (flet ((mk-seat ()
                  (setf (gethash :sp-seat *seats*)
                        (make-instance 'rhizome-seat :proxy :sp-seat))))
           (fresh)
           (let* ((seat (mk-seat))
                  (out (mk-out :sp-out))
                  (under (mk-win :sp-under out))
                  (win (scratchpad-on-screen "term" out :sp-focus)))
             (focus-window win seat)
             (toggle-scratchpad "term")
             (ok "hiding a scratchpad focuses the window underneath"
                 (seat-focused-window seat) under)
             (ok "a hidden scratchpad is not marked focused"
                 (window-focused-p win) nil)
             (toggle-scratchpad "term")
             (ok "showing a scratchpad focuses it"
                 (seat-focused-window seat) win))
           (fresh)
           (let* ((seat (mk-seat))
                  (out (mk-out :sp-out))
                  (win (scratchpad-on-screen "term" out :sp-alone)))
             (focus-window win seat)
             (toggle-scratchpad "term")
             (ok "hiding the only window on a workspace leaves no focus"
                 (seat-focused-window seat) nil))
           (fresh)
           (let* ((seat (mk-seat))
                  (out (mk-out :sp-out))
                  (under (mk-win :sp-under out))
                  (win (mk-float :sp-min out)))
             (focus-window win seat)
             (handle-window-event win :minimize-requested)
             (ok "a minimize request focuses the window underneath"
                 (seat-focused-window seat) under))
           (fresh)
           (let* ((seat (mk-seat))
                  (out (mk-out :sp-out))
                  (under (mk-win :sp-under out))
                  (win (mk-float :sp-menu out)))
             (focus-window win seat)
             (funcall (cdr (assoc "Minimize" (menu-item-list win)
                                  :test #'string=))
                      win)
             (ok "menu Minimize focuses the window underneath"
                 (seat-focused-window seat) under)))
      (setf (symbol-function 'focus-window) stub))))

(defun check-minimize-paths-hide-scratchpad ()
  (flet ((menu (win label)
           (funcall (cdr (assoc label (menu-item-list win)
                                :test #'string=))
                    win))
         (visible () (getf (scratchpad-find "term") :visible)))
    (fresh)
    (let* ((out (mk-out :sp-out))
           (win (scratchpad-on-screen "term" out :sp-menu-min)))
      (menu win "Minimize")
      (ok "menu Minimize leaves the scratchpad not visible" (visible) nil)
      (toggle-scratchpad "term")
      (ok "one toggle after menu Minimize shows the scratchpad"
          (list (window-minimized-p win) (visible)) '(nil t)))
    (fresh)
    (let* ((out (mk-out :sp-out))
           (win (scratchpad-on-screen "term" out :sp-req-min)))
      (handle-window-event win :minimize-requested)
      (ok "a minimize request leaves the scratchpad not visible"
          (visible) nil)
      (toggle-scratchpad "term")
      (ok "one toggle after a minimize request shows the scratchpad"
          (list (window-minimized-p win) (visible)) '(nil t)))
    (fresh)
    (let* ((out (mk-out :sp-out))
           (win (scratchpad-on-screen "term" out :sp-restore)))
      (menu win "Minimize")
      (menu win "Restore")
      (ok "menu Restore leaves the scratchpad visible" (visible) t)
      (toggle-scratchpad "term")
      (ok "one toggle after menu Restore hides the scratchpad"
          (list (window-minimized-p win) (visible)) '(t nil)))))

(defun check-restore-paths-show-scratchpad ()
  (labels ((menu-action (win prefix)
             (cdr (find-if (lambda (item)
                             (eql 0 (search prefix (car item))))
                           (menu-item-list win))))
           (menu (win prefix)
             (funcall (menu-action win prefix) win))
           (visible () (getf (scratchpad-find "term") :visible))
           (minimized-pad (proxy)
             (let* ((out (mk-out :sp-out))
                    (win (scratchpad-on-screen "term" out proxy)))
               (menu win "Minimize")
               (values win out))))
    (fresh)
    (let ((win (minimized-pad :sp-move-ws)))
      (move-window-to-workspace win 2)
      (ok "moving a minimized scratchpad to a workspace leaves it visible"
          (visible) t)
      (toggle-scratchpad "term")
      (ok "one toggle after a move to a workspace hides the scratchpad"
          (list (window-minimized-p win) (visible)) '(t nil)))
    (fresh)
    (let ((win (minimized-pad :sp-menu-ws)))
      (menu win "Move to workspace")
      (ok "menu Move to workspace leaves a minimized scratchpad visible"
          (visible) t)
      (toggle-scratchpad "term")
      (ok "one toggle after menu Move to workspace hides the scratchpad"
          (list (window-minimized-p win) (visible)) '(t nil)))
    (fresh)
    (multiple-value-bind (win out) (minimized-pad :sp-move-out)
      (declare (ignore out))
      (move-window-to-output win (mk-out :sp-right :x 1920))
      (ok "moving a minimized scratchpad to an output leaves it visible"
          (visible) t)
      (toggle-scratchpad "term")
      (ok "one toggle after a move to an output hides the scratchpad"
          (list (window-minimized-p win) (visible)) '(t nil)))
    ;; Each path that clears a minimize runs :window-restored once
    (let* ((count 0)
           (counter (lambda (win)
                      (declare (ignore win))
                      (incf count))))
      (add-hook :window-restored counter)
      (unwind-protect
           (flet ((restores (name restore)
                    (fresh)
                    (let* ((out (mk-out :sp-out))
                           (win (mk-float :sp-count out)))
                      (setf count 0)
                      (minimize-window win)
                      (funcall restore win out)
                      (ok (format nil "~A runs :window-restored once" name)
                          (list count (window-minimized-p win)) '(1 nil)))))
             (restores "menu Restore"
                       (lambda (win out)
                         (declare (ignore out))
                         (menu win "Restore")))
             (restores "menu Move to workspace"
                       (lambda (win out)
                         (declare (ignore out))
                         (menu win "Move to workspace")))
             (restores "move-window-to-workspace"
                       (lambda (win out)
                         (declare (ignore out))
                         (move-window-to-workspace win 2)))
             (restores "move-window-to-output"
                       (lambda (win out)
                         (declare (ignore out))
                         (move-window-to-output win
                                                (mk-out :sp-right :x 1920))))
             (restores "a window rule's workspace"
                       (lambda (win out)
                         (declare (ignore out))
                         (setf (window-app-id win) "sp-ruled")
                         (set-window-rules '((:match "^sp-ruled$"
                                              :workspace 2))
                                           :source "scratchpad-tests")
                         (unwind-protect
                              (progn
                                (remhash win *window-rules-applied*)
                                (apply-window-rules win))
                           (set-window-rules nil
                                             :source "scratchpad-tests"))))
             (restores "a remembered placement"
                       (lambda (win out)
                         (declare (ignore out))
                         (setf (window-identifier win) "sp-remembered")
                         (let ((*window-state* (make-hash-table
                                                 :test 'equal)))
                           (setf (gethash "sp-remembered" *window-state*)
                                 (list :workspace 2 :float t))
                           (restore-window-state win))))
             (fresh)
             (let ((win (mk-float :sp-shown (mk-out :sp-out))))
               (setf count 0)
               (move-window-to-workspace win 2)
               (ok "moving a window that is not minimized runs no :window-restored"
                   count 0)))
        (remove-hook :window-restored counter)))))

(dolist (check '(check-scratchpad-hide-and-show
                 check-minimize-request
                 check-menu-minimize-and-restore
                 check-module-reload-hooks
                 check-claim-hooks
                 check-centring
                 check-centre-on-first-size
                 check-show-keeps-scratchpad-on-output
                 check-decorated-frame-fits
                 check-hide-moves-focus
                 check-minimize-paths-hide-scratchpad
                 check-restore-paths-show-scratchpad))
  (handler-case (funcall check)
    (error (e)
      (ok (format nil "~(~A~) runs without error" check)
          (princ-to-string e) nil))))

(format t "~&=== ~A passed, ~A failed ===~%" *sp-pass* *sp-fail*)
(uiop:quit (if (zerop *sp-fail*) 0 1))
