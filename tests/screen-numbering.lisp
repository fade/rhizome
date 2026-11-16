;;; screen-numbering.lisp: how screens are numbered and how one index
;;; base serves workspaces, screens, and frames. Covers the shared
;;; (:workspaces :index-base) setting and the :index-base keyword that
;;; overrides it per call; the primary screen from (:screens :primary),
;;; else the first output river lists; the left-handed spiral from the
;;; primary by centre distance with counter-clockwise tie breaks; the
;;; explicit (:screens :order) connector list; and screen lookup by
;;; number or connector name. No compositor is needed.
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
                 (when config
                   (configure config))
                 (setf *outputs* (make-hash-table)
                       *display-heads* (make-hash-table)
                       *output-workspace* (make-hash-table)
                       *frame-trees* (make-hash-table :test 'equal)
                       *current-frames* (make-hash-table :test 'equal)))
               (make-output (proxy connector x &key (y 0) (width 1920)
                                                    (height 1080))
                 ;; An output at X,Y with an enabled head named CONNECTOR
                 (let ((out (make-instance 'rhizome-output :proxy proxy
                                           :x x :y y
                                           :width width :height height)))
                   (setf (gethash proxy *outputs*) out
                         (gethash out *output-workspace*) 1)
                   (when connector
                     (setf (gethash proxy *display-heads*)
                           (list :head proxy :name connector :enabled t
                                 :position (cons x y))))
                   out))
               (numbers (&rest outputs)
                 (mapcar #'screen-number outputs)))

        ;; --- One shared index base ---

        (reset)
        (ok "the shared index base is 1 by default" (index-base) 1)
        (ok "the workspace base is the shared base"
            (workspace-index-base) 1)
        (reset :workspaces '(:index-base 0))
        (ok "(:workspaces :index-base 0) sets the shared base"
            (index-base) 0)
        (ok "an :index-base keyword overrides the setting for workspaces"
            (list (workspace-to-display 3 :index-base 1)
                  (workspace-from-display 3 :index-base 1))
            '(3 3))
        (ok "the setting applies without the keyword"
            (list (workspace-to-display 3) (workspace-from-display 2))
            '(2 3))

        ;; --- Primary screen ---

        (reset)
        (let* ((dvi (make-output :p-dvi "DVI-D-1" 1920))
               (hdmi (make-output :p-hdmi "HDMI-A-1" 0)))
          (ok "without (:screens :primary) the first output river lists is primary"
              (eq (primary-screen) dvi) t)
          (ok "the primary is screen 1 and the other screen 2"
              (numbers dvi hdmi) '(1 2)))
        (reset :screens '(:primary "DVI-D-1"))
        (let* ((hdmi (make-output :q-hdmi "HDMI-A-1" 0))
               (dvi (make-output :q-dvi "DVI-D-1" 1920)))
          (ok "(:screens :primary) names the primary by connector"
              (eq (primary-screen) dvi) t)
          (ok "the configured primary is screen 1"
              (numbers dvi hdmi) '(1 2))
          (ok "a :index-base keyword renumbers from that base"
              (list (screen-number dvi :index-base 0)
                    (screen-number hdmi :index-base 0))
              '(0 1)))
        (reset :screens '(:primary "DP-9"))
        (let* ((hdmi (make-output :r-hdmi "HDMI-A-1" 0))
               (dvi (make-output :r-dvi "DVI-D-1" 1920)))
          (ok "a primary naming no connected screen falls back to the first listed"
              (numbers hdmi dvi) '(1 2)))

        ;; --- Left-handed spiral ---

        (reset :screens '(:primary "M"))
        (let* ((l2 (make-output :s-l2 "L2" 0))
               (l1 (make-output :s-l1 "L1" 1920))
               (m (make-output :s-m "M" 3840))
               (r1 (make-output :s-r1 "R1" 5760))
               (r2 (make-output :s-r2 "R2" 7680)))
          (ok "a row L2 L1 M R1 R2 numbers M, L1, R1, L2, R2"
              (numbers m l1 r1 l2 r2) '(1 2 3 4 5))
          (ok "screen-order lists the screens in number order"
              (equal (screen-order) (list m l1 r1 l2 r2)) t))
        (reset :screens '(:primary "M"))
        (let* ((below (make-output :t-below "B" 0 :y 2160))
               (m (make-output :t-m "M" 0 :y 1080))
               (above (make-output :t-above "A" 0 :y 0)))
          (ok "at equal distance the screen above comes before the one below"
              (numbers m above below) '(1 2 3)))
        (reset :screens '(:primary "M"))
        (let* ((far (make-output :u-far "F" 5760))
               (m (make-output :u-m "M" 0))
               (near (make-output :u-near "N" 1920 :width 1280)))
          (ok "nearer centres come first"
              (numbers m near far) '(1 2 3)))

        ;; --- Explicit order ---

        (reset :screens '(:primary "DVI-D-1" :order ("HDMI-A-1" "DVI-D-1")))
        (let* ((hdmi (make-output :v-hdmi "HDMI-A-1" 0))
               (dvi (make-output :v-dvi "DVI-D-1" 1920)))
          (ok "(:screens :order) numbers screens in its order over :primary"
              (numbers hdmi dvi) '(1 2)))
        (reset :screens '(:order ("DP-9" "R1")))
        (let* ((m (make-output :w-m "M" 3840))
               (l1 (make-output :w-l1 "L1" 1920))
               (r1 (make-output :w-r1 "R1" 5760)))
          (ok "a disconnected connector in :order is skipped; unlisted screens follow in spiral order from the first"
              (numbers r1 m l1) '(1 2 3)))

        ;; --- Lookup ---

        (reset :screens '(:primary "DVI-D-1"))
        (let* ((hdmi (make-output :x-hdmi "HDMI-A-1" 0))
               (dvi (make-output :x-dvi "DVI-D-1" 1920)))
          (ok "screen-by-number finds a screen by number"
              (list (eq (screen-by-number 1) dvi) (eq (screen-by-number 2) hdmi))
              '(t t))
          (ok "a numeric string names a screen too"
              (eq (screen-by-number "2") hdmi) t)
          (ok "a connector name names a screen"
              (eq (screen-by-number "hdmi-a-1") hdmi) t)
          (ok "a number outside the screens names none"
              (list (screen-by-number 0) (screen-by-number 3)) '(nil nil))
          (ok "screen-by-number honours :index-base"
              (eq (screen-by-number 0 :index-base 0) dvi) t)
          (remhash :x-dvi *outputs*)
          (ok "removing a screen renumbers the rest"
              (screen-number hdmi) 1))

        (reset)
        (ok "a screen with no outputs has no number" (screen-number nil) nil)

        (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
        (sb-ext:exit :code (if (zerop fail) 0 1))))))
