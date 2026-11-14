;;; Tests for the output layout read from (:displays (:layout ...)):
;;; relative placement against each head's current mode size,
;;; normalization to a 0,0 origin, explicit positions, mode and scale
;;; overrides, matching by connector name or description, heads named
;;; but absent, heads present but not named, the no-op when the layout
;;; already holds, the log lines, and the automatic apply on the
;;; manager's done event. The output manager, its heads and modes, and
;;; every configuration request are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (requests '()) (configs '()))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (try (name thunk expected)
             ;; An error is a failure, not a crash: every case reports
             (ok name
                 (handler-case (funcall thunk)
                   (error (e) (list :error (princ-to-string e))))
                 expected)))

    ;; Record configuration requests instead of talking to river.
    ;; A configuration head proxy is replaced by its head's name.
    (let ((conf-heads (make-hash-table)))
      (flet ((head-name (conf-head)
               (getf (gethash (gethash conf-head conf-heads)
                              *display-heads*)
                     :name)))
        (setf (symbol-function 'rwm:zwlr-output-manager-v1.create-configuration)
              (lambda (manager serial)
                (declare (ignore manager))
                (let ((config (make-instance 'wl:wl-proxy)))
                  (push (list :create serial) requests)
                  (push config configs)
                  config))
              (symbol-function 'rwm:zwlr-output-configuration-v1.enable-head)
              (lambda (config head)
                (declare (ignore config))
                (let ((conf-head (make-instance 'wl:wl-proxy)))
                  (setf (gethash conf-head conf-heads) head)
                  (push (list :enable (getf (gethash head *display-heads*)
                                            :name))
                        requests)
                  conf-head))
              (symbol-function 'rwm:zwlr-output-configuration-v1.disable-head)
              (lambda (config head)
                (declare (ignore config))
                (push (list :disable (getf (gethash head *display-heads*)
                                           :name))
                      requests)
                nil)
              (symbol-function 'rwm:zwlr-output-configuration-head-v1.set-mode)
              (lambda (conf-head mode)
                (push (list :mode (head-name conf-head)
                            (getf (gethash mode *display-modes*) :width)
                            (getf (gethash mode *display-modes*) :height))
                      requests)
                nil)
              (symbol-function 'rwm:zwlr-output-configuration-head-v1.set-scale)
              (lambda (conf-head scale)
                (push (list :scale (head-name conf-head) scale) requests)
                nil)
              (symbol-function 'rwm:zwlr-output-configuration-head-v1.set-position)
              (lambda (conf-head x y)
                (push (list :position (head-name conf-head) x y) requests)
                nil)
              (symbol-function 'rwm:zwlr-output-configuration-v1.apply)
              (lambda (config)
                (declare (ignore config))
                (push (list :apply) requests)
                nil))))

    (labels ((mode (width height hz)
               (let ((mode (make-instance 'wl:wl-proxy)))
                 (setf (gethash mode *display-modes*)
                       (list :width width :height height
                             :refresh (* hz 1000)))
                 mode))
             (head (name description modes current x y &key (scale 1)
                                                         (enabled t))
               (let ((head (make-instance 'wl:wl-proxy)))
                 (setf (gethash head *display-heads*)
                       (list :head head :name name :description description
                             :enabled enabled :modes modes
                             :current-mode current
                             :position (cons x y) :scale scale))
                 head))
             (reset (&optional layout)
               (setf *display-heads* (make-hash-table)
                     *display-modes* (make-hash-table)
                     *output-manager* (make-instance 'wl:wl-proxy)
                     *display-serial* 7
                     *display-layout-pending* nil
                     *display-head-set* nil
                     *display-layout-absent-keys* nil
                     requests '()
                     configs '())
               (configure (list :displays (list :layout layout))))
             (ghost (&key (hdmi-x 1920) (dvi-x 0))
               ;; River's order: DVI-D-1 at 0, HDMI-A-1 to its right
               (let ((dvi (mode 1920 1200 60))
                     (hdmi (mode 1920 1080 60))
                     (hdmi-low (mode 1280 720 60)))
                 (list (head "DVI-D-1" "Dell Inc. DELL U2412M 0FFXD"
                             (list dvi) dvi dvi-x 0)
                       (head "HDMI-A-1" "Samsung Electric Company SyncMaster"
                             (list hdmi hdmi-low) hdmi hdmi-x 0))))
             (positions ()
               ;; name x y of every set_position, sorted by name
               (sort (loop for r in requests
                           when (eq (first r) :position)
                             collect (rest r))
                     #'string< :key #'first))
             (sent-p ()
               (and (assoc :create requests) t))
             (fire (event)
               (dolist (hook (wl:wl-proxy-hooks (first configs)))
                 (funcall hook event)))
             (quietly (thunk)
               (let ((*standard-output* (make-broadcast-stream)))
                 (funcall thunk))))

      ;; Relative placement against heads of different heights
      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (try "left-of a taller primary places HDMI at 0,0 and DVI at 1920,0"
           (lambda ()
             (list (quietly #'display-apply-layout) (positions)))
           '(:applied (("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0))))
      (try "the configuration carries the last done serial and is applied"
           (lambda () (list (find :create requests :key #'first)
                            (and (assoc :apply requests) t)))
           '((:create 7) t))
      (try "each head is restated at its current mode and scale"
           (lambda ()
             (sort (loop for r in requests
                         when (member (first r) '(:mode :scale))
                           collect (rest r))
                   #'string< :key (lambda (r) (format nil "~S" r))))
           '(("DVI-D-1" 1) ("DVI-D-1" 1920 1200)
             ("HDMI-A-1" 1) ("HDMI-A-1" 1920 1080)))

      (reset '(("HDMI-A-1" :right-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost :dvi-x 1920 :hdmi-x 0)
      (try "right-of uses the anchor's width"
           (lambda () (quietly #'display-apply-layout) (positions))
           '(("DVI-D-1" 0 0) ("HDMI-A-1" 1920 0)))

      (reset '(("HDMI-A-1" :above "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (try "above uses the placed head's own height"
           (lambda () (quietly #'display-apply-layout) (positions))
           '(("DVI-D-1" 0 1080) ("HDMI-A-1" 0 0)))

      (reset '(("HDMI-A-1" :below "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (try "below uses the anchor's height"
           (lambda () (quietly #'display-apply-layout) (positions))
           '(("DVI-D-1" 0 0) ("HDMI-A-1" 0 1200)))

      ;; Normalization
      (reset '(("HDMI-A-1" :position (100 50)) ("DVI-D-1" :position (2020 50))))
      (ghost)
      (try "explicit positions are shifted so the minimum x and y are 0"
           (lambda () (quietly #'display-apply-layout) (positions))
           '(("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0)))

      (reset '(("DVI-D-1" :primary t) ("HDMI-A-1" :left-of "DVI-D-1")))
      (ghost)
      (try "a head placed left of the anchor shifts the anchor right"
           (lambda () (quietly #'display-apply-layout) (positions))
           '(("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0)))

      ;; No-op when the layout already holds
      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost :hdmi-x 0 :dvi-x 1920)
      (try "a layout that already holds sends nothing"
           (lambda () (list (quietly #'display-apply-layout) (sent-p)))
           '(:unchanged nil))

      ;; A head named but absent
      (reset '(("DP-1" :right-of "DVI-D-1")
               ("HDMI-A-1" :left-of "DVI-D-1")
               ("DVI-D-1" :primary t)))
      (ghost)
      (try "a head named in the layout but absent is skipped"
           (lambda () (list (quietly #'display-apply-layout) (positions)))
           '(:applied (("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0))))

      (reset '(("HDMI-A-1" :left-of "DP-1")))
      (ghost)
      (try "a head placed against an absent head keeps its position"
           (lambda () (list (quietly #'display-apply-layout) (sent-p)))
           '(:unchanged nil))

      ;; An absent head is logged when the layout resolves, once
      (flet ((absent-lines ()
               (remove-if-not
                (lambda (line) (search "no head matches" line))
                (uiop:split-string
                 (with-output-to-string (*standard-output*)
                   (display-apply-layout))
                 :separator '(#\Newline)))))
        (reset '(("HDMI-A-9" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
        (ghost)
        (try "a head named in the layout but absent is logged"
             #'absent-lines
             '("; Rhizome: display layout: no head matches \"HDMI-A-9\""))
        (try "a second identical apply does not log it again"
             #'absent-lines
             '())
        (reset '(("HDMI-A-1" :left-of "DP-7")))
        (ghost)
        (try "an absent head named as a relation target is logged"
             #'absent-lines
             '("; Rhizome: display layout: no head matches \"DP-7\"")))

      ;; A head present but not named
      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (let ((edp (mode 2560 1600 60)))
        (head "eDP-1" "BOE laptop panel" (list edp) edp 5000 300))
      (try "a head not named keeps its position and stays enabled"
           (lambda ()
             (quietly #'display-apply-layout)
             (list (positions)
                   (and (find '(:enable "eDP-1") requests :test #'equal) t)))
           '((("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0) ("eDP-1" 5000 300)) t))

      (reset '(("HDMI-A-1" :left-of "eDP-1")))
      (ghost)
      (let ((edp (mode 2560 1600 60)))
        (head "eDP-1" "BOE laptop panel" (list edp) edp 5000 300))
      (try "a head placed against an unnamed head is not shifted"
           (lambda () (quietly #'display-apply-layout)
             (remove "DVI-D-1" (positions) :key #'first :test #'string=))
           '(("HDMI-A-1" 3080 300) ("eDP-1" 5000 300)))

      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (let ((off (mode 1920 1080 60)))
        (head "DP-2" "Spare" (list off) nil 0 0 :enabled nil))
      (try "a disabled head stays disabled"
           (lambda ()
             (quietly #'display-apply-layout)
             (list (and (find '(:disable "DP-2") requests :test #'equal) t)
                   (and (find '(:enable "DP-2") requests :test #'equal) t)))
           '(t nil))

      ;; Description matching
      (reset '(("syncmaster" :left-of "U2412M") ("U2412M" :primary t)))
      (ghost)
      (try "heads match by a description substring, ignoring case"
           (lambda () (quietly #'display-apply-layout) (positions))
           '(("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0)))

      ;; Mode and scale per head
      (reset '(("HDMI-A-1" :left-of "DVI-D-1" :mode "1280x720@60")
               ("DVI-D-1" :primary t :scale 2)))
      (ghost)
      (try "a mode and a scale override set the size used for placement"
           (lambda ()
             (quietly #'display-apply-layout)
             (list (positions)
                   (find "HDMI-A-1" (remove :mode requests :key #'first
                                            :test-not #'eq)
                         :key #'second :test #'string=)
                   (find "DVI-D-1" (remove :scale requests :key #'first
                                           :test-not #'eq)
                         :key #'second :test #'string=)))
           '((("DVI-D-1" 1280 0) ("HDMI-A-1" 0 0))
             (:mode "HDMI-A-1" 1280 720)
             (:scale "DVI-D-1" 2)))

      (reset '(("HDMI-A-1" :position (0 0) :mode "800x600")))
      (ghost)
      (try "an unknown mode keeps the current mode"
           (lambda ()
             (quietly #'display-apply-layout)
             (find "HDMI-A-1" (remove :mode requests :key #'first
                                      :test-not #'eq)
                   :key #'second :test #'string=))
           '(:mode "HDMI-A-1" 1920 1080))

      ;; Logging
      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (quietly #'display-apply-layout)
      (try "success logs one line naming each placed head"
           (lambda ()
             (string-trim '(#\Newline)
                          (with-output-to-string (*standard-output*)
                            (fire :succeeded))))
           "; Rhizome: display layout applied: HDMI-A-1 0,0 DVI-D-1 1920,0")

      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (quietly #'display-apply-layout)
      (try "a rejected layout is logged"
           (lambda ()
             (and (search "display layout rejected"
                          (with-output-to-string (*standard-output*)
                            (fire :failed)))
                  t))
           t)

      ;; The manager's done event
      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (try "the first done after startup applies the layout"
           (lambda ()
             (quietly (lambda () (display-manager-event :done 8)))
             (list (find :create requests :key #'first) (positions)))
           '((:create 8) (("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0))))
      (try "a hotplug while the configuration is pending sends nothing"
           (lambda ()
             (let ((edp (mode 2560 1600 60)))
               (head "eDP-1" "BOE laptop panel" (list edp) edp 5000 0))
             (setf requests '())
             (quietly (lambda () (display-manager-event :done 9)))
             (sent-p))
           nil)
      (try "the done after the layout's own apply sends nothing"
           (lambda ()
             (quietly (lambda () (fire :succeeded)))
             (dolist (state (display-heads))
               (cond ((string= (getf state :name) "HDMI-A-1")
                      (display-head-put (getf state :head) :position '(0 . 0)))
                     ((string= (getf state :name) "DVI-D-1")
                      (display-head-put (getf state :head) :position
                                        '(1920 . 0)))))
             (setf requests '())
             (quietly (lambda () (display-manager-event :done 10)))
             (sent-p))
           nil)
      (try "a done with an unchanged head set leaves a hand-placed head alone"
           (lambda ()
             (dolist (state (display-heads))
               (when (string= (getf state :name) "HDMI-A-1")
                 (display-head-put (getf state :head) :position '(4000 . 0))))
             (quietly (lambda () (display-manager-event :done 11)))
             (sent-p))
           nil)
      (try "a hotplugged head triggers the layout again"
           (lambda ()
             (let ((dp (mode 1920 1080 60)))
               (head "DP-3" "Projector" (list dp) dp 9000 0))
             (quietly (lambda () (display-manager-event :done 12)))
             (list (find :create requests :key #'first)
                   (remove-if-not (lambda (p) (member (first p)
                                                      '("HDMI-A-1" "DVI-D-1")
                                                      :test #'string=))
                                  (positions))))
           '((:create 12) (("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0))))

      ;; Config reload and the command
      (reset)
      (ghost)
      (try "configuring a layout applies it at once"
           (lambda ()
             (quietly
               (lambda ()
                 (configure '(:displays (:layout (("HDMI-A-1" :left-of "DVI-D-1")
                                                  ("DVI-D-1" :primary t)))))))
             (positions))
           '(("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0)))

      (reset '(("HDMI-A-1" :left-of "DVI-D-1") ("DVI-D-1" :primary t)))
      (ghost)
      (try "the display-layout command applies the layout"
           (lambda ()
             (quietly (lambda () (execute-command nil :display-layout nil)))
             (positions))
           '(("DVI-D-1" 1920 0) ("HDMI-A-1" 0 0)))

      (reset)
      (ghost)
      (try "no layout configured sends nothing"
           (lambda () (list (quietly #'display-apply-layout) (sent-p)))
           '(:no-layout nil))

      (reset '(("HDMI-A-1" :left-of "DVI-D-1")))
      (setf *output-manager* nil)
      (ghost)
      (try "without an output manager nothing is sent"
           (lambda () (list (quietly #'display-apply-layout) (sent-p)))
           '(:no-manager nil)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
