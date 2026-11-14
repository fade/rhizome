;;; display.lisp — the display configurator
;;;
;;; zwlr_output_management_unstable_v1: the WM tracks every output
;;; head (name, description, modes, current mode, scale, position)
;;; and applies configurations through the manager. This is what
;;; the display-menu bar module drives. River 0.5 removed the old
;;; riverctl interface — this standard wlroots protocol is the way.
;;;
;;; Configuration lifecycle: create_configuration carries the last
;;; done serial; every head must be RESTATED (enable_head + current
;;; mode/scale/position) because the protocol disables unmentioned
;;; heads — a partial config blanks the other outputs. Overrides
;;; land on the target head only. River answers with
;;; succeeded/failed; we surface that via message.

(in-package #:rhizome)

(defvar *output-manager* nil
  "The bound zwlr_output_manager_v1, or NIL before the registry
offers it (or after the manager says finished).")
(defvar *display-serial* 0
  "The manager's last done serial — configurations must carry it.")
(defvar *display-heads* (make-hash-table)
  "Head proxy → state plist: :head :name :description
:physical-size (w h) :enabled :modes (mode proxies, newest first)
:current-mode :position (x . y) :scale.")
(defvar *display-modes* (make-hash-table)
  "Mode proxy → plist: :width :height :refresh (millihertz).")

(defun display-manager-init (registry global-name version)
  "Bind zwlr_output_manager_v1 and start tracking heads."
  (let ((manager (wl:wl-registry.bind registry global-name
                                       (wl:find-interface-named
                                         "zwlr_output_manager_v1")
                                       (min version 4))))
    (setf *output-manager* manager)
    (push (function display-manager-event) (wl:wl-proxy-hooks manager))
    (format t "~&; Rhizome: display: output manager bound (v~D)~%"
            version)))

(defun display-manager-event (event-name &rest args)
  (case event-name
    (:head
     (destructuring-bind (head) args
       (setf (gethash head *display-heads*) (list :head head))
       (push (lambda (event-name &rest args)
               (apply #'display-head-event head event-name args))
             (wl:wl-proxy-hooks head))))
    (:done
     (destructuring-bind (serial) args
       (setf *display-serial* serial)
       (display-layout-on-done)))
    (:finished
     (setf *output-manager* nil))))

(defun display-head-event (head event-name &rest args)
  (case event-name
    (:name (display-head-put head :name (first args)))
    (:description (display-head-put head :description (first args)))
    (:physical-size (display-head-put head :physical-size args))
    (:enabled (display-head-put head :enabled (plusp (first args))))
    (:mode
     (destructuring-bind (mode) args
       (let ((state (gethash head *display-heads*)))
         (display-head-put head :modes (cons mode (getf state :modes))))
       (push (lambda (event-name &rest args)
               (apply #'display-mode-event mode event-name args))
             (wl:wl-proxy-hooks mode))))
    (:current-mode (display-head-put head :current-mode (first args)))
    (:position
     (display-head-put head :position (cons (first args) (second args))))
    (:scale (display-head-put head :scale (first args)))
    (:finished (remhash head *display-heads*))))

(defun display-mode-event (mode event-name &rest args)
  (case event-name
    (:size
     (display-mode-put mode :width (first args))
     (display-mode-put mode :height (second args)))
    (:refresh (display-mode-put mode :refresh (first args)))
    (:finished (remhash mode *display-modes*))))

(defun display-head-put (head key value)
  "Threaded plist update — setf-getf rebinds locals instead of
mutating the list (the decorations.lisp lesson): copy, set, write
the result back to the hash."
  (let ((state (copy-list (gethash head *display-heads*))))
    (setf (getf state key) value)
    (setf (gethash head *display-heads*) state)))

(defun display-mode-put (mode key value)
  (let ((state (copy-list (gethash mode *display-modes*))))
    (setf (getf state key) value)
    (setf (gethash mode *display-modes*) state)))

(defun display-heads ()
  "All tracked heads as state plists, ordered by name."
  (sort (loop for state being the hash-values of *display-heads*
              when (getf state :name)
              collect state)
        #'string< :key (lambda (state) (getf state :name))))

(defun display-mode-label (mode)
  "Human label for a mode proxy: \"1920x1080 @ 60Hz\"."
  (let ((info (and mode (gethash mode *display-modes*))))
    (if (and info (getf info :width) (getf info :refresh))
        (format nil "~Ax~A @ ~DHz"
                (getf info :width) (getf info :height)
                (round (/ (getf info :refresh) 1000)))
        "?")))

(defun display-apply (target &key mode scale)
  "Send a configuration: every tracked head is restated at its
current state (the protocol disables unmentioned heads — a partial
config blanks the other outputs); TARGET gets the MODE/SCALE
override. Returns T when the apply was sent."
  (unless *output-manager*
    (message "display: no output manager")
    (return-from display-apply nil))
  (let ((config (rwm:zwlr-output-manager-v1.create-configuration
                  *output-manager* *display-serial*)))
    (push (lambda (event-name &rest args)
            (declare (ignore args))
            (case event-name
              (:succeeded (message "display: applied"))
              (:failed (message "display: configuration rejected"))
              (:cancelled (message "display: cancelled"))))
          (wl:wl-proxy-hooks config))
    (dolist (state (display-heads))
      (let* ((head (getf state :head))
             (targetp (eq head target))
             (conf-head (rwm:zwlr-output-configuration-v1.enable-head
                          config head)))
        (unless (and targetp mode)
          (let ((current (getf state :current-mode)))
            (when current
              (rwm:zwlr-output-configuration-head-v1.set-mode
                conf-head current))))
        (unless (and targetp scale)
          (let ((current (getf state :scale)))
            (when current
              (rwm:zwlr-output-configuration-head-v1.set-scale
                conf-head current))))
        (let ((pos (getf state :position)))
          (when pos
            (rwm:zwlr-output-configuration-head-v1.set-position
              conf-head (car pos) (cdr pos))))
        (when targetp
          (when mode
            (rwm:zwlr-output-configuration-head-v1.set-mode
              conf-head mode))
          (when scale
            (rwm:zwlr-output-configuration-head-v1.set-scale
              conf-head scale)))))
    (rwm:zwlr-output-configuration-v1.apply config)
    t))

;;; --- Output layout from config ---
;;;
;;; (:displays (:layout SPECS)) positions heads declaratively. Each
;;; spec is (KEY . OPTIONS); KEY matches a head's connector name, or
;;; failing that a substring of its description, ignoring case.
;;; OPTIONS:
;;;   :position (X Y)    explicit position
;;;   :primary T         anchor at 0,0 for relative placement
;;;   :left-of KEY, :right-of KEY, :above KEY, :below KEY
;;;   :mode "WxH" or "WxH@HZ", :scale N
;;; "Primary" names only the anchor of relative placement: river has
;;; no primary output. Heads placed from :position or :primary are
;;; shifted together so their minimum x and y are 0. A head placed
;;; against an unnamed head is placed against its current position
;;; and is not shifted.

(defvar *display-layout-pending* nil
  "True while a configuration sent by DISPLAY-APPLY-LAYOUT awaits
river's succeeded, failed or cancelled event.")

(defvar *display-head-set* nil
  "The head set seen at the last done the layout handled, as
DISPLAY-HEAD-SET returns it. NIL before the first done.")

(defun display-head-set ()
  "The current heads as (name . enabled-p) pairs, ordered by name."
  (mapcar (lambda (state)
            (cons (getf state :name) (and (getf state :enabled) t)))
          (display-heads)))

(defun display-parse-mode (string)
  "Parse \"WxH\" or \"WxH@HZ\". Return (values width height hz), HZ
rounded to an integer or NIL when absent; NIL when STRING is
malformed."
  (when (stringp string)
    (ppcre:register-groups-bind ((#'parse-integer width height) hz)
        ("^\\s*(\\d+)x(\\d+)(?:@(\\d+(?:\\.\\d*)?))?\\s*$" string)
      (values width height
              (and hz (round (let ((*read-eval* nil))
                               (read-from-string hz))))))))

(defun display-find-mode (state string)
  "The mode of head STATE named by STRING (see DISPLAY-PARSE-MODE),
the highest refresh when several match, or NIL."
  (multiple-value-bind (width height hz) (display-parse-mode string)
    (when width
      (let ((matches
              (remove-if-not
               (lambda (mode)
                 (let ((info (gethash mode *display-modes*)))
                   (and info
                        (eql (getf info :width) width)
                        (eql (getf info :height) height)
                        (or (null hz)
                            (and (getf info :refresh)
                                 (= hz (round (getf info :refresh) 1000)))))))
               (getf state :modes))))
        (first (sort matches #'>
                     :key (lambda (mode)
                            (or (getf (gethash mode *display-modes*)
                                      :refresh)
                                0))))))))

(defun display-layout-match (key states)
  "The state in STATES whose name is KEY, else the first whose
description contains KEY ignoring case, else NIL."
  (or (find key states :key (lambda (state) (getf state :name))
                       :test #'equal)
      (find-if (lambda (state)
                 (let ((description (getf state :description)))
                   (and (stringp key) (stringp description)
                        (search key description :test #'char-equal))))
               states)))

(defun display-layout-size (mode scale)
  "Logical size of a head showing MODE at SCALE, as (values w h);
0 0 when the mode size is unknown."
  (let ((info (and mode (gethash mode *display-modes*)))
        (scale (if (and (realp scale) (plusp scale)) scale 1)))
    (values (round (or (getf info :width) 0) scale)
            (round (or (getf info :height) 0) scale))))

(defun display-resolve-layout (specs states)
  "Resolve layout SPECS against the enabled head STATES. Return one
plist per placed spec, in spec order:
  :state  the head's state plist
  :x :y   the resolved position
  :mode   the mode to set (the override, else the current mode)
  :scale  the scale to set (the override, else the current scale)
A spec whose head is absent, or whose relation names an absent head,
is left out, and its head keeps its position."
  (let* ((states (remove-if-not (lambda (state) (getf state :enabled))
                                states))
         (entries
           (let ((free states))
             (loop for spec in specs
                   for key = (and (consp spec) (first spec))
                   for state = (and key (display-layout-match key free))
                   when state
                     do (setf free (remove state free))
                     and collect
                         (let* ((options (rest spec))
                                (wanted (getf options :mode))
                                (mode (and wanted
                                           (display-find-mode state wanted))))
                           (when (and wanted (null mode))
                             (format t "~&; Rhizome: display layout: ~A has no mode ~A; keeping ~A~%"
                                     (getf state :name) wanted
                                     (display-mode-label
                                      (getf state :current-mode))))
                           (list :key key :state state :options options
                                 :mode (or mode (getf state :current-mode))
                                 :scale (or (getf options :scale)
                                            (getf state :scale)
                                            1))))))
         ;; state -> (x y kind); kind :free is normalized, :fixed is not
         (placed (make-hash-table :test 'eq)))
    (labels ((size (entry)
               (display-layout-size (getf entry :mode) (getf entry :scale)))
             (entry-for (state)
               (find state entries :key (lambda (e) (getf e :state))))
             (relation (entry)
               (loop for (rel target) on (getf entry :options) by #'cddr
                     when (member rel '(:left-of :right-of :above :below))
                       return (list rel target)))
             (target-state (key)
               (let ((entry (find key entries
                                  :key (lambda (e) (getf e :key))
                                  :test #'equal)))
                 (if entry
                     (getf entry :state)
                     (display-layout-match key states))))
             (target-place (state)
               (or (gethash state placed)
                   (unless (entry-for state)
                     ;; an unnamed head stays where it is
                     (let ((pos (getf state :position)))
                       (list (or (car pos) 0) (or (cdr pos) 0) :fixed)))))
             (target-size (state)
               (let ((entry (entry-for state)))
                 (if entry
                     (size entry)
                     (display-layout-size (getf state :current-mode)
                                          (getf state :scale))))))
      ;; Seeds: explicit positions, the anchor, and specs with no
      ;; placement of their own (they keep their current position)
      (dolist (entry entries)
        (let ((options (getf entry :options))
              (state (getf entry :state)))
          (cond ((getf options :position)
                 (destructuring-bind (x y) (getf options :position)
                   (setf (gethash state placed) (list x y :free))))
                ((getf options :primary)
                 (setf (gethash state placed) (list 0 0 :free)))
                ((null (relation entry))
                 (let ((pos (getf state :position)))
                   (setf (gethash state placed)
                         (list (or (car pos) 0) (or (cdr pos) 0) :fixed)))))))
      ;; Relations, until no more resolve
      (loop for progress = nil
            do (dolist (entry entries)
                 (let ((state (getf entry :state)))
                   (unless (gethash state placed)
                     (destructuring-bind (rel key) (relation entry)
                       (let* ((target (target-state key))
                              (place (and target (not (eq target state))
                                          (target-place target))))
                         (when place
                           (destructuring-bind (tx ty kind) place
                             (multiple-value-bind (w h) (size entry)
                               (multiple-value-bind (tw th) (target-size target)
                                 (setf (gethash state placed)
                                       (ecase rel
                                         (:left-of (list (- tx w) ty kind))
                                         (:right-of (list (+ tx tw) ty kind))
                                         (:above (list tx (- ty h) kind))
                                         (:below (list tx (+ ty th) kind)))
                                       progress t))))))))))
            while progress)
      ;; Normalize the free group to a 0,0 origin
      (let ((free (loop for place being the hash-values of placed
                        when (eq (third place) :free)
                          collect place)))
        (when free
          (let ((dx (reduce #'min free :key #'first))
                (dy (reduce #'min free :key #'second)))
            (dolist (place free)
              (decf (first place) dx)
              (decf (second place) dy)))))
      (loop for entry in entries
            for place = (gethash (getf entry :state) placed)
            when place
              collect (list :state (getf entry :state)
                            :x (first place) :y (second place)
                            :mode (getf entry :mode)
                            :scale (getf entry :scale))))))

(defun display-layout-holds-p (plan)
  "Whether every head in PLAN already has its planned position, mode
and scale."
  (every (lambda (item)
           (let* ((state (getf item :state))
                  (pos (getf state :position)))
             (and (equal pos (cons (getf item :x) (getf item :y)))
                  (eq (getf item :mode) (getf state :current-mode))
                  (let ((scale (getf state :scale)))
                    (and scale (= (getf item :scale) scale))))))
         plan))

(defun display-layout-summary (plan)
  "\"NAME X,Y\" for each head in PLAN, space separated."
  (format nil "~{~A~^ ~}"
          (mapcar (lambda (item)
                    (format nil "~A ~D,~D" (getf (getf item :state) :name)
                            (getf item :x) (getf item :y)))
                  plan)))

(defun display-send-layout (plan)
  "Send one configuration: heads in PLAN take their planned position,
mode and scale; every other enabled head is restated as it is, and a
disabled head stays disabled. Log river's answer and clear
*DISPLAY-LAYOUT-PENDING* when it arrives."
  (let ((config (rwm:zwlr-output-manager-v1.create-configuration
                 *output-manager* *display-serial*))
        (summary (display-layout-summary plan)))
    (push (lambda (event-name &rest args)
            (declare (ignore args))
            (case event-name
              (:succeeded
               (setf *display-layout-pending* nil)
               (format t "~&; Rhizome: display layout applied: ~A~%" summary))
              (:failed
               (setf *display-layout-pending* nil)
               (format t "~&; Rhizome: display layout rejected by the compositor: ~A~%"
                       summary))
              (:cancelled
               (setf *display-layout-pending* nil)
               (format t "~&; Rhizome: display layout cancelled (heads changed): ~A~%"
                       summary))))
          (wl:wl-proxy-hooks config))
    (dolist (state (display-heads))
      (let ((head (getf state :head))
            (item (find (getf state :head) plan
                        :key (lambda (item) (getf (getf item :state) :head)))))
        (if (not (getf state :enabled))
            (rwm:zwlr-output-configuration-v1.disable-head config head)
            (let ((conf-head (rwm:zwlr-output-configuration-v1.enable-head
                              config head))
                  (mode (if item (getf item :mode) (getf state :current-mode)))
                  (scale (if item (getf item :scale) (getf state :scale)))
                  (pos (if item
                           (cons (getf item :x) (getf item :y))
                           (getf state :position))))
              (when mode
                (rwm:zwlr-output-configuration-head-v1.set-mode conf-head mode))
              (when scale
                (rwm:zwlr-output-configuration-head-v1.set-scale conf-head scale))
              (when pos
                (rwm:zwlr-output-configuration-head-v1.set-position
                 conf-head (car pos) (cdr pos)))))))
    (rwm:zwlr-output-configuration-v1.apply config)
    (setf *display-layout-pending* t)))

(defvar *display-layout-absent-keys* nil
  "The layout keys that matched no head when absent heads were last
logged, in spec order.")

(defun display-layout-absent-keys (specs states)
  "The keys SPECS names, as a head or as a relation target, that match
no state in STATES (see DISPLAY-LAYOUT-MATCH). Spec order, no
duplicates."
  (let ((keys '()))
    (dolist (spec specs)
      (when (consp spec)
        (pushnew (first spec) keys :test #'equal)
        (loop for (rel target) on (rest spec) by #'cddr
              when (member rel '(:left-of :right-of :above :below))
                do (pushnew target keys :test #'equal))))
    (remove-if (lambda (key) (display-layout-match key states))
               (nreverse keys))))

(defun display-note-absent-heads (specs states)
  "Log one line for each key in SPECS that matches no head in STATES.
Log only when that set of keys differs from the one last logged, so an
unchanged layout applied again stays quiet."
  (let ((absent (display-layout-absent-keys specs states)))
    (unless (equal absent *display-layout-absent-keys*)
      (setf *display-layout-absent-keys* absent)
      (dolist (key absent)
        (format t "~&; Rhizome: display layout: no head matches ~S~%" key)))))

(defun display-apply-layout (&optional (specs (config-getf '(:displays) :layout)))
  "Lay out the heads from SPECS, by default (:displays :layout).
Return :applied when a configuration was sent, :unchanged when the
layout already holds, :pending while an earlier layout awaits river's
answer, :no-layout when SPECS is empty, or :no-manager before the
output manager is bound. Heads named in SPECS that match no head are
logged; see DISPLAY-NOTE-ABSENT-HEADS."
  (cond ((null specs) :no-layout)
        ((null *output-manager*) :no-manager)
        (*display-layout-pending* :pending)
        (t
         (let ((heads (display-heads)))
           (display-note-absent-heads specs heads)
           (let ((plan (display-resolve-layout specs heads)))
             (if (display-layout-holds-p plan)
                 :unchanged
                 (progn (display-send-layout plan)
                        :applied)))))))

(defun display-layout-on-done ()
  "Apply the configured layout when the head set differs from the one
seen at the last handled done. Skip while a layout is pending: the
done that follows river's answer sees the change. Errors are logged."
  (unless *display-layout-pending*
    (let ((heads (display-head-set)))
      (unless (equal heads *display-head-set*)
        (setf *display-head-set* heads)
        (handler-case (display-apply-layout)
          (error (e)
            (format t "~&; Rhizome: display layout failed: ~A~%" e)))))))
