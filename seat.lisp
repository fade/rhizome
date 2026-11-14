;;; Seat (input) management for Rhizome
;;;
;;; Each river_seat_v1 is wrapped in a rhizome-seat CLOS instance
;;; that tracks keyboard focus, pointer state, and pointer bindings.

(in-package #:rhizome)

(defvar *keyboard-repeat-rate* 25
  "Key repeat rate in keys per second (from compositor repeat-info).")
(defvar *keyboard-repeat-delay* 300
  "Key repeat delay in milliseconds (from compositor repeat-info).")
(defvar *keyboard-repeat-thread* nil
  "Thread handling key repeat.")
(defvar *keyboard-repeat-key* nil
  "Currently repeating keysym, or nil.")
(defvar *keyboard-repeat-stop* nil
  "Flag to stop the repeat thread.")
(defparameter *keyboard-shift-p* nil
  "Whether shift is currently held (from wl_keyboard modifiers).")

;;; --- Pointer bindings (MOD+drag move/resize) ---

(defvar *pointer-bindings* nil
  "Active river_pointer_binding_v1 objects (MOD+drag bindings).")
(defvar *pointer-bindings-enabled* nil
  "Whether the MOD+drag bindings have been enabled (they may only be
enabled during a manage sequence).")
(defvar *seats-with-pointer-bindings* (make-hash-table)
  "Seats that already have MOD+drag bindings (skip re-creation).")

(defun setup-pointer-bindings (seat)
  "Create the StumpWM-style MOD+drag bindings on SEAT: MOD+Left-drag
moves the window under the pointer (floating it first if tiled),
MOD+Right-drag resizes it, and MOD+Middle-click opens the window
context menu (menus.lisp). The modifier is the user's *mod*."
  (dolist (spec '((:move 272) (:resize 273) (:menu 274)))
    (let* ((type (first spec))
           (button (second spec))
           (binding (rwm:river-seat-v1.get-pointer-binding
                      (seat-proxy seat) button *mod*)))
      (push (lambda (event-name &rest args)
              (declare (ignore args))
              (when (eq event-name :pressed)
                (pointer-drag-start seat type)))
            (wl:wl-proxy-hooks binding))
      (push binding *pointer-bindings*))))

(defun enable-pointer-bindings-if-needed ()
  "Give every seat its MOD+drag bindings and enable them. Pointer
bindings may only be enabled during a manage sequence, so this runs
from on-manage-sequence; it also covers seats that registered before
a hot reload added the feature."
  (dolist (seat (all-seats))
    (unless (gethash seat *seats-with-pointer-bindings*)
      (handler-case
          (progn
            (setup-pointer-bindings seat)
            (setf (gethash seat *seats-with-pointer-bindings*) t))
        (error (e)
          (format t "~&; Rhizome: error creating pointer bindings: ~A~%" e)))))
  (unless *pointer-bindings-enabled*
    (dolist (binding *pointer-bindings*)
      (handler-case
          (rwm:river-pointer-binding-v1.enable binding)
        (error (e)
          (format t "~&; Rhizome: error enabling pointer binding: ~A~%" e))))
    (setf *pointer-bindings-enabled* t)))

(defun pointer-drag-start (seat type)
  "An MOD+drag began. Placeholder: redefined by floating.lisp."
  (declare (ignore seat type))
  nil)

(defun expose-clicked (win)
  "A window was clicked (placeholder: redefined by expose.lisp —
during the expose overview a click picks the window)."
  (declare (ignore win))
  nil)

(defclass rhizome-seat ()
  ((proxy          :initarg :proxy          :accessor seat-proxy)
   (wl-seat-name   :initarg :wl-seat-name   :accessor seat-wl-seat-name :initform nil)
   (wl-seat-proxy  :initarg :wl-seat-proxy  :accessor seat-wl-seat-proxy :initform nil)
   (wl-keyboard    :initarg :wl-keyboard    :accessor seat-wl-keyboard :initform nil)
   (focused-window :initarg :focused-window :accessor seat-focused-window :initform nil)
   (pointer-x      :initarg :pointer-x      :accessor seat-pointer-x :initform 0)
   (pointer-y      :initarg :pointer-y      :accessor seat-pointer-y :initform 0)
   (op-pending     :initarg :op-pending     :accessor seat-op-pending :initform nil)
   (op-state       :initarg :op-state       :accessor seat-op-state :initform nil))
  (:documentation "Represents an input seat managed by rhizome."))

(defun register-seat (proxy)
  "Register a new river_seat_v1 proxy and set up event handlers."
  (let ((seat (make-instance 'rhizome-seat :proxy proxy)))
    (setf (gethash proxy *seats*) seat)
    (push (lambda (event-name &rest args)
            (apply #'handle-seat-event seat event-name args))
          (wl:wl-proxy-hooks proxy))
    (format t "~&; Rhizome: new seat registered~%")
    ;; xcursor theme through the protocol (river_seat_v1 v2+): the
    ;; compositor-rendered cursors follow the config (or the
    ;; XCURSOR_THEME/SIZE environment the session exports).
    (let* ((env-theme (uiop:getenv "XCURSOR_THEME"))
           (env-size (ignore-errors
                       (parse-integer
                         (or (uiop:getenv "XCURSOR_SIZE") "")
                         :junk-allowed t)))
           (theme (config-getf '(:pointer) :xcursor-theme
                               :default (or env-theme "")))
           (size (config-getf '(:pointer) :xcursor-size
                              :default (or env-size 24))))
      (when (and (stringp theme) (plusp (length theme)))
        (handler-case
            (rwm:river-seat-v1.set-xcursor-theme proxy theme size)
          (error ()
            ;; Pre-v2 river: cursors fall back to river's default
            nil))))
    ;; MOD+drag move/resize bindings (enabled during manage sequences)
    (handler-case
        (progn
          (setup-pointer-bindings seat)
          (setf (gethash seat *seats-with-pointer-bindings*) t))
      (error (e)
        (format t "~&; Rhizome: error creating pointer bindings: ~A~%" e)))
    ;; Register with river_layer_shell_v1 if available
    (when *river-layer-shell*
      (handler-case
          (let ((ls-seat (rwm:river-layer-shell-v1.get-seat
                           *river-layer-shell* proxy)))
            (push (lambda (event-name &rest args)
                    (case event-name
                      (:focus_exclusive
                       (format t "~&; Rhizome: layer shell exclusive focus~%"))
                      (:focus_non_exclusive
                       (format t "~&; Rhizome: layer shell non-exclusive focus~%"))
                      (:focus_none
                       (format t "~&; Rhizome: layer shell focus none~%"))))
                  (wl:wl-proxy-hooks ls-seat))
            (format t "~&; Rhizome: registered seat with river_layer_shell_v1~%"))
        (error (e)
          (format t "~&; Rhizome: failed to register seat with layer shell: ~A~%" e))))))

;;; --- Mouse focus policy ---
;;;
;;; (:pointer :focus-policy) is :click (only a click focuses) or
;;; :sloppy (the window the pointer moves into takes keyboard focus,
;;; without raising). River sends pointer_position only when the
;;; position changed, and a position change never starts a manage
;;; sequence on its own, so a pointer_enter that arrives without one
;;; came from a layout change, a workspace switch, or a window mapping
;;; under a still pointer, never from the operator moving it.

(defvar *pointer-focus-state* (make-hash-table :test 'eq)
  "Per-seat pointer focus bookkeeping, a plist per seat:
:entered is the window under the pointer, or NIL;
:x and :y are the pointer position at the previous manage sequence;
:focus is the seat's focused window when the previous pass ended.")

(defun pointer-focus-get (seat key)
  "KEY of SEAT's pointer focus bookkeeping."
  (getf (gethash seat *pointer-focus-state*) key))

(defun pointer-focus-put (seat key value)
  "Set KEY of SEAT's pointer focus bookkeeping to VALUE."
  (setf (getf (gethash seat *pointer-focus-state*) key) value))

(defun mouse-focus-policy ()
  "The configured mouse focus policy, :click or :sloppy. A missing or
unrecognized (:pointer :focus-policy) reads as :click."
  (if (eq (config-getf '(:pointer) :focus-policy :default :click) :sloppy)
      :sloppy
      :click))

(defun pointer-focus-suppressed-p (seat)
  "True while a modal interaction owns input: the expose overview, a
prompt, the eval prompt, the launcher, a menu, prefix-key capture
(which-key included), interactive frame resize, or an interactive
float move/resize on SEAT."
  (or (seat-op-state seat)
      (seat-op-pending seat)
      (some (lambda (name) (and (boundp name) (symbol-value name)))
            '(*expose-active* *prompt-active* *eval-prompt-active*
              *launcher-active* *menu-active* *prefix-mode-active*
              *iresize-active*))))

(defun pointer-focus-target (seat)
  "The managed, shown, unfocused window under SEAT's pointer, or NIL."
  (let ((win (pointer-focus-get seat :entered)))
    (and win
         (window-proxy win)
         (eq (gethash (window-proxy win) *windows*) win)
         (not (window-hidden-p win))
         (not (window-minimized-p win))
         (not (window-focused-p win))
         win)))

(defun pointer-focus-manage-pass ()
  "Apply the :sloppy mouse focus policy in a manage sequence, before
queued focus requests flush. For each seat, the window under the
pointer takes keyboard focus, without raising, only when both hold:
the pointer moved since the previous manage sequence;
the seat's focus is what the previous pass left it as.
The second condition skips the sequence in which a key or command
moved focus, because the motion that sequence reports may predate
the move. Focus then stays put until the pointer moves again."
  (let ((sloppy (eq (mouse-focus-policy) :sloppy)))
    (dolist (seat (all-seats))
      (let* ((x (seat-pointer-x seat))
             (y (seat-pointer-y seat))
             (last-x (pointer-focus-get seat :x))
             (last-y (pointer-focus-get seat :y))
             (moved (and last-x last-y
                         (or (/= x last-x) (/= y last-y))))
             (undisturbed (eq (seat-focused-window seat)
                              (pointer-focus-get seat :focus)))
             (target (pointer-focus-target seat)))
        (when (and sloppy moved undisturbed target
                   (not (pointer-focus-suppressed-p seat)))
          (focus-window target seat :raise nil))
        (pointer-focus-put seat :x x)
        (pointer-focus-put seat :y y)
        (pointer-focus-put seat :focus (seat-focused-window seat))))))

(defun handle-seat-event (seat event-name &rest args)
  "Dispatch river_seat_v1 events for SEAT."
  (case event-name
    (:wl-seat
     (destructuring-bind (name) args
       (setf (seat-wl-seat-name seat) name)
       ;; Bind to the wl_seat global and get a keyboard
       (setup-wl-keyboard seat name)))

    (:pointer-position
     (destructuring-bind (x y) args
       (setf (seat-pointer-x seat) x
             (seat-pointer-y seat) y)))

    (:pointer-enter
     ;; Recorded for the mouse focus policy, which acts on it in the
     ;; manage sequence that follows (pointer-focus-manage-pass)
     (destructuring-bind (window-proxy) args
       (pointer-focus-put seat :entered (gethash window-proxy *windows*))))

    (:pointer-leave
     (pointer-focus-put seat :entered nil))

    (:window-interaction
     (destructuring-bind (window-proxy) args
       (let ((win (gethash window-proxy *windows*)))
         (when win
           ;; River reports a pointer press / touch interaction with
           ;; this window so the WM can give it keyboard focus.
           ;; Actually send the focus request (previously only the
           ;; internal slot was updated, so clicks never moved focus).
           (focus-window win seat)
           ;; Expose: a click during the overview picks the window
           ;; (redefined by expose.lisp, which loads later)
           (expose-clicked win)
           (request-manage-dirty)))))

    (:op-delta
     ;; Interactive float move/resize: river reports the TOTAL
     ;; cumulative pointer motion since the op started. Applied by
     ;; float-op-manage-pass in the manage sequence that follows.
     (destructuring-bind (dx dy) args
       (when (seat-op-state seat)
         (setf (getf (seat-op-state seat) :dx) dx
               (getf (seat-op-state seat) :dy) dy))))

    (:op-release
     ;; Buttons released: the op ends in the next manage sequence.
     (when (seat-op-state seat)
       (setf (getf (seat-op-state seat) :released) t)))

    (:removed
     (format t "~&; Rhizome: seat removed~%")
     (remhash (seat-proxy seat) *seats*))

    (t
     (format t "~&; Rhizome: unhandled seat event ~A~%" event-name))))

(defun setup-wl-keyboard (seat wl-seat-name)
  "Bind to wl_seat global and get a wl_keyboard for keyboard events."
  (let ((iface-class (wl:find-interface-named "wl_seat")))
    (unless iface-class
      (format t "~&; Rhizome: wl_seat interface not found~%")
      (return-from setup-wl-keyboard))
    (unless *registry*
      (format t "~&; Rhizome: no registry, cannot bind wl_seat~%")
      (return-from setup-wl-keyboard))
    (let ((wl-seat (wl:wl-registry.bind *registry* wl-seat-name iface-class 7)))
      (setf (seat-wl-seat-proxy seat) wl-seat)
      ;; Set up event handler for wl_seat events
      (push (lambda (event-name &rest args)
              (case event-name
                (:capabilities
                 (destructuring-bind (caps) args
                   ;; Wayflan decodes bitfield as list of keywords
                   ;; Check if :keyboard is in the capabilities list
                   (when (member :keyboard caps :test #'eq)
                     (unless (seat-wl-keyboard seat)
                       (let ((kb (wl:wl-seat.get-keyboard wl-seat)))
                         (setf (seat-wl-keyboard seat) kb)
                         (setup-keyboard-handler seat kb))))))
                (:name
                 nil)))
            (wl:wl-proxy-hooks wl-seat))
      ;; Do a roundtrip to get capabilities
      (wl:wl-display-roundtrip *display*))))

(defun setup-keyboard-handler (seat keyboard)
  "Set up event handler for wl_keyboard key events."
  (declare (ignore seat))
  (push (lambda (event-name &rest args)
          (case event-name
            (:keymap nil)
            (:enter
             (format t "~&; Rhizome [debug]: keyboard enter surface~%"))
            (:leave
             (format t "~&; Rhizome [debug]: keyboard leave surface~%"))
            (:key
             (destructuring-bind (serial time key state) args
               (declare (ignore serial time))
               (let ((keysym (evdev-to-xkb key *keyboard-shift-p*)))
                 (when *prompt-active*
                   (format t "~&; Rhizome [debug]: prompt key keysym=~A state=~A~%"
                           keysym state))
                 (cond
                   ((eq state :pressed)
                    (keyboard-stop-repeat)
                    (cond
                      ;; Prefix key chains (C-t ...) consume keys first
                      ((and *prefix-mode-active* keysym)
                       (prefix-handle-key keysym))
                      ;; The context menu (Escape/arrows/Enter)
                      ((and *menu-active* keysym)
                       (menu-handle-key keysym))
                      ;; Interactive prompt next
                      ((and *prompt-active* keysym)
                       (prompt-handle-key keysym))
                      ;; Live eval prompt next
                      ((and *eval-prompt-active* keysym)
                       (eval-prompt-handle-key keysym))
                      ((and *launcher-active* keysym)
                       (launcher-handle-key keysym nil)
                       (when (member keysym '(65362 65364 65288))
                         (keyboard-start-repeat keysym)))))
                   ((eq state :released)
                    (keyboard-stop-repeat))))))
            (:modifiers
             (destructuring-bind (serial depressed latched locked group) args
               (declare (ignore serial latched locked group))
               ;; XKB shift is mask bit 0
               (setf *keyboard-shift-p* (logbitp 0 depressed))))
            (:repeat-info
             (destructuring-bind (rate delay) args
               (setf *keyboard-repeat-rate* rate
                     *keyboard-repeat-delay* delay)
               (format t "~&; Rhizome: keyboard repeat rate=~A delay=~A~%"
                       rate delay)))))
        (wl:wl-proxy-hooks keyboard)))

(defun keyboard-start-repeat (keysym)
  "Start a repeat thread for KEYSYM."
  (keyboard-stop-repeat)
  (setf *keyboard-repeat-key* keysym
        *keyboard-repeat-stop* nil)
  (setf *keyboard-repeat-thread*
        (bt:make-thread
          (lambda ()
            (sleep (/ *keyboard-repeat-delay* 1000.0))
            (loop while (not *keyboard-repeat-stop*) do
              (when *launcher-active*
                (launcher-handle-key *keyboard-repeat-key* nil))
              (sleep (/ 1.0 *keyboard-repeat-rate*))))
          :name "rhizome-key-repeat")))

(defun keyboard-stop-repeat ()
  "Stop the current repeat thread."
  (setf *keyboard-repeat-stop* t)
  (when *keyboard-repeat-thread*
    (bt:destroy-thread *keyboard-repeat-thread*)
    (setf *keyboard-repeat-thread* nil))
  (setf *keyboard-repeat-key* nil))

(defun evdev-to-xkb (keycode &optional (shift-p nil))
  "Convert a Linux evdev keycode to an xkb keysym, considering shift.
QWERTY layout assumed. Supports the full printable ASCII set (with
shift) so that Lisp expressions can be typed in the eval prompt."
  (let ((row1 "qwertyuiop")
        (row2 "asdfghjkl")
        (row3 "zxcvbnm")
        (digits "1234567890")
        (shifted "!@#$%^&*()"))
    (cond
      ;; Letter rows; shift produces uppercase
      ((<= 16 keycode 25)
       (let ((c (char row1 (- keycode 16))))
         (char-code (if shift-p (char-upcase c) c))))
      ((<= 30 keycode 38)
       (let ((c (char row2 (- keycode 30))))
         (char-code (if shift-p (char-upcase c) c))))
      ((<= 44 keycode 50)
       (let ((c (char row3 (- keycode 44))))
         (char-code (if shift-p (char-upcase c) c))))
      ;; Digit row
      ((<= 2 keycode 11)
       (char-code (if shift-p
                      (char shifted (- keycode 2))
                      (char digits (- keycode 2)))))
      ;; Punctuation with shift (raw evdev codes: 26=[ 27=] 39=;
      ;; 40=' 41=` 43=\ — 39/40 were previously swapped, which made
      ;; C-t ; dispatch as C-t ' (the window-number prompt))
      (shift-p
       (case keycode
         (12 (char-code #\_)) (13 (char-code #\+))
         (26 (char-code #\{)) (27 (char-code #\}))
         (39 (char-code #\:)) (40 (char-code #\"))
         (41 (char-code #\~)) (43 (char-code #\|))
         (51 (char-code #\<)) (52 (char-code #\>))
         (53 (char-code #\?))
         (t nil)))
      ;; Unshifted
      (t
       (case keycode
         (1 65307)         ; ESC
         (14 65288)        ; Backspace
         (15 65289)        ; Tab
         (28 65293)        ; Enter
         (103 65362)       ; Up
         (108 65364)       ; Down
         (105 65361)       ; Left
         (106 65363)       ; Right
         (57 32)           ; Space
         ;; Function keys F1-F12
         (59 65470) (60 65471) (61 65472) (62 65473)
         (63 65474) (64 65475) (65 65476) (66 65477)
         (67 65478) (68 65479) (87 65480) (88 65481)
         (12 (char-code #\-))
         (13 (char-code #\=))
         (26 (char-code #\[))
         (27 (char-code #\]))
         (39 (char-code #\;))
         (40 (char-code #\'))
         (41 (char-code #\`))
         (43 (char-code #\\))
         (51 (char-code #\,))
         (52 (char-code #\.))
         (53 (char-code #\/))
         (t nil))))))

(defun all-seats ()
  "Return a list of all registered seats."
  (loop for seat being the hash-values of *seats* collect seat))

(defun clear-focus (seat)
  "Clear keyboard focus on SEAT."
  (rwm:river-seat-v1.clear-focus (seat-proxy seat))
  (setf (seat-focused-window seat) nil))
