;;; Floating windows for Rhizome — the StumpWM float experience
;;;
;;; Not everything belongs in a tiling grid: GIMP toolboxes, palette
;;; dialogs, and picture-in-picture video want to keep their own size
;;; and be moved around freely. StumpWM handles these as floats and
;;; so does rhizome:
;;;
;;;   - A floating window keeps its own position and dimensions; the
;;;     tiling layouts never see it, and it stacks above the tiling.
;;;   - On first float we tell the window it is NOT part of the tiling
;;;     (river_window_v1.set_tiled none) so it draws its own shadows
;;;     and decorations, and propose 0x0 so it picks its own size —
;;;     a GIMP toolbox comes up exactly as GIMP intends.
;;;   - Interactive move/resize rides river's pointer op protocol:
;;;     clients request a move (titlebar drag), op_delta events carry
;;;     the cumulative pointer motion, and each manage sequence applies
;;;     it. Sizes are clamped to the window's own min/max hints.
;;;   - Window rules can float apps by regex:
;;;     (rhizome:configure '(:window-rules ((:match "gimp" :float t))))
;;;   - C-t f toggles the focused window between float and tiling.

(in-package #:rhizome)

(defparameter *float-min-size* 50
  "Smallest float dimension a user resize may produce.")

(defvar *float-centre-pending* (make-hash-table :test #'eq)
  "Floats not yet shown that the render pass places once their size is
known (WINDOW -> T); see float-place-new.")

(defvar *dialog-float-decided* (make-hash-table :test #'eq)
  "Windows float-new-dialogs has considered: WINDOW -> the internal real
time of that decision, or T once float-late-dialog has acted.")

(defparameter *dialog-late-parent-seconds* 2
  "How long after a window maps a parent arriving still floats it as a
dialog; see float-late-dialog.")

;;; --- Float state ---

(defun float-window (win &key x y width height (animate t) quiet)
  "Make WIN float, keeping its current geometry by default (a window
floated from its tiling position must not jump). A window never shown
and given no position is placed once its size is known; see
float-place-new. Explicit :x :y
:width :height overrides place it precisely. Placing at an
explicit position glides when the animation engine is on (expose
and maximize ride this). QUIET suppresses the \"floated\" message."
  (if (window-floating-p win)
      ;; Already floating: an explicit reposition glides from the
      ;; current rendered position to the target
      (when (or x y)
        (when (and animate (animations-enabled-p))
          (animation-start win :from-x (window-x win)
                                  :from-y (window-y win)))
        (when x (setf (window-float-x win) x))
        (when y (setf (window-float-y win) y))
        (message "moved: ~A" (or (window-title win)
                                 (window-app-id win) "window"))
        (request-manage-dirty))
      (progn
        ;; Never shown and no explicit position: place it once its
        ;; size is known (float-place-new)
        (when (and (null x) (null y) (null (window-node win)))
          (setf (gethash win *float-centre-pending*) t))
        (setf (window-floating-p win) t
              (window-float-x win) (or x (window-x win) 0)
              (window-float-y win) (or y (window-y win) 0)
              (window-float-width win) (or width
                                         (when (plusp (window-width win))
                                           (window-width win)))
              (window-float-height win) (or height
                                           (when (plusp (window-height win))
                                             (window-height win))))
        ;; Leave the frame tree: a float must not be laid out as tiled
        (let ((frame (window-frame win)))
          (when frame
            (setf (frame-windows frame) (remove win (frame-windows frame))
                  (window-frame win) nil)))
        ;; First float at an explicit position glides from the
        ;; tiling position; keeping geometry is instant
        (when (and animate (or x y) (animations-enabled-p))
          (animation-start win :from-x (window-x win)
                                  :from-y (window-y win)))
        (unless quiet
          (message "floated: ~A" (or (window-title win)
                                     (window-app-id win) "window")))
        (request-manage-dirty))))

(defun unfloat-window (win)
  "Return WIN to the tiling."
  (unless (window-floating-p win)
    (return-from unfloat-window))
  (remhash win *float-centre-pending*)
  (setf (window-floating-p win) nil
        ;; Manual unfloat supersedes any maximize state
        (window-maximized-p win) nil
        (window-maximize-prev win) nil)
  ;; Glide back to wherever the tiling places it
  (when (animations-enabled-p)
    (animation-start win :from-x (window-x win)
                            :from-y (window-y win)))
  (message "tiled: ~A" (or (window-title win) (window-app-id win) "window"))
  (request-manage-dirty))

;;; --- Maximize ---
;;;
;;; river_window_v1.maximize_requested / unmaximize_requested arrive
;;; whenever a client wants to be maximized (xdg_toplevel.set_maximized
;;; under the hood). We honor them as floats covering the output's
;;; usable area — the same geometry the tiling uses, bar strip
;;; included — and restore the exact prior state on unmaximize.

(defun output-usable-area (out)
  "The usable area of OUT as four values (x y width height): output
geometry minus the bar's reserved strip at the top. Matches the
tiling's convention everywhere bar-reserved-height appears."
  (values (output-x out)
          (+ (output-y out) (bar-reserved-height))
          (output-width out)
          (max 0 (- (output-height out) (bar-reserved-height)))))

(defun maximize-window (win)
  "Float WIN to the usable area of its output, saving the
pre-maximize state: tiled windows return to the grid on unmaximize,
floats restore their own geometry. Size hints still clamp (a
fixed-size dialog won't stretch), since they express a preference
the WM is free to exceed but usually shouldn't."
  (when (window-maximized-p win)
    (return-from maximize-window))
  (setf (window-maximize-prev win)
        (if (window-floating-p win)
            (list :floating
                  (window-float-x win) (window-float-y win)
                  (window-float-width win) (window-float-height win))
            :tiled)
        (window-maximized-p win) t)
  (let ((out (window-output win)))
    (when out
      (multiple-value-bind (ux uy uw uh) (output-usable-area out)
        (let ((w (float-clamp-dim win uw :width))
              (h (float-clamp-dim win uh :height)))
          (if (window-floating-p win)
              (setf (window-float-x win) ux
                    (window-float-y win) uy
                    (window-float-width win) w
                    (window-float-height win) h)
              (float-window win :x ux :y uy :width w :height h))))))
  (message "maximized: ~A" (or (window-title win) (window-app-id win) "window"))
  (request-manage-dirty))

(defun unmaximize-window (win)
  "Restore WIN's pre-maximize state."
  (unless (window-maximized-p win)
    (return-from unmaximize-window))
  (let ((prev (window-maximize-prev win)))
    (setf (window-maximized-p win) nil
          (window-maximize-prev win) nil)
    (if (eq prev :tiled)
        (unfloat-window win)
        (progn
          (destructuring-bind (x y w h) (cdr prev)
            (setf (window-float-x win) x
                  (window-float-y win) y
                  (window-float-width win) w
                  (window-float-height win) h))
          (message "unmaximized: ~A"
                   (or (window-title win) (window-app-id win) "window"))
          (request-manage-dirty)))))

(defun toggle-maximize (win)
  "Toggle WIN's maximize (IPC: rhizome-cmd maximize)."
  (when win
    (if (window-maximized-p win)
        (unmaximize-window win)
        (maximize-window win))))

(defun toggle-float (win)
  "Toggle WIN between floating and tiled."
  (if (window-floating-p win)
      (unfloat-window win)
      (float-window win)))

(defun focused-window ()
  "The window with keyboard focus, if any."
  (let ((seats (all-seats)))
    (and seats (seat-focused-window (first seats)))))

(defun toggle-float-focused ()
  "Float/tile the focused window."
  (let ((win (focused-window)))
    (when win
      (toggle-float win))))

;;; --- Geometry helpers ---

(defun float-clamp-position (win)
  "Keep WIN's float position reachable: at least a ~30px strip of the
window stays on its output, so a float can never be lost off-screen.
A float that carries a titlebar keeps the whole strip below the bar:
its top edge is at least the strip height plus the border below the
usable area's top."
  (let* ((out (window-output win))
         (w (or (window-float-width win) (window-width win) 100))
         (h (or (window-float-height win) (window-height win) 100)))
    (when out
      (let ((ox (output-x out)) (oy (output-y out))
            (ow (output-width out)) (oh (output-height out)))
        (setf (window-float-x win)
              (min (max (window-float-x win) (- ox (- w 30)))
                   (+ ox ow -30))
              (window-float-y win)
              (min (max (window-float-y win) (- oy (- h 30)))
                   (+ oy oh -30)))
        ;; The focused border width (2) for both focus states, so a
        ;; focus change never moves a float
        (when (window-titlebar-p win)
          (setf (window-float-y win)
                (max (window-float-y win)
                     (+ oy (bar-reserved-height) (titlebar-height) 2))))))))

(defun float-clamp-dim (win size dimension)
  "Clamp SIZE (a :width or :height) to WIN's min/max size hints and
the *float-min-size* floor. A hint of 0 means no preference."
  (let ((min (ecase dimension
               (:width (window-hint-min-width win))
               (:height (window-hint-min-height win))))
        (max (ecase dimension
               (:width (window-hint-max-width win))
               (:height (window-hint-max-height win)))))
    (setf size (max size (max *float-min-size* (or min 0))))
    (when (and max (plusp max))
      (setf size (min size max)))
    size))

;;; --- Where a new float first appears ---

(defun window-placement-pending-p (win)
  "True while WIN, a float never shown, awaits its first placement.
Autofocus waits for it, so focus follows the window to where it lands."
  (and (gethash win *float-centre-pending*) t))

(defun frame-layout-area (output frame)
  "FRAME's rectangle on OUTPUT as (VALUES X Y WIDTH HEIGHT) in layout
coordinates."
  (values (+ (output-x output) (frame-x frame))
          (+ (output-y output) (bar-reserved-height) (frame-y frame))
          (frame-width frame)
          (frame-height frame)))

(defun dialog-active-output (win)
  "The output the operator works on, for the dialog WIN: the output of
the focused window other than WIN, else the leftmost output, else WIN's
own output."
  (let ((focused (find-if (lambda (w)
                            (and (not (eq w win))
                                 (window-focused-p w)
                                 (window-output w)))
                          (all-windows))))
    (or (and focused (window-output focused))
        (first (all-outputs-sorted))
        (window-output win))))

(defun dialog-target (win)
  "Where the dialog WIN first appears, as (VALUES OUTPUT WORKSPACE FRAME).
- The parent is tiled in a frame at least WIN's size in both
  dimensions: that frame, on the parent's output and workspace, shown
  or not.
- Otherwise (the parent floats, is minimized, is in no frame, is not
  managed, or its frame is smaller than WIN in either dimension): the
  current frame of the active output's current workspace."
  (let* ((parent (window-parent win))
         (frame (and parent (window-frame parent))))
    (if (and frame
             (not (window-floating-p parent))
             (not (window-minimized-p parent))
             (window-output parent)
             (window-workspace parent)
             (<= (window-width win) (frame-width frame))
             (<= (window-height win) (frame-height frame)))
        (values (window-output parent) (window-workspace parent) frame)
        (let* ((output (dialog-active-output win))
               (workspace (current-workspace output)))
          (values output workspace (current-frame-for output workspace))))))

(defun frame-location (output workspace frame)
  "FRAME's location as (VALUES STRING SCREEN INDEX). STRING is
\"group:screen:frame\": the workspace number, SCREEN (OUTPUT's 1-based
position left to right), and INDEX (FRAME's 0-based position in
layout order)."
  (let ((screen (1+ (or (position output (all-outputs-sorted)) 0)))
        (index (or (position frame
                             (frame-leaves (frame-tree-for output workspace)))
                   0)))
    (values (format nil "~D:~D:~D" workspace screen index) screen index)))

(defun notify-dialog-elsewhere (win output workspace frame)
  "Show a notice naming the dialog WIN and the frame it opened in, for
(:dialogs :notify-seconds) seconds in (:dialogs :notify-color)."
  (multiple-value-bind (location screen index)
      (frame-location output workspace frame)
    (show-message
     (format nil "Dialog ~S (~A) opened on workspace ~D, screen ~D, frame ~D (~A)"
             (or (window-title win) "untitled")
             (or (window-app-id win) "unknown app")
             workspace screen index location)
     (config-getf '(:dialogs) :notify-seconds :default 10)
     (config-getf '(:dialogs) :notify-color :default "#ff5fd7"))))

(defun float-place-new (win)
  "Place WIN when it awaits its first placement and its client has
committed a size, then clear the pending mark; otherwise do nothing.
A window with a parent moves to the output, workspace, and frame of
dialog-target and is centred on that frame; a notice follows when that
workspace is not the one its output shows. A window with no parent is
centred on its output's usable area. Either way WIN then moves the
least distance that keeps it inside that output's usable area, or to
the area's top-left corner when it is larger. Returns true when WIN
was placed."
  (when (and (gethash win *float-centre-pending*)
             (window-output win)
             (plusp (window-width win))
             (plusp (window-height win)))
    (remhash win *float-centre-pending*)
    (flet ((centre (ax ay aw ah)
             (setf (window-float-x win) (+ ax (floor (- aw (window-width win)) 2))
                   (window-float-y win) (+ ay (floor (- ah (window-height win)) 2)))))
      (if (window-has-parent-p win)
          (multiple-value-bind (output workspace frame) (dialog-target win)
            (setf (window-output win) output)
            (assign-window-workspace win workspace)
            (multiple-value-call #'centre (frame-layout-area output frame))
            (unless (eql workspace (current-workspace output))
              (notify-dialog-elsewhere win output workspace frame)))
          (multiple-value-call #'centre (output-usable-area (window-output win))))
      ;; Keep the window on its output's usable area: a dialog centred
      ;; on a small frame moves the least distance to fit, and one
      ;; larger than the area starts at its top-left corner
      (multiple-value-bind (ux uy uw uh) (output-usable-area (window-output win))
        (flet ((fit (pos size start length)
                 (if (> size length)
                     start
                     (min (max pos start) (- (+ start length) size)))))
          (setf (window-float-x win)
                (fit (window-float-x win) (window-width win) ux uw)
                (window-float-y win)
                (fit (window-float-y win) (window-height win) uy uh)))))
    ;; Autofocus waits on the placement: a manage pass applies it
    (request-manage-dirty)
    t))

(defun float-place-pending ()
  "Place every float awaiting its first placement whose size is known
(float-place-new), on any output."
  (dolist (win (all-windows))
    (float-place-new win)))

(defun window-rule-tiles-dialogs-p (win)
  "True when the first window rule matching WIN carries :float NIL."
  (let ((rule (find-if (lambda (rule) (window-rule-matches-p win rule))
                       (config-get '(:window-rules)))))
    (and rule
         (loop for (key value) on rule by #'cddr
               thereis (and (eq key :float) (null value))))))

(defun float-new-dialogs ()
  "Float each window not yet seen by a manage pass whose client named a
parent, unless (:dialogs :float) is NIL or the first window rule
matching it carries :float NIL. Each window is considered once, so a
dialog later tiled by hand stays tiled; float-late-dialog covers a
parent that arrives just after. Run at the start of a manage pass,
before new windows are stamped onto a workspace."
  (dolist (win (all-windows))
    (unless (or (gethash win *dialog-float-decided*)
                (window-node win)
                (window-frame win))
      (setf (gethash win *dialog-float-decided*) (get-internal-real-time))
      (when (and (window-has-parent-p win)
                 (not (window-floating-p win))
                 (not (window-minimized-p win))
                 (config-getf '(:dialogs) :float :default t)
                 (not (window-rule-tiles-dialogs-p win)))
        (float-window win :animate nil :quiet t)))))

(defun float-late-dialog (win)
  "Float WIN when its parent arrives within *dialog-late-parent-seconds*
of float-new-dialogs leaving it tiled, under the same rule and
(:dialogs :float) checks, and place it as a new dialog. Clients that
name a parent through xdg-foreign, such as a portal's file picker, do
so only after the window maps. Returns true when WIN floated."
  (let ((decided (gethash win *dialog-float-decided*)))
    (when (and (realp decided)
               (< (- (get-internal-real-time) decided)
                  (* *dialog-late-parent-seconds*
                     internal-time-units-per-second))
               (window-has-parent-p win)
               (not (window-floating-p win))
               (not (window-minimized-p win))
               (config-getf '(:dialogs) :float :default t)
               (not (window-rule-tiles-dialogs-p win)))
      (setf (gethash win *dialog-float-decided*) t)
      (float-window win :animate nil :quiet t)
      (setf (gethash win *float-centre-pending*) t)
      t)))

(defun forget-float-centre (win)
  "Drop WIN's pending first placement and its dialog float decision."
  (remhash win *float-centre-pending*)
  (remhash win *dialog-float-decided*)
  nil)

(defun float-apply-resize (win op)
  "Apply the cumulative resize delta of OP (a seat op-state plist) to
WIN, honoring which EDGES the drag started from."
  (let* ((dx (getf op :dx 0)) (dy (getf op :dy 0))
         (edges (getf op :edges)))
    (let ((x (getf op :origin-x)) (y (getf op :origin-y))
          (w (getf op :origin-w)) (h (getf op :origin-h)))
      (when (member :left edges)
        (incf x dx) (decf w dx))
      (when (member :right edges)
        (incf w dx))
      (when (member :top edges)
        (incf y dy) (decf h dy))
      (when (member :bottom edges)
        (incf h dy))
      (setf (window-float-x win) x
            (window-float-y win) y
            (window-float-width win) (float-clamp-dim win w :width)
            (window-float-height win) (float-clamp-dim win h :height)))))

;;; --- Manage/render sequence integration ---
;;;
;;; Called from the tiling engine's passes (tiling.lisp), which own
;;; the manage/render sequence split.

(defun float-propose (win)
  "Manage-sequence requests for one floating window: inform it that it
is not part of the tiling, and propose its float dimensions — 0x0
(= the window picks its own size) until a size is known."
  (set-window-tiled-state win '(:none))
  (if (and (window-float-width win) (window-float-height win))
      (propose-window-dimensions win
                                 (window-float-width win)
                                 (window-float-height win))
      (propose-window-dimensions win 0 0)))

(defun float-manage-decisions (output)
  "Manage-sequence decisions for floating windows on OUTPUT: adopt
new/unassigned floats onto the current workspace, and re-propose the
dimensions of floats on the current workspace."
  (let ((workspace (current-workspace output)))
    (dolist (win (all-windows))
      (unless (or (window-minimized-p win)
                   (not (window-floating-p win)))
        (cond ((null (window-output win))
               ;; Adopt: same as the tiling path stamping placements
               (setf (window-output win) output)
               (assign-window-workspace win workspace)
               ;; configure_bounds hint for fresh floats too
               (recommend-dimension-bounds win)
               (float-propose win))
              ((and (eq (window-output win) output)
                    (eql (window-workspace win) workspace))
               (float-propose win)))))))

(defun float-render-decisions (output tiled-wins)
  "Render-sequence decisions for floating windows on OUTPUT's current
workspace: place new floats whose size is known (float-place-pending),
position each at its float coordinates, border it, show it
if hidden, and stack the floats above the tiling (focused float on
top). Returns the floats actually rendered, so the render pass can
exclude them from its hide logic."
  ;; First placements may move a float to another output or workspace:
  ;; settle them before choosing what this output renders
  (float-place-pending)
  (let* ((workspace (current-workspace output))
         (floats (remove-if-not
                   (lambda (win)
                     (and (window-floating-p win)
                          (not (window-minimized-p win))
                          (eql (window-workspace win) workspace)))
                   (windows-on-output output)))
         (focused (find-if #'window-focused-p floats)))
    ;; Focused float renders last: it lands on top of the stack.
    (when focused
      (setf floats (append (remove focused floats) (list focused))))
    (let ((below (let ((top (first (last tiled-wins))))
                   (and top (window-node top)))))
      (dolist (win floats)
        (float-clamp-position win)
        (make-window-node win)
        (multiple-value-bind (ax ay)
            (animation-apply win (window-float-x win) (window-float-y win))
          (set-window-position win ax ay))
        (multiple-value-bind (r g b) (theme-border-rgb (window-focused-p win))
          (set-window-borders win '(:top :bottom :left :right)
                              (if (window-focused-p win) 2 1)
                              r g b #xff))
        ;; Show only on transition: river treats every show/hide as a
        ;; state change and starts a new manage/render sequence.
        (when (window-hidden-p win)
          (show-window win))
        ;; Stack above the previous node (or the top tiled node)
        (when (window-node win)
          (when below
            (place-node-above (window-node win) below))
          (setf below (window-node win)))))
    floats))

;;; --- Interactive move/resize (river pointer ops) ---

(defun float-op-request (win seat type &optional edges)
  "A client asked to move/resize WIN interactively (titlebar drag, or
an MOD+drag binding). op-start-pointer is a manage-sequence request,
so record the request here and start the op in the next manage pass.
Floating windows only: dragging the tiling is a no-op."
  (when (and (window-floating-p win) (gethash (window-proxy win) *windows*))
    (setf (seat-op-pending seat)
          (list :win win :type type :edges edges))))

(defun window-at-pointer (seat)
  "The window under the pointer: the focused window if the pointer is
inside it, else the topmost float, else any window containing the
point."
  (let ((px (seat-pointer-x seat)) (py (seat-pointer-y seat)))
    (flet ((inside-p (win)
             (and (gethash (window-proxy win) *windows*)
                  (not (window-minimized-p win))
                  (>= px (window-x win))
                  (< px (+ (window-x win) (window-width win)))
                  (>= py (window-y win))
                  (< py (+ (window-y win) (window-height win))))))
      (let ((focused (find-if #'window-focused-p (all-windows))))
        (or (and focused (inside-p focused) focused)
            (find-if (lambda (w) (and (window-floating-p w) (inside-p w)))
                     (all-windows))
            (find-if #'inside-p (all-windows)))))))

(defun pointer-drag-start (seat type)
  "A MOD+drag began on SEAT: StumpWM behavior — dragging a tiled
window floats it first — then an interactive move/resize op starts
(the river pointer-op protocol; see float-op-manage-pass). Right-drag
resizes from the classic bottom-right corner. Middle-click opens
the window context menu (menus.lisp) instead of an op."
  (if (eq type :menu)
      (menu-open-pointer-trigger seat)
      (let ((win (window-at-pointer seat)))
        (when win
          (dolist (s (all-seats))
            (focus-window win s))
          (unless (window-floating-p win)
            (float-window win))
          (float-op-request win seat type
                            (if (eq type :resize) '(:bottom :right) nil))
          (request-manage-dirty)))))

(defun float-op-manage-pass ()
  "Advance interactive float ops during a manage sequence: start
pending ops, apply op deltas (positions are applied by the render
pass; resizes propose dimensions here), and end released ops.
Cumulative deltas make this idempotent: origin + total motion."
  (dolist (seat (all-seats))
    (let ((pending (seat-op-pending seat)))
      (when pending
        (setf (seat-op-pending seat) nil)
        (let ((win (getf pending :win)))
          (when (and win (gethash (window-proxy win) *windows*))
            (setf (seat-op-state seat)
                  (list :win win
                        :type (getf pending :type)
                        :edges (getf pending :edges)
                        :origin-x (window-float-x win)
                        :origin-y (window-float-y win)
                        :origin-w (or (window-float-width win)
                                      (window-width win) 100)
                        :origin-h (or (window-float-height win)
                                      (window-height win) 100)
                        :dx 0 :dy 0 :released nil))
            (rwm:river-seat-v1.op-start-pointer (seat-proxy seat))
            (format t "~&; Rhizome: float ~A op started~%"
                    (getf pending :type))))))
    (let ((op (seat-op-state seat)))
      (when op
        (let ((win (getf op :win)))
          (cond ((or (null win)
                     (not (gethash (window-proxy win) *windows*)))
                 ;; The op's window vanished: drop the op
                 (setf (seat-op-state seat) nil))
                ((getf op :released)
                 (rwm:river-seat-v1.op-end (seat-proxy seat))
                 (setf (seat-op-state seat) nil))
                (t
                 (when (or (not (zerop (getf op :dx 0)))
                           (not (zerop (getf op :dy 0))))
                   (case (getf op :type)
                     (:move
                      (setf (window-float-x win)
                            (+ (getf op :origin-x) (getf op :dx))
                            (window-float-y win)
                            (+ (getf op :origin-y) (getf op :dy)))
                      (float-clamp-position win)
                      ;; No dimensions change to propose; trigger the
                      ;; manage/render cycle that applies positions
                      (request-manage-dirty))
                     (:resize
                      (float-apply-resize win op)
                      (propose-window-dimensions win
                                                 (window-float-width win)
                                                 (window-float-height win))))))))))))

(add-hook :window-closed #'forget-float-centre)
