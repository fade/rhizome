;;; Generalized input prompt for Rhizome
;;;
;;; An interactive layer-shell input bar with filtering and
;;; selection — the machinery behind windowlist, workspace selection
;;; and the command palette. Type to filter, Up/Down (or Tab) to
;;; select, Enter to complete, Escape to cancel.
;;;
;;;   (prompt-open "Window:" :candidates '("a" "b")
;;;                :on-complete (lambda (choice) ...))

(in-package #:rhizome)

(defparameter *prompt-active* nil)
(defparameter *prompt-title* "")
(defparameter *prompt-query* "")
(defparameter *prompt-candidates* nil
  "Full candidate list (strings).")
(defparameter *prompt-selection* 0
  "Index into the filtered candidates.")
(defparameter *prompt-on-complete* nil
  "Function called with the selected string on Enter.")

(defparameter *prompt-complete-line* nil
  "When true, the prompt is a complete-line prompt: it filters on the
first word of the query, and Enter may complete with the typed line
(see prompt-choice).")

(defparameter *prompt-exact-names* nil
  "Names that a complete-line prompt treats as exact matches, as it
does its candidates, without listing them as candidates.")

(defparameter *prompt-state* nil
  "Surface state hash: surface, layer-surface, shm-*, wl-buffer,
width, height.")

(defparameter *prompt-width* 700)

(defparameter *prompt-height* 200
  "Input line plus visible candidate rows; sized on open by
prompt-surface-height.")

(defparameter *prompt-padding* 6
  "Pixels above the input line and below the last candidate row.")

(defparameter *prompt-max-rows* 10
  "Most candidate rows a prompt shows at once; the rest scroll.")

;;; --- Filtering ---

(defparameter *prompt-expiry* 0
  "Universal time after which an unattended prompt auto-closes.")

(defparameter *prompt-idle-seconds* 30
  "Seconds without a key after which an open prompt closes.")

(defun prompt-extend-deadline ()
  "Push the open prompt's auto-close *prompt-idle-seconds* past now."
  (setf *prompt-expiry* (+ (get-universal-time) *prompt-idle-seconds*)))

(defun prompt-drain ()
  "Close a prompt that has had no key for *prompt-idle-seconds*, so a
keyboard-exclusive surface the keys no longer reach cannot trap input.
Called from the main loop."
  (when (and *prompt-active*
             (plusp *prompt-expiry*)
             (>= (get-universal-time) *prompt-expiry*))
    (format t "~&; Rhizome: prompt timed out~%")
    (prompt-close)))

(defun prompt-query-words ()
  "The query trimmed of spaces, split at its first space. Return two
values: the first word, and the rest of the line or NIL when there is
none."
  (let* ((line (string-trim " " *prompt-query*))
         (space (position #\Space line)))
    (if space
        (values (subseq line 0 space)
                (string-left-trim " " (subseq line space)))
        (values line nil))))

(defun prompt-filtered ()
  "Candidates matching the current query (fuzzy: characters in
order, case-insensitive; substring matches rank first). A complete-line
prompt matches on the first word of the query only."
  (fuzzy-filter (if *prompt-complete-line*
                    (values (prompt-query-words))
                    *prompt-query*)
                *prompt-candidates*))

(defun prompt-selected ()
  "The currently selected candidate string, or NIL."
  (nth *prompt-selection* (prompt-filtered)))

(defun prompt-choice ()
  "The string Enter completes with, or NIL to complete with nothing.
A prompt without candidates yields the query when it is non-empty.
A candidates prompt yields the selected candidate, unless it was
opened with COMPLETE-LINE. A complete-line prompt yields:
- NIL for a blank query;
- the trimmed query when it has arguments after its first word, or
  when its first word names a candidate or one of
  *prompt-exact-names* exactly, ignoring case;
- otherwise the selected candidate."
  (cond
    ((null *prompt-candidates*)
     (and (plusp (length *prompt-query*)) *prompt-query*))
    ((not *prompt-complete-line*)
     (prompt-selected))
    (t
     (multiple-value-bind (name arguments) (prompt-query-words)
       (cond ((zerop (length name)) nil)
             ((or arguments
                  (member name *prompt-candidates* :test #'string-equal)
                  (member name *prompt-exact-names* :test #'string-equal))
              (string-trim " " *prompt-query*))
             (t (prompt-selected)))))))

;;; --- Key handling ---

(defun prompt-handle-key (keysym)
  "Handle KEYSYM while the prompt is open. Return T.
Enter closes the prompt and calls the completion function with the
string prompt-choice returns; when that is NIL the function is not
called. Every key restarts the idle deadline (prompt-drain)."
  (prompt-extend-deadline)
  (let ((filtered-count (length (prompt-filtered))))
    (cond
      ;; Escape — cancel
      ((= keysym 65307)
       (prompt-close))
      ;; Enter: complete with prompt-choice (typed line, selection, or nothing)
      ((= keysym 65293)
       ;; Capture the callback before closing: prompt-close clears it
       (let* ((choice (prompt-choice))
              (on-complete *prompt-on-complete*))
         (prompt-close)
         (when (and choice on-complete)
           (handler-case
               (funcall on-complete choice)
             (error (e)
               (format t "~&; Rhizome: prompt complete error: ~A~%" e))))))
      ;; Up / Down / Tab — cycle selection
      ((= keysym 65362)
       (when (plusp *prompt-selection*)
         (decf *prompt-selection*))
       (prompt-redraw))
      ((= keysym 65364)
       (when (< *prompt-selection* (1- filtered-count))
         (incf *prompt-selection*))
       (prompt-redraw))
      ((= keysym 65289)                    ; Tab: next, wrap around
       (setf *prompt-selection*
             (mod (1+ *prompt-selection*) (max 1 filtered-count)))
       (prompt-redraw))
      ;; Backspace
      ((= keysym 65288)
       (when (plusp (length *prompt-query*))
         (setf *prompt-query*
               (subseq *prompt-query* 0 (1- (length *prompt-query*)))))
       (setf *prompt-selection* 0)
       (prompt-redraw))
      ;; Printable ASCII — extend the query
      ((and (>= keysym 32) (<= keysym 126))
       (setf *prompt-query*
             (concatenate 'string *prompt-query*
                          (string (code-char keysym))))
       (setf *prompt-selection* 0)
       (prompt-redraw))))
  t)

;;; --- Rendering ---

(defun prompt-visible-rows (height row-height)
  "How many whole candidate rows of ROW-HEIGHT pixels fit below the
input line of a prompt HEIGHT pixels tall, inside its padding; at
least 1."
  (max 1 (1- (floor (- height (* 2 *prompt-padding*)) row-height))))

(defun prompt-row-top (slot row-height)
  "The top pixel row of visible candidate row SLOT, counted from 0
below the input line, for rows ROW-HEIGHT pixels tall."
  (+ *prompt-padding* (* row-height (1+ slot))))

(defun prompt-surface-height (rows row-height)
  "The height of a prompt that shows ROWS candidate rows of ROW-HEIGHT
pixels: padding, the input line, the rows, and padding."
  (+ (* 2 *prompt-padding*) (* row-height (1+ rows))))

(defun prompt-scroll-offset (selection visible)
  "The first candidate row to draw so that row SELECTION shows when
VISIBLE rows fit: 0 until the selection passes the last visible row,
then enough to keep it on the bottom row."
  (max 0 (- selection (1- visible))))

(defun prompt-paint (ptr width height)
  "Draw the prompt into PTR, a premultiplied ARGB buffer WIDTH by
HEIGHT pixels. The background is opaque whatever the bar's opacity.
The input line holds the title and the query. Below it, the filtered
candidates scroll so the selected one shows, on a highlight the full
height of its row."
  (font-ensure-initialized)
  (let* ((row-height (font-line-height))
         (visible (prompt-visible-rows height row-height))
         (offset (prompt-scroll-offset *prompt-selection* visible))
         (text-x 10))
    (multiple-value-bind (r g b) (theme-rgb :prompt-background)
      (multiple-value-bind (br bg bb) (theme-rgb :prompt-border)
        (paint-rounded-surface ptr width height r g b
                               :radius (config-getf '(:bar) :corner-radius
                                                    :default 8)
                               :border-r br :border-g bg :border-b bb)))
    (flet ((text (string x y element)
             (multiple-value-bind (r g b) (theme-rgb element)
               (font-draw-text ptr width height string x y r g b))))
      (text *prompt-title* text-x *prompt-padding* :prompt-title)
      (text *prompt-query* (+ text-x (font-text-width *prompt-title*)
                              (font-text-width " "))
            *prompt-padding* :prompt-query)
      (loop for candidate in (nthcdr offset (prompt-filtered))
            for slot below visible
            for top = (prompt-row-top slot row-height)
            for selected = (= (+ offset slot) *prompt-selection*)
            do (when selected
                 (multiple-value-bind (r g b) (theme-rgb :prompt-selection)
                   (fill-rounded-rect ptr width height 4 top (- width 8)
                                      row-height r g b :radius 4)))
               (text candidate text-x top
                     (if selected :prompt-selected :prompt-candidate))))))

(defun prompt-redraw ()
  "Render the prompt into its shm buffer and commit."
  (let ((state *prompt-state*))
    (when state
      (let ((ptr (gethash :shm-ptr state)))
        (when (and ptr (not (cffi:null-pointer-p ptr)))
          (let ((width (gethash :width state))
                (height (gethash :height state)))
            (prompt-paint ptr width height)
            ;; Attach, damage, commit
            (wl:wl-surface.attach (gethash :surface state)
                                  (gethash :wl-buffer state) 0 0)
            (wl:wl-surface.damage (gethash :surface state) 0 0 width height)
            (wl:wl-surface.commit (gethash :surface state))))))))

(defun prompt-configure-handler (state layer-surface serial width height)
  "Acknowledge the layer surface configure."
  (declare (ignore width height))
  (setf (gethash :width state) *prompt-width*
        (gethash :height state) *prompt-height*)
  (rwm:zwlr-layer-surface-v1.ack-configure layer-surface serial)
  (bar-ensure-shm state)
  (prompt-redraw))

;;; --- Open / close ---

(defun prompt-open (title &key candidates on-complete (initial "")
                              complete-line)
  "Open an interactive prompt titled TITLE, with INITIAL as the query.
CANDIDATES is a list of strings. With candidates, Enter calls
ON-COMPLETE with the selected candidate. Without candidates, the prompt
takes free text and Enter calls ON-COMPLETE with the non-empty query.
COMPLETE-LINE makes a candidates prompt a complete-line prompt:
candidates match the first word, and Enter may complete with the whole typed
line (see prompt-choice). Clear *prompt-exact-names*. The prompt is
tall enough for every candidate row up to *prompt-max-rows*."
  (when *prompt-active*
    (prompt-close))
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Rhizome: cannot show prompt (missing globals)~%")
    (return-from prompt-open))
  ;; Free any buffer the previous state still holds; a no-op when
  ;; its close already freed it
  (when *prompt-state*
    (bar-destroy-shm *prompt-state*))
  (font-ensure-initialized)
  (setf *prompt-height* (prompt-surface-height
                         (min *prompt-max-rows* (length candidates))
                         (font-line-height))
        *prompt-title* title
        *prompt-query* initial
        *prompt-candidates* candidates
        *prompt-complete-line* complete-line
        *prompt-exact-names* nil
        *prompt-selection* 0
        *prompt-on-complete* on-complete
        *prompt-state* (make-hash-table)
        *prompt-expiry* (+ (get-universal-time) *prompt-idle-seconds*))
  (let ((state *prompt-state*))
    ;; The render helpers expect the global wl_shm in the state hash
    (setf (gethash :shm state) *shm*)
    (let ((surface (wl:wl-compositor.create-surface *compositor*)))
      (setf (gethash :surface state) surface)
      (let ((layer-surface
              (rwm:zwlr-layer-shell-v1.get-layer-surface
                *layer-shell* surface (layer-surface-output)
                :top "rhizome-prompt")))
        (setf (gethash :layer-surface state) layer-surface)
        (push (lambda (event-name &rest args)
                (case event-name
                  (:configure
                   (destructuring-bind (serial width height) args
                     (prompt-configure-handler
                       state layer-surface serial width height)))
                  (:closed
                   (bar-destroy-shm state)
                   (setf *prompt-active* nil)
                   (reassert-seat-focus))))
              (wl:wl-proxy-hooks layer-surface))
        (rwm:zwlr-layer-surface-v1.set-anchor layer-surface nil)
        (rwm:zwlr-layer-surface-v1.set-size layer-surface
          *prompt-width* *prompt-height*)
        (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
          layer-surface :exclusive)
        (rwm:zwlr-layer-surface-v1.set-margin layer-surface 60 0 0 0)
        (setf *prompt-active* t)
        (wl:wl-surface.commit surface)))))

(defun prompt-close ()
  "Close the interactive prompt."
  (unless *prompt-active*
    (return-from prompt-close))
  (setf *prompt-active* nil
        *prompt-expiry* 0)
  (let ((state *prompt-state*))
    (handler-case
        (progn
          (when (gethash :layer-surface state)
            (rwm:zwlr-layer-surface-v1.destroy (gethash :layer-surface state)))
          (when (gethash :surface state)
            (wl:wl-surface.destroy (gethash :surface state))))
      (error (e)
        (format t "~&; Rhizome: error closing prompt: ~A~%" e)))
    (bar-destroy-shm state)
    (setf *prompt-state* nil
          *prompt-on-complete* nil))
  (reassert-seat-focus))

(defun rhizome-rounded-clear (ptr width height r g b
                              &key (border :prompt-border))
  "The shared surface treatment for message window and prompts:
rounded corners with a hairline border in the colour of the theme
element BORDER, routed through paint-rounded-surface (surface.lisp)."
  (multiple-value-bind (ar ag ab) (theme-rgb border)
    (paint-rounded-surface ptr width height r g b
                           :alpha (bar-surface-alpha)
                           :radius (config-getf '(:bar) :corner-radius
                                     :default 8)
                           :border-r ar :border-g ag :border-b ab)))
