;;; Non-hue cues: how each drawn state differs from the state it is
;;; read against, other than by colour.
;;;
;;; Every state rhizome draws is shown by colour plus at least one of
;;; width, shape, glyph, or text weight, so a state still reads in
;;; greyscale or for an operator who cannot tell the two hues apart.
;;; *state-cues* holds the pairs as plain data (keywords, numbers,
;;; and lists only) so tools outside the image can read it too; the
;;; drawing sites take their flags and widths from it.

(in-package #:rhizome)

(defparameter *non-hue-attributes* '(:width :shape :glyph :weight)
  "The attributes that may carry a state besides its colour.")

(defparameter *state-cues*
  '((:workspace-occupied
     :against :workspace-empty
     :elements (:bar-workspace-occupied :bar-workspace-empty)
     :cues ((:weight :bold :normal) (:glyph :dot :none)))
    (:workspace-current
     :against :workspace-occupied
     :elements (:bar-workspace-current :bar-workspace-occupied)
     :cues ((:shape :pill :none)))
    (:launcher-match
     :against :launcher-unmatched
     :elements (:launcher-match :launcher-entry)
     :cues ((:weight :bold :normal) (:shape :underline :none)))
    (:launcher-selected
     :against :launcher-unselected
     :elements (:launcher-selected :launcher-entry)
     :cues ((:shape :pill :none)))
    (:launcher-placeholder
     :against :launcher-query
     :elements (:launcher-placeholder :launcher-query)
     :cues ((:glyph :hint-after-cursor :typed-before-cursor)))
    (:menu-hover
     :against :menu-item
     :elements (:menu-hover-foreground :menu-item)
     :cues ((:shape :pill :none)))
    (:prompt-selected
     :against :prompt-unselected
     :elements (:prompt-selection :prompt-candidate)
     :cues ((:shape :rule :none)))
    (:bar-hovered
     :against :bar-unhovered
     :elements (:bar-hover :bar-background)
     :cues ((:shape :pill :none)))
    (:recording
     :against :not-recording
     :elements (:bar-recording :bar-background)
     :cues ((:glyph :rec :none)))
    (:window-focused
     :against :window-unfocused
     :elements (:border-focused :border-unfocused)
     :cues ((:width 2 1))))
  "Each drawn state against the state it is read against, as
(STATE :against OTHER :elements (ELEMENT OTHER-ELEMENT)
       :cues ((ATTRIBUTE STATE-VALUE OTHER-VALUE) ...)).
ATTRIBUTE is one of *non-hue-attributes*. A pair whose cues all hold
equal values differs in colour alone; hue-only-state-pairs reports
it.")

(defun state-cue (state attribute &key (table *state-cues*))
  "The value of ATTRIBUTE for STATE in TABLE, or NIL when no pair
gives STATE that attribute."
  (dolist (entry table)
    (destructuring-bind (name &key against cues &allow-other-keys) entry
      (let ((cue (assoc attribute cues)))
        (when cue
          (cond ((eq state name) (return (second cue)))
                ((eq state against) (return (third cue)))))))))

(defun state-cue-conflicts (&key (table *state-cues*))
  "Every (STATE ATTRIBUTE VALUES) where TABLE gives STATE more than
one value for ATTRIBUTE. NIL for a consistent table."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (entry table)
      (destructuring-bind (name &key against cues &allow-other-keys) entry
        (dolist (cue cues)
          (pushnew (second cue) (gethash (list name (first cue)) seen)
                   :test #'equal)
          (pushnew (third cue) (gethash (list against (first cue)) seen)
                   :test #'equal))))
    (loop for key being the hash-keys of seen using (hash-value values)
          when (rest values)
            collect (append key (list values)))))

(defun state-cue-flags (state &key (table *state-cues*))
  "The draw flags STATE takes, as a plist for a bar part or a text
draw: :bold, :underline, :dot, and :pill, each T when present."
  (append (when (eq (state-cue state :weight :table table) :bold)
            '(:bold t))
          (when (eq (state-cue state :shape :table table) :underline)
            '(:underline t))
          (when (eq (state-cue state :glyph :table table) :dot)
            '(:dot t))
          (when (eq (state-cue state :shape :table table) :pill)
            '(:pill t))))

(defun border-width (focused-p)
  "The window border width in pixels: the focused width when
FOCUSED-P, the unfocused width otherwise."
  (state-cue (if focused-p :window-focused :window-unfocused) :width))

(defun state-pair-report (&key (table *state-cues*))
  "One plist per pair in TABLE under the active theme:
:state, :against, :colours (the two resolved element colours), and
:non-hue (the non-hue attributes whose values differ)."
  (loop for (name . plist) in table
        collect (destructuring-bind (&key against elements cues) plist
                  (list :state name
                        :against against
                        :colours (mapcar #'theme-color elements)
                        :non-hue (loop for (attribute a b) in cues
                                       when (and (member attribute
                                                         *non-hue-attributes*)
                                                 (not (equal a b)))
                                         collect attribute)))))

(defun hue-only-state-pairs (&key (table *state-cues*))
  "Every (STATE AGAINST) pair in TABLE that differs in no non-hue
attribute, so only its colours tell the two states apart. NIL when
every pair carries a non-hue cue."
  (loop for row in (state-pair-report :table table)
        unless (getf row :non-hue)
          collect (list (getf row :state) (getf row :against))))
