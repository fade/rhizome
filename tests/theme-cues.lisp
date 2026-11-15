;;; Tests for the non-hue cues: every state rhizome draws differs from
;;; the state it is read against in width, shape, glyph, or text
;;; weight, never in colour alone. Covers the state cue table, the
;;; hue-only check over every theme the search path offers, and the
;;; drawing sites that honour the table: bold and a dot on occupied
;;; workspace numbers, bold and an underline on the launcher match,
;;; and the 2 px focused border against 1 px. River requests are
;;; stubbed; pixels are drawn into a private buffer.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: configures below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(defparameter *cue-fixture-theme-directory*
  (asdf:system-relative-pathname :rhizome "tests/fixtures/themes/"))

(defun call-with-pixel-buffer (width height thunk)
  "Call THUNK with a zeroed WIDTH x HEIGHT ARGB buffer; free it after."
  (let ((ptr (cffi:foreign-alloc :uint32 :count (* width height)
                                         :initial-element 0)))
    (unwind-protect (funcall thunk ptr)
      (cffi:foreign-free ptr))))

(defun lit-pixels (ptr width x0 x1 y0 y1)
  "How many pixels in columns X0..X1-1, rows Y0..Y1-1 are not zero."
  (loop for y from y0 below y1
        sum (loop for x from x0 below x1
                  count (/= 0 (cffi:mem-aref ptr :uint32
                                             (+ x (* y width)))))))

;; Resolve the font before any metric is read, so the rows the
;; tests probe match the face the drawing uses.
(font-ensure-initialized)

(let ((pass 0) (fail 0)
      (saved-config *config*)
      (saved-search-path *theme-search-path*))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (unwind-protect
         (handler-bind ((warning #'muffle-warning))
           (setf *theme-search-path*
                 (list *cue-fixture-theme-directory* *shipped-theme-directory*))
           (clear-theme-cache)

           ;; --- the state cue table ---
           (ok "the non-hue attributes are width, shape, glyph, and weight"
               *non-hue-attributes* '(:width :shape :glyph :weight))
           (ok "every pair names two states, two elements, and its cues"
               (every (lambda (entry)
                        (destructuring-bind (state &key against elements cues)
                            entry
                          (and (keywordp state) (keywordp against)
                               (= 2 (length elements))
                               (every (lambda (e)
                                        (member e (theme-element-names)))
                                      elements)
                               (consp cues))))
                      *state-cues*)
               t)
           (ok "every cue uses a non-hue attribute"
               (every (lambda (entry)
                        (every (lambda (cue)
                                 (member (first cue) *non-hue-attributes*))
                               (getf (rest entry) :cues)))
                      *state-cues*)
               t)
           (ok "no state takes two values for one attribute"
               (state-cue-conflicts) nil)
           (ok "the ruled cues are in the table"
               (list (state-cue :workspace-occupied :weight)
                     (state-cue :workspace-occupied :glyph)
                     (state-cue :workspace-empty :glyph)
                     (state-cue :launcher-match :weight)
                     (state-cue :launcher-match :shape)
                     (state-cue :launcher-unmatched :shape)
                     (state-cue :window-focused :width)
                     (state-cue :window-unfocused :width))
               '(:bold :dot :none :bold :underline :none 2 1))
           (ok "draw flags for an occupied workspace"
               (state-cue-flags :workspace-occupied) '(:bold t :dot t))
           (ok "draw flags for the launcher match"
               (state-cue-flags :launcher-match) '(:bold t :underline t))
           (ok "draw flags for the current workspace"
               (state-cue-flags :workspace-current) '(:pill t))
           (ok "an empty workspace draws plain"
               (state-cue-flags :workspace-empty) nil)

           ;; --- the hue-only check, over every theme on the path ---
           (ok "the themes checked: shipped and fixture"
               (available-theme-names)
               '("deepsky-moonunit" "ef-summer" "kusanagi"
                 "modus-operandi" "modus-vivendi" "test-fixture-light"
                 "tokyo-night" "tokyo-night-emacs"))
           (dolist (name (available-theme-names))
             (set-theme name)
             (ok (format nil "~A: no state pair differs in hue alone" name)
                 (hue-only-state-pairs) nil)
             (ok (format nil "~A: every pair's elements resolve" name)
                 (every (lambda (row)
                          (every #'theme-colour-string-p
                                 (getf row :colours)))
                        (state-pair-report))
                 t))
           (set-theme "tokyo-night")
           (ok "a pair with no non-hue cue is reported"
               (hue-only-state-pairs
                :table '((:a :against :b
                          :elements (:bar-workspace-occupied
                                     :bar-workspace-empty)
                          :cues ((:weight :bold :bold)))
                         (:c :against :d
                          :elements (:launcher-match :launcher-entry)
                          :cues ((:shape :underline :none)))))
               '((:a :b)))
           (ok "the report carries colours and the attributes that differ"
               (let ((row (find :workspace-occupied (state-pair-report)
                                :key (lambda (r) (getf r :state)))))
                 (list (getf row :colours) (getf row :non-hue)))
               '(("#c0caf5" "#565f89") (:weight :glyph)))

           ;; --- bar: occupied workspaces carry bold and the dot ---
           (let ((real-snapshot (symbol-function 'bar-snapshot)))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'bar-snapshot)
                          (lambda () '(:workspace 1 :workspace-count 4
                                       :occupied (1 3))))
                    (let ((parts (bar-module-parts :workspaces)))
                      (ok "colours are unchanged"
                          (mapcar #'second parts)
                          '("#7aa2f7" "#565f89" "#c0caf5" "#565f89"))
                      (ok "occupied numbers are bold"
                          (mapcar (lambda (p)
                                    (getf (bar-part-flags p) :bold))
                                  parts)
                          '(t nil t nil))
                      (ok "occupied numbers carry the dot"
                          (mapcar (lambda (p)
                                    (getf (bar-part-flags p) :dot))
                                  parts)
                          '(t nil t nil))
                      (ok "the current workspace keeps its pill"
                          (mapcar (lambda (p)
                                    (getf (bar-part-flags p) :pill))
                                  parts)
                          '(t nil nil nil))))
               (setf (symbol-function 'bar-snapshot) real-snapshot)))
           (ok "a bold part is one pixel wider"
               (- (bar-part-width '("3" "#ffffff" :bold t))
                  (bar-part-width '("3" "#ffffff")))
               1)
           (let* ((w 40) (h 24) (y 2)
                  (below (+ y (font-ascent) 1)))
             (flet ((draw (part)
                      (let (lit total)
                        (call-with-pixel-buffer
                         w h
                         (lambda (ptr)
                           (bar-draw-module ptr w h 4 y (list part)
                                            255 255 255 nil)
                           (setf lit (lit-pixels ptr w 0 w below
                                                 (min h (+ below 2)))
                                 total (lit-pixels ptr w 0 w 0 h))))
                        (list lit total))))
               (let ((plain (draw '("3" "#ffffff")))
                     (cued (draw '("3" "#ffffff" :bold t :dot t))))
                 (ok "a plain number draws nothing below its baseline"
                     (first plain) 0)
                 (ok "the dot draws below the baseline"
                     (plusp (first cued)) t)
                 (ok "bold draws more of the number"
                     (> (- (second cued) (first cued)) (second plain)) t))))

           ;; --- font: bold and underline ---
           (font-ensure-initialized)
           (let* ((w 80) (h 24) (y 2)
                  (under (+ y (font-ascent) 1)))
             (flet ((draw (&rest keys)
                      (let (row total)
                        (call-with-pixel-buffer
                         w h
                         (lambda (ptr)
                           (apply #'font-draw-text ptr w h "Fire" 2 y
                                  255 255 255 keys)
                           (setf row (lit-pixels ptr w 0 w under (1+ under))
                                 total (lit-pixels ptr w 0 w 0 h))))
                        (list row total))))
               (let ((plain (draw))
                     (bold (draw :bold t))
                     (underline (draw :underline t)))
                 (ok "plain text leaves the underline row empty"
                     (first plain) 0)
                 (ok "bold text draws more pixels"
                     (> (second bold) (second plain)) t)
                 (ok "underline spans the text"
                     (first underline) (font-text-width "Fire")))))
           (ok "bold text measures one pixel wider"
               (- (font-text-width "Fire" :bold t) (font-text-width "Fire"))
               1)
           (ok "empty bold text measures zero"
               (font-text-width "" :bold t) 0)

           ;; --- launcher: the match is bold and underlined ---
           (let* ((w 200) (h 40) (y 4)
                  (under (+ y (font-ascent) 1))
                  (x 10)
                  (*launcher-height* h))
             (let ((*launcher-query* "fi"))
               (call-with-pixel-buffer
                w h
                (lambda (ptr)
                  (launcher-draw-entry-label ptr w "Firefox" x y 200 200 200)
                  (let ((match-end (+ x (font-text-width "Fi" :bold t))))
                    (ok "the match is underlined"
                        (lit-pixels ptr w x match-end under (1+ under))
                        (- match-end x))
                    (ok "the rest of the label is not underlined"
                        (lit-pixels ptr w match-end w under (1+ under))
                        0)))))
             (let ((*launcher-query* ""))
               (call-with-pixel-buffer
                w h
                (lambda (ptr)
                  (launcher-draw-entry-label ptr w "Firefox" x y 200 200 200)
                  (ok "no query, no underline"
                      (lit-pixels ptr w 0 w under (1+ under)) 0)))))

           ;; --- borders: focused 2 px, unfocused 1 px ---
           (ok "border width by focus" (list (border-width t) (border-width nil))
               '(2 1))
           (let ((widths '())
                 (saved (mapcar (lambda (s) (cons s (symbol-function s)))
                                '(set-window-borders make-window-node
                                  set-window-position show-window
                                  place-node-above))))
             (unwind-protect
                  (progn
                    (setf (symbol-function 'set-window-borders)
                          (lambda (win edges width &rest rgba)
                            (declare (ignore edges rgba))
                            (push (cons (window-focused-p win) width) widths)))
                    (dolist (s '(make-window-node set-window-position
                                 show-window place-node-above))
                      (setf (symbol-function s)
                            (lambda (&rest args) (declare (ignore args)) nil)))
                    (let* ((*windows* (make-hash-table))
                           (*output-workspace* (make-hash-table))
                           (*window-workspace* (make-hash-table))
                           (*minimized-windows* (make-hash-table))
                           (out (make-instance 'rhizome-output :proxy :cue-out
                                                               :width 1920
                                                               :height 1080))
                           (a (make-instance 'rhizome-window :proxy :cue-a))
                           (b (make-instance 'rhizome-window :proxy :cue-b)))
                      (dolist (win (list a b))
                        (setf (gethash (window-proxy win) *windows*) win
                              (window-output win) out
                              (window-floating-p win) t
                              (window-float-x win) 100
                              (window-float-y win) 100
                              (window-float-width win) 400
                              (window-float-height win) 300
                              (gethash win *window-workspace*) 1))
                      (setf (window-focused-p a) t)
                      (float-render-decisions out nil)
                      (ok "a focused float draws 2 px, an unfocused one 1 px"
                          (sort (copy-list widths)
                                (lambda (p q) (declare (ignore q)) (car p)))
                          '((t . 2) (nil . 1)))))
               (loop for (s . f) in saved
                     do (setf (symbol-function s) f)))))
      (setf *config* saved-config
            *theme-search-path* saved-search-path)
      (clear-theme-cache)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
