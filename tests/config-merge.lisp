;;; Tests for config merging (MERGE-PLIST, CONFIGURE): a group named in
;;; an override keeps the keys the override leaves out, whatever its
;;; values are, while module lists, placement preference lists and
;;; window rules are replaced wholesale. Pure config operations; the
;;; bar is disabled before CONFIGURE runs.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (keys (plist &rest names)
           (mapcar (lambda (name) (getf plist name :missing)) names)))

    ;; --- groups merge whatever their values are ---

    (let ((merged (merge-plist '(:pointer (:warp-on-switch nil
                                           :xcursor-size 32))
                               '(:pointer (:focus-policy :sloppy)))))
      (ok "a keyword value in a group keeps its sibling keys"
          (keys (getf merged :pointer)
                :warp-on-switch :xcursor-size :focus-policy)
          '(nil 32 :sloppy)))

    (let ((merged (merge-plist '(:frames (:prune-empty :auto
                                          :split-mode :even))
                               '(:frames (:prune-empty :never)))))
      (ok "a group holding only keyword values merges key by key"
          (keys (getf merged :frames) :prune-empty :split-mode)
          '(:never :even)))

    (let ((merged (merge-plist
                   '(:tiling (:auto-split t
                              :placement (:policy :stumpwm
                                          :new-window (:focused))))
                   '(:tiling (:placement (:new-window (:empty :focused)))))))
      (ok "a nested group with keyword values keeps its sibling keys"
          (list (getf (getf merged :tiling) :auto-split)
                (keys (getf (getf merged :tiling) :placement)
                      :policy :new-window))
          '(t (:stumpwm (:empty :focused)))))

    ;; --- lists that are values replace wholesale ---

    (let ((merged (merge-plist (default-config)
                               '(:bar (:modules (:right (:layout :windows)))))))
      (ok "a bar module list replaces wholesale"
          (keys (getf (getf merged :bar) :modules) :left :right)
          '((:workspaces :layout :windows) (:layout :windows))))

    (let ((merged (merge-plist
                   '(:tiling (:placement (:new-window (:empty :focused))))
                   '(:tiling (:placement (:new-window (:last :unfocused)))))))
      (ok "a placement preference list replaces wholesale"
          (getf (getf (getf merged :tiling) :placement) :new-window)
          '(:last :unfocused)))

    (let ((merged (merge-plist '(:window-rules ((:match "a" :float t)))
                               '(:window-rules ((:match "b" :workspace 2))))))
      (ok "window rules replace wholesale"
          (getf merged :window-rules)
          '((:match "b" :workspace 2))))

    (let* ((base (list :gaps (list :inner 6)))
           (merged (merge-plist base '(:gaps (:inner 0)))))
      (ok "a scalar replaces and the base is left unchanged"
          (list (getf (getf merged :gaps) :inner)
                (getf (getf base :gaps) :inner))
          '(0 6)))

    ;; --- configure ---

    (let ((*config* (default-config)))
      (configure '(:pointer (:focus-policy :sloppy)))
      (ok "configure of :pointer keeps :warp-on-switch"
          (keys (getf *config* :pointer) :warp-on-switch :focus-policy)
          '(t :sloppy))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
