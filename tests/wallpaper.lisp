;;; Tests for the wallpaper module: directory specs ("~/", "$HOME/",
;;; and a trailing "*" for subdirectories) against a temporary tree,
;;; random and sequential selection, per-output and shared images, the
;;; swaybg command line, and the painter lifecycle (start the new
;;; painter, then stop only the one the module started for that
;;; output; stop a removed output's painter; launch nothing without
;;; images). The process launcher and killer are stubbed: no swaybg
;;; runs.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; The wallpaper painter is a MODULE: load it from the repo's modules/
;; directory, the same way a user loads it
(let ((*module-directory*
        (make-pathname
          :directory (append (pathname-directory
                               (asdf:system-source-directory :rhizome))
                              '("modules")))))
  (load-module "rhizome-wallpaper"))

;; Bar off: no call made while this suite runs may spawn a bar process.
(configure '(:bar (:enabled nil)))

(defparameter *wallpaper-test-files*
  '("pics/a.jpg" "pics/b.PNG" "pics/notes.txt"
    "city/top.jpeg" "city/night/n1.webp" "city/day/d1.bmp"
    "city/day/deep/x.JPG" "empty/readme.md")
  "The fixture tree, relative to the temporary home directory.")

(defun run-wallpaper-tests ()
  "Run the checks; return the pass and fail counts."
  (let* ((pass 0) (fail 0)
         (home (format nil "/tmp/rhizome-test-wallpaper-~A-~A/"
                       (get-universal-time) (random 100000)))
         (events '())
         (counter 0)
         (ever-launched '())
         (outputs '()))
    (labels ((ok (name actual expected)
               (if (equal actual expected)
                   (progn (incf pass)
                          (format t "~&PASS: ~A~%" name))
                   (progn (incf fail)
                          (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                  name actual expected))))
             (path (relative) (concatenate 'string home relative))
             (paths (&rest relatives) (mapcar #'path relatives))
             (call (name &rest args)
               (apply (symbol-function (find-symbol name '#:rhizome)) args))
             (launches () (remove :kill (reverse events) :key #'first))
             (kills () (remove :launch (reverse events) :key #'first))
             (reset () (setf events '()))
             (launched-output (event) (third (second event))))
      (macrolet ((check (name actual expected)
                   `(handler-case (ok ,name ,actual ,expected)
                      (error (e)
                        (incf fail)
                        (format t "~&FAIL: ~A (error ~A)~%" ,name e)))))
        (unwind-protect
             (progn
               ;; --- fixture tree ---
               (dolist (relative *wallpaper-test-files*)
                 (let ((file (path relative)))
                   (ensure-directories-exist file)
                   (with-open-file (s file :direction :output
                                           :if-exists :supersede)
                     (write-string "x" s))))
               (ensure-directories-exist (path "nothing/"))

               ;; --- directory specs ---
               (check "~/ names the home directory"
                      (namestring (call "WALLPAPER-EXPAND-SPEC" "~/pics/"
                                        :home home))
                      (path "pics/"))
               (check "$HOME/ names the home directory, slash optional"
                      (namestring (call "WALLPAPER-EXPAND-SPEC" "$HOME/pics"
                                        :home home))
                      (path "pics/"))
               (check "a trailing * asks for subdirectories"
                      (multiple-value-list
                       (call "WALLPAPER-EXPAND-SPEC" "~/city/*" :home home))
                      (list (pathname (path "city/")) t))
               (check "no trailing * means this directory only"
                      (nth-value 1 (call "WALLPAPER-EXPAND-SPEC" "~/city/"
                                         :home home))
                      nil)
               (check "image files only, extensions in any case"
                      (call "WALLPAPER-FIND-IMAGES" '("~/pics/") :home home)
                      (paths "pics/a.jpg" "pics/b.PNG"))
               (check "$HOME/city/* takes images from every subdirectory"
                      (call "WALLPAPER-FIND-IMAGES" '("$HOME/city/*")
                            :home home)
                      (paths "city/day/d1.bmp" "city/day/deep/x.JPG"
                             "city/night/n1.webp" "city/top.jpeg"))
               (check "~/city/ without * skips its subdirectories"
                      (call "WALLPAPER-FIND-IMAGES" '("~/city/") :home home)
                      (paths "city/top.jpeg"))
               (check "an absolute directory works as written"
                      (call "WALLPAPER-FIND-IMAGES" (list (path "pics/"))
                            :home home)
                      (paths "pics/a.jpg" "pics/b.PNG"))
               (check "several specs combine, without duplicates"
                      (call "WALLPAPER-FIND-IMAGES"
                            '("~/pics/" "$HOME/pics" "~/city/") :home home)
                      (paths "city/top.jpeg" "pics/a.jpg" "pics/b.PNG"))
               ;; A directory of tens of thousands of images, named by two
               ;; specs, scans within a few seconds: the scan runs on the
               ;; compositor thread.
               (let ((count 20000))
                 (dotimes (i count)
                   (let ((file (path (format nil "many/~6,'0D.jpg" i))))
                     (ensure-directories-exist file)
                     (with-open-file (s file :direction :output
                                             :if-exists :supersede)
                       (declare (ignore s)))))
                 (let* ((start (get-internal-real-time))
                        (found (call "WALLPAPER-FIND-IMAGES"
                                     '("~/many/" "$HOME/many") :home home))
                        (seconds (/ (- (get-internal-real-time) start)
                                    internal-time-units-per-second)))
                   (check "a large directory: every image once, sorted"
                          (and (= count (length found))
                               (loop for (a b) on found
                                     always (or (null b) (string< a b))))
                          t)
                   (check "a large directory scans within 5 seconds"
                          (< seconds 5)
                          t)))
               (check "a missing directory gives no images"
                      (call "WALLPAPER-FIND-IMAGES" '("~/no-such-dir/")
                            :home home)
                      nil)

               ;; --- selection ---
               (let ((images '("1" "2" "3" "4" "5"))
                     (names '("DP-1" "DP-2" "HDMI-A-1")))
                 (check "random: each output its own image, every time"
                        (loop repeat 25
                              always (let ((picks (mapcar #'cdr
                                                          (call "WALLPAPER-ASSIGN"
                                                                names images
                                                                :selection :random
                                                                :per-output t))))
                                       (and (= 3 (length picks))
                                            (= 3 (length (remove-duplicates
                                                          picks :test #'string=)))
                                            (subsetp picks images
                                                     :test #'string=))))
                        t)
                 (check "random with per-output nil: one image everywhere"
                        (loop repeat 25
                              always (= 1 (length
                                           (remove-duplicates
                                            (mapcar #'cdr
                                                    (call "WALLPAPER-ASSIGN"
                                                          names images
                                                          :selection :random
                                                          :per-output nil))
                                            :test #'string=))))
                        t)
                 (check "random: fewer images than outputs still paints all"
                        (length (remove nil (mapcar #'cdr
                                                    (call "WALLPAPER-ASSIGN"
                                                          names '("1")
                                                          :selection :random
                                                          :per-output t))))
                        3))
               (setf (symbol-value (find-symbol "*WALLPAPER-CURSOR*" '#:rhizome)) 0)
               (flet ((seq (per-output)
                        (call "WALLPAPER-ASSIGN" '("A" "B") '("1" "2" "3")
                              :selection :sequential :per-output per-output)))
                 (check "sequential: outputs take images in order"
                        (seq t) '(("A" . "1") ("B" . "2")))
                 (check "sequential: the next pick continues and wraps"
                        (seq t) '(("A" . "3") ("B" . "1")))
                 (check "sequential: and continues again"
                        (seq t) '(("A" . "2") ("B" . "3")))
                 (setf (symbol-value (find-symbol "*WALLPAPER-CURSOR*" '#:rhizome)) 0)
                 (check "sequential, shared image: 1, 2, 3, then 1"
                        (list (seq nil) (seq nil) (seq nil) (seq nil))
                        '((("A" . "1") ("B" . "1")) (("A" . "2") ("B" . "2"))
                          (("A" . "3") ("B" . "3")) (("A" . "1") ("B" . "1")))))

               ;; --- the painter command line ---
               (check "painter command line, fill"
                      (call "WALLPAPER-COMMAND-LINE" "DP-1" "/w/a.jpg" :fill)
                      '("swaybg" "-o" "DP-1" "-i" "/w/a.jpg" "-m" "fill"))
               (check "painter modes map to swaybg's"
                      (mapcar (lambda (mode)
                                (car (last (call "WALLPAPER-COMMAND-LINE"
                                                 "DP-1" "/w/a.jpg" mode))))
                              '(:fit :stretch :center :centre :tile))
                      '("fit" "stretch" "center" "center" "tile"))
               (check "an unknown mode paints as fill"
                      (car (last (call "WALLPAPER-COMMAND-LINE"
                                       "DP-1" "/w/a.jpg" :bogus)))
                      "fill")

               ;; --- painter lifecycle, launcher and killer stubbed ---
               (setf (symbol-value (find-symbol "*WALLPAPER-LAUNCHER*" '#:rhizome))
                     (lambda (argv)
                       (let ((handle (list :process (incf counter))))
                         (push handle ever-launched)
                         (push (list :launch argv handle) events)
                         handle))
                     (symbol-value (find-symbol "*WALLPAPER-KILLER*" '#:rhizome))
                     (lambda (handle) (push (list :kill handle) events))
                     (symbol-value (find-symbol "*WALLPAPER-HANDOVER-DELAY*" '#:rhizome))
                     0
                     (symbol-function (find-symbol "WALLPAPER-OUTPUT-NAMES" '#:rhizome))
                     (lambda () outputs))
               (setf (symbol-value (find-symbol "*WALLPAPER-CURSOR*" '#:rhizome)) 0)
               (configure (list :wallpaper
                                (list :directories (list (path "pics/")
                                                         (path "city/"))
                                      :selection :sequential
                                      :per-output t
                                      :mode :fit
                                      :enabled t)))
               (check "the module registers its output hook"
                      (and (member (find-symbol "WALLPAPER-SYNC" '#:rhizome)
                                   (gethash :outputs-changed *hooks*))
                           t)
                      t)
               (check "the backgrounds alias runs wallpaper"
                      (eq (find-command :backgrounds)
                          (gethash :wallpaper *command-table*))
                      t)

               (setf outputs '("DP-1" "HDMI-A-1"))
               (reset)
               (let ((log (with-output-to-string (*standard-output*)
                            (run-hook :outputs-changed))))
                 (check "outputs appearing: one painter each, nothing stopped"
                        (list (length (launches)) (kills))
                        '(2 nil))
                 (check "each painter paints its own output"
                        (mapcar #'launched-output (launches))
                        '("DP-1" "HDMI-A-1"))
                 (check "the painter argv carries output, file, and mode"
                        (second (first (launches)))
                        (list "swaybg" "-o" "DP-1" "-i" (path "city/top.jpeg")
                              "-m" "fit"))
                 (check "one log line per paint, naming output and file"
                        (list (count #\Newline log)
                              (and (search "DP-1" log) t)
                              (and (search (path "pics/a.jpg") log) t))
                        '(2 t t)))

               (let ((first-handles (mapcar #'third (launches))))
                 (reset)
                 (let ((reply (execute-command nil :wallpaper nil)))
                   (check "wallpaper replies with a string"
                          (stringp reply) t))
                 (check "re-pick: start the new painter, then stop the old"
                        (mapcar (lambda (e)
                                  (if (eq (first e) :launch)
                                      (list :launch (launched-output e))
                                      (list :kill (second e))))
                                (reverse events))
                        (list '(:launch "DP-1") (list :kill (first first-handles))
                              '(:launch "HDMI-A-1")
                              (list :kill (second first-handles))))
                 (check "re-pick: the sequence moves on"
                        (mapcar (lambda (e) (fifth (second e))) (launches))
                        (paths "pics/b.PNG" "city/top.jpeg")))

               (let ((launched (mapcar #'third (launches))))
                 (reset)
                 (run-hook :outputs-changed)
                 (check "unchanged outputs: nothing launched or stopped"
                        events nil)
                 (setf outputs '("DP-1"))
                 (run-hook :outputs-changed)
                 (check "a removed output's painter is stopped, nothing else"
                        events (list (list :kill (second launched))))
                 (reset)
                 (setf outputs '("DP-1" "DP-2"))
                 (run-hook :outputs-changed)
                 (check "a new output gets its own painter only"
                        (list (mapcar #'launched-output (launches)) (kills))
                        '(("DP-2") nil)))

               ;; Only handles this module launched are ever stopped
               (check "every stopped process is one the module started"
                      (let ((all-killed '()))
                        (reset)
                        (execute-command nil :wallpaper nil)
                        (execute-command nil :wallpaper nil)
                        (dolist (e events)
                          (when (eq (first e) :kill)
                            (push (second e) all-killed)))
                        (and all-killed
                             (subsetp all-killed ever-launched :test #'equal)
                             (= (length all-killed)
                                (length (remove-duplicates all-killed
                                                           :test #'equal)))))
                      t)

               ;; A shared image reaches a newly added output too
               (configure '(:wallpaper (:per-output nil :selection :random)))
               (reset)
               (execute-command nil :wallpaper nil)
               (let ((shared (fifth (second (first (launches))))))
                 (reset)
                 (setf outputs '("DP-1" "DP-2" "DP-3"))
                 (run-hook :outputs-changed)
                 (check "per-output nil: a new output gets the shared image"
                        (mapcar (lambda (e) (list (launched-output e)
                                                  (fifth (second e))))
                                (launches))
                        (list (list "DP-3" shared))))

               ;; A launcher failure keeps the painter already running
               (let ((saved (symbol-value (find-symbol "*WALLPAPER-LAUNCHER*"
                                                       '#:rhizome))))
                 (setf (symbol-value (find-symbol "*WALLPAPER-LAUNCHER*" '#:rhizome))
                       (lambda (argv)
                         (declare (ignore argv))
                         (error "no such program")))
                 (reset)
                 (with-output-to-string (*standard-output*)
                   (execute-command nil :wallpaper nil))
                 (check "a failed launch stops no painter" events nil)
                 (setf (symbol-value (find-symbol "*WALLPAPER-LAUNCHER*" '#:rhizome))
                       saved))

               ;; No images: one log line, nothing launched
               (configure (list :wallpaper
                                (list :directories (list (path "nothing/")
                                                         (path "empty/")))))
               (reset)
               (let* ((reply nil)
                      (log (with-output-to-string (*standard-output*)
                             (setf reply (execute-command nil :wallpaper nil)))))
                 (check "no images: nothing launched or stopped" events nil)
                 (check "no images: the reply says so"
                        reply "wallpaper: no images found")
                 (check "no images: one log line"
                        (count #\Newline log) 1))

               ;; Disabled: nothing painted, the module's painters stop
               (configure (list :wallpaper
                                (list :directories (list (path "pics/"))
                                      :enabled nil)))
               (reset)
               (check "disabled: the command paints nothing and stops the module's painters"
                      (list (execute-command nil :wallpaper nil)
                            (launches)
                            (length (kills)))
                      '("wallpaper: disabled" nil 3))
               (reset)
               (setf outputs '("DP-1" "DP-2" "DP-3" "DP-4"))
               (run-hook :outputs-changed)
               (check "disabled: a new output gets no painter" events nil)

               ;; The output manager's done event runs the hook
               (let* ((ran nil)
                     (handler (lambda () (setf ran t))))
                 (setf (symbol-function 'display-layout-on-done)
                       (lambda () nil))
                 (add-hook :outputs-changed handler)
                 (unwind-protect
                      (progn
                        (display-manager-event :done 7)
                        (check "a done event runs :outputs-changed" ran t))
                   (remove-hook :outputs-changed handler))))
          (ignore-errors (uiop:delete-directory-tree
                          (pathname home) :validate t)))))
    (values pass fail)))

(multiple-value-bind (pass fail) (run-wallpaper-tests)
  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
