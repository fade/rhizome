;;; rhizome-wallpaper: the wallpaper module.
;;;
;;; Paints a background image on every output with one swaybg process
;;; per output, and picks again on the wallpaper command (alias
;;; backgrounds). Loaded with (rhizome:load-module "rhizome-wallpaper");
;;; nothing is painted unless the module is loaded and
;;; (:wallpaper (:enabled t)) holds, which is the default. With
;;; :enabled nil, the next pick or output change stops the module's
;;; painters.
;;;
;;; Config:
;;;   (:wallpaper (:directories ("~/Pictures/Wallpapers/")
;;;                :selection :random      ; or :sequential
;;;                :per-output t           ; nil: one image everywhere
;;;                :mode :fill             ; :fit :stretch :center :tile
;;;                :enabled t))
;;;
;;; A directory may start with "~/", "$HOME/", or "${HOME}/"; a
;;; trailing "*" adds every subdirectory below it.
;;;
;;; Only processes this module started are ever stopped. A re-pick
;;; starts the new painter first and stops the old one
;;; *wallpaper-handover-delay* seconds later. Another painter, such as
;;; variety's swaybg, is left alone.

(in-package #:rhizome)

(defvar *wallpaper-painters* (make-hash-table :test #'equal)
  "Connector name -> plist (:process HANDLE :pid PID :file FILE) for
the swaybg process this module started for that output.")

(defvar *wallpaper-cursor* 0
  "Index of the next image for :sequential selection.")

(defvar *wallpaper-random-state* (make-random-state t)
  "Random state for :random selection.")

(defparameter *wallpaper-program* "swaybg"
  "The painter program.")

(defparameter *wallpaper-image-types* '("jpg" "jpeg" "png" "webp" "bmp")
  "Image file types, matched without regard to case.")

(defparameter *wallpaper-modes*
  '((:fill . "fill") (:fit . "fit") (:stretch . "stretch")
    (:center . "center") (:centre . "center") (:tile . "tile"))
  "Mode keyword -> swaybg -m argument.")

(defparameter *wallpaper-handover-delay* 1
  "Seconds between starting a new painter and stopping the one it
replaces. 0 stops the old painter at once, on the calling thread.")

(defun wallpaper-launch-process (argv)
  "Start ARGV without waiting and return its process-info."
  (launch-user-program argv :output nil :error-output nil))

(defun wallpaper-kill-process (process)
  "Terminate PROCESS and reap it. Errors are ignored: the process may
already have exited."
  (ignore-errors (uiop:terminate-process process))
  (ignore-errors (uiop:wait-process process)))

(defvar *wallpaper-launcher* 'wallpaper-launch-process
  "Function of an argv list that starts a painter and returns a
process handle.")

(defvar *wallpaper-killer* 'wallpaper-kill-process
  "Function of a process handle from *wallpaper-launcher* that stops
it.")

;;; --- Configuration ---

(defun wallpaper-setting (key default)
  "The (:wallpaper KEY) setting, or DEFAULT."
  (config-getf '(:wallpaper) key :default default))

(defun wallpaper-enabled-p ()
  "True when (:wallpaper :enabled) is true; the default is T."
  (and (wallpaper-setting :enabled t) t))

(defun wallpaper-home ()
  "The home directory, as a namestring with a trailing slash."
  (namestring
   (uiop:ensure-directory-pathname
    (or (uiop:getenv "HOME") (user-homedir-pathname)))))

;;; --- Finding images ---

(defun wallpaper-expand-spec (spec &key (home (wallpaper-home)))
  "Return the directory SPEC names, as a pathname, and a second value
true when SPEC ends in \"*\", which asks for every subdirectory too. A
leading \"~/\", \"$HOME/\", or \"${HOME}/\" stands for HOME."
  (let* ((text (string-trim " " spec))
         (recursive (and (plusp (length text))
                         (char= (char text (1- (length text))) #\*)))
         (text (if recursive (subseq text 0 (1- (length text))) text))
         (home (namestring (uiop:ensure-directory-pathname home))))
    (if (member text '("~" "$HOME" "${HOME}") :test #'string=)
        (setf text home)
        (loop for prefix in '("~/" "$HOME/" "${HOME}/")
              when (and (>= (length text) (length prefix))
                        (string= prefix text :end2 (length prefix)))
                do (setf text (concatenate 'string home
                                           (subseq text (length prefix))))
                   (return)))
    (values (uiop:ensure-directory-pathname
             (uiop:parse-native-namestring text))
            recursive)))

(defun wallpaper-spec-directories (spec &key (home (wallpaper-home)))
  "The existing directories SPEC names: its directory, and with a
trailing \"*\" every directory below it."
  (multiple-value-bind (dir recursive) (wallpaper-expand-spec spec :home home)
    (cond ((not (uiop:directory-exists-p dir)) '())
          (recursive
           (let ((found '()))
             (uiop:collect-sub*directories dir t t
                                           (lambda (d) (push d found)))
             (nreverse found)))
          (t (list dir)))))

(defun wallpaper-image-p (file)
  "True when FILE's type is one of *wallpaper-image-types*."
  (let ((type (pathname-type file)))
    (and (stringp type)
         (member type *wallpaper-image-types* :test #'string-equal)
         t)))

(defun wallpaper-find-images (specs &key (home (wallpaper-home)))
  "The image files in the directories SPECS name, as native
namestrings, sorted and without duplicates."
  (let ((files '()))
    (dolist (spec specs)
      (dolist (dir (wallpaper-spec-directories spec :home home))
        (dolist (file (uiop:directory-files dir))
          (when (wallpaper-image-p file)
            (push (uiop:native-namestring file) files)))))
    (sort (remove-duplicates files :test #'string=) #'string<)))

;;; --- Choosing images ---

(defun wallpaper-shuffle (list)
  "A shuffled copy of LIST."
  (let ((vector (coerce list 'vector)))
    (loop for i from (1- (length vector)) downto 1
          do (rotatef (aref vector i)
                      (aref vector (random (1+ i) *wallpaper-random-state*))))
    (coerce vector 'list)))

(defun wallpaper-assign (names images &key (selection :random) (per-output t))
  "Return an alist (NAME . FILE) giving each output in NAMES an image
from IMAGES. PER-OUTPUT true gives each output its own image, distinct
while IMAGES has enough; NIL gives every output the same one.
SELECTION :random picks at random; :sequential takes IMAGES in order
from *wallpaper-cursor*, wrapping, and advances the cursor. NIL when
NAMES or IMAGES is empty."
  (when (and names images)
    (let* ((count (length images))
           (wanted (if per-output (length names) 1))
           (picks
             (if (eq selection :sequential)
                 (prog1 (loop for i below wanted
                              collect (nth (mod (+ *wallpaper-cursor* i) count)
                                           images))
                   (setf *wallpaper-cursor*
                         (mod (+ *wallpaper-cursor* wanted) count)))
                 (let ((shuffled (wallpaper-shuffle images)))
                   (loop for i below wanted
                         collect (nth (mod i count) shuffled))))))
      (if per-output
          (mapcar #'cons names picks)
          (mapcar (lambda (name) (cons name (first picks))) names)))))

;;; --- Painting ---

(defun wallpaper-output-names ()
  "Connector names of the enabled heads, sorted."
  (mapcar (lambda (state) (getf state :name))
          (remove-if-not (lambda (state) (getf state :enabled))
                         (display-heads))))

(defun wallpaper-mode-argument (mode)
  "The swaybg -m argument for MODE; \"fill\" for an unknown MODE."
  (or (cdr (assoc mode *wallpaper-modes*)) "fill"))

(defun wallpaper-command-line (name file mode)
  "The painter argv that paints FILE on the output NAME in MODE."
  (list *wallpaper-program* "-o" name "-i" file
        "-m" (wallpaper-mode-argument mode)))

(defun wallpaper-retire (process)
  "Stop PROCESS, a painter this module started, after
*wallpaper-handover-delay* seconds on a separate thread, or at once
when the delay is 0."
  (let ((delay *wallpaper-handover-delay*)
        (killer *wallpaper-killer*))
    (flet ((stop ()
             (handler-case (funcall killer process)
               (error (e)
                 (format t "~&; Rhizome: wallpaper: stopping a painter failed: ~A~%"
                         e)))))
      (if (and (realp delay) (plusp delay))
          (bt:make-thread (lambda () (sleep delay) (stop))
                          :name "rhizome wallpaper handover")
          (stop)))
    process))

(defun wallpaper-paint-output (name file mode)
  "Start a painter for FILE on the output NAME, then retire the
painter this module started there before. Return FILE, or NIL when
the painter fails to start, which leaves the old painter running."
  (handler-case
      (let* ((process (funcall *wallpaper-launcher*
                               (wallpaper-command-line name file mode)))
             (old (gethash name *wallpaper-painters*)))
        (setf (gethash name *wallpaper-painters*)
              (list :process process
                    :pid (ignore-errors (uiop:process-info-pid process))
                    :file file))
        (format t "~&; Rhizome: wallpaper ~A: ~A~%" name file)
        (when old
          (wallpaper-retire (getf old :process)))
        file)
    (error (e)
      (format t "~&; Rhizome: wallpaper ~A: painter failed to start: ~A~%"
              name e)
      nil)))

(defun wallpaper-forget-output (name)
  "Retire the painter this module started for the output NAME."
  (let ((old (gethash name *wallpaper-painters*)))
    (remhash name *wallpaper-painters*)
    (when old
      (wallpaper-retire (getf old :process)))))

(defun wallpaper-stop ()
  "Retire every painter this module started."
  (dolist (name (loop for name being the hash-keys of *wallpaper-painters*
                      collect name))
    (wallpaper-forget-output name)))

(defun wallpaper-repaint (&optional (names (wallpaper-output-names)))
  "Pick images for the outputs NAMES and paint them. Return the
alist (NAME . FILE) painted, or :disabled, :no-outputs, or :no-images.
When disabled, retire every painter this module started and return
:disabled."
  (cond ((not (wallpaper-enabled-p)) (wallpaper-stop) :disabled)
        ((null names) :no-outputs)
        (t
         (let* ((specs (wallpaper-setting :directories
                                          '("~/Pictures/Wallpapers/")))
                (images (wallpaper-find-images specs)))
           (if (null images)
               (progn
                 (format t "~&; Rhizome: wallpaper: no images in ~{~A~^, ~}~%"
                         specs)
                 :no-images)
               (let ((mode (wallpaper-setting :mode :fill)))
                 (loop for (name . file)
                         in (wallpaper-assign
                             names images
                             :selection (wallpaper-setting :selection :random)
                             :per-output (wallpaper-setting :per-output t))
                       when (wallpaper-paint-output name file mode)
                         collect (cons name file))))))))

(defun wallpaper-sync ()
  "Match the painters to the enabled outputs: retire the painter of an
output that is gone and paint each output that has none. With
per-output NIL, a new output gets the image the others show. When
disabled, retire every painter this module started. Runs on the
:outputs-changed hook."
  (if (not (wallpaper-enabled-p))
      (progn (wallpaper-stop) nil)
      (let ((current (wallpaper-output-names)))
        (dolist (name (loop for name being the hash-keys of *wallpaper-painters*
                            collect name))
          (unless (member name current :test #'string=)
            (wallpaper-forget-output name)))
        (let ((new (remove-if (lambda (name) (gethash name *wallpaper-painters*))
                              current))
              (shared (and (not (wallpaper-setting :per-output t))
                           (loop for painter being the hash-values
                                   of *wallpaper-painters*
                                 return (getf painter :file)))))
          (cond ((null new) nil)
                (shared
                 (let ((mode (wallpaper-setting :mode :fill)))
                   (loop for name in new
                         when (wallpaper-paint-output name shared mode)
                           collect (cons name shared))))
                (t (wallpaper-repaint new)))))))

(define-command :wallpaper (output args)
  "Pick and paint a new wallpaper on every output. When disabled, stop
the painters this module started."
  (declare (ignore output args))
  (let ((result (wallpaper-repaint)))
    (case result
      (:disabled "wallpaper: disabled")
      (:no-outputs "wallpaper: no outputs")
      (:no-images "wallpaper: no images found")
      (t (format nil "wallpaper: ~D output~:P painted" (length result))))))

(add-hook :outputs-changed 'wallpaper-sync)

;; Paint now when outputs are already known (a live load); at startup
;; the hook paints once the output manager names the heads.
(wallpaper-sync)
