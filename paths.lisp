;;; paths.lisp: where rhizome keeps its files, in the XDG base
;;; directories
;;;
;;;   config  $XDG_CONFIG_HOME/rhizome/  config.lisp, modules/
;;;   data    $XDG_DATA_HOME/rhizome/    frame-dumps/
;;;   cache   $XDG_CACHE_HOME/rhizome/   bar.log, snapshot.sexp, rhizome.log
;;;   state   $XDG_STATE_HOME/rhizome/   takeover.sexp, window-state.sexp,
;;;                                      launcher-history.sexp
;;;
;;; A variable that is unset, empty, or relative is ignored, as the
;;; specification requires, and its default under HOME applies
;;; (~/.config, ~/.local/share, ~/.cache, ~/.local/state). The WM
;;; resolves all four once at start; the bar process resolves them at
;;; its own start from the environment it inherits from the WM, so
;;; both read the same files.
;;;
;;; Each kept config, data, and state file also has a fixed place
;;; under HOME, listed in *relocated-files*. At start the WM moves a
;;; file from that place to its resolved one when only that place
;;; holds it.

(in-package #:rhizome)

(defvar *config-directory* nil
  "Resolved config directory, or NIL until resolved. Rebind in tests.")

(defvar *data-directory* nil
  "Resolved data directory, or NIL until resolved. Rebind in tests.")

(defvar *cache-directory* nil
  "Resolved cache directory, or NIL until resolved. Rebind in tests.")

(defvar *state-directory* nil
  "Resolved state directory, or NIL until resolved. Rebind in tests.")

(defun xdg-variable-directory (variable)
  "VARIABLE's value as a directory pathname, or NIL when it is unset,
empty, or not an absolute path."
  (let ((value (uiop:getenv variable)))
    (and value
         (plusp (length value))
         (char= (char value 0) #\/)
         (uiop:ensure-directory-pathname
           (uiop:parse-native-namestring value)))))

(defun home-subdirectory (relative)
  "The directory RELATIVE (a string ending in /) under the user's
home."
  (merge-pathnames (uiop:parse-unix-namestring relative
                                               :ensure-directory t)
                   (user-homedir-pathname)))

(defun xdg-base-home (variable uiop-resolver default)
  "The base directory named by VARIABLE: UIOP-RESOLVER's answer when
VARIABLE holds an absolute path, else DEFAULT under the home. UIOP
signals on a relative value, so an ignored value never reaches
UIOP-RESOLVER."
  (if (xdg-variable-directory variable)
      (funcall uiop-resolver)
      (home-subdirectory default)))

(defun xdg-state-home ()
  "The XDG state base directory. The UIOP bundled with SBCL has no
resolver for it."
  (or (xdg-variable-directory "XDG_STATE_HOME")
      (home-subdirectory ".local/state/")))

(defun base-home (kind)
  "The XDG base directory for KIND (:config, :data, :cache, :state),
read from the environment now."
  (ecase kind
    (:config (xdg-base-home "XDG_CONFIG_HOME" #'uiop:xdg-config-home
                            ".config/"))
    (:data (xdg-base-home "XDG_DATA_HOME" #'uiop:xdg-data-home
                          ".local/share/"))
    (:cache (xdg-base-home "XDG_CACHE_HOME" #'uiop:xdg-cache-home
                           ".cache/"))
    (:state (xdg-state-home))))

(defun rhizome-subdirectory (base)
  "The rhizome/ directory under BASE."
  (merge-pathnames (make-pathname :directory '(:relative "rhizome")) base))

(defun resolve-base-directories ()
  "Read the four XDG base directories from the environment and keep
them for this process. Returns the config, data, cache, and state
directories as a list."
  (setf *config-directory* (rhizome-subdirectory (base-home :config))
        *data-directory* (rhizome-subdirectory (base-home :data))
        *cache-directory* (rhizome-subdirectory (base-home :cache))
        *state-directory* (rhizome-subdirectory (base-home :state)))
  (list *config-directory* *data-directory*
        *cache-directory* *state-directory*))

(defun config-directory ()
  "Rhizome's config directory: config.lisp and modules/."
  (or *config-directory*
      (progn (resolve-base-directories) *config-directory*)))

(defun data-directory ()
  "Rhizome's data directory: frame-dumps/."
  (or *data-directory*
      (progn (resolve-base-directories) *data-directory*)))

(defun cache-directory ()
  "Rhizome's cache directory: logs and the bar snapshot, all of which
rhizome rewrites."
  (or *cache-directory*
      (progn (resolve-base-directories) *cache-directory*)))

(defun state-directory ()
  "Rhizome's state directory: takeover state, remembered window
placements, and launcher history."
  (or *state-directory*
      (progn (resolve-base-directories) *state-directory*)))

;;; --- One-time move from the old home-relative places ---

(defparameter *relocated-files*
  '((:config ".config/rhizome/" "config.lisp")
    (:config ".config/rhizome/" "modules/")
    (:data ".local/share/rhizome/" "frame-dumps/")
    (:state ".local/share/rhizome/" "takeover.sexp")
    (:state ".local/share/rhizome/" "window-state.sexp")
    (:state ".local/share/rhizome/" "launcher-history.sexp"))
  "Each kept file or directory (a name ending in /) as (KIND
OLD-DIRECTORY NAME): its base directory now, and where under the home
it lived before. Cache files are absent: rhizome rewrites them.")

(defun relocated-pathname (directory name)
  "NAME (a file, or a directory when it ends in /) inside DIRECTORY."
  (merge-pathnames (uiop:parse-unix-namestring
                     name :ensure-directory
                     (char= (char name (1- (length name))) #\/))
                   directory))

(defun legacy-file-moves ()
  "The (OLD NEW) pathname pairs for every kept file whose resolved
place differs from its old one, in *relocated-files* order."
  (loop for (kind old-directory name) in *relocated-files*
        for old = (relocated-pathname (home-subdirectory old-directory)
                                      name)
        for new = (relocated-pathname
                    (ecase kind
                      (:config (config-directory))
                      (:data (data-directory))
                      (:state (state-directory)))
                    name)
        unless (equal (namestring old) (namestring new))
          collect (list old new)))

(defun relocated-present-p (path)
  "Whether the file or directory PATH exists."
  (if (uiop:directory-pathname-p path)
      (and (uiop:directory-exists-p path) t)
      (and (uiop:file-exists-p path) t)))

(defun move-relocated (old new)
  "Move the file or directory OLD to NEW, creating NEW's parent. A
file that cannot be renamed (another file system) is copied, then
deleted."
  (let ((directory-p (uiop:directory-pathname-p old)))
    (ensure-directories-exist
      (if directory-p (uiop:pathname-parent-directory-pathname new) new))
    (if directory-p
        (rename-file old (uiop:ensure-pathname
                           (string-right-trim "/" (namestring new))))
        (handler-case (rename-file old new)
          (file-error ()
            (uiop:copy-file old new)
            (delete-file old))))))

(defun migrate-legacy-files ()
  "Move each kept file from its old place to its resolved one, when
the old place holds it and the new one does not. Logs at most two
lines:
- one naming every move;
- one warning naming every file found in both places (both are
  kept; the new one is read) and every move that failed.
Logs nothing when no old place holds a kept file. Returns the moved
pairs and the conflicting pairs as two values; a failed move appears
only in the warning, and signals nothing."
  (let ((moved '()) (conflicts '()) (failed '()))
    (loop for (old new) in (legacy-file-moves)
          when (relocated-present-p old)
            do (if (relocated-present-p new)
                   (push (list old new) conflicts)
                   (handler-case
                       (progn (move-relocated old new)
                              (push (list old new) moved))
                     (error (e)
                       (push (list old new e) failed)))))
    (setf moved (nreverse moved)
          conflicts (nreverse conflicts)
          failed (nreverse failed))
    (when moved
      (format t "~&; Rhizome: moved to the XDG base directories: ~
                 ~{~{~A -> ~A~}~^; ~}~%"
              moved))
    (when (or conflicts failed)
      (format t "~&; Rhizome: WARNING: ~{~A~^; ~}~%"
              (append
                (when conflicts
                  (list (format nil "old and new places both hold a ~
                                     file, so the new one is read and ~
                                     the old one is left in place: ~
                                     ~{~{~A (new: ~A)~}~^, ~}"
                                conflicts)))
                (when failed
                  (list (format nil "could not move: ~
                                     ~{~{~A -> ~A (~A)~}~^, ~}"
                                failed))))))
    (values moved conflicts)))
