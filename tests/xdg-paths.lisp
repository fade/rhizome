;;; xdg-paths.lisp: where rhizome keeps its files. The four XDG base
;;; directories (set, unset, empty, relative), the per-file paths that
;;; derive from them, resolution once per start, and the one-time move
;;; of files from their old home-relative places (old only, new only,
;;; both, neither). Every case runs under a throwaway HOME and throwaway
;;; XDG variables; the real home is never read or written.
(require :sb-posix)
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defparameter *xdg-test-variables*
  '("HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_CACHE_HOME"
    "XDG_STATE_HOME"))

(defun xdg-test-setenv (alist)
  "Set each (NAME . VALUE) in ALIST; a NIL value unsets NAME."
  (loop for (name . value) in alist
        do (if value
               (sb-posix:setenv name value 1)
               (sb-posix:unsetenv name))))

(defun xdg-test-write (path text)
  "Write TEXT to PATH, creating its directories."
  (ensure-directories-exist path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-string text s))
  path)

(defun xdg-test-read (path)
  "PATH's contents as a string, or NIL when PATH does not exist."
  (and (probe-file path) (uiop:read-file-string path)))

(defun run-xdg-path-tests ()
  "Run every case; returns the pass and fail counts as two values."
  (let ((pass 0) (fail 0)
        (saved (mapcar (lambda (v) (cons v (uiop:getenv v)))
                       *xdg-test-variables*))
        (root (format nil "/tmp/rhizome-test-xdg-~A-~A/"
                      (sb-posix:getpid) (get-universal-time))))
    (labels ((ok (name actual expected)
               (if (equal actual expected)
                   (progn (incf pass)
                          (format t "~&PASS: ~A~%" name))
                   (progn (incf fail)
                          (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                  name actual expected))))
             (under (rel) (concatenate 'string root rel))
             (env (&rest pairs)
               ;; a fresh HOME under ROOT, every XDG variable unset,
               ;; then PAIRS on top
               (xdg-test-setenv
                 (append (list (cons "HOME" (under "home")))
                         (mapcar (lambda (v) (cons v nil))
                                 (rest *xdg-test-variables*))
                         pairs))
               (resolve-base-directories))
             (dirs ()
               (mapcar #'namestring
                       (list (config-directory) (data-directory)
                             (cache-directory) (state-directory)))))
      (unwind-protect
           (progn
             (ensure-directories-exist (under "home/"))

             ;; --- unset: the spec defaults under HOME ---
             (env)
             (ok "unset variables give the defaults under HOME"
                 (dirs)
                 (list (under "home/.config/rhizome/")
                       (under "home/.local/share/rhizome/")
                       (under "home/.cache/rhizome/")
                       (under "home/.local/state/rhizome/")))

             ;; --- set to absolute paths ---
             (env (cons "XDG_CONFIG_HOME" (under "cfg"))
                  (cons "XDG_DATA_HOME" (under "dat/"))
                  (cons "XDG_CACHE_HOME" (under "cch"))
                  (cons "XDG_STATE_HOME" (under "stt/")))
             (ok "absolute variables, with or without a trailing slash"
                 (dirs)
                 (list (under "cfg/rhizome/") (under "dat/rhizome/")
                       (under "cch/rhizome/") (under "stt/rhizome/")))

             ;; --- empty: ignored, as if unset ---
             (env (cons "XDG_CONFIG_HOME" "") (cons "XDG_DATA_HOME" "")
                  (cons "XDG_CACHE_HOME" "") (cons "XDG_STATE_HOME" ""))
             (ok "empty variables fall back to the defaults"
                 (dirs)
                 (list (under "home/.config/rhizome/")
                       (under "home/.local/share/rhizome/")
                       (under "home/.cache/rhizome/")
                       (under "home/.local/state/rhizome/")))

             ;; --- relative: ignored, as if unset, and no error ---
             (ok "relative variables fall back to the defaults"
                 (handler-case
                     (progn
                       (env (cons "XDG_CONFIG_HOME" "rel/cfg")
                            (cons "XDG_DATA_HOME" "rel/dat")
                            (cons "XDG_CACHE_HOME" "./cch")
                            (cons "XDG_STATE_HOME" "stt"))
                       (dirs))
                   (error (e) (list :error (princ-to-string e))))
                 (list (under "home/.config/rhizome/")
                       (under "home/.local/share/rhizome/")
                       (under "home/.cache/rhizome/")
                       (under "home/.local/state/rhizome/")))

             ;; --- resolved once: a later change waits for the next start ---
             (env (cons "XDG_STATE_HOME" (under "stt")))
             (xdg-test-setenv (list (cons "XDG_STATE_HOME" (under "other"))))
             (ok "a resolved directory holds until the next resolve"
                 (namestring (state-directory)) (under "stt/rhizome/"))
             (resolve-base-directories)
             (ok "resolving again picks up the new value"
                 (namestring (state-directory)) (under "other/rhizome/"))

             ;; --- every file follows its base directory ---
             (env (cons "XDG_CONFIG_HOME" (under "cfg"))
                  (cons "XDG_DATA_HOME" (under "dat"))
                  (cons "XDG_CACHE_HOME" (under "cch"))
                  (cons "XDG_STATE_HOME" (under "stt")))
             (ok "config file and module directory live in config"
                 (let ((*module-directory* nil))
                   (list (namestring (config-pathname))
                         (namestring (module-directory))))
                 (list (under "cfg/rhizome/config.lisp")
                       (under "cfg/rhizome/modules/")))
             (ok "frame dumps live in data"
                 (namestring (frame-dump-pathname "work"))
                 (under "dat/rhizome/frame-dumps/work.lisp"))
             (ok "bar log and snapshot live in cache"
                 (list (namestring (bar-log-path))
                       (namestring (snapshot-pathname)))
                 (list (under "cch/rhizome/bar.log")
                       (under "cch/rhizome/snapshot.sexp")))
             (ok "takeover, window state, and launcher history live in state"
                 (mapcar #'namestring
                         (list (takeover-state-path) (window-state-path)
                               (launcher-history-path)))
                 (list (under "stt/rhizome/takeover.sexp")
                       (under "stt/rhizome/window-state.sexp")
                       (under "stt/rhizome/launcher-history.sexp")))
             (ok "a bound path variable still wins (test isolation)"
                 (let ((*takeover-state-path* #P"/tmp/elsewhere.sexp"))
                   (namestring (takeover-state-path)))
                 "/tmp/elsewhere.sexp")
             (ok "a bound module directory still wins"
                 (let ((*module-directory* #P"/tmp/mods/"))
                   (namestring (module-directory)))
                 "/tmp/mods/")

             ;; --- what moves: only what changed place ---
             (env)
             (ok "with default variables only the state files move"
                 (mapcar (lambda (pair) (mapcar #'namestring pair))
                         (legacy-file-moves))
                 (list (list (under "home/.local/share/rhizome/takeover.sexp")
                             (under "home/.local/state/rhizome/takeover.sexp"))
                       (list (under "home/.local/share/rhizome/window-state.sexp")
                             (under "home/.local/state/rhizome/window-state.sexp"))
                       (list (under "home/.local/share/rhizome/launcher-history.sexp")
                             (under "home/.local/state/rhizome/launcher-history.sexp"))))
             (env (cons "XDG_CONFIG_HOME" (under "cfg"))
                  (cons "XDG_DATA_HOME" (under "dat")))
             (ok "moved config and data homes add their files"
                 (mapcar (lambda (pair) (namestring (first pair)))
                         (legacy-file-moves))
                 (list (under "home/.config/rhizome/config.lisp")
                       (under "home/.config/rhizome/modules/")
                       (under "home/.local/share/rhizome/frame-dumps/")
                       (under "home/.local/share/rhizome/takeover.sexp")
                       (under "home/.local/share/rhizome/window-state.sexp")
                       (under "home/.local/share/rhizome/launcher-history.sexp")))

             ;; --- migration: old only ---
             (env)
             (let ((old (under "home/.local/share/rhizome/window-state.sexp"))
                   (new (under "home/.local/state/rhizome/window-state.sexp"))
                   (log nil))
               (xdg-test-write old "(old-only)")
               (setf log (with-output-to-string (*standard-output*)
                           (migrate-legacy-files)))
               (ok "old only: the file moves" (xdg-test-read new) "(old-only)")
               (ok "old only: the old file is gone" (probe-file old) nil)
               (ok "old only: one log line naming both places"
                   (list (count #\Newline (string-trim '(#\Newline) log))
                         (and (search old log) (search new log) t))
                   (list 0 t))
               ;; --- new only: nothing to do, nothing said ---
               (setf log (with-output-to-string (*standard-output*)
                           (migrate-legacy-files)))
               (ok "new only: the file stays" (xdg-test-read new) "(old-only)")
               (ok "new only: nothing logged" log "")
               ;; --- both: keep both, warn once, read the new ---
               (xdg-test-write old "(stale)")
               (setf log (with-output-to-string (*standard-output*)
                           (migrate-legacy-files)))
               (ok "both: the new file is untouched"
                   (xdg-test-read new) "(old-only)")
               (ok "both: the old file is left in place"
                   (xdg-test-read old) "(stale)")
               (ok "both: one warning naming both places"
                   (list (count #\Newline (string-trim '(#\Newline) log))
                         (and (search "WARNING" log) (search old log)
                              (search new log) t))
                   (list 0 t))
               (ok "both: the state path reads the new place"
                   (namestring (window-state-path)) new)
               (delete-file old))

             ;; --- neither: nothing happens, nothing is created ---
             (env (cons "XDG_STATE_HOME" (under "empty-state")))
             (let ((log (with-output-to-string (*standard-output*)
                          (migrate-legacy-files))))
               (ok "neither: nothing logged" log "")
               (ok "neither: no state directory created"
                   (uiop:directory-exists-p (under "empty-state/")) nil))

             ;; --- several files and a directory move in one pass ---
             (env (cons "XDG_CONFIG_HOME" (under "cfg2"))
                  (cons "XDG_STATE_HOME" (under "stt2")))
             (xdg-test-write (under "home/.config/rhizome/config.lisp") "(cfg)")
             (xdg-test-write (under "home/.config/rhizome/modules/m/m.asd") "(asd)")
             (xdg-test-write (under "home/.local/share/rhizome/takeover.sexp") "(tk)")
             (let ((log (with-output-to-string (*standard-output*)
                          (migrate-legacy-files))))
               (ok "several: config, module tree, and takeover all move"
                   (list (xdg-test-read (under "cfg2/rhizome/config.lisp"))
                         (xdg-test-read (under "cfg2/rhizome/modules/m/m.asd"))
                         (xdg-test-read (under "stt2/rhizome/takeover.sexp")))
                   (list "(cfg)" "(asd)" "(tk)"))
               (ok "several: the old module directory is gone"
                   (uiop:directory-exists-p
                     (under "home/.config/rhizome/modules/"))
                   nil)
               (ok "several: still one log line"
                   (count #\Newline (string-trim '(#\Newline) log)) 0)))
        ;; --- cleanup: environment back, throwaway tree gone ---
        (xdg-test-setenv saved)
        (resolve-base-directories)
        (ignore-errors
          (uiop:delete-directory-tree (pathname root) :validate t))))
    (values pass fail)))

(multiple-value-bind (pass fail) (run-xdg-path-tests)
  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
