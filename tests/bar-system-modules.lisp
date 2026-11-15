;;; bar-system-modules.lisp - the :hostname, :memory, and :cpu bar
;;; modules: /proc/meminfo and /proc/stat parsing against captures
;;; recorded on a real host (tests/fixtures/proc, provenance in its
;;; README), the CPU delta between two samples, the placeholder for a
;;; missing, unreadable, or malformed file, hostname memoization, and
;;; registration by name in *bar-modules*.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defparameter *proc-fixtures*
  (uiop:merge-pathnames* "fixtures/proc/"
                         (uiop:pathname-directory-pathname
                          (or *load-truename* *default-pathname-defaults*)))
  "Directory holding the recorded /proc captures.")

(defun proc-fixture (name)
  "Namestring of the recorded capture NAME."
  (namestring (uiop:merge-pathnames* (make-pathname :name name :type nil)
                                    *proc-fixtures*)))

(let ((pass 0) (fail 0))
  (flet ((ok (name thunk expected)
           (let ((actual (handler-case (funcall thunk)
                           (error (e) (list :signalled (princ-to-string e))))))
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))))
    (macrolet ((check (name form expected)
                 `(ok ,name (lambda () ,form) ,expected)))

      ;; Registration by name
      (dolist (module '(:hostname :memory :cpu))
        (check (format nil "~(~S~) is registered in *bar-modules*" module)
               (let ((fn (cdr (assoc module *bar-modules*))))
                 (and fn (fboundp fn) t))
               t))

      ;; /proc/meminfo, recorded: MemTotal 65886992 kB,
      ;; MemAvailable 51932368 kB
      (check "meminfo capture parses to total and available kB"
             (multiple-value-list
              (bar-read-meminfo (proc-fixture "meminfo")))
             '(65886992 51932368))
      (check "memory percent: (total - available) / total, rounded"
             (bar-memory-percent 65886992 51932368)
             21)
      (clrhash *bar-cache*)
      (check "memory module renders the recorded capture"
             (let ((*bar-meminfo-path* (proc-fixture "meminfo")))
               (bar-module-memory))
             "MEM 21%")

      ;; /proc/stat, recorded aggregate cpu lines. Total sums user
      ;; through steal; idle is idle + iowait.
      (check "first stat capture parses to total and idle jiffies"
             (bar-read-proc-stat (proc-fixture "stat-first"))
             '(24288624 . 22531756))
      (check "second stat capture parses to total and idle jiffies"
             (bar-read-proc-stat (proc-fixture "stat-second"))
             '(24293387 . 22536248))
      (check "cpu percent is the busy share of the delta"
             (bar-cpu-percent '(24288624 . 22531756)
                              '(24293387 . 22536248))
             6)
      (check "cpu percent: no previous sample gives NIL"
             (bar-cpu-percent nil '(24293387 . 22536248))
             nil)
      (check "cpu percent: identical samples give NIL"
             (bar-cpu-percent '(24288624 . 22531756)
                              '(24288624 . 22531756))
             nil)
      (check "cpu percent: counters running backwards give NIL"
             (bar-cpu-percent '(24293387 . 22536248)
                              '(24288624 . 22531756))
             nil)

      ;; The module samples once per refresh: the first refresh has
      ;; no delta, the next reports the delta since the first.
      (clrhash *bar-cache*)
      (setf *bar-cpu-previous* nil)
      (check "cpu module: first sample shows the placeholder"
             (let ((*bar-proc-stat-path* (proc-fixture "stat-first")))
               (bar-module-cpu))
             "CPU --")
      (clrhash *bar-cache*)
      (check "cpu module: second sample shows the delta"
             (let ((*bar-proc-stat-path* (proc-fixture "stat-second")))
               (bar-module-cpu))
             "CPU 6%")
      (check "cpu module: the second sample is kept for the next delta"
             *bar-cpu-previous*
             '(24293387 . 22536248))

      ;; Missing, unreadable, and malformed files: placeholder, no signal
      (let ((missing "/nonexistent/rhizome-test/proc-file")
            (directory (namestring *proc-fixtures*)))
        (check "meminfo reader: missing file gives NIL"
               (bar-read-meminfo missing) nil)
        (check "meminfo reader: a directory gives NIL"
               (bar-read-meminfo directory) nil)
        (check "meminfo reader: content without MemAvailable gives NIL"
               (bar-read-meminfo (proc-fixture "stat-first")) nil)
        (check "stat reader: missing file gives NIL"
               (bar-read-proc-stat missing) nil)
        (check "stat reader: a directory gives NIL"
               (bar-read-proc-stat directory) nil)
        (check "stat reader: content without a cpu line gives NIL"
               (bar-read-proc-stat (proc-fixture "meminfo")) nil)
        (clrhash *bar-cache*)
        (check "memory module: missing file shows the placeholder"
               (let ((*bar-meminfo-path* missing))
                 (bar-module-memory))
               "MEM --")
        (clrhash *bar-cache*)
        (setf *bar-cpu-previous* '(24288624 . 22531756))
        (check "cpu module: missing file shows the placeholder"
               (let ((*bar-proc-stat-path* missing))
                 (bar-module-cpu))
               "CPU --")
        (check "cpu module: a failed read drops the previous sample"
               *bar-cpu-previous* nil)
        (clrhash *bar-cache*)
        (check "bar-module-parts renders the placeholder part"
               (let ((*bar-meminfo-path* missing))
                 (bar-module-parts :memory))
               '(("MEM --"))))

      ;; Hostname: looked up once, then the same string every tick
      (setf *bar-hostname* nil)
      (check "hostname module returns a non-empty string"
             (let ((name (bar-module-hostname)))
               (and (stringp name) (plusp (length name))))
             t)
      (check "hostname module is memoized"
             (eq (bar-module-hostname) (bar-module-hostname))
             t)
      (setf *bar-hostname* "memoized-host")
      (check "hostname module returns the memoized value"
             (bar-module-hostname)
             "memoized-host")
      (setf *bar-hostname* nil)
      (clrhash *bar-cache*))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (when (plusp fail)
      (uiop:quit 1))))
