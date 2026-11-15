;;; Bounded printing for replies and log lines
;;;
;;; IPC replies, the eval prompt result, and the log lines that echo
;;; them print arbitrary values on the WM's main thread. A circular,
;;; deep, or very long value printed without bounds exhausts the stack
;;; or the heap there. FORMAT-REPLY prints with depth limits, length
;;; limits, and a hard cap on the reply's length, and returns a string
;;; for any value.

(in-package #:rhizome)

(defparameter *reply-print-level* 16
  "*PRINT-LEVEL* in effect while a reply is printed.")
(defparameter *reply-print-length* 1000
  "*PRINT-LENGTH* in effect while a reply is printed.")
(defparameter *reply-max-length* 4096
  "Maximum characters in a reply; longer output is cut and ends in ...")

(defclass bounded-output (sb-gray:fundamental-character-output-stream)
  ((buffer :initform (make-string-output-stream)
           :reader bounded-output-buffer)
   (count :initform 0 :accessor bounded-output-count)
   (column :initform 0 :accessor bounded-output-column)
   (limit :initarg :limit :reader bounded-output-limit))
  (:documentation "Character output stream that accepts at most LIMIT
characters, then throws to the tag BOUNDED-OUTPUT-FULL."))

(defmethod sb-gray:stream-write-char ((stream bounded-output) char)
  (when (>= (bounded-output-count stream) (bounded-output-limit stream))
    (throw 'bounded-output-full nil))
  (write-char char (bounded-output-buffer stream))
  (incf (bounded-output-count stream))
  (setf (bounded-output-column stream)
        (if (char= char #\Newline)
            0
            (1+ (bounded-output-column stream))))
  char)

(defmethod sb-gray:stream-line-column ((stream bounded-output))
  (bounded-output-column stream))

(defun format-reply (control &rest args)
  "Return the string FORMAT produces for CONTROL and ARGS, cut to
*REPLY-MAX-LENGTH* characters plus ... when longer.

Printing binds:
  *PRINT-CIRCLE* to false
  *PRINT-LEVEL* to *REPLY-PRINT-LEVEL*
  *PRINT-LENGTH* to *REPLY-PRINT-LENGTH*

The level and length limits end a circular value; the cap ends a very
long one. Never signals: a serious condition while printing returns a
one-line error string naming the condition's type."
  (handler-case
      (let* ((out (make-instance 'bounded-output :limit *reply-max-length*))
             (complete (catch 'bounded-output-full
                         (let ((*print-circle* nil)
                               (*print-level* *reply-print-level*)
                               (*print-length* *reply-print-length*)
                               (*print-readably* nil))
                           (apply #'format out control args))
                         t)))
        (concatenate 'string
                     (get-output-stream-string (bounded-output-buffer out))
                     (if complete "" "...")))
    (serious-condition (c)
      (or (ignore-errors
           (format nil "error: cannot print the reply (~(~S~))" (type-of c)))
          "error: cannot print the reply"))))
