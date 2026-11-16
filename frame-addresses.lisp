;;; Frame addresses for Rhizome
;;;
;;; Every leaf frame has an address, W:S:F: the workspace, the screen
;;; (screens.lisp), and the frame's position in fselect order within
;;; that workspace on that screen. All three count from the shared
;;; index base (index-base), so with the default base the primary
;;; screen's first workspace starts at 1:1:1.
;;;
;;;   2:1:3          workspace 2, screen 1, frame 3
;;;   irc:DVI-D-1:1  workspace named irc, screen by connector, frame 1
;;;   ::2            frame 2 of the focused screen's current workspace
;;;   3:2            workspace 3 on screen 2, no frame named
;;;
;;; W is a number or a workspace name; S is a number or a connector
;;; name; F is a number. An empty part is omitted: an omitted screen is
;;; the focused one, an omitted workspace is that screen's current one,
;;; and an omitted frame is left to the caller (launch-into lets the
;;; placement policy choose). A workspace name given without a screen
;;; is looked up on the focused screen first, then on the others in
;;; screen order. A string without a colon is not an address.
;;;
;;; Each leaf stores its address as zero-based ordinals (frame-ordinals)
;;; that refresh-frame-addresses recomputes whenever a frame tree, the
;;; screens, or a workspace count changes (note-frame-trees-changed).
;;; frame-address renders it through the index base when read, so a
;;; change of base needs no refresh.
;;;
;;;   rhizome-cmd frame-address [ADDRESS]   the current frame's address
;;;   rhizome-cmd frame-addresses           every frame, with its window

(in-package #:rhizome)

;;; --- Keeping addresses current ---

(defun refresh-frame-addresses ()
  "Recompute the stored address of every frame in every tree: each
leaf of a live screen's workspace within its count gets its ordinals;
split nodes and every other frame get NIL."
  (let ((order (screen-order)))
    (maphash
     (lambda (key root)
       (let* ((output (car key))
              (workspace (cdr key))
              (screen (position output order))
              (live (and screen
                         (integerp workspace)
                         (<= 1 workspace (workspace-count output)))))
         (labels ((clear (frame)
                    (setf (frame-ordinals frame) nil)
                    (when (frame-first-child frame)
                      (clear (frame-first-child frame))
                      (clear (frame-second-child frame)))))
           (clear root))
         (when live
           (loop for leaf in (frame-leaves root)
                 for index from 0
                 do (setf (frame-ordinals leaf)
                          (list (1- workspace) screen index))))))
     *frame-trees*)))

;;; --- Reading an address ---

(defun frame-address-parts (frame &key (index-base (index-base)))
  "FRAME's workspace, screen, and frame numbers under INDEX-BASE as
three values, or NIL when FRAME has no address."
  (let ((ordinals (and frame (frame-ordinals frame))))
    (when ordinals
      (destructuring-bind (workspace screen index) ordinals
        (values (display-index workspace :index-base index-base)
                (display-index screen :index-base index-base)
                (display-index index :index-base index-base))))))

(defun frame-address (frame &key (index-base (index-base)))
  "FRAME's address as the string \"W:S:F\" under INDEX-BASE, or NIL
when FRAME has no address."
  (multiple-value-bind (workspace screen index)
      (frame-address-parts frame :index-base index-base)
    (and workspace (format nil "~D:~D:~D" workspace screen index))))

;;; --- Parsing and resolving ---

(defun parse-frame-address (string)
  "The parts of the address STRING as the plist (:workspace W :screen S
:frame F), each a trimmed string or NIL when omitted. NIL when STRING
holds no colon or more than three parts."
  (when (and (stringp string) (find #\: string))
    (let ((parts (uiop:split-string (string-trim " " string)
                                    :separator ":")))
      (when (<= 2 (length parts) 3)
        (flet ((part (n)
                 (let ((text (and (nth n parts)
                                  (string-trim " " (nth n parts)))))
                   (and text (plusp (length text)) text))))
          (list :workspace (part 0) :screen (part 1) :frame (part 2)))))))

(defun workspace-named (output name)
  "The workspace (1-based) on OUTPUT whose configured name is NAME,
case ignored, or NIL."
  (loop for workspace from 1 to (workspace-count output)
        when (let ((label (workspace-name output workspace)))
               (and label (string-equal label name)))
          return workspace))

(defun address-workspace (designator output screen-given index-base)
  "The output and workspace (1-based) the address part DESIGNATOR
names, as two values, or NIL. NIL DESIGNATOR is OUTPUT's current
workspace; a number is that workspace on OUTPUT, unchecked; a name is
looked up on OUTPUT, and, unless SCREEN-GIVEN, then on every other
screen in screen order."
  (cond ((null designator)
         (values output (current-workspace output)))
        ((index-ordinal designator :index-base index-base)
         (values output
                 (workspace-from-display designator :index-base index-base)))
        (t
         (loop for candidate in (if screen-given
                                    (list output)
                                    (cons output (remove output (screen-order))))
               for workspace = (workspace-named candidate designator)
               when workspace
                 return (values candidate workspace)))))

(defun frame-range-text (number output workspace &key (index-base (index-base)))
  "\"no frame NUMBER: W:S has N frames\" for WORKSPACE on OUTPUT."
  (format nil "no frame ~A: ~D:~D has ~D frame~:P"
          number
          (workspace-to-display workspace :index-base index-base)
          (screen-number output :index-base index-base)
          (length (frame-leaves (frame-tree-for output workspace)))))

(defun resolve-frame-address (string &key (index-base (index-base)))
  "Resolve the address STRING under INDEX-BASE. Returns the plist
(:output OUTPUT :workspace WORKSPACE :frame FRAME :ordinal ORDINAL),
where WORKSPACE is 1-based, ORDINAL is the zero-based frame position
or NIL when the address omits the frame, and FRAME is the leaf at
ORDINAL or NIL when there is none yet. On failure, returns NIL and a
second value: the reply text naming what is missing."
  (let ((parts (parse-frame-address string)))
    (unless parts
      (return-from resolve-frame-address
        (values nil (format nil "not a frame address: ~A" string))))
    (destructuring-bind (&key workspace screen frame) parts
      (let ((output (if screen
                        (screen-by-number screen :index-base index-base)
                        (current-output))))
        (unless output
          (return-from resolve-frame-address
            (values nil (if screen
                            (format nil "no screen ~A" screen)
                            "no screen"))))
        (multiple-value-bind (out ws)
            (address-workspace workspace output screen index-base)
          (cond
            ((null out)
             (values nil (format nil "no workspace ~A" workspace)))
            ((not (valid-workspace-p out ws))
             (values nil (workspace-range-text out workspace)))
            ((null frame)
             (list :output out :workspace ws :frame nil :ordinal nil))
            (t
             (let ((ordinal (index-ordinal frame :index-base index-base)))
               (cond ((null ordinal)
                      (values nil (format nil "no frame ~A" frame)))
                     ((minusp ordinal)
                      (values nil (format nil "no frame ~A: frames count from ~D"
                                          frame index-base)))
                     (t
                      (list :output out :workspace ws
                            :frame (nth ordinal
                                        (frame-leaves
                                         (frame-tree-for out ws)))
                            :ordinal ordinal)))))))))))

;;; --- Listing ---

(defun frame-address-context (frame)
  "What FRAME shows, for a listing: its window's title, else its app
id, else \"empty\"."
  (let ((win (frame-current-window frame)))
    (if win
        (or (window-title win) (window-app-id win) "window")
        "empty")))

(defun all-frame-addresses (&key (index-base (index-base)))
  "Every frame of every workspace on every screen, as a list of
(ADDRESS FRAME OUTPUT WORKSPACE) in workspace, screen, frame order.
Creates the frame trees of workspaces not yet shown."
  (let* ((screens (screen-order))
         (most (reduce #'max screens :key #'workspace-count
                                     :initial-value 0))
         (rows '()))
    (loop for workspace from 1 to most
          do (dolist (output screens)
               (when (<= workspace (workspace-count output))
                 (dolist (frame (frame-leaves (frame-tree-for output workspace)))
                   (push (list (frame-address frame :index-base index-base)
                               frame output workspace)
                         rows)))))
    (nreverse rows)))

;;; --- Commands ---

(define-command :frame-address (output args)
  "Reply with the address of the current frame, or of the frame
ADDRESS names. Usage: frame-address [ADDRESS]"
  (if args
      (multiple-value-bind (target error)
          (resolve-frame-address (princ-to-string (first args)))
        (cond (error error)
              ((getf target :frame)
               (frame-address (getf target :frame)))
              ((null (getf target :ordinal))
               (frame-address (current-frame-for (getf target :output)
                                                 (getf target :workspace))))
              (t
               (frame-range-text (getf (parse-frame-address
                                        (princ-to-string (first args)))
                                       :frame)
                                 (getf target :output)
                                 (getf target :workspace)))))
      (let ((out (or output (current-output))))
        (and out
             (frame-address (current-frame-for out (current-workspace out)))))))

(define-command :frame-addresses (output args)
  "Reply with every frame's address and what it shows, one per line,
in workspace, screen, frame order. Usage: frame-addresses"
  (declare (ignore output args))
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (row)
                    (format nil "~A ~A" (first row)
                            (frame-address-context (second row))))
                  (all-frame-addresses))))
