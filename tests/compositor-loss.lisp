;;; Compositor loss: when river exits, the WM and the bar must see
;;; their connection close. WL-DISPLAY-LISTEN returns NIL for an idle
;;; connection and for a closed one alike, so the event loops slept
;;; forever on a dead socket. These tests connect a real wayflan
;;; display to a Unix socket this file listens on, close the server
;;; end, and check DISPLAY-CONNECTION-LOST-P against each state.
(ql:quickload :rhizome)
(require :sb-bsd-sockets)

(in-package #:rhizome)

(defun compositor-loss-socket-path ()
  "A fresh Unix socket path for a stand-in compositor."
  (format nil "~A/rhizome-compositor-loss-~D-~D"
          (string-right-trim "/" (or (uiop:getenv "XDG_RUNTIME_DIR")
                                     (uiop:getenv "TMPDIR")
                                     "/tmp"))
          (sb-posix:getpid) (random 1000000)))

(defun call-with-stand-in-compositor (function)
  "Listen on a fresh Unix socket, connect a wayflan display to it, and
call FUNCTION with the display and the accepted server-side socket.
Close everything on exit."
  (let ((path (compositor-loss-socket-path))
        (server (make-instance 'sb-bsd-sockets:local-socket :type :stream))
        (display nil)
        (peer nil))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind server path)
           (sb-bsd-sockets:socket-listen server 1)
           (setf display (wl:wl-display-connect (pathname path))
                 peer (sb-bsd-sockets:socket-accept server))
           (funcall function display peer))
      (ignore-errors (sb-bsd-sockets:socket-close server))
      (when peer (ignore-errors (sb-bsd-sockets:socket-close peer)))
      (when display (ignore-errors (wl:wl-display-disconnect display)))
      (ignore-errors (delete-file path)))))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (lost-p (display)
           (if (fboundp 'display-connection-lost-p)
               (and (funcall 'display-connection-lost-p display) t)
               :undefined)))

    (call-with-stand-in-compositor
     (lambda (display peer)
       (declare (ignore peer))
       (ok "an idle open connection reads as no input"
           (wl:wl-display-listen display) nil)
       (ok "an idle open connection is not lost"
           (lost-p display) nil)))

    (call-with-stand-in-compositor
     (lambda (display peer)
       (sb-bsd-sockets:socket-close peer)
       (ok "a closed connection also reads as no input"
           (wl:wl-display-listen display) nil)
       (ok "a closed connection is lost"
           (lost-p display) t)
       (ok "a closed connection stays lost"
           (lost-p display) t)))

    (call-with-stand-in-compositor
     (lambda (display peer)
       ;; Input the server sent before closing still waits to be
       ;; dispatched: the connection is not lost until it is drained
       (sb-bsd-sockets:socket-send
        peer (make-array 8 :element-type '(unsigned-byte 8)
                           :initial-element 0)
        8)
       (sb-bsd-sockets:socket-close peer)
       (ok "undispatched input keeps a closed connection open"
           (lost-p display) nil)))

    (call-with-stand-in-compositor
     (lambda (display peer)
       (declare (ignore peer))
       (wl:wl-display-disconnect display)
       (ok "a disconnected display is lost"
           (lost-p display) t))))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
