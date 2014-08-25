; example using sockets in SBCL
; to test, run (main) and then from a shell use netcat:
; $ nc localhost 8080

(require 'sb-bsd-sockets)

(defun make-listen-socket ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
			       :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket '(0 0 0 0) 8080)
    (sb-bsd-sockets:socket-listen socket 1)
    socket))

(defun gen-string (n)
  (coerce
   (loop for i from 1 to n
      append (list (code-char (+ (char-code #\a) (mod i 26)))))
   'string))

(defparameter *response-content-length* 1024)
(defparameter CRLF (format nil "~C~C" #\return #\linefeed))
(defparameter *response*
  (concatenate 'string
   "HTTP/1.1 200 OK" CRLF
   (format nil "Content-Length: ~a" *response-content-length*) CRLF
   CRLF
   (gen-string *response-length*)))
(defparameter *response-length* (length *response*))

(defun accept-one (l)
  (let ((c (sb-bsd-sockets:socket-accept l)))
    (unwind-protect
	 (sb-bsd-sockets:socket-send c *response* *response-length* :external-format :utf-8)
      (sb-bsd-sockets:socket-close c))))

(defun runloop (l)
  (accept-one l)
  (runloop l))

(defun main ()
  (let ((l (make-listen-socket)))
    (unwind-protect
	 (runloop l)
      (sb-bsd-sockets:socket-close l))))
