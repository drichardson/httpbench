; hunchentoot example

; use quicklisp to load
; (ql:quickload "hunchentoot")

;(defpackage :hunchentoot-test
;    (:use :hunchentoot))

(defun gen-string (n)
  (coerce
   (loop for i from 1 to n
      append (list (code-char (+ (char-code #\a) (mod i 26)))))
   'string))

(defparameter *1k-response* (gen-string 1024))
(defparameter *acceptor* nil)

(defun hunchentoot-main ()
  (hunchentoot:define-easy-handler (say-yo :uri "/1k") ()
    (setf (hunchentoot:content-type*) "text/plain")
    *1k-response*)
  (setf *acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))))

(defun hunchentoot-stop ()
  (hunchentoot:stop *acceptor*)
  (setf *acceptor* nil))