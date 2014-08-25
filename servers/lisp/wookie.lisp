; Test of wookie web framework, which is built on cl-async and http-parse.
; https://github.com/orthecreedence/wookie

; use quicklisp to load wookie package by entering this in the REPL
; (ql:quickload "wookie")

(defpackage :wookie-test
     (:use :cl :wookie))
(in-package :wookie-test)

;; load Wookie's core plugins
(load-plugins)

(defun gen-string (n)
  (coerce
   (loop for i from 1 to n
      append (list (code-char (+ (char-code #\a) (mod i 26)))))
   'string))

(defparameter *1k-response* (gen-string 1024))

;; define our homepage route
(defroute (:get "/1k") (req res)
    (send-response res :body *1k-response*))

;; start serving requests!
(as:with-event-loop ()
    (start-server (make-instance 'listener :port 8080)))