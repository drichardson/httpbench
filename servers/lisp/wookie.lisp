; Test of wookie web framework, which is built on cl-async and http-parse.
; https://github.com/orthecreedence/wookie

; use quicklisp to load wookie package by entering this in the REPL
(ql:quickload "wookie")

(defun gen-string (n)
  (coerce
   (loop for i from 1 to n
      append (list (code-char (+ (char-code #\a) (mod i 26)))))
   'string))

(defun main ()
  (wookie:load-plugins)
  (let ((1k-response (gen-string 1024)))
    (wookie:defroute (:get "/1k") (req res)
      (wookie:send-response res :body 1k-response))  
    (as:with-event-loop ()
      (wookie:start-server (make-instance 'wookie:listener :port 8080)))))
  
