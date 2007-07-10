#!/usr/local/bin/gosh

(use text.html-lite)
(use www.cgi)
(use www.cgi.ext)

;; Note that the parameter *session-cookie-domain* is set correctly.
(*session-cookie-domain* "localhost")

(define (main args)
  (cgi-main
   (lambda (params)
     (let ((session (session-begin)))
       `(,(cgi-header :cookies (construct-cookie-string session))
         ,(html-doctype)
         ,(html:html
           (html:head (html:title "example: counter"))
           (html:body
            (html:h1 "Counter")
            (html:p "Your deposit: "
                    (let ((current (session-get-variable session 'counter)))
                      (if current
                          (session-set-variable session 'counter (+ current 1))
                          (session-set-variable session 'counter 1))
                      (or current
                          0)))))))))
  0)
