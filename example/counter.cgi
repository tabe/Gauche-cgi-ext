#!/usr/bin/env gosh

(use text.html-lite)
(use www.cgi)
(use www.cgi.session.file)

;; Note that the parameter *session-cookie-domain* is set correctly.
(*session-cookie-domain* "localhost")

(define (main args)
  (cgi-main
   (lambda (params)
     (let ((session (session-begin <cgi-session-file>)))
       `(,(cgi-header :cookies (construct-cookie-string session))
         ,(html-doctype)
         ,(html:html
           (html:head (html:title "example: counter"))
           (html:body
            (html:h1 "Counter")
            (html:p "Your deposit: "
                    (let ((current (session-get session 'counter)))
                      (session-set session 'counter (if current (+ current 1) 1))
                      (or current
                          0))))))))))
