;;;
;;; Test www.cgi-ext
;;;

(use gauche.test)

(test-start "www.cgi-ext")
(use www.cgi-ext)
(test-module 'www.cgi-ext)

(test-section "parameters")
(test* "*session-directory*" "/tmp" (*session-directory*))
(test* "*session-max-age*" 86400 (*session-max-age*))
(test* "*session-cookie-name*" "gauche" (*session-cookie-name*))
(test* "*session-cookie-domain*" (sys-gethostname) (*session-cookie-domain*))
(test* "*session-cookie-path*" "/" (*session-cookie-path*))
(test* "*session-cookie-max-age*" 86400 (*session-cookie-max-age*))

(*session-directory* "test")
(*session-max-age* 3600)
(*session-cookie-name* "test")
(*session-cookie-domain* "www.example.com")
(*session-cookie-path* "/foo/")
(*session-cookie-max-age* 3600)

(test-section "<cgi-session>")
(define session (session-begin "test"))
(print (id-of session))
(test* "id-of" #t (string? (id-of session)))
(print (path-of session))
(test* "path-of" #t (string? (path-of session)))
(test* "variables-of" '() (variables-of session))
(test* "destroyed?" #f (destroyed? session))
(print (timestamp-of session))
(test* "timestamp-of" #t (number? (timestamp-of session)))

;; epilogue
(test-end)
