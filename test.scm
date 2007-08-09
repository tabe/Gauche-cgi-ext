;;;
;;; Test www.cgi-ext
;;;

(use gauche.test)

(test-start "www.cgi.ext")
(use www.cgi.session.file)
(test-module 'www.cgi.session.file)

(test-section "parameters")
(test* "*session-max-age*" 86400 (*session-max-age*))
(test* "*session-cookie-name*" "sid" (*session-cookie-name*))
(test* "*session-cookie-domain*" (sys-gethostname) (*session-cookie-domain*))
(test* "*session-cookie-path*" "/" (*session-cookie-path*))
(test* "*session-cookie-max-age*" 86400 (*session-cookie-max-age*))

(test* "*session-directory*" "/tmp" (*session-directory*))

(*session-max-age* 3600)
(*session-cookie-name* "test")
(*session-cookie-domain* "www.example.com")
(*session-cookie-path* "/foo/")
(*session-cookie-max-age* 3600)
(*session-directory* "test")

(test-section "<cgi-session-file>")
(define session (session-begin <cgi-session-file>))
(test* "is-a?" #t (is-a? session <cgi-session-file>))
(test* "id-of" #t (string? (id-of session)))
(let ((p (path-of session)))
  (test* (format #f "path-of [~s]" p)
         #t
         (file-exists? p)))
(test* "variables-of (init)" '() (variables-of session))
(test* "destroyed?" #f (destroyed? session))
(let ((t (timestamp-of session)))
  (test* (format #f "timestamp-of [~s]" t)
         #t
         (number? t)))
(session-set session 'foo "bar")
(test* "session-get" "bar" (session-get session 'foo))
(session-set session 'x '(a b d) 'y 345)
(test* "variables-of" '((y . 345) (x . (a b d)) (foo . "bar")) (variables-of session))
(test* "session-destroy" #t (session-destroy session))
(test* "destroyed?" #t (destroyed? session))
(let ((p (path-of session)))
  (test* (format #f "destroyed? [~s]" p)
         #f
         (file-exists? p)))

;; epilogue
(test-end)
