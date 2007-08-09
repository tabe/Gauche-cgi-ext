;;;
;;; www.cgi.session - abstract session handler
;;;
;;;   Copyright (c) 2007 Takeshi Abe. All rights reserved.
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;  $Id$

(define-module www.cgi.session
  (use gauche.parameter)
  (use rfc.base64)
  (use rfc.cookie)
  (use rfc.sha1)
  (use srfi-1)
  (use srfi-2)
  (use srfi-27)
  (use util.digest)
  (use www.cgi)
  (export <cgi-session-meta> <cgi-session>
          id-of variables-of destroyed? timestamp-of
          *session-max-age*
          *session-cookie-name*
          *session-cookie-domain*
          *session-cookie-path*
          *session-cookie-max-age*
          construct-cookie-string
          session-begin session-close session-destroy
          session-get session-set
          session-let session-and-let*))

(select-module www.cgi.session)

(define-class <cgi-session-meta> (<class>)
  ())

(define-class <cgi-session> ()
  ((id :init-keyword :id
       :getter id-of))
  :metaclass <cgi-session-meta>)

(define-method variables-of ((session <cgi-session>))
  '())
(define-method destroyed? ((session <cgi-session>))
  #f)
(define-method timestamp-of ((session <cgi-session>))
  #f)

(define *session-max-age* (make-parameter 86400))

(define *session-cookie-name* (make-parameter "sid"))
(define *session-cookie-domain* (make-parameter (sys-gethostname)))
(define *session-cookie-path* (make-parameter "/"))
(define *session-cookie-max-age* (make-parameter 86400))

(define-method construct-cookie-string ((session <cgi-session>) . specs)
  (let-keywords* specs
      ((name (*session-cookie-name*))
       (domain (*session-cookie-domain*))
       (path (*session-cookie-path*))
       (max-age (*session-cookie-max-age*))
       (expires (+ (sys-time) (*session-cookie-max-age*))))
    (let ((params `(,name
                    ,(id-of session)
                    :domain ,domain
                    :path ,path
                    :expires ,expires
                    :max-age ,max-age)))
      (construct-cookie-string (list params) 0))))

;; generate a ``unique'' ID for the session.
(define-method make-id ((class <cgi-session-meta>))
  (digest-hexify
   (sha1-digest-string
    (format #f "~d~a~d"
            (sys-time)
            (or (cgi-get-metavariable "REMOTE_ADDR") "unknown-host")
            (random-real)))))

(define-method valid-id? ((class <cgi-session-meta>) id)
  id)

(define-method set-variables ((session <cgi-session>) vars)
  #f)

(define (cookie->id cookie-name)
  (and-let* ((cookie-string (cgi-get-metavariable "HTTP_COOKIE"))
             (s (assoc cookie-name (parse-cookie-string cookie-string))))
    (cadr s)))

;; Initializes a session by instantiating <cgi-session>.
;; First it looks for a cookie attribute indicating an existing session
;; and if it does not find one it creates a ``unique'' session ID.
;; The variable list is initialized to null and the session filename
;; is determined.
(define-method session-begin ((class <cgi-session-meta>) . opt)
  (let ((id (cookie->id (if (null? opt) (*session-cookie-name*) (car opt)))))
    (make class :id (if (valid-id? class id) id (make-id class)))))

;; gets the value of a session variable.
;; returns #f if the variable name is not found.
(define-method session-get ((session <cgi-session>) symbol . opt)
  (let-keywords* opt ((consume #f))
    (and-let* ((vars (variables-of session))
               (pair (assq symbol vars)))
      (when consume (session-set session symbol #f))
      (cdr pair))))

;; sets the value of a session variable. it removes an existing value
;; first.
(define-method session-set ((session <cgi-session>) . rest)
  (let lp ((rest rest)
           (vars (or (variables-of session) '())))
    (if (null? rest)
        (begin
          (set-variables session vars)
          session)
        (let ((symbol (car rest))
              (value (cadr rest)))
          (if value
              (lp (cddr rest)
                  (acons symbol value (remove (lambda (x) (eq? symbol (car x))) vars)))
              (lp (cddr rest)
                  (remove (lambda (x) (eq? symbol (car x))) vars)))))))

;; closes session after flushing its variables.
(define-method session-close ((session <cgi-session>))
  #f)

;; deletes the session data.
(define-method session-destroy ((session <cgi-session>))
  #f)

;;
;; some of macros
;;
(define-syntax session-let
  (syntax-rules ()
    ((_ session ((a b ...) ...) c ...)
      (let ((a (session-get session b ...)) ...) c ...))))

(define-syntax session-and-let*
  (syntax-rules ()
    ((_ session ((a b ...) ...) c ...)
     (and-let* ((a (session-get session b ...)) ...) c ...))))

(provide "www/cgi/session")
