;;;
;;; www.cgi.session - php-like session utility
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
  (use file.util)
  (use gauche.parameter)
  (use rfc.base64)
  (use rfc.cookie)
  (use rfc.sha1)
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use srfi-19)
  (use srfi-27)
  (use text.tree)
  (use util.digest)
  (use www.cgi)
  (export <cgi-session>
		  id-of path-of variables-of
		  destroyed? timestamp-of
		  *session-directory*
		  *session-max-age*
		  *session-cookie-name*
		  *session-cookie-domain*
		  *session-cookie-path*
		  *session-cookie-max-age*
		  construct-cookie-string
		  session-begin session-destroy
		  session-get-variable session-set-variable
		  session-let-variable session-and-let*-variable))

(select-module www.cgi.session)

(define (id->path id) (format #f "~a/sess_~a" (*session-directory*) id))
(define (path->id path) (string-scan (sys-basename path) "sess_" 'after))

(define (*session-gc*)
  (let ((dir (*session-directory*)))
	(directory-fold
	 dir
	 (lambda (path knil)
	   (when (and (file-is-regular? path)
				  (string-prefix? "sess_" (sys-basename path))
				  (> (sys-time) (+ (sys-stat->mtime (sys-stat path)) (*session-max-age*))))
		 (sys-unlink path)))
	 #f
   :lister (lambda (path knil) (if (string=? dir path) (directory-list path :add-path? #t :children? #t) '())))))

(define-class <cgi-session> ()
  ((id        :init-keyword :id
			  :getter id-of
			  :init-form (make-id))
   (path      :getter path-of
			  :allocation :virtual
			  :slot-ref (lambda (this)
						  (id->path (slot-ref this 'id))))
   (variables :getter variables-of
			  :allocation :virtual
			  :slot-ref (lambda (this)
						  (and (not (slot-ref this 'destroyed))
							   (let ((path (slot-ref this 'path)))
								 (begin0
								   (call-with-input-file path
									 (lambda (iport)
									   (and-let* ((iport)
												  (s (sys-stat->mtime (sys-fstat iport)))
												  (alive (<= (sys-time) (+ s (*session-max-age*))))
												  (variables (read iport)))
										 (if (eof-object? variables)
											 '()
											 variables)))
									 :if-does-not-exist #f)
								   (touch-file path)))))
			  :slot-set! (lambda (this value)
						   (unless (slot-ref this 'destroyed)
							 (call-with-output-file (slot-ref this 'path)
							   (cut write value <>)))
						   (*session-gc*)))
   (destroyed :getter destroyed?
			  :allocation :virtual
			  :slot-ref (lambda (this)
						  (not (file-is-regular? (slot-ref this 'path))))
			  :slot-set! (lambda (this value)
						   (when value (sys-unlink (slot-ref this 'path)))))
   (timestamp :getter timestamp-of
			  :allocation :virtual
			  :slot-ref (lambda (this)
						  (and (not (slot-ref this 'destroyed))
							   (call-with-input-file (slot-ref this 'path)
								 (lambda (iport)
								   (and iport (sys-stat->mtime (sys-fstat iport))))
								 :if-does-not-exist #f))))))

(define *session-directory* (make-parameter "/tmp"))
(define *session-max-age* (make-parameter 86400))

(define *session-cookie-name* (make-parameter "gauche"))
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

;
; make-id
;
; generate a ``unique'' ID for the session.
;
(define (make-id)
;  (random-source-randomize! default-random-source)
  (let ((seed
		 (digest-hexify
		  (sha1-digest-string
;    (let ((addr (sys-getenv "REMOTE_ADDR")))
		   (format #f "~d~a~d"
				   (sys-time)
				   (or (cgi-get-metavariable "REMOTE_ADDR") "unknown-host")
				   (random-real))))))
	(receive (oport path)
		(sys-mkstemp (id->path seed))
	  (close-output-port oport)
	  (path->id path))))

(define (cookie->id cookie-name)
;  (let ((cookie-string (sys-getenv "HTTP_COOKIE")))
  (and-let* ((cookie-string (cgi-get-metavariable "HTTP_COOKIE"))
			 (s (assoc cookie-name (parse-cookie-string cookie-string))))
	(cadr s)))

;
; session-begin
;
; Initializes a session by instantiating wwwsession.
; First it looks for a cookie attribute indicating an existing session
; and if it does not find one it creates a ``unique'' session ID.
; The variable list is initialized to null and the session filename
; is determined.
;
(define (session-begin . opt)
  (let ((id (cookie->id (if (null? opt) (*session-cookie-name*) (car opt)))))
	(if (and id (file-is-regular? (id->path id)))
		(make <cgi-session> :id id)
		(make <cgi-session>))))

;
; session-get-variable
;
; gets the value of a session variable. returns #f if the variable name
; is not found.
;
(define-method session-get-variable ((session <cgi-session>) symbol . opt)
  (let-keywords* opt ((consume #f))
	(and-let* ((vars (variables-of session))
			   (pair (assq symbol vars)))
	  (when consume (session-set-variable session symbol #f))
	  (cdr pair))))

;
; session-set-variable
;
; sets the value of a session variable. it removes an existing value
; first.
;
(define-method session-set-variable ((session <cgi-session>) . rest)
  (let lp ((rest rest)
		   (vars (or (variables-of session) '())))
	(if (null? rest)
		(begin
		  (slot-set! session 'variables vars)
		  session)
		(let ((symbol (car rest))
			  (value (cadr rest)))
		  (if value
			  (lp (cddr rest)
				  (acons symbol value (remove (lambda (x) (eq? symbol (car x))) vars)))
			  (lp (cddr rest)
				  (remove (lambda (x) (eq? symbol (car x))) vars)))))))

;
; session-destroy
;
; deletes the session data file.
;
(define-method session-destroy ((session <cgi-session>))
  (slot-set! session 'destroyed #t))

;; some of macros
(define-syntax session-let-variable
  (syntax-rules ()
	((_ session ((a b ...) ...) c ...)
	  (let ((a (session-get-variable session b ...)) ...) c ...))))

(define-syntax session-and-let*-variable
  (syntax-rules ()
	((_ session ((a b ...) ...) c ...)
	  (and-let* ((a (session-get-variable session b ...)) ...) c ...))))

(provide "www/cgi/session")
