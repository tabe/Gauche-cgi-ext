#!/usr/local/gauche/bin/gosh


;; Define the module interface
(define-module session
  (use www.cgi)
  (use srfi-27)
  (use rfc.base64)
  (use srfi-19)
  (use rfc.sha1)
  (use util.digest)
  (use rfc.cookie)
  (use srfi-1)
  (use text.tree)
  (export session-write session-begin session-read session-get-var
	  session-set-var session-get-id session-destroy))
  
;; Enter the module
(select-module session)

(define-class <wwwsession> ()
  ((sid       :init-keyword :sid       :accessor sid-slot)
   (varlist   :init-keyword :varlist   :accessor varlist-slot)
   (filename  :init-keyword :filename  :accessor filename-slot)
   (existing  :init-keyword :existing  :accessor existing-slot)
   (destroyed :init-keyword :destroyed :accessor destroyed-slot)
   (stamp     :init-keyword :stamp     :accessor stamp-slot)))

;
; session-get-id
;
; convienence!
;
(define (session-get-id session)
  (sid-slot session))

;
; session-write
;
; write a file that contains the session variable array along with a
; date stamp.
;
; writes things out as base64 to avoid hairy characters causing trouble.
;
(define (session-write session)
  (if (not (destroyed-slot session))
      (with-output-to-port (open-output-file (filename-slot session))
	(lambda ()
	  (print (stamp-slot session))
	  (for-each
	   (lambda (var)
	     (print (base64-encode-string (car var)))
	     (print (base64-encode-string (format #f "~a" (cdr var)))))
	   (varlist-slot session))))))

;
; session-read
;
; read a file that contains session variable data.
; if the current time minus the date stamp is greater
; than max-age then return an empty list.
;
(define (session-read-vars varlist)
  (let ((varname  (read-line))
	(val      (read-line)))
    (if (or (eof-object? varname) (eof-object? val))
	varlist
	(session-read-vars (acons (base64-decode-string varname)
				  (base64-decode-string val)
				  varlist)))))

(define (session-read session max-age)
  (slot-set!
   session 'varlist
   (with-error-handler
    (lambda (eo)
      (slot-set! session 'stamp (time->seconds (current-time)))
      '())
    (lambda ()
      (with-input-from-port (open-input-file (filename-slot session))
	(lambda ()
	  (slot-set! session 'stamp (string->number (read-line)))
	  (if (> (time->seconds (current-time))
		 (+ (stamp-slot session) max-age))
	      ((lambda ()
		 (slot-set! session 'stamp (time->seconds (current-time)))
		 '()))
	      (session-read-vars '()))))))))

;
; session-make-id
;
; generate a ``unique'' ID for the session.
;
(define (session-make-id)
  (random-source-randomize! default-random-source)
  (digest-hexify
   (sha1-digest-string
    (let ((addr (sys-getenv "REMOTE_ADDR")))
      (format #f "~d~a~d"
	      (time->seconds (current-time))
	      (if (not addr) "unknown-host" addr)
	      (random-real))))))

(define (session-retrieve-id new-id cookie-name)
  (let ((cookie-string (sys-getenv "HTTP_COOKIE")))
    (if (or (not cookie-string) new-id)
	(session-make-id)
	(let ((s (assoc cookie-name (parse-cookie-string cookie-string))))
	  (if (not s)
	      (session-make-id)
	      (car (cdr s)))))))

(define (session-existing-id cookie-name)
  (let ((cookie-string (sys-getenv "HTTP_COOKIE")))
    (if (not cookie-string)
	#f
	(let ((s (assoc cookie-name (parse-cookie-string cookie-string))))
	  (if (not s)
	      #f
	      (car (cdr s)))))))

;
; session-make-filename
;
; The session filename is hashed to prevent weirdo
; characters being snuck into the filename by a malicious cookie
; attribute.
;
(define (session-make-filename session-id session-dir)
  (format #f "~a/~a" session-dir (digest-hexify session-id)))

;
; session-begin
;
; Initializes a session by instantiating wwwsession.
; First it looks for a cookie attribute indicating an existing session
; and if it does not find one it creates a ``unique'' session ID.
; The variable list is initialized to null and the session filename
; is determined.
;
(define (session-begin new-id session-dir cookie-name)
  (let ((existing #t)
	(sid #f))
    (if (not new-id)
	(set! sid (session-existing-id cookie-name))
	(set! existing #f))
    (if (not sid) (set! sid (session-make-id)))
     (make <wwwsession>
      :varlist '()
      :sid sid
      :destroyed #f
      :existing existing
      :stamp (time->seconds (current-time))
      :filename (session-make-filename sid session-dir))))

;
; session-get-var
;
; gets the value of a session variable.  returns #f if the variable name
; is not found.
;
(define (session-get-var session varname)
  (let ((vpair (assoc varname (varlist-slot session))))
    (if (not vpair) #f (cdr vpair))))

;
; session-set-var
;
; sets the value of a session variable.  it removes an existing value
; first.
;
(define (session-set-var session varname value)
  (slot-set! session 'varlist
   (acons varname value
	  (remove (lambda (x) (string=? varname (car x)))
		  (varlist-slot session)))))

;
; session-destroy
;
; deletes the session data file.
;
(define (session-destroy session)
  (slot-set! session 'destroyed #t)
  (sys-unlink (filename-slot session)))

;
;
; Test function
;
; Currently very trivial.  And broken.
;

(define (session-test)
  (let ((session (session-begin #f))
	(count 0))
    (session-read session)
    (set! count (session-get-var session "count"))
    (if (not count) (set! count 0) (set! count (+ (string->number count) 1)))
    (session-set-var session "count" count)
    (session-write session)
    (display
     (tree->string
      (cgi-header
       :cookies (construct-cookie-string
		 (list (list "SESSIONNAME" (sid-slot session)))))))
    (print count)))

;(define (main args) (session-test))

;; Module stuff
(provide "session")
