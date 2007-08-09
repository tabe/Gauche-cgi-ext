;;;
;;; www.cgi.session.file - filesystem based handler
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

(define-module www.cgi.session.file
  (extend www.cgi.session)
  (use file.util)
  (use gauche.parameter)
  (use srfi-13)
  (export <cgi-session-file> *session-directory* path-of))

(select-module www.cgi.session.file)

(define-class <cgi-session-file-meta> (<cgi-session-meta>)
  ())

(define-class <cgi-session-file> (<cgi-session>)
  ()
  :metaclass <cgi-session-file-meta>)

(define *session-directory* (make-parameter "/tmp"))

(define (%id->path id) (format #f "~a/sess-~a" (*session-directory*) id))
(define (%path->id path) (string-scan (sys-basename path) "sess-" 'after))

(define (%gc)
  (let ((dir (*session-directory*)))
    (directory-fold
     dir
     (lambda (path knil)
       (when (and (file-is-regular? path)
                  (string-prefix? "sess-" (sys-basename path))
                  (> (sys-time) (+ (sys-stat->mtime (sys-stat path)) (*session-max-age*))))
         (sys-unlink path)))
     #f
   :lister (lambda (path knil) (if (string=? dir path) (directory-list path :add-path? #t :children? #t) '())))))

(define-method make-id ((class <cgi-session-file-meta>))
  (let ((seed (next-method)))
    (receive (oport path)
        (sys-mkstemp (%id->path seed))
      (close-output-port oport)
      (%path->id path))))

(define-method valid-id? ((class <cgi-session-file-meta>) id)
  (and (next-method)
       (file-is-regular? (%id->path id))))

(define-method path-of ((session <cgi-session-file>))
  (%id->path (id-of session)))

(define-method variables-of ((session <cgi-session-file>))
  (and (not (destroyed? session))
       (let ((path (path-of session)))
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

(define-method set-variables ((session <cgi-session-file>) vars)
  (unless (destroyed? session)
    (call-with-output-file (path-of session)
      (cut write vars <>)))
  (%gc))

(define-method destroyed? ((session <cgi-session-file>))
  (not (file-is-regular? (path-of session))))

(define-method session-close ((session <cgi-session-file>))
  #t)

(define-method session-destroy ((session <cgi-session-file>))
  (sys-unlink (path-of session)))

(define-method timestamp-of ((session <cgi-session-file>))
  (and (not (destroyed? session))
       (call-with-input-file (path-of session)
         (lambda (iport)
           (and iport (sys-stat->mtime (sys-fstat iport))))
         :if-does-not-exist #f)))

(provide "www/cgi/session/file")
