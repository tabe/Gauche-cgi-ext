#!/usr/bin/env gosh
;; -*- coding: euc-jp -*-

(use text.html-lite)
(use text.tree)

(define *version* "0.1.0")
(define *last-update* "Fri Aug 10 2007")

(define-syntax def
  (syntax-rules (en ja procedure method)
	((_ lang)
	 '())
	((_ en ((type name ...) (p ...) (q ...)) rest ...)
	 (def en ((type name ...) (p ...)) rest ...))
	((_ ja ((type name ...) (p ...) (q ...)) rest ...)
	 (def ja ((type name ...) (q ...)) rest ...))
	((_ lang ((procedure (name arg ...) ...) (p ...)) rest ...)
	 (list*
	  (html:h3 (html:span :class "type" "procedure") ": "
			   (html:span :class "procedure" (html-escape-string (symbol->string 'name))) " "
			   (cons (html:span :class "argument" 'arg) " ") ...)
	  ...
	  (html:p (html-escape-string p))
	  ...
	  (html:hr)
	  (def lang rest ...)))
	((_ lang ((method (name arg ...) ...) (p ...)) rest ...)
	 (list*
	  (html:h3 (html:span :class "type" "method") ": "
			   (html:span :class "method" (html-escape-string (symbol->string 'name))) " "
			   (cons (html:span :class "argument" (html-escape-string (x->string 'arg))) " ") ...)
	  ...
	  (html:p (html-escape-string p))
	  ...
	  (html:hr)
	  (def lang rest ...)))
	((_ lang ((type name ...) (p ...)) rest ...)
	 (list*
	  (html:h3 (html:span :class "type" 'type) ": "
			   (html:span :class 'type (html-escape-string (symbol->string 'name))))
	  ...
	  (html:p (html-escape-string p))
	  ...
	  (html:hr)
	  (def lang rest ...)))))

(define-macro (api-session lang)
  `(def ,lang
		((class <cgi-session>
                <cgi-session-meta>)
		 ("The former abstracts the current session while the latter is its metaclass.")
		 ("���Ԥϥ��å�������ݲ��������֥������ȤǤ�����ԤϤ��Υ᥿���饹�Ǥ���"))

		((method (variables-of (session <cgi-session>)))
		 ("Return an alist consisting of pairs (name . value) of session variables.")
		 ("���å�����ѿ���̾�����ͤ���ʤ�ڥ���Ϣ��������֤��ޤ���"))

		((method (destroyed? (session <cgi-session>)))
		 ("Return #t if session is destroyed.")
		 ("���å���󤬴����˲�����Ƥ����鿿���֤��ޤ���"))

		((method (timestamp-of (session <cgi-session>)))
		 ("Return the timesampe of session.")
		 ("���å����Υ����ॹ����פ��֤��ޤ���"))

        ((parameter *session-max-age*
                    *session-cookie-name*
                    *session-cookie-domain*
                    *session-cookie-path*
                    *session-cookie-max-age*)
         ("These are used for the setting of session. *session-max-age* represents the max age of session. *session-cookie-max-age*, *session-cookie-domain*, *session-cookie-path*, or *session-cookie-max-age* corresponds to the name, domain name, path, or max age of cookie of the session, respectively.")
         ("�����Υѥ�᡼�����ϥ��å����˴ؤ�����������ѤǤ��ޤ���*session-max-age* �ϥ��å������ݻ����֤�ɽ���ޤ���*session-cookie-name*��*session-domain-name*��*session-cookie-path*������� *session-cookie-max-age* �Ϥ��줾�쥻�å���� ID ���Ǽ���륯�å�����̾�����ɥᥤ��̾���ѥ�������ӥ��å�����ͭ�����¤Ǥ���"))

        ((method (make-id (class <cgi-session-meta>)))
         ("THIS METHOD IS PRIVATE FOR IMPLEMENTATION OF SESSION: ")
         ("���Υ᥽�åɤϥ��å����μ���������ˤΤߴط����ޤ���"))

        ((method (valid-id? (class <cgi-session-meta>) id))
         ("THIS METHOD IS PRIVATE FOR IMPLEMENTATION OF SESSION: ")
         ("���Υ᥽�åɤϥ��å����μ���������ˤΤߴط����ޤ���"))

        ((method (set-variables (session <cgi-session>) vars))
         ("THIS METHOD IS PRIVATE FOR IMPLEMENTATION OF SESSION: ")
         ("���Υ᥽�åɤϥ��å����μ���������ˤΤߴط����ޤ���"))

        ((method (session-begin (class <cgi-session-meta>) &optional rest))
         ("Begin a session of class class, and return it.")
         ("Ϳ����줿���饹 class �Υ��å����򳫻Ϥ������å������֤��ޤ���"))

        ((method (session-get (session <cgi-session>) symbol &optional opt))
         ("Return the value of name symbol.")
         ("̾�� symbol ����ĥ��å�����ѿ����ͤ��֤��ޤ���"))

        ((method (session-set (session <cgi-session>) &optional rest))
         ("Set the session variables, e.g.:")
         ("���å�����ѿ���̾�����ͤ򤳤ν��֤ǳ�����Ƥޤ���ʣ����̾�����ͤ����Ǥ��ޤ�:"))

        ((method (session-close (session <cgi-session>)))
         ("Save and close the session.")
         ("���å�������¸���Ĥ��ޤ���"))

        ((method (session-destroy (session <cgi-session>)))
         ("Destroy the session.")
         ("���å������˲����ޤ���"))

		((macro session-let
                session-and-let*)
		 ("A utility for binding variables according to session variables.")
		 ("���å�����ѿ����ѿ��˳�����Ƥ뤿��������ʥޥ���Ǥ���"))
		))

(define-macro (api-misc lang)
  `(def ,lang
		((macro cgi-let-parameter
                cgi-and-let*-parameter)
		 ("A utility for bining variables according to cgi parameters.")
		 ("CGI �ѥ�᡼�����ѿ��˳�����Ƥ뤿��������ʥޥ���Ǥ���"))))

(define (document-tree lang)
  (let ((title (if (eq? 'ja lang) "Gauche-cgi-ext ��ե���󥹥ޥ˥奢��" "Gauche-cgi-ext Reference Manual")))
	(html:html
	 (html:head
	  (if (eq? 'ja lang) (html:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8") '())
	  (html:title title))
	 (html:body
	  (html:h1 title)
	  (html:style
	   :type "text/css"
	   "<!-- \n"
	   "h2 { background-color:#dddddd; }\n"
	   "address { text-align: right; }\n"
	   ".type { font-size: medium; text-decoration: underline; }\n"
	   ".procedure { font-size: medium; font-weight: normal; }\n"
	   ".method { font-size: medium; font-weight: normal; }\n"
	   ".argument { font-size: small; font-style: oblique; font-weight: normal; }\n"
	   ".constant { font-size: medium; font-weight: normal; }\n"
	   ".variable { font-size: medium; font-weight: normal; }\n"
	   "#last_update { text-align: right; font-size: small; }\n"
	   "#project { text-align: right; }\n"
	   " -->")
	  (html:p "For version " *version*)
	  (html:p :id "last_update" "last update: " *last-update*)
	  (html:p :id "project" (html:a :href "http://www.fixedpoint.jp/gauche-cgi-ext/" "http://www.fixedpoint.jp/gauche-cgi-ext/"))
	  (if (eq? 'en lang)
		  (html:p (html:span :style "color:red;" "Warning:") " still unstable.")
		  (html:p (html:span :style "color:red;" "�ٹ�:") " �����ѹ��β�ǽ��������ޤ���"))
      (html:h2 "API for Session")
	  (if (eq? 'en lang)
		  (api-session en)
		  (api-session ja))
      (html:h2 "Miscellaneous")
	  (if (eq? 'en lang)
		  (api-misc en)
		  (api-misc ja))
	  (html:address "&copy; 2007 Takeshi Abe")
	  ))))

(define (main args)
  (define (usage)
	(format (current-error-port) "usage: gosh reference.scm (en|ja)\n")
	(exit 1))
  (when (< (length args) 2)
	(usage))
  (write-tree (document-tree (string->symbol (cadr args))))
  0)
