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
		 ("前者はセッションを抽象化したオブジェクトです。後者はそのメタクラスです。"))

		((method (variables-of (session <cgi-session>)))
		 ("Return an alist consisting of pairs (name . value) of session variables.")
		 ("セッション変数の名前と値からなるペアの連想配列を返します。"))

		((method (destroyed? (session <cgi-session>)))
		 ("Return #t if session is destroyed.")
		 ("セッションが既に破壊されていたら真を返します。"))

		((method (timestamp-of (session <cgi-session>)))
		 ("Return the timesampe of session.")
		 ("セッションのタイムスタンプを返します。"))

        ((parameter *session-max-age*
                    *session-cookie-name*
                    *session-cookie-domain*
                    *session-cookie-path*
                    *session-cookie-max-age*)
         ("These are used for the setting of session. *session-max-age* represents the max age of session. *session-cookie-max-age*, *session-cookie-domain*, *session-cookie-path*, or *session-cookie-max-age* corresponds to the name, domain name, path, or max age of cookie of the session, respectively.")
         ("これらのパラメーターはセッションに関する設定に利用できます。*session-max-age* はセッションの保持期間を表します。*session-cookie-name*、*session-domain-name*、*session-cookie-path*、および *session-cookie-max-age* はそれぞれセッション ID を格納するクッキーの名前、ドメイン名、パス、およびクッキーの有効期限です。"))

        ((method (make-id (class <cgi-session-meta>)))
         ("THIS METHOD IS PRIVATE FOR IMPLEMENTATION OF SESSION: ")
         ("このメソッドはセッションの実装する場合にのみ関係します。"))

        ((method (valid-id? (class <cgi-session-meta>) id))
         ("THIS METHOD IS PRIVATE FOR IMPLEMENTATION OF SESSION: ")
         ("このメソッドはセッションの実装する場合にのみ関係します。"))

        ((method (set-variables (session <cgi-session>) vars))
         ("THIS METHOD IS PRIVATE FOR IMPLEMENTATION OF SESSION: ")
         ("このメソッドはセッションの実装する場合にのみ関係します。"))

        ((method (session-begin (class <cgi-session-meta>) &optional rest))
         ("Begin a session of class class, and return it.")
         ("与えられたクラス class のセッションを開始し、セッションを返します。"))

        ((method (session-get (session <cgi-session>) symbol &optional opt))
         ("Return the value of name symbol.")
         ("名前 symbol を持つセッション変数の値を返します。"))

        ((method (session-set (session <cgi-session>) &optional rest))
         ("Set the session variables, e.g.:")
         ("セッション変数の名前と値をこの順番で割り当てます。複数の名前と値を指定できます:"))

        ((method (session-close (session <cgi-session>)))
         ("Save and close the session.")
         ("セッションを保存し閉じます。"))

        ((method (session-destroy (session <cgi-session>)))
         ("Destroy the session.")
         ("セッションを破壊します。"))

		((macro session-let
                session-and-let*)
		 ("A utility for binding variables according to session variables.")
		 ("セッション変数を変数に割り当てるための便利なマクロです。"))
		))

(define-macro (api-misc lang)
  `(def ,lang
		((macro cgi-let-parameter
                cgi-and-let*-parameter)
		 ("A utility for bining variables according to cgi parameters.")
		 ("CGI パラメータを変数に割り当てるための便利なマクロです。"))))

(define (document-tree lang)
  (let ((title (if (eq? 'ja lang) "Gauche-cgi-ext リファレンスマニュアル" "Gauche-cgi-ext Reference Manual")))
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
		  (html:p (html:span :style "color:red;" "警告:") " 今後変更の可能性があります。"))
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
