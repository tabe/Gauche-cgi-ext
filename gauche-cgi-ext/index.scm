#!/usr/bin/env gosh

(use file.util)
(use fixedpoint.site)
(use text.html-lite)
(use text.tree)

(//
 (PHP "http://www.php.net/")
 (memcached "http://www.danga.com/memcached/")
 )

(define *last-update* "Fri Aug 10 2007")
(define *gauche-cgi-ext-version* (file->string "../VERSION"))
(define *gauche-cgi-ext-tarball-basename* (string-append "Gauche-cgi-ext-" *gauche-cgi-ext-version* ".tgz"))
(define *gauche-cgi-ext-tarball-size* (file-size (string-append "../../" *gauche-cgi-ext-tarball-basename*)))
(define *gauche-cgi-ext-tarball-url* *gauche-cgi-ext-tarball-basename*)

(define (index lang)
  (let-syntax ((en/ja (syntax-rules ()
						((_ en ja)
						 (if (string=? "en" lang) en ja)))))
	((fixedpoint:frame "Gauche-cgi-ext")
	 (html:p :id "lang_navi" (html:a :href (en/ja "index.html" "index.en.html")
										"[" (en/ja "Japanese" "English") "]"))
	 (html:p :id "last_update" "Last update: " *last-update*)
	 (fixedpoint:separator)
	 (fixedpoint:adsense)
	 (fixedpoint:separator)
	 (html:p (html:dfn /Gauche-cgi-ext/)
			 (en/ja
				 (list " is an extension package of " /Gauche/ " containing a kind of CGI utilities such as session.")
				 (list " は " /Scheme/ " 処理系 " /Gauche/ " で CGI で利用するセッションなどの機能を提供する拡張パッケージです。")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "News" "最新情報"))
	 (html:ul
	  (html:li "[2007-08-10] " (en/ja "Release 0.1.0." "バージョン 0.1.0 を公開しました。")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Features" "特徴"))
	 (html:ul
	  (html:li (en/ja "an extensible session handling."
					  "拡張可能なセッションハンドリング"))
      (html:li (en/ja "an implementation of session based on files."
                      "ファイルによるセッションの実装。"))
	  (html:li (en/ja "some of neat macros."
					  "いくつかの便利なマクロ。"))
	  )

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Requirements" "導入"))
	 (html:p (en/ja "This package is for Gauche 0.8.10 or later."
					"このパッケージは Gauche 0.8.10 またはそれ以上で動作します。"))
	 (html:ul
	  (html:li (en/ja (list "If you would like memcache-based session, "
                            /memcached/ " and also " /Gauche-memcache/ " are necessary.")
					  (list "また memcache によるセッションを利用する場合には"
                            /memcached/ " および "/Gauche-memcache/ " が必要です。"))))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Download" "ダウンロード"))
	 (html:p (html:a :href *gauche-cgi-ext-tarball-url*
					 *gauche-cgi-ext-tarball-basename* " (" *gauche-cgi-ext-tarball-size*  " bytes)"))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Documentation" "文書"))
	 (html:ul
	  (html:li (html:a :href (en/ja "reference.en.html" "reference.ja.html")
					   "Gauche-cgi-ext " (en/ja "Reference Manual" "リファレンスマニュアル"))))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" "FYI")
	 (html:ul
	  (html:li /PHP/)
	  (html:li /memcached/)
	  (html:li /Gauche-memcache/)
	  )
	 )))

(define (main args)
  (define (usage)
	(format (current-error-port) "usage: gosh ~a (en|ja)\n" *program-name*)
	(exit 1))
  (when (< (length args) 2)
	(usage))
  (write-tree (index (cadr args)))
  0)
