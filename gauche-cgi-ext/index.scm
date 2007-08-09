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
				 (list " �� " /Scheme/ " ������ " /Gauche/ " �� CGI �����Ѥ��륻�å����ʤɤε�ǽ���󶡤����ĥ�ѥå������Ǥ���")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "News" "�ǿ�����"))
	 (html:ul
	  (html:li "[2007-08-10] " (en/ja "Release 0.1.0." "�С������ 0.1.0 ��������ޤ�����")))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Features" "��ħ"))
	 (html:ul
	  (html:li (en/ja "an extensible session handling."
					  "��ĥ��ǽ�ʥ��å����ϥ�ɥ��"))
      (html:li (en/ja "an implementation of session based on files."
                      "�ե�����ˤ�륻�å����μ�����"))
	  (html:li (en/ja "some of neat macros."
					  "�����Ĥ��������ʥޥ���"))
	  )

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Requirements" "Ƴ��"))
	 (html:p (en/ja "This package is for Gauche 0.8.10 or later."
					"���Υѥå������� Gauche 0.8.10 �ޤ��Ϥ���ʾ��ư��ޤ���"))
	 (html:ul
	  (html:li (en/ja (list "If you would like memcache-based session, "
                            /memcached/ " and also " /Gauche-memcache/ " are necessary.")
					  (list "�ޤ� memcache �ˤ�륻�å��������Ѥ�����ˤ�"
                            /memcached/ " ����� "/Gauche-memcache/ " ��ɬ�פǤ���"))))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Download" "���������"))
	 (html:p (html:a :href *gauche-cgi-ext-tarball-url*
					 *gauche-cgi-ext-tarball-basename* " (" *gauche-cgi-ext-tarball-size*  " bytes)"))

	 (html:h2 :style "border-bottom: 1px solid #bbbbbb;" (en/ja "Documentation" "ʸ��"))
	 (html:ul
	  (html:li (html:a :href (en/ja "reference.en.html" "reference.ja.html")
					   "Gauche-cgi-ext " (en/ja "Reference Manual" "��ե���󥹥ޥ˥奢��"))))

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
