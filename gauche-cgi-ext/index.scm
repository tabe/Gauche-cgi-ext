#!/usr/bin/env gosh
;; -*- encoding: utf-8 -*-

(use fixedpoint.package)
(use fixedpoint.site)
(use text.html-lite)

(//
 (PHP "http://www.php.net/")
 (memcached "http://www.danga.com/memcached/")
 )

(define-package Gauche-cgi-ext 2007 8 10)

(define-index Gauche-cgi-ext
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
  (*package-download*)

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
  )

(define main package-main)
