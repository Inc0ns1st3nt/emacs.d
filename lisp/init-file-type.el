;; -*- coding: utf-8; lexical-binding: t; -*-

;;; {{ shell and conf
(add-auto-mode 'conf-mode
               "\\.[^b][^a][a-zA-Z]*rc$"
               "\\.aspell\\.en\\.pws$"
               "\\.i3/config-base$"
               "\\mimeapps\\.list$"
               "\\mimeapps\\.list$"
               "\\.editorconfig$"
               "\\.meta$"
               "\\.?muttrc$"
               "\\.mailcap$")
;; }}

;; {{ emacs lisp
(add-auto-mode 'emacs-lisp-mode
               "\\.emacs-project\\'"
               "archive-contents\\'"
               "\\.emacs\\.bmk\\'")
;; }}

;; {{ ruby
(add-auto-mode 'ruby-mode
               "\\.\\(rb\\|rake\\|rxml\\|rjs\\|irbrc\\|builder\\|ru\\|gemspec\\)$"
               "\\(Rakefile\\|Gemfile\\)$")

;; }}

;; {{ perl
;; Use cperl-mode instead of the default perl-mode
(add-auto-mode 'cperl-mode
               "\\.\\([pP][Llm]\\|al\\)$"
               "\\.\\([pP][Llm]\\|al\\)$")

(add-to-list 'interpreter-mode-alist
			 (cons "perl5?\\|minperl" 'cperl-mode))
;; }}

(add-auto-mode 'text-mode
               "TAGS\\'"
               "\\.ctags\\'")

(add-auto-mode 'java-mode
               ;; java
               "\\.aj\\'"
               ;; makefile
               "\\.ninja$" )

(add-auto-mode 'groovy-mode
               "\\.groovy\\'"
               "\\.gradle\\'" )

(add-auto-mode 'sh-mode
               "\\.bash\\(_profile\\|_history\\|rc\\.local\\|rc\\)?$"
               "\\.z?sh$"
               "\\.env$")

(add-auto-mode 'cmake-mode
               "CMakeLists\\.txt\\'"
               "\\.cmake\\'" )

;; vimrc
(add-auto-mode 'vimrc-mode "\\.?vim\\(rc\\)?$")

(add-auto-mode 'texile-mode "\\.textile\\'")

(add-auto-mode 'tcl-mode "Portfile\\'")

;; pyim
(add-auto-mode 'text-mode "\\.pyim\\'")

;; arduino setup
;; (add-auto-mode 'c-mode "\\.\\(pde\\|ino\\)$")

;; objc-mode
;; (rx (or ".xm" ".x"))
(add-to-list 'auto-mode-alist `(,(regexp-opt '(".xm" ".x") t) . objc-mode))

;; {{ web/html
(add-auto-mode 'web-mode
               "\\.\\(cmp\\|app\\|page\\|component\\|wp\\|vue\\|tmpl\\|php\\|module\\|inc\\|hbs\\|tpl\\|[gj]sp\\|as[cp]x\\|erb\\|mustache\\|djhtml\\|ftl\\|[rp]?html?\\|xul?\\|eex?\\|xml?\\|jst\\|ejs\\|erb\\|rbxlx\\)$")
;; }}

(add-auto-mode 'snippet-mode "\\.yasnippet\\'")

;; racket
(add-auto-mode 'racket-mode "\\.rkt$")

(provide 'init-file-type)
