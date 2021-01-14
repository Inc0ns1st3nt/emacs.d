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

;; {{ lisp like language
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

(add-interpreter-mode 'cperl-mode "perl5?\\|minperl")
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

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(add-auto-mode 'rust-mode "\\.rs\\'")

;; {{ verilog
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-auto-mode 'verilog-mode "\\.[ds]?vh?\\'")
;; }}

(add-auto-mode 'texile-mode "\\.textile\\'")

(add-auto-mode 'tcl-mode "Portfile\\'")

;; pyim
(add-auto-mode 'text-mode "\\.pyim\\'")

;; arduino setup
(add-auto-mode 'c-mode "\\.\\(pde\\|ino\\)$")

;; objc-mode
;; (rx (or ".xm" ".x"))
(add-to-list 'auto-mode-alist `(,(regexp-opt '(".xm" ".x") t) . objc-mode))

;; {{ web/html
(add-auto-mode 'web-mode
               "\\.\\(cmp\\|app\\|page\\|component\\|wp\\|vue\\|tmpl\\|php\\|module\\|inc\\|hbs\\|tpl\\|[gj]sp\\|as[cp]x\\|erb\\|mustache\\|djhtml\\|ftl\\|[rp]?html?\\|xul?\\|eex?\\|xml?\\|jst\\|ejs\\|erb\\|rbxlx\\)$")
;; }}

(add-auto-mode 'snippet-mode "\\.yasnippet\\'")

(add-auto-mode 'markdown-mode "\\.\\(m[k]d\\|markdown\\)\\'")

(add-auto-mode 'nov-mode "\\.epub\\'")

(add-auto-mode 'adoc-mode "\\.adoc\\'")

(add-auto-mode 'octave-mode "\\.m$")

;; racket
(add-auto-mode 'lisp-mode "\\.rkt\\'")

;; javascript
(add-auto-mode 'js-mode
               "\\.ja?son$"
               "\\.pac$"
               "\\.jshintrc$")

(add-auto-mode 'js2-mode "\\.js\\(\\.erb\\)?\\'")
;; JSX
(add-auto-mode 'rjsx-mode
               "\\.jsx\\'"
               "components\\/.*\\.js\\'")

;; mock file
(add-auto-mode 'js-mode "\\.mock.js\\'")

(add-auto-mode 'typescript-mode "\\.ts$")

(add-interpreter-mode 'js2-mode "node")

(provide 'init-file-type)
