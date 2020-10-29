;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'racket-mode)
(require-package 'rainbow-delimiters)
(require-package 'paredit)

;; {{ eldoc
(with-eval-after-load 'eldoc
  ;; multi-line message should not display too soon
  (setq eldoc-idle-delay 0.5)
  (setq eldoc-echo-area-use-multiline-p t))
;;}}

;; elisp
(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;; lisp mode setups
(defun inc0n/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)
  (turn-on-eldoc-mode)
  (checkdoc-minor-mode 1))

(dolist (hook '(emacs-lisp-mode-hook
				lisp-mode-hook
                racket-mode-hook
                inferior-lisp-mode-hook
                lisp-interaction-mode-hook
                ;;
                scheme-mode-hook
                gerbil-mode-hook))
  (add-hook hook #'inc0n/lisp-setup))

;; racket
(with-eval-after-load 'racket-mode
  ;; this would breaks pyim (chinese input)
  ;; (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
  ;; (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)
  (setq racket-images-system-viewer "feh"))

;; slime swank

(defun gerbil-scheme-start-swank (file encoding)
  (format "%S\n\n" `(begin (import :drewc/r7rs/gerbil-swank) (start-swank ,file))))

(defun slime-gerbil ()
  (interactive)
  (setq slime-lisp-implementations
        '((gerbil-scheme ("gxi" "-:d-") :init gerbil-scheme-start-swank)))
  (slime))

(defun slime-common-lisp ()
  (interactive)
  (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
  (slime))

;; gerbil tag table

;; Gerbil package manager generates TAGS tables for all installed packages at this path
;; (add-to-list 'tags-table-list "~/.gerbil/pkg/TAGS")
;; to generate tags for your own code by using gxtags. The invocation is very simple:
;; gxtags [-a] [-o TAGS] source-file-or-directory ...

(provide 'init-lisp)
