;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'racket-mode)
(require-package 'paredit)
(require-package 'gambit)

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

(autoload 'slime-dispatch-media-event "slime-media" nil)

(with-eval-after-load 'slime
  ;; (require 'slime-media)
  ;; in-case not loaded properly, i will do it mysel
  (add-hook 'slime-event-hooks 'slime-dispatch-media-event)
  ;; (setq slime-enable-evaluate-in-emacs nil)
  ;;
  (setq slime-lisp-implementations
		'((sbcl ("/usr/bin/sbcl"))
		  (gerbil-scheme ("gxi" "-:d-") :init gerbil-scheme-start-swank))))

(defun slime-gerbil ()
  (interactive)
  (setq slime-lisp-implementations
        '((gerbil-scheme ("gxi" "-:d-") :init gerbil-scheme-start-swank)))
  (slime))

(defun slime-common-lisp ()
  (interactive)
  (setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
  (slime))

;;; gerbil

;; gambit
(autoload 'gambit-inferior-mode "gambit" "gambit package for gerbil")
(add-hook 'inferior-scheme-mode-hook #'gambit-inferior-mode)

;; gerbil setup
(defvar inc0n/gerbil-home (getenv "GERBIL_HOME"))
(let ((gerbil-program-name (concat inc0n/gerbil-home "/bin/gxi")))
  ;; gerbil mode
  (add-to-list 'load-path (concat inc0n/gerbil-home "/etc/"))
  (autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
  ;; gerbil tags
  ;; (add-to-list 'tags-table-list (concat inc0n/gerbil-home "/src/TAGS"))
  (setq scheme-program-name gerbil-program-name))

(defun gerbil-mode-setup ()
  (when-let ((tag (locate-dominating-file (concat inc0n/gerbil-home "/src/") "TAGS")))
	(visit-tags-table tag t)))
(add-hook 'gerbil-mode-hook 'gerbil-mode-setup)

(add-auto-mode 'gerbil-mode "\\.ss$")

;; gerbil tag table

;; Gerbil package manager generates TAGS tables for all installed packages at this path
;; (add-to-list 'tags-table-list "~/.gerbil/pkg/TAGS")
;; to generate tags for your own code by using gxtags. The invocation is very simple:
;; gxtags [-a] [-o TAGS] source-file-or-directory ...

(provide 'init-lisp)
;;; init-lisp ends here
