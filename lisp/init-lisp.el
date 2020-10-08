;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'racket-mode)

(defun show-scratch-buffer-message ()
  (if-let ((fortune-prog (or (executable-find "fortune-zh")
                             (executable-find "fortune"))))
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; "                    ; comment each line
        (replace-regexp-in-string
         "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9]m *\\)" "" ; remove trailing linebreak
         (shell-command-to-string fortune-prog))))
    (concat ";; Happy hacking "
            (or user-login-name "")
            " - Emacs loves you!\n\n")))

(setq-default initial-scratch-message (show-scratch-buffer-message))

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") #'find-function-on-key)

;; paredit
(with-eval-after-load 'paredit
  (diminish 'paredit-mode " Par"))

;; elisp
(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun elisp-mode-hook-setup ()
  (unless (buffer-file-temp-p)
    (util/ensure 'eldoc)
    (turn-on-eldoc-mode)
    (enable-paredit-mode)
    (rainbow-delimiters-mode t)
    (set-up-hippie-expand-for-elisp)
    (checkdoc-minor-mode 1)))
(add-hook 'emacs-lisp-mode-hook #'elisp-mode-hook-setup)

;; ----------------------------------------------------------------------------
;; Enable desired features for all lisp modes
;; ----------------------------------------------------------------------------
(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)
  (turn-on-eldoc-mode))

(dolist (hook '(lisp-mode-hook
                racket-mode-hook
                inferior-lisp-mode-hook
                lisp-interaction-mode-hook
                ;;
                scheme-mode-hook
                gerbil-mode-hook))
  (add-hook hook #'sanityinc/lisp-setup))

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
  ;; (let ((slime-lisp-implementations
  ;;        )))
  (slime))

(defun slime-common-lisp ()
  (interactive)
  (setq '((sbcl ("/usr/bin/sbcl"))))
  (slime))

;; gerbil tag table

;; Gerbil package manager generates TAGS tables for all installed packages at this path
;; (add-to-list 'tags-table-list "~/.gerbil/pkg/TAGS")
;; to generate tags for your own code by using gxtags. The invocation is very simple:
;; gxtags [-a] [-o TAGS] source-file-or-directory ...

(provide 'init-lisp)
