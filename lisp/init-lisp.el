;; -*- coding: utf-8; lexical-binding: t; -*-

(defun show-scratch-buffer-message ()
  (let* ((fortune-prog (or (executable-find "fortune-zh")
                           (executable-find "fortune"))))
    (cond
     (fortune-prog
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; "                    ; comment each line
        (replace-regexp-in-string
         "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9]m *\\)" "" ; remove trailing linebreak
         (shell-command-to-string fortune-prog)))))
     (t
      (concat ";; Happy hacking "
              (or user-login-name "")
              " - Emacs loves you!\n\n")))))

(setq-default initial-scratch-message (show-scratch-buffer-message))

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") #'find-function-on-key)

;; paredit
(with-eval-after-load 'paredit
  (diminish 'paredit-mode " Par"))

(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")


;; gambit
(require 'gambit)
(add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)

;; gerbil setup
(defvar inc0n/gerbil-home (getenv "GERBIL_HOME"))
(let ((gerbil-program-name (concat inc0n/gerbil-home "/bin/gxi")))
  ;; gerbil mode
  (add-to-list 'load-path (concat inc0n/gerbil-home "/etc/"))
  (autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
  ;; gerbil tags
  (add-to-list 'tags-table-list (concat inc0n/gerbil-home "/src/TAGS"))
  (setq scheme-program-name gerbil-program-name))

(add-auto-mode 'gerbil-mode "\\.ss$")

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

(let* ((hooks '(lisp-mode-hook
                inferior-lisp-mode-hook
                lisp-interaction-mode-hook
                ;;
                scheme-mode-hook
                gerbil-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook #'sanityinc/lisp-setup)))

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
  (setq slime-lisp-implementations
        '((sbcl ("/usr/bin/sbcl"))))
  (slime))


;; gerbil tag table

;; Gerbil package manager generates TAGS tables for all installed packages at this path
;; (add-to-list 'tags-table-list "~/.gerbil/pkg/TAGS")
;; to generate tags for your own code by using gxtags. The invocation is very simple:
;; gxtags [-a] [-o TAGS] source-file-or-directory ...

(provide 'init-lisp)
