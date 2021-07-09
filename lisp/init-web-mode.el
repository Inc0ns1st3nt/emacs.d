;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; See `flymake-xml-program' for html flymake check
;; No extra setup is required.

;;; Code:

(define-hook-setup 'web-mode-hook
  (unless (buffer-file-temp-p)
    (setq inc0n/flyspell-check-doublon nil)
    (remove-hook 'yas-after-exit-snippet-hook
                 #'web-mode-yasnippet-exit-hook t)
    (remove-hook 'yas/after-exit-snippet-hook
                 #'web-mode-yasnippet-exit-hook t)))

(with-eval-after-load 'web-mode
  ;; make org-mode export fail, I use evil and evil-matchit
  ;; to select text, so expand-region.el is not used
  (remove-hook 'web-mode-hook 'er/add-web-mode-expansions)
  (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

;;; Css

(defvar css-imenu-expression
    '((nil "^ *\\([a-zA-Z0-9&,.: _-]+\\) *{ *$" 1)
      ("Variable" "^ *\\$\\([a-zA-Z0-9_]+\\) *:" 1)
      ;; post-css mixin
      ("Function" "^ *@define-mixin +\\([^ ]+\\)" 1)))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2)
  :init
  ;; node plugins can compile css into javascript
  ;; flymake-css is obsolete
  (define-hook-setup 'css-mode-hook
    (unless (buffer-file-temp-p)
      (rainbow-mode 1)
      (counsel-css-imenu-setup)
      (setq imenu-generic-expression css-imenu-expression))))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :config
  ;; compile *.scss to *.css on the pot could break the project build
  (setq scss-compile-at-save nil)
  :init
  (define-hook-setup 'scss-mode-hook
    (unless (buffer-file-temp-p)
      (setq imenu-generic-expression css-imenu-expression))))

(provide 'init-web-mode)
