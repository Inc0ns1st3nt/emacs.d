;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:
(require-package 'scss-mode)

(defun inc0n/css-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([a-zA-Z0-9&,.: _-]+\\) *{ *$" 1)
                               ("Variable" "^ *\\$\\([a-zA-Z0-9_]+\\) *:" 1)
                               ;; post-css mixin
                               ("Function" "^ *@define-mixin +\\([^ ]+\\)" 1)))))

;; node plugins can compile css into javascript
;; flymake-css is obsolete
(add-hook 'css-mode-hook
          (defun css-mode-hook-setup ()
            (unless (buffer-file-temp-p)
              (rainbow-mode 1)
              (counsel-css-imenu-setup)
              (setq imenu-create-index-function
                    'counsel-css--imenu-create-index-function))))

;; compile *.scss to *.css on the pot could break the project build
(with-eval-after-load 'scss-mode
  (setq scss-compile-at-save nil))

(add-hook 'scss-mode-hook
          (defun scss-mode-hook-setup ()
            (unless (buffer-file-temp-p)
              (setq imenu-create-index-function 'inc0n/css-imenu-make-index))))

(with-eval-after-load 'css-mode
  (setq css-indent-offset 2))

(provide 'init-css)