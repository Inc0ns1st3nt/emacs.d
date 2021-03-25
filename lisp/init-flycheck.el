;;; init-flycheck.el --- Configure Flycheck -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(when (require-package 'flycheck)
  (add-hook 'after-init-hook (lambda () (global-flycheck-mode 0)))
  (setq flycheck-display-errors-function
        'flycheck-display-error-messages-unless-error-list)
  ;; (when (require-package 'flycheck-color-mode-line)
  ;;   (setq flycheck-color-mode-line-face-to-color 'mode-line)
  ;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  )

(provide 'init-flycheck)