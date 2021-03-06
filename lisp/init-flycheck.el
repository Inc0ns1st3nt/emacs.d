;;; init-flycheck.el --- Configure Flycheck -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(when (require-package 'flycheck)
  ;; (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function
        'flycheck-display-error-messages-unless-error-list)
  ;; (when (require-package 'flycheck-color-mode-line)
  ;;   (setq flycheck-color-mode-line-face-to-color 'mode-line)
  ;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  (defun inc0n/transient-flycheck-error (&optional nextp)
    "Transient version of `flycheck-error'."
    (interactive)
    (if flycheck-mode
        (let ((echo-keystrokes nil))
          (if nextp
              (flycheck-next-error)
            (flycheck-previous-error))
          (message "Flycheck goto error: [n]ext [p]revious")
          (set-transient-map
           (let ((map (make-sparse-keymap)))
             (define-key map [?n] #'flycheck-next-error)
             (define-key map [?p] #'flycheck-previous-error)
             map)
           t))
      (message "turn on flycheck-mode first"))))

(provide 'init-flycheck)
;;; init-flycheck.el ends here