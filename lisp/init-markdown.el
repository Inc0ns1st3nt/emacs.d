;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(use-package markdown-mode
  :mode "\\.\\(m[k]d\\|markdown\\)\\'"
  :config
  ;; `pandoc' is better than obsolete `markdown'
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown"))
  (custom-set-faces
   '(markdown-header-face-1 ((t (:height 1.25 :weight extra-bold :inherit markdown-header-face))))
   '(markdown-header-face-2 ((t (:height 1.15 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-3 ((t (:height 1.08 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-4 ((t (:height 1.0 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-5 ((t (:height 0.9 :weight bold :inherit markdown-header-face))))
   '(markdown-header-face-6 ((t (:height 0.75 :weight extra-bold :inherit markdown-header-face))))))

(defun markdown-imenu-index ()
  "Markdown imenu index function."
  (let ((patterns '((nil "^#\\([# ]*[^#\n\r]+\\)" 1))))
    (save-excursion
      (imenu--generic-function patterns))))

(autoload 'orgtbl-mode "org-table")

(define-hook-setup 'markdown-mode-hook
  "Make markdown tables saner via `orgtbl-mode'.
Insert org table and it will be automatically converted
to markdown table.
Check Stolen from http://stackoverflow.com/a/26297700"
  ;; (util/ensure 'org-table)
  (add-hook 'after-save-hook
            (defun cleanup-org-tables ()
              (save-excursion
                (goto-char (point-min))
                (while (search-forward "-+-" nil t) (replace-match "-|-"))))
            nil 'make-it-local)
  (orgtbl-mode 1)                       ; enable key bindings
  ;; don't wrap lines because there is table in `markdown-mode'
  (setq truncate-lines t)
  (setq imenu-create-index-function 'markdown-imenu-index))

(provide 'init-markdown)
;;; init-markdown ends here
