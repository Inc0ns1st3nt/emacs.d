;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; my latex note taking setup

;;; Code:

(require-package 'auctex)
;; (local-require 'calctex)

;; (setq auto-insert-query nil)
;; (setq auto-insert-directory (inc0n/emacs-d "auto-insert"))
;; (define-auto-insert "\\.tex$" "latex-notes-template.tex")

(defun inc0n/yas-insert-template (name)
  (when (= (point-min)
           (point-max))
	(flet ((dummy-prompt
			(prompt choices &optional display-fn)
			(declare (ignore prompt))
			(or (find name choices :key display-fn :test #'string=)
				(throw 'notfound nil))))
      (let ((yas-prompt-functions '(dummy-prompt)))
		(catch 'notfound
          (yas-insert-snippet t))))))

(with-eval-after-load 'tex
  (setq tex-command "latex")
  (setq TeX-parse-self t)			 ; Enable parse on load.
  (setq TeX-auto-save t)			 ; Enable parse on save.
  (setq TeX-PDF-mode t)				 ; PDF mode (rather than DVI-mode)
  (setq TeX-electric-math '("$" . "$")))

(with-eval-after-load 'preview
  (setq preview-scale-function 1.4)
  (setq preview-auto-cache-preamble t
		preview-preserve-counters t)
  ;; (add-to-list 'preview-default-option-list "showbox")
  (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}")
  ;; (add-to-list 'preview-default-preamble "\\PreviewEnvironment{enumerate}")
  ;; "\n\\PreviewEnvironment{tabular}"
  )

;; (custom/reset-var 'preview-default-preamble)

(define-hook-setup 'LaTeX-mode-hook
  (flyspell-mode 1)
  (LaTeX-math-mode 1)
  (electric-pair-mode 1)
  (general-define-key
   :keymaps 'local
   "$" 'self-insert-command
   :prefix "SPC lp"
   "b" 'preview-buffer
   "r" 'preview-region
   "s" 'preview-section)
  ;; (inc0n/yas-insert-template "template")
  (setq-local word-wrap t))

(provide 'init-latex)
;;; init-latex.el ends here
