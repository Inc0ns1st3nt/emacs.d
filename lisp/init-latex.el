;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; my latex note taking setup

;;; Code:

(require-package 'auctex)

;; (setq auto-insert-query nil)
;; (setq auto-insert-directory (inc0n/emacs-d "auto-insert"))
;; (define-auto-insert "\\.tex$" "latex-notes-template.tex")

(defun inc0n/yas-insert-template (name)
  (when (= (point-min) (point-max))
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
  (setq TeX-electric-math '("$" . "$"))
  (general-define-key
   :keymaps 'latex-mode-map
   "$" 'self-insert-command
   :prefix "SPC lp"
   "b" 'preview-buffer
   "r" 'preview-region
   "s" 'preview-section))

(with-eval-after-load 'preview
  (setq preview-scale-function 1.4)
  (setq preview-auto-cache-preamble t
		preview-preserve-counters t)
  ;; (add-to-list 'preview-default-option-list "showbox")
  (setq preview-default-preamble
		(append preview-default-preamble
				'(;; "\n\\PreviewEnvironment{tabular}"
				  ;;"\\PreviewEnvironment{enumerate}"
				  "\\PreviewEnvironment{tikzpicture}"))))

;; (custom/reset-var 'preview-default-preamble)

(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
(defun latex-mode-setup ()
  ;; (inc0n/yas-insert-template "template")
  ;; (setq-local electric-pair-pairs
  (setq-local word-wrap t))
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'latex-mode-setup)
(add-hook 'LaTeX-mode-hook 'electric-pair-mode)

(provide 'init-latex)
