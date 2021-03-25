;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode

(defvar inc0n/linum-inhibit-modes
  '(eshell-mode
    shell-mode
    js-comint-mode
    profiler-report-mode
    ffip-diff-mode
    dictionary-mode
    erc-mode
    help-mode
    text-mode
    fundamental-mode
    jabber-roster-mode
    jabber-chat-mode
    inferior-js-mode
    inferior-python-mode
    ivy-occur-grep-mode ; better performance
    ivy-occur-mode ; better performance
    twittering-mode
    compilation-mode
    woman-mode
    Info-mode
    calc-mode
    calc-trail-mode
    comint-mode
    gnus-group-mode
    gud-mode
    vc-git-log-edit-mode
    log-edit-mode
    term-mode
    w3m-mode
    eww-mode
	nov-mode
    doc-view-mode
    speedbar-mode
    gnus-summary-mode
    gnus-article-mode
    calendar-mode)
  "Major modes without line number.")

(setq-default display-line-numbers t
			  display-line-numbers-type t)

(defun display-line-numbers-mode-hook-setup ()
  "This setup will disable 'display-line-numbers-mode' for temp buffer, or if `major-mode' in `inc0n/linum-inhibit-modes'."
  (when (or (memq major-mode inc0n/linum-inhibit-modes)
			;; don't show line number for certain file extensions
            ;; (buffer-file-temp-p)
            (minibufferp)
			(should-use-minimum-resource))
    (setq-local display-line-numbers nil
				display-line-numbers-type t)))
(add-hook 'display-line-numbers-mode-hook #'display-line-numbers-mode-hook-setup)
(add-hook 'after-init-hook 'global-display-line-numbers-mode)

(provide 'init-linum-mode)