;; -*- coding: utf-8; lexical-binding: t; -*-

;; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(defvar inc0n/linum-inhibit-modes
  '(eshell-mode
    shell-mode
    js-comint-mode
    profiler-report-mode
    ffip-diff-mode
    dictionary-mode
    erc-mode
    dired-mode
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
    weibo-timeline-mode
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
	nov-mode
    speedbar-mode
    gnus-summary-mode
    gnus-article-mode
    calendar-mode)
  "Major modes without line number.")

(unless (fboundp 'global-display-line-numbers-mode)
  (error "global-display-line-numbers-mode not bound"))

(setq-default display-line-numbers 'relative
			  display-line-numbers-type 'relative)

(defun display-line-numbers-mode-hook-setup ()
  (when (or (memq major-mode inc0n/linum-inhibit-modes)
			;; don't show line number for certain file extensions
			(should-use-minimum-resource))
    (setq-local display-line-numbers t
				display-line-numbers-type t)))
(add-hook 'display-line-numbers-mode-hook #'display-line-numbers-mode-hook-setup)
(add-hook 'after-init-hook 'global-display-line-numbers-mode)

(provide 'init-linum-mode)
;; init-linum-mode.el ends here