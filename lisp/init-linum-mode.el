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
    org-mode
    vc-git-log-edit-mode
    log-edit-mode
    term-mode
    w3m-mode
    speedbar-mode
    gnus-summary-mode
    gnus-article-mode
    calendar-mode)
  "Major modes without line number.")

;; I don't care Emacs 25 performance any more
(when (fboundp 'global-display-line-numbers-mode)
  (defun display-line-numbers-mode-hook-setup ()
    (if (or (memq major-mode inc0n/linum-inhibit-modes)
            ;; don't show line number for certain file extensions
            (should-use-minimum-resource))
        (setq display-line-numbers t)
      (setq display-line-numbers 'relative)
      (setq display-line-numbers-type 'relative)))
  (add-hook 'display-line-numbers-mode-hook #'display-line-numbers-mode-hook-setup)
  (global-display-line-numbers-mode t))

(provide 'init-linum-mode)
