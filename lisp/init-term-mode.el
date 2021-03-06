;; -*- coding: utf-8; lexical-binding: t; -*-

(defun inc0n/kill-process-buffer-when-exit (process event)
  "Kill buffer of PROCESS when it's terminated.
EVENT is ignored."
  (ignore event)
  (when (memq (process-status process) '(signal exit))
    (kill-buffer (process-buffer process))))

;; {{ @see https://coredumped.dev/2020/01/04/native-shell-completion-in-emacs/
;; Enable auto-completion in `shell'.
(with-eval-after-load 'shell
  ;; `comint-terminfo-terminal' is introduced in Emacs 26.1
  (unless (and (boundp 'comint-terminfo-terminal)
               comint-terminfo-terminal)
    (setq comint-terminfo-terminal "dumb"))
  (native-complete-setup-bash))

;; `bash-completion-tokenize' can handle garbage output of "complete -p"
(defun inc0n/bash-completion-tokenize-hack (orig-fun &rest args)
  "Original code extracts tokens line by line of output of \"complete -p\"."
  (let ((beg (nth 0 args))
        (end (nth 1 args)))
    (and
     ;; filter out some weird lines
     (string-match-p "^complete " (buffer-substring beg end))
     (apply orig-fun args))))
(advice-add 'bash-completion-tokenize :around #'inc0n/bash-completion-tokenize-hack)

(defun shell-mode-hook-setup ()
  "Set up `shell-mode'."
  ;; hook `completion-at-point', optional
  (add-hook 'completion-at-point-functions #'native-complete-at-point nil t)
  (setq-local company-backends '((company-files company-native-complete)))
  ;; `company-native-complete' is better than `completion-at-point'
  (local-set-key (kbd "TAB") #'company-complete)
  ;; try to kill buffer when exit shell
  (let* ((proc (get-buffer-process (current-buffer)))
         (shell (file-name-nondirectory (car (process-command proc)))))
    ;; Don't waste time on dumb shell which `shell-write-history-on-exit' is binding to
    (unless (string-match shell-dumb-shell-regexp shell)
      (set-process-sentinel proc #'inc0n/kill-process-buffer-when-exit))))
(add-hook 'shell-mode-hook #'shell-mode-hook-setup)
;; }}

(defun eshell-mode-hook-setup ()
  "Set up `eshell-mode'."
  (local-set-key (kbd "M-n") 'counsel-esh-history))
(add-hook 'eshell-mode-hook #'eshell-mode-hook-setup)

;; {{ @see http://emacs-journey.blogspot.com.au/2012/06/improving-ansi-term.html
;; TODO - see if process buffer would exit without this advice
;; (advice-add 'term-sentinel :after #'inc0n/kill-process-buffer-when-exit)

;; utf8
;; (defun inc0n/term-use-utf8 ()
;;   (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
;; (add-hook 'term-exec-hook #'inc0n/term-use-utf8)
(defadvice ansi-term (after advise-ansi-term-coding-system)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
;; }}

;; {{ hack counsel-browser-history
(defvar var/comint-full-input nil)
(defun inc0n/counsel-shell-history-hack (orig-func &rest args)
  (let ((var/comint-full-input (util/comint-current-input)))
	(util/comint-kill-current-input)
	(apply orig-func args)))
(advice-add 'counsel-shell-history :around #'inc0n/counsel-shell-history-hack)

(defun inc0n/ivy-history-contents-hack (orig-func &rest args)
  (let ((rlt (apply orig-func args))
        (input var/comint-full-input))
    (if (and input (not (string-empty-p input)))
        ;; filter shell history with current input
        (mapcan
		 (lambda (s)
           (and (string-match (regexp-quote input) s)
                (list s)))
		 rlt)
      rlt)))
(advice-add 'ivy-history-contents :around #'inc0n/ivy-history-contents-hack)
;; }}

;; {{ comint-mode
(with-eval-after-load 'comint
  ;; Don't echo passwords when communicating with interactive programs:
  ;; Github prompt is like "Password for 'https://user@github.com/':"
  (setq comint-password-prompt-regexp
        (format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp))
  (add-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt))

(defun comint-mode-hook-setup ()
  ;; look up shell command history
  (local-set-key (kbd "M-n") #'counsel-shell-history)
  ;; Don't show trailing whitespace in REPL.
  (local-set-key (kbd "M-;") #'comment-dwim))

(add-hook 'comint-mode-hook #'comint-mode-hook-setup)
;; }}

(provide 'init-term-mode)
