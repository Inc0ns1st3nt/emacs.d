;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

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
  (unless (bound-and-true-p comint-terminfo-terminal)
    (setq comint-terminfo-terminal "dumb"))
  (native-complete-setup-bash))

;; `bash-completion-tokenize' can handle garbage output of "complete -p"
(defun inc0n/bash-completion-tokenize-hack (orig-fun beg end &rest args)
  "Original code extracts tokens line by line of output of \"complete -p\"."
  (and
   ;; filter out some weird lines
   (string-match-p "^complete " (buffer-substring beg end))
   (apply orig-fun beg end args)))
(advice-add 'bash-completion-tokenize
            :around #'inc0n/bash-completion-tokenize-hack)

(define-hook-setup 'shell-mode-hook
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
;; }}

(define-hook-setup 'eshell-mode-hook
  "Set up `eshell-mode'."
  (local-set-key (kbd "M-n") 'counsel-esh-history))

;; {{ @see http://emacs-journey.blogspot.com.au/2012/06/improving-ansi-term.html
;; TODO - see if process buffer would exit without this advice
;; (advice-add 'term-sentinel :after #'inc0n/kill-process-buffer-when-exit)

;; utf8
;; (defun inc0n/term-use-utf8 ()
;;   (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
;; (add-hook 'term-exec-hook #'inc0n/term-use-utf8)
(advice-add 'ansi-term :after
            (defun advise-ansi-term-coding-system ()
              "Ensure the ansi=term has the utf8 encoding."
              (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))
;; }}

;; {{ comint-mode
(with-eval-after-load 'comint
  ;; Don't echo passwords when communicating with interactive programs:
  ;; Github prompt is like "Password for 'https://user@github.com/':"
  (setq comint-password-prompt-regexp
        (format "%s\\|^ *Password for .*: *$" comint-password-prompt-regexp))
  (general-define-key
   :keymaps 'comint-mode-map
   ;; look up shell command history
   (kbd "M-n") #'counsel-shell-history
   ;; Don't show trailing whitespace in REPL.
   (kbd "M-;") #'comment-dwim)
  (add-hook 'comint-output-filter-functions #'comint-watch-for-password-prompt))
;; }}

(provide 'init-term-mode)
