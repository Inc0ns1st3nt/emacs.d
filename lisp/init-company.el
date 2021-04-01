;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:
(require-package 'company)
(require-package 'native-complete)
(require-package 'company-native-complete)
(require-package 'company-c-headers)
(require-package 'company-statistics)

(add-hook 'after-init-hook
		  (lambda () (run-with-idle-timer 2 nil #'global-company-mode)))

(when (fboundp 'evil-declare-change-repeat)
  (mapc #'evil-declare-change-repeat
        '(company-complete-common
          company-select-next
          company-select-previous
          company-complete-selection
          company-complete-number)))

(with-eval-after-load 'company
  ;; @see https://github.com/company-mode/company-mode/issues/348
  (company-statistics-mode)
  (add-to-list 'company-backends 'company-cmake)
  (add-to-list 'company-backends 'company-c-headers)
  ;; can't work with TRAMP
  (setq company-backends (delete 'company-ropemacs company-backends))

  ;; company completion with just numbering
  (defun inc0n/company-number (num)
	"Forward to `company-complete-number'. Unless the number is
	potentially part of the candidate. In that case, insert the
	number."
	(interactive (list (string-to-number (this-command-keys))))
    (message "company %s" (company-tooltip-visible-p))
	(let ((n (if (zerop num)
				 10
			   num))
		  (re (concat "^" company-prefix (number-to-string num))))
	  (if (or (not (company-tooltip-visible-p))
              (cl-find-if (lambda (s) (string-match re s))
						  company-candidates)
			  (null (cdr company-candidates)) ;; if list is single (len==1)
			  (> n (length company-candidates))
			  (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
		  (self-insert-command 1)
		(company-complete-number n))))
  (dotimes (i 10)
	(define-key company-active-map
	  (number-to-string i)
	  'inc0n/company-number))

  (defun inc0n/company-tab ()
	(interactive)
	(if (cl-find-if (lambda (s) (string-equal company-prefix s))
					company-candidates)
		(company-abort)
	  (company-complete-common)))
  (define-key company-active-map [tab] #'inc0n/company-tab)

  ;; Press SPACE will accept the highlighted candidate and insert a space
  ;; "M-x describe-variable company-auto-complete-chars" for details.
  ;; So that's BAD idea.
  (setq company-auto-commit nil
		company-auto-commit-chars '())

  ;; company-ctags is much faster out of box. No further optimiation needed
  (unless (featurep 'company-ctags)
    (local-require 'company-ctags)
    ;; (autoload 'company-ctags-auto-setup "company-ctags")
	(company-ctags-auto-setup))

  (setq company-backends (delete 'company-capf company-backends))

  ;; I don't like the downcase word in company-dabbrev
  (setq company-dabbrev-downcase nil
        ;; make previous/next selection in the popup cycles
        company-selection-wrap-around t
        ;; Some languages use camel case naming convention,
        ;; so company should be case sensitive.
        company-dabbrev-ignore-case nil
        ;; press M-number to choose candidate
        company-show-numbers t
        company-minimum-prefix-length 2
        company-idle-delay 0.05
        company-clang-insert-arguments nil
        company-require-match nil
        ;; @see https://github.com/company-mode/company-mode/issues/146
        company-tooltip-align-annotations t)
  (setq company-ctags-ignore-case t)		; I use company-ctags instead

  ;; NOT to load company-mode for certain major modes.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
        '(not
          eshell-mode
          comint-mode
          erc-mode
          gud-mode
          rcirc-mode
          minibuffer-inactive-mode))
  (define-key company-active-map (kbd "TAB") 'company-complete-or-yasnippet-expand)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(with-eval-after-load 'company-ispell
  (defun inc0n/company-ispell-available-hack (orig-func &rest args)
    (unless
		;; auto-complete in comment only
		;; only use company-ispell in comment when coding
		(and (derived-mode-p 'prog-mode)
             ;; respect advice in `company-in-string-or-comment'
			 (or (not (company-in-string-or-comment))
				 (not (comment-only-p (line-beginning-position)
									  (line-end-position)))))
      (apply orig-func args)))
  (advice-add 'company-ispell-available
              :around #'inc0n/company-ispell-available-hack))

;; {{ setup company-ispell
(defun toggle-company-ispell ()
  "Toggle company-ispell."
  (interactive)
  (cond ((memq 'company-ispell company-backends)
         (setq company-backends (delq 'company-ispell company-backends))
         (message "company-ispell disabled"))
        (t
         (add-to-list 'company-backends 'company-ispell)
         (message "company-ispell enabled!"))))

(defun company-ispell-setup ()
  "My company Ispell setup."
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    ;; @see https://github.com/redguardtoo/emacs.d/issues/473
    (setq company-ispell-dictionary
          (if (boundp 'ispell-alternate-dictionary)
              ispell-alternate-dictionary
			(inc0n/emacs-d "var/misc/english-words.txt")))))

;; message-mode use company-bbdb.
;; So we should NOT turn on company-ispell
(add-hook 'org-mode-hook #'company-ispell-setup)
;; }}

;; yasnippet
;; too much accidental yasnippet expansion
(defun company-complete-or-yasnippet-expand ()
  "Let yasnippet over company."
  ;; (let ((yas-fallback-behavior 'return-nil))
  ;;   (unless (yas-expand)))
  (company-complete-common))

(provide 'init-company)
;;; init-company.el ends here
