;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'company)
(require-package 'native-complete)
(require-package 'company-native-complete)
(require-package 'company-c-headers)
(require-package 'company-statistics)

(add-hook 'after-init-hook #'global-company-mode)

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
  (push 'company-cmake company-backends)
  (push 'company-c-headers company-backends)
  ;; can't work with TRAMP
  (setq company-backends (delete 'company-ropemacs company-backends))

  ;; company-ctags is much faster out of box. No further optimiation needed
  (unless (featurep 'company-ctags)
    (local-require 'company-ctags))
  (company-ctags-auto-setup)

  ;; (setq company-backends (delete 'company-capf company-backends))

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
        company-ctags-ignore-case t ; I use company-ctags instead
        ;; @see https://github.com/company-mode/company-mode/issues/146
        company-tooltip-align-annotations t)

  ;; Press SPACE will accept the highlighted candidate and insert a space
  ;; "M-x describe-variable company-auto-complete-chars" for details.
  ;; So that's BAD idea.
  (setq company-auto-complete nil)

  ;; NOT to load company-mode for certain major modes.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
        '(not
          eshell-mode
          comint-mode
          erc-mode
          gud-mode
          rcirc-mode
          minibuffer-inactive-mode)))

(with-eval-after-load 'company-ispell
  (defun inc0n/company-ispell-available-hack (orig-func &rest args)
    ;; in case evil is disabled
    (util/ensure 'evil-nerd-commenter)
    (cond
     ((and (derived-mode-p 'prog-mode)
           (or (not (company-in-string-or-comment)) ; respect advice in `company-in-string-or-comment'
               (not (evilnc-is-pure-comment (point))))) ; auto-complete in comment only
      ;; only use company-ispell in comment when coding
      nil)
     (t
      (apply orig-func args))))
  (advice-add 'company-ispell-available :around #'inc0n/company-ispell-available-hack))

;; {{ setup company-ispell
(defun toggle-company-ispell ()
  "Toggle company-ispell."
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(defun company-ispell-setup ()
  ;; @see https://github.com/company-mode/company-mode/issues/50
  (when (boundp 'company-backends)
    (make-local-variable 'company-backends)
    (add-to-list 'company-backends 'company-ispell)
    ;; @see https://github.com/redguardtoo/emacs.d/issues/473
    (setq company-ispell-dictionary
          (cond
           ((and (boundp 'ispell-alternate-dictionary)
                 ispell-alternate-dictionary)
            ispell-alternate-dictionary)
           (t
            (file-truename (concat inc0n/emacs-d "misc/english-words.txt")))))))

;; message-mode use company-bbdb.
;; So we should NOT turn on company-ispell
(add-hook 'org-mode-hook #'company-ispell-setup)
;; }}

(provide 'init-company)
