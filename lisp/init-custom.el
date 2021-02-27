;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; old user custom file check ../custom.el instead

;; night
;; (load-theme 'doom-spacegrey)
;; (load-theme 'doom-molokai)
;; (load-theme 'doom-monokai-pro)
;; (load-theme 'doom-gruvbox)
;; (load-theme 'gruvbox-dark-hard)

;; (load-theme 'doom-dark+)
;; (load-theme 'solarized-dark)

;; iedit quit fix
(with-eval-after-load 'evil-iedit-state
  (fset 'iedit-cleanup 'iedit-lib-cleanup))

;;;;
;; keyboard
;;;;

;; (add-to-list 'quail-keyboard-layout-alist
;;              `("workman" . ,(concat "                               "
;;                                     " `¬1!2@3£4$5%6^7&8*9(0)-_=+    "
;;                                     "  qQdDrRwWbBjJfFuUpP;:[{]}     "
;;                                     "  aAsShHtTgGyYnNeEoOiI'\"#~    "
;;                                     "  \\|zZxXmMcCvVkKlL,<.>/?      "
;;                                     "                               ")))

;; (defun +evil-exit-insert+ ()
;;   (set-input-method "pyim"))

;; (defun +evil-enter-insert+ ()
;;   (set-input-method "workman-im"))

;; (require 'workman-im)

;; (add-hook 'evil-insert-state-entry-hook #'+evil-enter-insert+)
;; (add-hook 'evil-insert-state-exit-hook #'+evil-exit-insert+)

;;;;
;; fine tune
;;;;

;; writeroom


;; (setq evil-emacs-state-modes (append evil-normal-state-modes evil-motion-state-modes))
;; (setq evil-normal-state-modes nil)
;; (setq evil-motion-state-modes nil)
;; (setq evil-emacs-state-modes nil)
;; (setq evil-default-state 'emacs)

;; clipboard
;; (setq x-select-enable-clipboard t) ;; enable emacs -> os clipboard
;; (setq x-select-enable-primary t) ;; enable os -> emacs clipboard

;; (defun my-insert-char-aux ()
;;   "My aux fn for inserting charcter."
;;   (interactive)
;;   (let ((char (read-char "press char to insert: ")))
;;     (forward-char)
;;     (insert-char char)))

;;; emacs singleton setup
(defun run-server ()
  "Run a singleton Emacs server."
  (require 'server)
  (cond ((server-running-p)
         (message "server already started"))
        (t (message "server started")
           (server-start))))
;; (run-server)

;;; lsp setup
(setq lsp-keymap-prefix "M-n")

(provide 'init-custom)
;;; init-custom.el ends here
