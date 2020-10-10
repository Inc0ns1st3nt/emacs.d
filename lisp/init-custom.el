;; -*- coding: utf-8; lexical-binding: t; -*-

;; night
;; (load-theme 'doom-spacegrey)
;; (load-theme 'doom-molokai)
;; (load-theme 'doom-monokai-pro)
;; (load-theme 'doom-gruvbox)
;; (load-theme 'gruvbox-dark-hard)

;; (load-theme 'doom-dark+)
;; (load-theme 'solarized-dark)

;; (setq-default line-spacing 0)

;; amx
(setq amx-save-file (concat inc0n/emacs-d "cache/amx-items"))
(setq ido-save-directory-list-file (concat inc0n/emacs-d "cache/ido.last"))

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
(setq writeroom-width 100)

;; (setq evil-emacs-state-modes (append evil-normal-state-modes evil-motion-state-modes))
;; (setq evil-normal-state-modes nil)
;; (setq evil-motion-state-modes nil)
;; (setq evil-emacs-state-modes nil)
;; (setq evil-default-state 'emacs)

;; clipboard
;; (fset 'evil-visual-update-x-selection 'ignore)
;; (setq x-select-enable-clipboard t) ;; enable emacs -> os clipboard
;; (setq x-select-enable-primary t) ;; enable os -> emacs clipboard

;; paren mode
(show-paren-mode 1)
;; (setq show-paren-delay 0.125)

;;;;
;; functions
;;;;

(defun surround-with-char (beg end)
  "Up-case the last letter of the word at point.
Or region from `BEG' to `END'."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (1+ (point)))))
  (let ((selection (buffer-substring-no-properties beg end)))
    (let ((char (read-char "press char to surround: ")))
      (kill-region beg end)
      (insert (format "%c%c" char char)))
    (backward-char)
    (insert selection)))

;; (fmakunbound 'evil-insert-char-aux)

;; (defun my-insert-char-aux ()
;;   "My aux fn for inserting charcter."
;;   (interactive)
;;   (let ((char (read-char "press char to insert: ")))
;;     (forward-char)
;;     (insert-char char)))

;;; latex
(add-hook 'latex-mode-hook
          (lambda () (setq word-wrap t)))

;;; emacs singleton setup
(defun run-server ()
  "Run a singleton Emacs server."
  (require 'server)
  (cond ((server-running-p)
         (message "server already started"))
        (t (message "server started")
           (server-start))))
;; (run-server)

;; gambit
(require 'gambit)
(add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)

;; gerbil setup
(defvar inc0n/gerbil-home (getenv "GERBIL_HOME"))
(let ((gerbil-program-name (concat inc0n/gerbil-home "/bin/gxi")))
  ;; gerbil mode
  (add-to-list 'load-path (concat inc0n/gerbil-home "/etc/"))
  (autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
  ;; gerbil tags
  (add-to-list 'tags-table-list (concat inc0n/gerbil-home "/src/TAGS"))
  (setq scheme-program-name gerbil-program-name))

(add-auto-mode 'gerbil-mode "\\.ss$")


;;; lsp setup
(setq lsp-keymap-prefix "M-n")

;;; objc-mode
;; (rx (or ".xm" ".x"))
(add-to-list 'auto-mode-alist `(,(regexp-opt '(".xm" ".x") t) . objc-mode))

(provide 'init-custom)
;;; init-custom.el ends here
