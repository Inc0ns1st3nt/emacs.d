;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'ace-window)

;; move focus between sub-windows
(use-package winum
  :defer t
  :config
  (setq winum-format " %s ")
  (setq winum-mode-line-position 0
        winum-auto-setup-mode-line nil)
  (set-face-attribute 'winum-face nil
                      :foreground "DeepPink"
                      :underline "DeepPink"
                      :weight 'bold)
  (dotimes (i 10)
    (define-key winum-keymap (kbd (format "C-%d" i)) 'winum-select-nth-window))
  :init
  (setq winum-keymap (make-sparse-keymap))
  (add-hook 'after-init-hook 'winum-mode))

(defun winum-select-nth-window (&optional arg)
  "Select window with numbering command-keys method.
ARG will negate the numbering."
  (interactive "P")
  (let* ((command-keys (key-description (this-command-keys)))
         (last-key (aref command-keys (1- (length command-keys))))
         (num (- last-key ?0)))
    (winum-select-window-by-number (if arg (- num) num))))

;; @see https://emacs-china.org/t/emacs-builtin-mode/11937/63
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(add-hook 'after-init-hook #'winner-mode)

(global-set-key [?\C-x ?4 u] 'winner-undo)
(global-set-key [?\C-x ?2]
                (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key [?\C-x ?3]
                (lambda () (interactive) (split-window-horizontally) (other-window 1)))
 ;; https://github.com/abo-abo/ace-window
 ;; `M-x ace-window ENTER m` to swap window
(global-set-key [?\C-x ?o] 'ace-window)

;; https://emacs.stackexchange.com/questions/46664/switch-between-horizontal-and-vertical-splitting
(defun rotate-two-split-window ()
  "Toggle two window layout vertically or horizontally."
  (interactive)
  (if (= (count-windows) 2)
      (let ((this-win (selected-window))
            (next-win (next-window)))
        (let ((this-win-edges (window-edges this-win))
              (next-win-edges (window-edges next-win))
              (this-win-buffer (window-buffer this-win))
              (next-win-buffer (window-buffer next-win)))
          (delete-window next-win)
          (let ((new-next-win
                 (funcall (if (= (car this-win-edges)
                                 (car next-win-edges))
                              #'split-window-horizontally
                            #'split-window-vertically))))
            ;; if this window was the 2nd window
            (when (not (and (<= (car this-win-edges)
                                (car next-win-edges))
                            (<= (cadr this-win-edges)
                                (cadr next-win-edges))))
              (setq this-win (next-window)
                    new-next-win (selected-window)))
            (select-window this-win)
            (set-window-buffer this-win this-win-buffer)
            (set-window-buffer new-next-win next-win-buffer))))
    (message "can only rotate two windows at a time")))

(provide 'init-windows)
;;; init-windows.el ends here
