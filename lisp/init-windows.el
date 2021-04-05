;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'ace-window)
;; (require-package 'winum)

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
  :init
  (setq winum-keymap
	    (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-0") 'winum-select-window-0)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map))
  (add-hook 'after-init-hook 'winum-mode))

;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(add-hook 'after-init-hook #'winner-mode)

;; @see https://emacs-china.org/t/emacs-builtin-mode/11937/63
;; press u undo and r to redo
;; (defun inc0n/transient-winner-undo ()
;;   "Transient version of `winner-undo'."
;;   (interactive)
;;   (if winner-mode
;;       (let ((echo-keystrokes nil))
;;         (winner-undo)
;;         (message "Winner: [u]ndo [r]edo [q]uit")
;;         (set-transient-map
;;          (let ((map (make-sparse-keymap)))
;;            (define-key map [?u] #'winner-undo)
;;            (define-key map [?r] #'winner-redo)
;;            map)
;;          t))
;;     (message "turn on winner-mode first")))

(global-set-key [?\C-x ?4 u] 'winner-undo)
(global-set-key [?\C-x ?2] 'split-window-vertically)
(global-set-key [?\C-x ?3] 'split-window-horizontally)
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
