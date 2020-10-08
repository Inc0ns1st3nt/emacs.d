;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'ace-window)

;; Navigate window layouts with "C-c <left>" and "C-c <right>"
(winner-mode 1)
;; @see https://emacs-china.org/t/emacs-builtin-mode/11937/63
;; press u undo and r to redo
(defun inc0n/transient-winner-undo ()
  "Transient version of `winner-undo'."
  (interactive)
  (let ((echo-keystrokes nil))
    (winner-undo)
    (message "Winner: [u]ndo [r]edo [q]uit")
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?u] #'winner-undo)
       (define-key map [?r] #'winner-redo)
       map)
     t)))
(global-set-key (kbd "C-x 4 u") #'inc0n/transient-winner-undo)

(global-set-key (kbd "C-x 2") #'split-window-vertically)
(global-set-key (kbd "C-x 3") #'split-window-horizontally)

(defun scroll-other-window-up ()
  (interactive)
  (scroll-other-window '-))

;; https://emacs.stackexchange.com/questions/46664/switch-between-horizontal-and-vertical-splitting
(defun toggle-two-split-window ()
  "Toggle two window layout vertically or horizontally."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((this-win-buffer (window-buffer))
           (next-win-buffer (window-buffer (next-window)))
           (this-win-edges (window-edges (selected-window)))
           (next-win-edges (window-edges (next-window)))
           (this-win-2nd (not (and (<= (car this-win-edges)
                                       (car next-win-edges))
                                   (<= (cadr this-win-edges)
                                       (cadr next-win-edges)))))
           (splitter
            (if (= (car this-win-edges)
                   (car (window-edges (next-window))))
                #'split-window-horizontally
              #'split-window-vertically)))
      (delete-other-windows)
      (let ((first-win (selected-window)))
        (funcall splitter)
        (when this-win-2nd
          (other-window 1))
        (set-window-buffer (selected-window) this-win-buffer)
        (set-window-buffer (next-window) next-win-buffer)
        (select-window first-win)
        (when this-win-2nd
          (other-window 1))))))

;; https://github.com/abo-abo/ace-window
;; `M-x ace-window ENTER m` to swap window
(global-set-key (kbd "C-x o") #'ace-window)

;; {{ move focus between sub-windows
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

(require-package 'winum)
(util/ensure 'winum)

(with-eval-after-load 'winum
  (setq winum-format "%s")
  (setq winum-mode-line-position 0)
  (set-face-attribute 'winum-face nil
                      :foreground "DeepPink"
                      :underline "DeepPink"
                      :weight 'bold)
  (winum-mode 1))
;; }}

(provide 'init-windows)
