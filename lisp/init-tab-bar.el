
;;; Commentary:
;;

;;; Code:

(defun my/tab-bar-find-file (file)
  "Open a new tab, before `find-file' while variable `tab-bar-mode' is active.
FILE is ignored."
  (when tab-bar-mode (tab-bar-new-tab)))
(advice-add 'find-file :before 'my/tab-bar-find-file)

(defun my/tab-bar-kill-buffer ()
  "Close the tab, before the buffer is killed.
Only if `tab-bar-mode' is active and current tab is not the last tab."
  (when (and tab-bar-mode
             (not (= 1 (length (tab-bar-tabs)))))
    (tab-bar-close-tab)))
(add-hook 'kill-buffer-hook 'my/tab-bar-kill-buffer)
;; (remove-hook 'kill-buffer-hook 'my/tab-bar-kill-buffer)

(provide 'init-tab-bar)
