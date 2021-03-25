;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar inc0n/ediff-panel-name nil)

(defun inc0n/server-save-buffers-kill-terminal-hack (&optional arg)
  "Kill buffers to create ediff panel and call `ediff-startup-hook-setup'.
Also remove buffers whose binding files already merged in `buffer-list'."
  (mapc 'kill-buffer (buffer-list)))
(advice-add 'server-save-buffers-kill-terminal :after #'inc0n/server-save-buffers-kill-terminal-hack)

(when (inc0n/vc-merge-p)
  ;; remove `org-mode' from `auto-mode-alist'.
  ;; So nodes in org file do NOT collapse at all
  (setq auto-mode-alist  (rassq-delete-all 'org-mode auto-mode-alist))
  ;; associate simpler major mode with org file instead
  (add-auto-mode 'outline-mode "\\.org\\(_archive\\)?$")

  (defun inc0n/ediff-command (cmd &optional no-arg)
    (lambda (&optional arg)
      (interactive "P")
      (when-let* ((w (get-buffer-window))
                  (p (get-buffer-window inc0n/ediff-panel-name)))
        ;; go to panel window
        (select-window p)
        ;; execute ediff command, ignore any error
        (condition-case e
            (if no-arg (funcall cmd) (funcall cmd arg))
          (error
           (message "%s" (error-message-string e))))

        ;; back to original window
        (select-window w))))

  (util/ensure 'ediff)

  ;; @see https://stackoverflow.com/a/29757750/245363
  (defun ediff-copy-both-to-C (&optional arg)
    "Copy code from both A and B to C."
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (inc0n/space-leader-def
    "a" (lambda () (interactive) (jump-to-register ?a))
    "t" (inc0n/ediff-command 'ediff-toggle-show-clashes-only t)
    "n" (inc0n/ediff-command (lambda (arg)
                               (if (< ediff-current-difference
                                      (1- ediff-number-of-differences))
                                   (ediff-next-difference arg)
                                 (message "This is last difference!"))))
    "p" (inc0n/ediff-command (lambda (arg)
                               (if (> ediff-current-difference 0)
                                   (ediff-previous-difference arg)
                                 (message "This is first difference!"))))
    "r" (inc0n/ediff-command 'ediff-restore-diff-in-merge-buffer)
    ;; press "1-space-R" to revert without confirmation
    "R" (inc0n/ediff-command 'ediff-revert-buffers-then-recompute-diffs)
    "xa" (lambda () (interactive) (save-buffers-kill-terminal t)) ; similar to vim
    ;; use 1 3 as hotkey to be consistent with vim
    "1" (inc0n/ediff-command 'ediff-copy-A-to-C)
    "3" (inc0n/ediff-command 'ediff-copy-B-to-C)
    "b" (inc0n/ediff-command 'ediff-copy-both-to-C))

  (define-hook-setup 'ediff-startup-hook
    "Hide control panel if it's current buffer."
    (when (string-match-p "\*Ediff Control Panel.*\*" (buffer-name))
      (unless inc0n/ediff-panel-name
        (setq inc0n/ediff-panel-name (buffer-name)))
      ;; load color theme for merge
      (load-theme 'tao-yang t)
      ;; show only clashed area
      (ediff-toggle-show-clashes-only)
      ;; move to the first difference
      (ediff-next-difference)
      ;; move to the merged buffer window
      (winum-select-window-by-number 3)
      ;; save the windows layout
      (window-configuration-to-register ?a))))

(provide 'init-ediff)
;;; init-ediff ends here
