;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; clipboard configurations

;;; Code:

;; Use the system clipboard
;; @see https://www.emacswiki.org/emacs/CopyAndPaste
;; So `C-y' could paste from clipbord if you are NOT using emacs-nox
;; I only use `paste-from-x-clipboard', not `C-y'.
(setq x-select-enable-clipboard t
      x-select-enable-primary t)

;; kill-ring and clipboard are same? No, it's annoying!
(setq save-interprogram-paste-before-kill nil)

(defun cp-filename-of-current-buffer (&optional n)
  "Copy file name (NOT full path) into the yank ring and OS clipboard.
If N is not nil, copy file name and line number."
  (interactive "P")
  (when buffer-file-name
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (s (if n
                  (format "%s:%s" filename (line-number-at-pos))
                filename)))
      (util/set-clip s)
      (message "%s => clipboard & kill-ring" s))))

(defun cp-fullpath-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
    (util/set-clip (file-truename buffer-file-name))
    (message "file full path => clipboard & yank ring")))

(defun paste-from-x-clipboard (&optional n)
  "paste string clipboard."
  (interactive)
  ;; (when (and (functionp 'evil-normal-state-p)
  ;;            (functionp 'evil-move-cursor-back)
  ;;            (evil-normal-state-p)
  ;;            (not (eolp))
  ;;            (not (eobp)))
  ;;   (forward-char))
  (insert (util/get-clip)))

(provide 'init-clipboard)
