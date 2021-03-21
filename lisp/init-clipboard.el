;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; clipboard configurations

;;; Code:

;; Use the system clipboard
;; @see https://www.emacswiki.org/emacs/CopyAndPaste
;; So `C-y' could paste from clipbord if you are NOT using emacs-nox
;; I only use `paste-from-x-clipboard', not `C-y'.
(setq select-enable-clipboard nil  ;; if t might cause sway to crash
      select-enable-primary t)

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
  "Copy full path into the yank ring and OS clipboard."
  (interactive)
  (when buffer-file-name
    (util/set-clip (file-truename buffer-file-name))
    (message "file full path => clipboard & yank ring")))

(defun copy-to-clipboard (string)
  "Paste STRING to clipboard."
  (interactive (if (region-active-p)
				   (list (util/selected-str))
				 (message "no string selected")
				 (list nil)))
  (when string
    ;; added redirection to /dev/null for immediate return
    ;; @see https://emacs.stackexchange.com/questions/39019/xclip-hangs-shell-command
    (shell-command (format "wl-copy %S &> /dev/null" string) nil)))

(defun paste-from-clipboard (&optional n)
  "Paste string clipboard.  After the current cursor.
Optional argument N does nothing right now"
  (interactive "P")
  (util/insert-str (shell-command-to-string "wl-paste -n")))

(provide 'init-clipboard)
;;; init-clipboard.el ends here