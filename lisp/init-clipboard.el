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

(defun copy-yank-str (msg &optional clipboard-only)
  (unless clipboard-only (kill-new msg))
  (util/set-clip msg)
  msg)

(defun cp-filename-of-current-buffer (&optional n)
  "Copy file name (NOT full path) into the yank ring and OS clipboard.
If N is not nil, copy file name and line number."
  (interactive "P")
  (when buffer-file-name
    (let* ((filename (file-name-nondirectory buffer-file-name))
           (s (if n
                  (format "%s:%s" filename (line-number-at-pos))
                filename)))
      (copy-yank-str s)
      (message "%s => clipboard & kill-ring" s))))

(defun cp-ffip-ivy-last ()
  "Copy visible keys of `ivy-last' into `kill-ring' and clipboard."
  (interactive)
  (util/ensure 'find-file-in-project)
  (when ffip-ivy-last-saved
    (copy-yank-str
     (mapconcat (lambda (e)
                  (format "%S" (if (consp e) (car e) e)))
                (ivy-state-collection ffip-ivy-last-saved)
                "\n"))
    (message "%d items from ivy-last => clipboard & yank ring" (length ivy-last))))

(defun cp-fullpath-of-current-buffer ()
  "Copy full path into the yank ring and OS clipboard"
  (interactive)
  (when buffer-file-name
    (copy-yank-str (file-truename buffer-file-name))
    (message "file full path => clipboard & yank ring")))

(defun kill-ring-to-clipboard ()
  "Copy from `kill-ring' to clipboard."
  (interactive)
  (inc0n/select-from-kill-ring
   (lambda (s)
     (let* ((summary (car s))
            (hint " => clipboard" )
            (msg (if (string-match-p "\.\.\.$" summary)
                     (substring summary 0 (- (length summary) (length hint)))
                   msg)))
       ;; cc actual string
       (util/set-clip (cdr s))
       ;; echo
       (message "%s%s" msg hint)))))

(defun paste-from-x-clipboard (&optional n)
  "Remove selected text and paste string clipboard.
If N is 1, we paste diff hunk whose leading char should be removed.
If N is 2, paste into `kill-ring' too.
If N is 3, converted dashed to camel-cased then paste.
If N is 4, rectangle paste. "
  (interactive "P")
  (when (and (functionp 'evil-normal-state-p)
             (functionp 'evil-move-cursor-back)
             (evil-normal-state-p)
             (not (eolp))
             (not (eobp)))
    (forward-char))
  (let ((str
         (util/get-clip)))
    (when (> (length str) (* 256 1024))
      ;; use light weight `major-mode' like `js-mode'
      (when (derived-mode-p 'js2-mode)
        (js-mode 1))
      ;; turn off syntax highlight
      (font-lock-mode -1))

    ;; past a big string, stop lsp temporarily
    (when (and (> (length str) 1024)
               (boundp 'lsp-mode)
               lsp-mode)
      (lsp-disconnect)
      (run-at-time 300 nil #'lsp-deferred))

    (util/delete-selected-region)

    ;; paste after the cursor in evil normal state
    (cond ((null n))                     ; do nothing
          ((= 1 n)
           (insert (replace-regexp-in-string "^\\(+\\|-\\|@@ $\\)" "" str)))
          ((= 2 n)
           (kill-new str)
           (insert str))
          ((= 3 n)
           (insert (mapconcat (lambda (s) (capitalize s))
                              (split-string str "-")
                              "")))
          ((= 4 n)
           (insert-rectangle (split-string str "[\r]?\n"))))))

(provide 'init-clipboard)
