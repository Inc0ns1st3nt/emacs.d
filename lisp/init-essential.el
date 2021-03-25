;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Like "init-misc.el", the difference is this file is always loaded

;;; Code:

;; {{ narrow region
(defun narrow-to-region-indirect-buffer-maybe (start end use-indirect-buffer)
  "Indirect buffer could multiple widen on same file between START and END."
  (when (region-active-p)
	(deactivate-mark))
  (if use-indirect-buffer
      (with-current-buffer (clone-indirect-buffer
                            (generate-new-buffer-name
                             (format "%s-indirect-:%s-:%s"
                                     (buffer-name)
                                     (line-number-at-pos start)
                                     (line-number-at-pos end)))
                            'display)
        (narrow-to-region start end)
        (goto-char (point-min)))
      (narrow-to-region start end)))

;; @see https://gist.github.com/mwfogleman/95cc60c87a9323876c6c
;; fixed to behave correctly in org-src buffers; taken from:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2019-09/msg00094.html
(defun narrow-or-widen-dim (&optional use-indirect-buffer)
  "If the buffer is narrowed, it widens.
Otherwise, it narrows to region, or Org subtree.
If USE-INDIRECT-BUFFER is not nil, use `indirect-buffer' to hold the widen content."
  (interactive "P")
  (cond
   ((and (not use-indirect-buffer) (buffer-narrowed-p))
    (widen))

   ((and (not use-indirect-buffer)
         (eq major-mode 'org-mode)
         (fboundp 'org-src-edit-buffer-p)
         (org-src-edit-buffer-p))
    (org-edit-src-exit))

   ;; narrow to region
   ((region-active-p)
    (narrow-to-region-indirect-buffer-maybe (region-beginning)
                                            (region-end)
                                            use-indirect-buffer))

   ;; narrow to specific org element
   ((derived-mode-p 'org-mode)
    (cond
     ((ignore-errors (org-edit-src-code)) t)
     ((ignore-errors (org-narrow-to-block) t))
     ((ignore-errors (org-narrow-to-element) t))
     (t (org-narrow-to-subtree))))

   ((derived-mode-p 'diff-mode)
    (save-excursion
      ;; If the (point) is already beginning or end of file diff,
      ;; the `diff-beginning-of-file' and `diff-end-of-file' return nil
      (when-let* ((b (progn (diff-beginning-of-file) (point)))
				  (e (progn (diff-end-of-file) (point))))
        (when (< b e)
          (narrow-to-region-indirect-buffer-maybe
		   b e use-indirect-buffer)))))

   ((derived-mode-p 'prog-mode)
    (mark-defun)
    (narrow-to-region-indirect-buffer-maybe (region-beginning)
                                            (region-end)
                                            use-indirect-buffer))
   (t (error "Please select a region to narrow to"))))
;; }}


;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
;; speed up font rendering for special characters
(setq inhibit-compacting-font-caches t)

;; Key fixes
;; @see https://emacs.stackexchange.com/questions/20240/how-to-distinguish-c-m-from-return
(define-key input-decode-map [?\C-m] [C-m])   ;; let C-m be C-m instead of RET
;; actually let's use this one in place of evil-escape
;; (define-key input-decode-map [?\C-\]] [C-\]]) ;; let C-] be C-] instead of ESC
(define-key input-decode-map [?\C-i] [C-i])   ;; let C-i be C-i instead of TAB


;; {{ Write backup files to its own directory
;; @see https://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html
(defvar inc0n/binary-file-name-regexp
  "\\.\\(avi\\|wav\\|pdf\\|mp[34g]\\|mkv\\|exe\\|3gp\\|rmvb\\|rm\\)$"
  "Is binary file name?")

(setq backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (string-match-p inc0n/binary-file-name-regexp name)))))

(let ((backup-dir (expand-file-name "~/.backups")))
  (unless (file-exists-p backup-dir)
	(make-directory backup-dir))
  (setq backup-directory-alist (list (cons "." backup-dir))))

(setq backup-by-copying t ; don't clobber symlinks
      delete-old-versions t
      version-control t  ;use versioned backups
      kept-new-versions 6
      kept-old-versions 2)

;; Don't make backups of files, not safe
;; @see https://github.com/joedicastro/dotfiles/tree/master/emacs
(setq vc-make-backup-files nil)
;; }}

;; {{ tramp setup
(with-eval-after-load 'tramp
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil))
  ;; @see https://github.com/syl20bnr/spacemacs/issues/1921
  ;; If you tramp is hanging, you can uncomment below line.
  ;; (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq tramp-chunksize 8192))
;; }}

;; Startup

;; uniquify
;; Nicer naming of buffers for files with identical names
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator " • ")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; hippie-expand
;; Since we got company-ispell and `M-x toggle-company-ispell'
;; Done, now we just use it as a clause in our make-hippie-expand-function (as above)
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))
(global-set-key (kbd "M-/") 'hippie-expand)
;; (global-set-key (kbd "C-TAB") 'hippie-expand)

(provide 'init-essential)
;;; init-essential ends here
