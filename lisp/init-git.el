;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

(require-package 'magit)

;; ;; {{ Solution 1: disable all vc backends
;; @see http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; (setq vc-handled-backends ())
;; }}

;; {{ Solution 2: if NO network mounted drive involved
(setq vc-handled-backends '(Git SVN Hg))
;; @see https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; open files faster but you can't check if file is version
;; controlled. other VCS functionality still works.
(remove-hook 'find-file-hooks #'vc-find-file-hook)
;; }}

;; ;; {{ Solution 3: setup `vc-handled-backends' per project
;; (setq vc-handled-backends nil)
;; (defun my-setup-develop-environment ()
;;   "Default setup for project under vcs."
;;   (interactive)
;;   (cond
;;     ((string-match-p (file-truename user-emacs-directory)
;;                      (file-name-directory (buffer-file-name)))
;;       (setq vc-handled-backends '(Git)))
;;     (t
;;       (setq vc-handled-backends nil))))
;; (dolist (hook '(java-mode-hook emacs-lisp-mode-hook org-mode-hook
;;                 js-mode-hook javascript-mode-hook web-mode-hook
;;                 c++-mode-hook c-mode-hook))
;;   (add-hook hook #'my-setup-develop-environment))
;; ;; }}

;; {{ git-gutter
;; (local-require 'git-gutter)

(defun git-gutter-reset-to-head-parent()
  "Reset gutter to HEAD^.  Support Subversion and Git."
  (interactive)
  (let* ((filename (buffer-file-name))
         (cmd (concat "git --no-pager log --oneline -n1 --pretty=\"format:%H\" "
                      filename))
         (parent (cond
                  ((eq git-gutter:vcs-type 'svn)
                   "PREV")
                  (filename
                   (concat (shell-command-to-string cmd) "^"))
                  (t
                   "HEAD^"))))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))

(defun git-gutter-reset-to-default ()
  "Restore git gutter to its original status.
Show the diff between current working code and git head."
  (interactive)
  (git-gutter:set-start-revision nil)
  (message "git-gutter reset"))

(autoload 'global-git-gutter-mode "git-gutter")
;; (add-hook 'after-init-hook 'global-git-gutter-mode)

(with-eval-after-load 'git-gutter
  (setq git-gutter:update-interval 2)
  ;; nobody use bzr
  ;; I could be forced to use subversion or hg which has higher priority
  ;; Please note my $HOME directory is under git control
  (setq git-gutter:handled-backends '(svn hg git))
  (setq git-gutter:disabled-modes
        '(asm-mode
          org-mode
          outline-mode
          markdown-mode
          image-mode))

  (unless (fboundp 'global-display-line-numbers-mode)
    ;; git-gutter's workaround for linum-mode bug.
    ;; should not be used in `display-line-number-mode'
    (git-gutter:linum-setup))

  (global-set-key (kbd "C-x C-g") 'git-gutter-mode)
  (global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
  ;; Stage current hunk
  (global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
  ;; Revert current hunk
  (global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk))

;; }}

(defun inc0n/git-commit-id ()
  "Select commit id from current branch."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
         (collection (util/shell-command-to-lines git-cmd))
         (item (completing-read "git log:" collection)))
    (when item
	  (car (split-string item "|" t)))))

(defun inc0n/git-show-commit-internal ()
  "Show git commit."
  (when-let ((id (inc0n/git-commit-id)))
    (shell-command-to-string (format "git show %s" id))))

(defun inc0n/git-show-commit ()
  "Show commit using ffip."
  (interactive)
  (let ((ffip-diff-backends '(("Show git commit" . inc0n/git-show-commit-internal))))
    (ffip-show-diff 0)))

;; {{ git-timemachine
(defun inc0n/git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let* ((collection (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions))))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        ;; compatible with ivy 8+ and later ivy version
                        (unless (string-match-p "^[a-z0-9]*$" (car rev))
                          (setq rev (cdr rev)))
                        (git-timemachine-show-revision rev)))))

(defun inc0n/git-timemachine ()
  "Open git snapshot with the selected version."
  (interactive)
  (util/ensure 'git-timemachine)
  (git-timemachine--start #'inc0n/git-timemachine-show-selected-revision))
;; }}

(defun git-get-current-file-relative-path ()
  "Get relative path of current file for Git."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

(defun git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (message "DONE! git checkout %s" filename))))

(defvar git-commit-message-history nil)
(defun git-commit-tracked ()
  "Run 'git add -u' and commit."
  (interactive)
  (let* ((hint "Commit tracked files. Please input commit message (Enter to abort):")
         (msg (read-from-minibuffer hint
                                    nil
                                    nil
                                    nil
                                    'git-commit-message-history)))
    (cond
     ((and msg (> (length msg) 3))
      (shell-command "git add -u")
      (shell-command (format "git commit -m \"%s\"" msg))
      (message "Tracked files is committed."))
     (t
      (message "Do nothing!")))))

(defun git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (when buffer-file-name
    (let* ((filename (git-get-current-file-relative-path))
           (status (shell-command (concat "git add " filename))))
      (if (= status 0)
          (message "DONE! git add %s" filename)
        (message "Failed! %s in a git repo?" filename)))))

;; {{ goto next/previous hunk
(defun inc0n/goto-next-hunk (arg)
  "Goto next hunk."
  (interactive "p")
  (if (memq major-mode '(diff-mode))
      (diff-hunk-next)
    (forward-line)
    ;; (if (re-search-forward "\\(^<<<<<<<\\|^=======\\|^>>>>>>>\\)" (point-max) t)
    ;;     (goto-char (line-beginning-position))
    ;;   (forward-line -1))
    (git-gutter:next-hunk arg)))

(defun inc0n/goto-previous-hunk (arg)
  "Goto previous hunk."
  (interactive "p")
  (if (memq major-mode '(diff-mode))
      (diff-hunk-prev)
    (forward-line -1)
    (if (re-search-backward "\\(^>>>>>>>\\|^=======\\|^<<<<<<<\\)" (point-min) t)
        (goto-char (line-beginning-position))
      (forward-line -1)
      (git-gutter:previous-hunk arg))))
;; }}

;; {{
(defun inc0n/git-extract-based (target lines)
  "Extract based version from TARGET from LINES."
  (cl-loop with regexp-target = (regexp-quote target)
		   for i from 0
		   for line in lines
		   until (string-match regexp-target line)
		   finally return
		   ;; find child of target commit
		   (and (< 0 i)
				(< i (length lines))
				(replace-regexp-in-string "^tag: +"
										  ""
										  (car (split-string (nth (1- i) lines)
															 " +"))))))

(defun inc0n/git-rebase-interactive (&optional user-select-branch)
  "Rebase interactively on the closest branch or tag in git log output.
If USER-SELECT-BRANCH is not nil, rebase on the tag or branch selected by user."
  (interactive "P")
  (let* ((cmd "git --no-pager log --decorate --oneline -n 1024")
         (lines (util/shell-command-to-lines cmd))
         (targets (mapcan (lambda (e)
                            (and (string-match "^[a-z0-9]+ (\\([^()]+\\)) " e)
                                 (not (string-match "^[a-z0-9]+ (HEAD " e))
								 (list (match-string 1 e))))
                          lines))
         (based (cond
				 ((or (not targets) (eq (length targets) 0))
				  (message "No tag or branch is found to base on."))
				 ((or (null user-select-branch)) (eq (length targets) 1)
				  ;; select the closest/only tag or branch
				  (inc0n/git-extract-based (nth 0 targets) lines))
				 (t
				  ;; select the one tag or branch
				  (inc0n/git-extract-based (completing-read "Select based: " targets)
										   lines)))))
	;; start git rebase
    (when based
      (magit-rebase-interactive based nil))))
;; }}

;; {{ git-gutter use ivy
(defun inc0n/reshape-git-gutter (gutter)
  "Re-shape GUTTER for `ivy-read'."
  (let ((linenum-start (aref gutter 3))
        (linenum-end (aref gutter 4))
        (target-line "")
        (target-linenum 1)
        (max-line-length 0))
    (save-excursion
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (let ((tmp-line
               (string-trim (util/line-str))))
          (when (> (length tmp-line) max-line-length)
			(setq target-linenum linenum-start)
			(setq target-line tmp-line)
			(setq max-line-length (length tmp-line)))

          (setq linenum-start (1+ linenum-start)))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1))
					  "-"
					"+")
                  target-linenum
				  target-line)
          target-linenum)))

(defun inc0n/goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (let* ((cands (mapcar #'inc0n/reshape-git-gutter
							git-gutter:diffinfos))
             (e (completing-read "git-gutters:" cands)))
        (setq e (cdr (assoc e cands)))
		;; (unless (numberp e) (setq e (cdr e)))
		(goto-line e))
    (message "NO git-gutters!")))

;; }}

(defun inc0n/git-find-file-in-commit (&optional arg)
  "Find file in previous commit with ARG.
If ARG is 1, find file in previous commit."
  (interactive "P")
  (util/ensure 'magit)
  (let* ((rev (concat "HEAD" (and (eq arg 1) "^")))
         (prompt (format "Find file from commit %s" rev))
         (cmd (inc0n/git-files-in-rev-command rev arg))
         (default-directory (inc0n/git-root-dir))
         (file (completing-read prompt (util/shell-command-to-lines cmd))))
    (when file
      (find-file file))))

(defun inc0n/git-log-trace-definition ()
  "Similar to `magit-log-trace-definition' but UI is simpler.
If multi-lines are selected, trace the definition of line range.
If only one line is selected, use current selection as function name to look up.
If nothing is selected, use the word under cursor as function name to look up."
  (interactive)
  (when buffer-file-name
    (let* ((range-or-func (if (region-active-p)
                              (if (util/in-one-line-p (region-beginning) (region-end))
								  (format ":%s" (util/selected-str))
								(format "%s,%s"
										(line-number-at-pos (region-beginning))
										(line-number-at-pos (1- (region-end)))))
							(format ":%s" (thing-at-point 'symbol))))
           (cmd (format "git log -L%s:%s"
						range-or-func
						(file-truename buffer-file-name)))
           (content (shell-command-to-string cmd)))
      (when (string-match-p "no match" content)
        ;; mark current function and try again
        (mark-defun)
        (setq range-or-func (format "%s,%s"
                                    (line-number-at-pos (region-beginning))
                                    (line-number-at-pos (1- (region-end)))))
        (setq cmd (format "git log -L%s:%s" range-or-func (file-truename buffer-file-name))))
      ;; (message cmd)
      (util/ensure 'find-file-in-project)
      (ffip-show-content-in-diff-mode (shell-command-to-string cmd)))))

(with-eval-after-load 'vc-msg-git
  ;; open file of certain revision
  (push '("m" "[m]agit-find-file"
          (lambda ()
            (let* ((info vc-msg-previous-commit-info))
              (magit-find-file (plist-get info :id )
                               (concat (vc-msg-sdk-git-rootdir)
                                       (plist-get info :filename))))))
        vc-msg-git-extra)

  ;; copy commit hash
  (push '("h" "[h]ash"
          (lambda ()
            (let* ((info vc-msg-previous-commit-info)
                   (id (plist-get info :id)))
              (kill-new id)
              (message "%s => kill-ring" id))))
        vc-msg-git-extra)

  ;; copy author
  (push '("a" "[a]uthor"
          (lambda ()
            (let* ((info vc-msg-previous-commit-info)
                   (author (plist-get info :author)))
              (kill-new author)
              (message "%s => kill-ring" author))))
        vc-msg-git-extra))

(provide 'init-git)
;;; init-git ends here
