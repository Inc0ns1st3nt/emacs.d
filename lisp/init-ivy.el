;; -*- coding: utf-8; lexical-binding: t; -*-

(ivy-mode 1) ; it enables ivy UI for `kill-buffer'

(defun inc0n/rename-file (x)
  (let ((new-path (counsel--find-file-1
                   "Rename file to: " ""
                   #'identity
                   'counsel-find-file)))
    (rename-file x new-path)))

(with-eval-after-load 'counsel
  ;; automatically pick up cygwin cli tools for counsel
  (cond
   ((executable-find "rg")
    ;; ripgrep says that "-n" is enabled actually not,
    ;; so we manually add it
    (setq counsel-grep-base-command
          (concat (executable-find "rg")
                  " -n -M 512 --no-heading --color never -i \"%s\" %s"))))

  ;; @see https://oremacs.com/2015/07/23/ivy-multiaction/
  ;; press "M-o" to choose ivy action
  (ivy-set-actions
   'counsel-find-file
   '(("j" find-file-other-frame "other frame")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("x" counsel-find-file-extern "open externally")
     ("d" delete-file "delete")
     ("r" counsel-find-file-as-root "open as root")
     ("c" inc0n/rename-file "change file name"))))

;; (setq ivy-use-virtual-buffers t) ; not good experience
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)

(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

(defvar inc0n/git-recent-files-extra-options ""
  "Extra options for git recent files.
For example, could be \"---author=MyName\"")

(defun inc0n/git-recent-files ()
  "Get files in my recent git commits."
  (let* ((default-directory (inc0n/git-root-dir))
         ;; two weeks is a sprint, minus weekend and days for sprint review and test
         (cmd (format "git --no-pager log %s --name-status --since=\"10 days ago\" --pretty=format:"
                      inc0n/git-recent-files-extra-options))
         (lines (util/lines-from-command-output cmd))
         rlt)
    (when lines
      (dolist (l lines)
        (let ((items (split-string l "[ \t]+" l)))
          ;; get file if exist only
          (reduce (lambda (acc file)
                    (let ((file (string-trim file)))
                      (if (file-exists-p file)
                          (acons file (file-truename file) acc)
                        acc)))
                  (cdr items)))))
    rlt))

(defun inc0n/counsel-recentf (&optional n)
  "Find a file on `recentf-list'.
If N is 1, only list files in current project.
If N is 2, list files in my recent 20 commits."
  (interactive "P")
  (util/ensure 'recentf)
  (unless n (setq n 0))
  (recentf-mode 1)
  (let* ((files (mapcar #'substring-no-properties recentf-list))
         (root-dir (if (ffip-project-root) (file-truename (ffip-project-root))))
         (hint "Recent files: "))
    (cond
     ((and (eq n 1) root-dir)
      (setq hint (format "Recent files in %s: " root-dir))
      (setq files (delq nil (delete-dups (mapcar (lambda (f) (path-in-directory-p f root-dir)) files)))))
     ((eq n 2)
      (setq hint (format "Files in recent Git commits: "))
      (setq files (inc0n/git-recent-files))))

    (ivy-read hint
              files
              :initial-input (if (region-active-p) (util/selected-str))
              :action (lambda (f)
                        (if (consp f) (setq f (cdr f)))
                        (with-ivy-window
                          (find-file f)))
              :caller 'counsel-recentf)))

;; grep by author is bad idea. Too slow

(defun inc0n/build-bookmark-candidate (bookmark)
  "Re-shape BOOKMARK."
  (let ((key (cond
              ((and (assoc 'filename bookmark)
                    (cdr (assoc 'filename bookmark)))
               (format "%s (%s)"
                       (car bookmark) (cdr (assoc 'filename bookmark))))
              ((and (assoc 'location bookmark)
                    (cdr (assoc 'location bookmark)))
               (format "%s (%s)"
                       (car bookmark) (cdr (assoc 'location bookmark))))
              (t
               (car bookmark)))))
    ;; key will be displayed
    ;; re-shape the data so full bookmark be passed to ivy-read
    (cons key bookmark)))

(defun counsel-bookmark-goto ()
  "Open ANY bookmark."
  (interactive)
  (autoload 'bookmark-maybe-load-default-file "bookmark" "" t)
  (bookmark-maybe-load-default-file)
  ;; do the real thing
  (ivy-read "bookmarks:"
            (delq nil (mapcar #'inc0n/build-bookmark-candidate
                              (and (boundp 'bookmark-alist)
                                   bookmark-alist)))
            :action #'bookmark-jump))

(defun counsel-insert-bash-history ()
  "Yank one command from the bash history."
  (interactive)
  (shell-command "history -r")          ; reload history
  (let ((collection
         (nreverse
          (util/read-lines (file-truename "~/.bash_history")))))
    (ivy-read (format "Bash history:") collection
              :action (lambda (val)
                        (kill-new val)
                        (message "%s => kill-ring" val)
                        (insert val)))))

(defun counsel-recent-directory (&optional n)
  "Goto recent directories.
If N is not nil, only list directories in current project."
  (interactive "P")
  (unless recentf-mode (recentf-mode 1))
  (let* ((cands (delete-dups
                 (append inc0n/dired-directory-history
                         (mapcar 'file-name-directory recentf-list)
                         ;; fasd history
                         (if (executable-find "fasd")
                             (nonempty-lines (shell-command-to-string "fasd -ld"))))))
         (root-dir (if (ffip-project-root) (file-truename (ffip-project-root)))))
    (when (and n root-dir)
      (setq cands (delq nil (mapcar (lambda (f) (path-in-directory-p f root-dir)) cands))))
    (ivy-read "directories:" cands :action 'dired)))

(defun ivy-occur-grep-mode-hook-setup ()
  "Set up ivy occur grep mode."
  ;; no syntax highlight, I only care performance when searching/replacing
  (font-lock-mode -1)
  ;; @see https://emacs.stackexchange.com/questions/598/how-do-i-prevent-extremely-long-lines-making-emacs-slow
  (column-number-mode -1)
  ;; turn on wgrep right now
  ;; (ivy-wgrep-change-to-wgrep-mode) ; doesn't work, don't know why
  (local-set-key (kbd "RET") #'ivy-occur-press-and-switch))
(add-hook 'ivy-occur-grep-mode-hook 'ivy-occur-grep-mode-hook-setup)

(defun inc0n/counsel-git-grep (&optional level)
  "Git grep in project.  If LEVEL is not nil, grep files in parent commit."
  (interactive "P")
  (let* ((str (if (region-active-p) (util/selected-str))))
    (cond
     (level
      (unless str
        (setq str (util/use-selected-string-or-ask "Grep keyword: ")))
      (when str
        (let* ((default-directory (inc0n/git-root-dir))
               ;; C-u 1 command to grep files in HEAD
               (cmd-opts (concat (inc0n/git-files-in-rev-command "HEAD" (1- level))
                                 " | xargs -I{} "
                                 "git --no-pager grep -n --no-color -I -e \"%s\" -- {}"))
               (cmd (format cmd-opts str)))
          (ivy-read "git grep in commit: "
                    (util/lines-from-command-output cmd)
                    :caller 'counsel-etags-grep
                    :history 'counsel-git-grep-history
                    :action #'counsel-git-grep-action))))
     (t
      (counsel-git-grep str)))))

(defun counsel-browse-kill-ring (&optional n)
  "If N > 1, assume just yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (inc0n/select-from-kill-ring
   (lambda (s)
     (let ((plain-str (util/insert-str s)))
       (kill-new plain-str)))))

(defun ivy-switch-buffer-matcher-pinyin (regexp candidates)
  (util/ensure 'pinyinlib)
  (let* ((pys (split-string regexp "[ \t]+"))
         (regexp (format ".*%s.*"
                         (mapconcat 'pinyinlib-build-regexp-string pys ".*"))))
    (ivy--switch-buffer-matcher regexp candidates)))

(defun ivy-switch-buffer-by-pinyin ()
  "Switch to another buffer."
  (interactive)
  (util/ensure 'ivy)
  (let* ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: " 'internal-complete-buffer
              :matcher #'ivy-switch-buffer-matcher-pinyin
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer)))

(with-eval-after-load 'ivy
  ;; work around ivy issue.
  ;; @see https://github.com/abo-abo/swiper/issues/828
  (setq ivy-display-style 'fancy))

(defun inc0n/swiper ()
  (interactive)
  (counsel-grep-or-swiper (util/thing-at-point)))

;; {{ swiper&ivy-mode
(global-set-key (kbd "C-s") 'inc0n/swiper)
;; }}

(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h f") 'counsel-describe-function)

;; {{  C-o f to toggle case sensitive, @see https://github.com/abo-abo/swiper/issues/1104
(defun re-builder-extended-pattern (str)
  "Build regex compatible with pinyin from STR."
  (let* ((len (length str)))
    (cond
     ;; do nothing
     ((<= (length str) 0))

     ;; If the first charater of input in ivy is ":",
     ;; remaining input is converted into Chinese pinyin regex.
     ;; For example, input "/ic" match "isController" or "isCollapsed"
     ((string= (substring str 0 1) ":")
      (setq str (pinyinlib-build-regexp-string (substring str 1 len) t)))

     ;; If the first charater of input in ivy is "/",
     ;; remaining input is converted to pattern to search camel case word
     ((string= (substring str 0 1) "/")
      (let* ((rlt "")
             (i 0)
             (subs (substring str 1 len))
             c)
        (when (> len 2)
          (setq subs (upcase subs))
          (while (< i (length subs))
            (setq c (elt subs i))
            (setq rlt (concat rlt (cond
                                   ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
                                    (format "%c" c))
                                   (t
                                    (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
                                              (format "%c" c))
                                            "[a-z]+")))))
            (setq i (1+ i))))
        (setq str rlt))))
    (ivy--regex-plus str)))
;; }}

(defun inc0n/imenu-or-list-tag-in-current-file ()
  "Combine the power of counsel-etags and imenu."
  (interactive)
  (counsel-etags-push-marker-stack)
  (cond
   ((inc0n/use-tags-as-imenu-function-p)
    ;; see code of `inc0n/use-tags-as-imenu-function-p'. Currently we only use ctags for imenu
    ;; in typescript because `lsp-mode' is too damn slow
    (let* ((imenu-create-index-function 'counsel-etags-imenu-default-create-index-function))
      (counsel-imenu)))
   (t
    (counsel-imenu))))

(with-eval-after-load 'ivy
  ;; better performance on everything (especially windows), ivy-0.10.0 required
  ;; @see https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 75)

  ;; Press C-p and Enter to select current input as candidate
  ;; hhttps://oremacs.com/2017/11/30/ivy-0.10.0/ttps://oremacs.com/2017/11/30/ivy-0.10.0/
  (setq ivy-use-selectable-prompt t)

  ;; use fuzzy for ivy everywhere except swiper
  ;; https://emacs.stackexchange.com/questions/36745/enable-ivy-fuzzy-matching-everywhere-except-in-swiper
  (setq ivy-re-builders-alist
        '((counsel-M-x . ivy--regex-fuzzy)
          (counsel-describe-function . ivy--regex-fuzzy)
          (counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))) ;; (t . re-builder-extended-pattern)
  ;; set actions when running C-x b
  ;; replace "frame" with window to open in new window
  (ivy-set-actions
   'ivy-switch-buffer-by-pinyin
   '(("j" switch-to-buffer-other-frame "other frame")
     ("k" kill-buffer "kill")
     ("r" ivy--rename-buffer-action "rename"))))

(defun inc0n/counsel-company ()
  "Input code from company backend using fuzzy matching."
  (interactive)
  (company-abort)
  (let* ((company-backends '(company-ctags))
         (company-ctags-fuzzy-match-p t))
    (counsel-company)))


(defun counsel-ag-thing-at-point ()
  (interactive)
  (counsel-ag (ivy-thing-at-point)))

(defun counsel-rg-thing-at-point ()
  (interactive)
  (counsel-rg (ivy-thing-at-point)))

(provide 'init-ivy)
