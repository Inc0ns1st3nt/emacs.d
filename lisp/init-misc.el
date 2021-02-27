;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:

;; Miscellaneous configurations

;;; Code:

;; Avoid potential lag:
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
;; `next-line' triggers the `format-mode-line' which triggers `projectile-project-name'
;; I use find-file-in-project instead of projectile. So I don't have this issue at all.
;; Set `auto-window-vscroll' to nil to avoid triggering `format-mode-line'.
(setq auto-window-vscroll nil)

;; midnight mode purges buffers which haven't been displayed in configured period
;; (require-package 'midnight)
(setq midnight-period (* 3600 24)) ;; 24 hours
(add-hook 'after-init-hook 'midnight-mode)

;; @see http://www.emacswiki.org/emacs/SavePlace
;; (add-hook 'after-init-hook 'save-place-mode)
(util/add-to-timed-init-hook 1 'save-place-mode)
(util/add-to-timed-init-hook 1 'amx-mode)

(setq confirm-kill-emacs 'y-or-n-p)

(defun transient/kill-buffer ()
  "Transient interface for `evil-jump'."
  (interactive)
  (let ((echo-keystrokes echo-keystrokes))
	(call-interactively 'kill-buffer)
	(message "kill-buffer: [k]ill more [q]uit")
	(set-transient-map
	 (let ((map (make-sparse-keymap)))
	   (define-key map [?k] #'transient/kill-buffer)
	   map)
	 t)))

(defun inc0n/unbound-symbol (arg)
  (interactive (list (thing-at-point 'symbol)))
  (cond ((stringp arg)
		 (inc0n/unbound-symbol (intern arg)))
		((symbolp arg)
		 (if (functionp arg)
			 (progn (fmakunbound arg)
					(message "fmakunbounded %s" arg))
		   (makunbound arg)
		   (message "makunbounded %s" arg)))
		(t (message "unexpected %s" arg))))

(general-define-key
 "C-c C-u" 'inc0n/unbound-symbol
 "<C-backspace>" 'backward-delete-word
 "C-x C-o" 'ffap
 ;;
 "C-h C-f" 'find-function
 "C-h K" 'find-function-on-key
 "C-k" 'kill-sexp
 ;;
 "TAB" 'tab-out-delimiter) ;; 'indent-for-tab-command

(defun backward-delete-word ()
  "my function that doesn't actually register deleted word into clipboard"
  (interactive)
  (delete-region (point)
				 (progn (forward-word -1)
						(point))))

(defun tab-out-delimiter ()
  "Move cursor out of a consecutive block of delimiters"
  (interactive)
  (if (not
	   (memq (char-syntax (following-char)) '(?\) ?\")))
	  (indent-for-tab-command)
	(just-one-space 0) ;; delete any space before delimiter
	(forward-char 1)))

;; {{ auto-yasnippet
(require-package 'auto-yasnippet)
;; Use C-q instead tab to complete snippet
;; - aya-create at first, input ~ to mark the thing next
;; - aya-expand to expand snippet
;; - aya-open-line to finish
(global-set-key (kbd "C-q") 'aya-open-line)
;; }}

;; {{ ace-link
(require-package 'ace-link)
(with-eval-after-load 'ace-link
  (global-set-key (kbd "M-o") 'ace-link)
  (ace-link-setup-default))
;; }}

;; {{ isearch
;; Use regex to search by default
(global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward-regexp)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
;; }}

(setq-default buffers-menu-max-size 30
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function #'split-window-horizontally
              ediff-window-setup-function #'ediff-setup-windows-plain
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode t
              mouse-yank-at-point nil
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              ;; void problems with crontabs, etc.
              ;; require-final-newline t ; bad idea, could accidentally edit others' code
              truncate-lines nil
              truncate-partial-width-windows nil
              ;; visible-bell has some issue
              ;; @see https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/issues/9#issuecomment-97848938
              visible-bell nil
			  line-spacing 1)

(defun toggle-indent-tabs-mode ()
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode))
  (message "indent-tabs-mode is turned %s" (if indent-tabs-mode "off" "on")))

;; {{ find-file-in-project (ffip)
(require-package 'find-file-in-project)
(with-eval-after-load 'find-file-in-project
  (defun inc0n/search-git-reflog-code ()
    (let ((default-directory
            (inc0n/git-root-dir)))
      (ffip-shell-command-to-string
       (format "git --no-pager reflog --date=short -S\"%s\" -p"
               (read-string "Regex: ")))))
  (push #'inc0n/search-git-reflog-code ffip-diff-backends)
  (setq ffip-match-path-instead-of-filename t))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (if-let ((project-dir (ffip-get-project-root-directory))
           (file-name (buffer-file-name)))
      (progn
        (neotree-dir project-dir)
        (neotree-find file-name))
    (message "Could not find git project root.")))
;; }}

;; {{ gradle
(defun inc0n/run-gradle-in-shell (cmd)
  (interactive "sEnter a string:")
  (when-let ((root-dir
              (locate-dominating-file default-directory "build.gradle")))
    (let ((default-directory root-dir))
      (shell-command (concat "gradle " cmd "&")))))
;; }}

;; {{ dictionary setup
(defun inc0n/lookup-dict-org (word)
  (interactive (list (util/thing-at-point)))
  (dictionary-new-search
   (cons (util/use-selected-string-or-ask "Input word for dict.org"
                                          word)
         dictionary-default-dictionary)))
;; }}

;; {{ bookmark
;; use my own bookmark if it exists
(with-eval-after-load 'bookmark
  (when (file-exists-p (file-truename "~/.emacs.bmk"))
    (setq bookmark-file (file-truename "~/.emacs.bmk"))))
;; }}

(defun lookup-doc-in-man ()
  "Read man by querying keyword at point."
  (interactive)
  (man (concat "-k " (util/use-selected-string-or-ask))))

;; @see http://blog.binchen.org/posts/effective-code-navigation-for-web-development.html
;; don't let the cursor go into minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))

(defun compilation-finish-hide-buffer-on-success (buffer str)
  "Bury BUFFER whose name marches STR.
This function can be re-used by other major modes after compilation."
  (if (string-match "exited abnormally" str)
      ;;there were errors
      (message "compilation errors, press C-x ` to visit")
    ;;no errors, make the compilation window go away in 0.5 seconds
    (when (and (buffer-name buffer)
               (string-match "*compilation*" (buffer-name buffer)))
      ;; @see http://emacswiki.org/emacs/ModeCompile#toc2
      (bury-buffer buffer)
	  (with-selected-window (get-buffer-window buffer)
		(delete-window))
      ;; (winner-undo)
      (message "NO COMPILATION ERRORS!"))))

(defun inc0n/electric-pair-inhibit (char)
  ;; (electric-pair-conservative-inhibit char)
  (or (and (memq major-mode '(minibuffer-inactive-mode))
		   (not (string-match "^Eval:" (buffer-string))))
	  ;; input single/double quotes at the end of word
	  (and (memq char '(?\" ?\'))
           (char-before (1- (point)))
           (eq (char-syntax (char-before (1- (point)))) ?w))
	  ;; I find it more often preferable not to pair when the
	  ;; same char is next.
	  (eq char (char-after))
	  ;; Don't pair up when we insert the second of "" or of ((.
	  (and (eq char ?\")
		   (eq char (char-before (1- (point)))))
	  ;; I also find it often preferable not to pair next to a word.
	  (eq (char-syntax (following-char)) ?w)))

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate #'inc0n/electric-pair-inhibit))


(defun my-prog-nuke-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
	;; only operate in the visible region of the window
	;; with exception to the current line
	(let ((b (line-beginning-position))
		  (e (line-end-position)))
	  (if (or (< b (window-start))
			  (> e (window-end)))
		  (delete-trailing-whitespace (window-start)
									  (window-end))
		(delete-trailing-whitespace (window-start)
									(line-beginning-position))
		(delete-trailing-whitespace (line-end-position)
									(window-end))))))
(add-hook 'before-save-hook 'my-prog-nuke-trailing-whitespace)

(defun buffer-too-big-p ()
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

(with-eval-after-load 'flymake
  (setq flymake-gui-warnings-enabled nil))

(defun generic-prog-mode-hook-setup ()
  (when (buffer-too-big-p)
    ;; Turn off `linum-mode' when there are more than 5000 lines
	(linum-mode -1)
    (when (should-use-minimum-resource)
      (font-lock-mode -1)))

  (util/ensure 'lazyflymake)
  (lazyflymake-start)

  (company-ispell-setup)

  (unless (buffer-file-temp-p)
    ;; @see http://xugx2007.blogspot.com.au/2007/06/benjamin-rutts-emacs-c-development-tips.html
    (setq compilation-finish-functions '(compilation-finish-hide-buffer-on-success))

    ;; enable for all programming modes
    ;; http://emacsredux.com/blog/2013/04/21/camelcase-aware-editing/
    (unless (derived-mode-p 'js2-mode)
      (subword-mode 1))

    (electric-pair-mode 1) ;; auto insert pairing delimiter
	(hs-minor-mode)			;; code/comment fold
	;; (turn-on-auto-fill)		;; auto indent
    (turn-on-eldoc-mode) ;; eldoc, show API doc in minibuffer echo area
    (setq show-trailing-whitespace t)))

(add-hook 'prog-mode-hook #'generic-prog-mode-hook-setup)
;; some major-modes NOT inherited from prog-mode

;; {{ display long lines in truncated style (end line with $)
(add-hook 'grep-mode-hook (lambda ()
							(setf truncate-lines nil)))
;; }}

;; turn on auto-fill-mode, don't use `text-mode-hook' because for some
;; mode (org-mode for example), this will make the exported document
;; ugly!
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)
(add-hook 'change-log-mode-hook #'turn-on-auto-fill)
(add-hook 'cc-mode-hook #'turn-on-auto-fill)

;; some project prefer tab, so be it
;; @see http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
(setq-default tab-width 4)

(setq history-delete-duplicates t)

;; NO automatic new line when scrolling down at buffer bottom
(setq next-line-add-newlines nil)

;; {{ time format 'built-in'
;; If you want to customize time format, read document of `format-time-string'
;; and customize `display-time-format'.
;; (setq display-time-format "%a %b %e")

;; from RobinH, Time management
(setq display-time-24hr-format t) ; the date in modeline is English too, magic!
(setq display-time-day-and-date t)
(util/add-to-timed-init-hook 1 'display-time-mode) ;; show date in modeline
;; }}

;; (defalias 'list-buffers #'ibuffer)

;; {{ show email sent by `git send-email' in gnus
(with-eval-after-load 'gnus
  (when (local-require 'gnus-article-treat-patch)
    (setq gnus-article-patch-conditions
          '("^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@"))))
;; }}

(defun add-pwd-into-load-path ()
  "add current directory into load-path, useful for elisp developers"
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (when (not (memq dir load-path))
      (add-to-list 'load-path dir))
    (message "Directory added into load-path:%s" dir)))

(setq system-time-locale "C")

(setq imenu-max-item-length 128)

;; {{ recentf-mode
(with-eval-after-load 'recentf
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-saved-items 512
		recentf-exclude '("/tmp/"
                          "/ssh:"
                          "/sudo:"
                          "recentf$"
                          "company-statistics-cache\\.el$"
                          ;; ctags
                          "/TAGS$"
                          ;; global
                          "/GTAGS$"
                          "/GRAGS$"
                          "/GPATH$"
                          ;; binary
                          "\\.mkv$"
                          "\\.mp[34]$"
                          "\\.avi$"
                          "\\.wav$"
                          "\\.docx?$"
                          "\\.xlsx?$"
                          ;; sub-titles
                          "\\.sub$"
                          "\\.srt$"
                          "\\.ass$")))
;; }}

;; {{ popup functions
(defun inc0n/which-file ()
  "Return current file name for Yasnippets."
  (if (buffer-file-name)
      (format "%s:" (file-name-nondirectory (buffer-file-name)))
    ""))

(defun inc0n/which-function ()
  "Return current function name."
  ;; @see http://stackoverflow.com/questions/13426564/how-to-force-a-rescan-in-imenu-by-a-function
  (util/ensure 'imenu)
  (let ((imenu-auto-rescan t)
        (imenu-create-index-function
         (if (inc0n/use-tags-as-imenu-function-p)
             #'counsel-etags-imenu-default-create-index-function
           imenu-create-index-function))
        (imenu-auto-rescan-maxout (buffer-size)))
	;; (delete (assoc "*Rescan*" items) items)
    (imenu--make-index-alist t))
  (which-function))

(defun popup-which-function ()
  "Popup which function message."
  (interactive)
  (popup-tip (inc0n/which-function)))
;; }}

;; (require-package 'ace-pinyin)
;; (add-hook 'after-init-hook 'ace-pinyin-global-mode)

;; {{ avy, jump between texts, like easymotion in vim
;; @see http://emacsredux.com/blog/2015/07/19/ace-jump-mode-is-dead-long-live-avy/ for more tips
;; dired
(with-eval-after-load 'dired
  ;; (diredfl-global-mode 1)
  (add-hook 'dired-mode-hook 'diredfl-mode)
  (define-key dired-mode-map (kbd ";") 'avy-goto-subword-1))
;; }}

;; {{start dictionary lookup
;; use below commands to create dictionary
;; mkdir -p ~/.stardict/dic
;; # wordnet English => English
;; check out https://willschenk.com/articles/2020/getting_websters/
;; # Langdao Chinese => English
;; curl http://abloz.com/huzheng/stardict-dic/zh_CN/stardict-langdao-ec-gb-2.4.2.tar.bz2 | tar jx -C ~/.stardict/dic
(with-eval-after-load 'sdcv
  (setq sdcv-dictionary-simple-list
		'("Webster's Revised Unabridged Dictionary (1913)"))
  (setq sdcv-dictionary-complete-list
		'("Webster's Revised Unabridged Dictionary (1913)")))
;; }}

;; ANSI-escape coloring in compilation-mode
;; {{ http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(when (require 'ansi-color nil t)
  (defun inc0n/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook #'inc0n/colorize-compilation-buffer))
;; }}

(defun inc0n/minibuffer-setup-hook ()
  ;; (local-set-key (kbd "C-k") #'kill-line)
  (subword-mode t) ; enable subword movement in minibuffer
  (setq gc-cons-threshold most-positive-fixnum))

(defun inc0n/minibuffer-exit-hook ()
  ;; evil-mode also use minibuf
  (setq gc-cons-threshold normal-gc-cons-threshold))

;; @see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(add-hook 'minibuffer-setup-hook #'inc0n/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'inc0n/minibuffer-exit-hook)

;; {{ Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
;; Press "q" in evil-mode or "C-c C-c" to exit the diff output buffer
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (if (> b e)
	  ;; swap b e, make sure b < e
	  (diff-region-format-region-boundary e b)
	;; select lines
	(save-excursion
	  ;; Another workaround for evil-visual-line bug:
	  ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
	  ;; the (line-beginning-position) of the line which is after the last selected
	  ;; line is always (region-end)! Don't know why.
	  (when (and (> e b)
				 (bolp e) ;; beginning-of-line-p
				 (boundp 'evil-state)
				 (eq evil-state 'visual))
		(setq e (1- e)))
	  (goto-char b)
	  (let ((b (line-beginning-position)))
		(goto-char e)
		(list b (line-end-position))))))

(defun diff-region-open-diff-output (content buffer-name)
  (let ((rlt-buf (get-buffer-create buffer-name)))
    (save-current-buffer
      (switch-to-buffer-other-window rlt-buf)
      (set-buffer rlt-buf)
      (erase-buffer)
      (insert content)
      ;; `ffip-diff-mode' is more powerful than `diff-mode'
      (ffip-diff-mode)
      (goto-char (point-min)))))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare."
  (interactive)
  (when (region-active-p)
    (let ((tmp (diff-region-format-region-boundary (region-beginning)
                                                   (region-end)))
          (buf (get-buffer-create "*Diff-regionA*")))
      ;; select lines
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b'"))

(defun diff-region-compare-with-b ()
  "Compare current region with the region set by `diff-region-tag-selected-as-a'.
If no region is selected, `kill-ring' or clipboard is used instead."
  (interactive)
  (let ((prefix (expand-file-name "diff-region"
                                  (or small-temporary-file-directory
                                      temporary-file-directory))))
	(let ((fa (make-temp-file prefix))	;; file A
		  (fb (make-temp-file prefix)))	;; file B
	  (when (and fa (file-exists-p fa)
				 fb (file-exists-p fb))
		(if (region-active-p)
			;; text from selected region
			(let ((tmp
				   (diff-region-format-region-boundary (region-beginning) (region-end))))
			  (write-region (car tmp) (cadr tmp) fb))
		  ;; text from `kill-ring' or clipboard
		  (let* ((choice (completing-read "Since no region selected, compare text in:"
										  '("kill-ring" "clipboard")))
				 (txt (cond ((string= choice "kill-ring") (car kill-ring))
							((string= choice "clipboard") (util/get-clip)))))
			(with-temp-file fb
			  (insert txt))))
		;; save region A as file A
		(save-current-buffer
		  (set-buffer (get-buffer-create "*Diff-regionA*"))
		  (write-region (point-min) (point-max) fa))
		;; diff NOW!
		;; show the diff output
		(let ((diff-output
			   (shell-command-to-string
				(format "%s -Nabur %s %s" diff-command fa fb))))
		  (cond ((string-empty-p diff-output)
				 (message "Two regions are SAME!"))
				((executable-find "git")
				 (util/ensure 'magit)
				 (magit-diff-setup nil (list "--no-index" "--indent-heuristic" "--histogram")
								   nil (list (magit-convert-filename-for-git
											  (expand-file-name fa))
											 (magit-convert-filename-for-git
											  (expand-file-name fb))))
				 (ffip-diff-mode))
				(t
				 (diff-region-open-diff-output diff-output
											   "*Diff-region-output*"))))
		;; clean the temporary files
		(when (and fa (file-exists-p fa))
		  (delete-file fa))
		(when (and fb (file-exists-p fb))
		  (delete-file fb))))))
;; }}

(defun extract-list-from-package-json ()
  "Extract package list from package.json."
  (interactive)
  (let ((str (util/use-selected-string-or-ask))
	(setq str (replace-regexp-in-string ":.*$\\|\"" "" str))
	;; join lines)
	(setq str (replace-regexp-in-string "[\r\n \t]+" " " str))
    (util/set-clip str)
    (message "%s => clipboard & yank ring" str))))

(defun inc0n/insert-absolute-path ()
  "Relative path to full path."
  (interactive)
  (util/insert-str
   (file-truename
	(read-file-name "Input relative path"))))

(defun inc0n/insert-relative-path ()
  "Full path to relative path."
  (interactive)
  (util/insert-str
   (file-relative-name
	(read-file-name "Input relative path"))))

;; {{ auto-save - builtin emacs >= 26.1 package
(setq auto-save-timeout 2)
(setq auto-save-interval 100) ;; 100 characters interval
(setq auto-save-default nil)
(setq auto-save-no-message t)
(add-hook 'after-init-hook 'auto-save-mode)

(setq auto-save-visited-interval 2) ;; in seconds
(add-hook 'after-init-hook 'auto-save-visited-mode)
;; (when (local-require 'auto-save)
;;   (add-to-list 'auto-save-exclude 'file-too-big-p t)
;;   (setq auto-save-idle 1) ; 1 seconds
;;   (setq auto-save-slient t)
;;   (add-hook 'after-init-hook 'auto-save-enable))
;; }}

;; {{ csv
(with-eval-after-load 'csv-mode
  (setq csv-separators '("," ";" "|" " ")))
;; }}

;; {{ regular expression tools
(defun inc0n/create-regex-from-kill-ring (&optional n)
  "Create extended regex from first N items of `kill-ring'."
  (interactive "p")
  (when (and kill-ring (> (length kill-ring) 0))
    (when (> n (length kill-ring))
      (setq n (length kill-ring)))
    (let ((str
		   (mapconcat 'identity (subseq kill-ring 0 n) "|")))
	  (setq str
			(replace-regexp-in-string "(" "\\\\(" str))
      (util/set-clip str)
      (message (format "%s => kill-ring&clipboard" str)))))
;; }}

(defun inc0n/get-total-hours (str)
  (interactive (list (if (region-active-p)
						 (util/selected-str)
					   (util/buffer-str))))
  (let ((total-hours
		 (cl-reduce (lambda (total-hours line)
					  (if (string-match " \\([0-9][0-9.]*\\)h[ \t]*$" line)
						  (+ total-hours (string-to-number (match-string 1 line)))
						total-hours))
					(split-string str "[\r\n]+" t)
					:initial-value 0)))
    (message "total-hours=%s" total-hours)))

;; {{ emmet (auto-complete html tags)
;; @see https://github.com/rooney/zencoding for original tutorial
;; @see https://github.com/smihica/emmet for new tutorial
;; C-j or C-return to expand the line
(add-hook 'sgml-mode-hook 'emmet-mode) ; `sgml-mode` is parent of `html-mode'
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'rjsx-mode-hook  'emmet-mode)
;; }}

(defun sgml-mode-hook-setup ()
  "sgml/html mode setup."
  ;; let web-mode handle indentation by itself since it does not
  ;; derive from `sgml-mode'
  (setq-local indent-region-function #'sgml-pretty-print))
(add-hook 'sgml-mode-hook #'sgml-mode-hook-setup)

;; {{ xterm
(defun run-after-make-frame-hooks (frame)
  (select-frame frame)
  (unless window-system
    ;; Mouse in a terminal (Use shift to paste with middle button)
    (xterm-mouse-mode 1)))
(add-hook 'after-make-frame-functions #'run-after-make-frame-hooks)
;; }}

;; {{ check attachments
(defun inc0n/message-current-line-cited-p ()
  "Indicate whether the line at point is a cited line."
  (save-match-data
    (string-match (concat "^" message-cite-prefix-regexp)
                  (buffer-substring (line-beginning-position) (line-end-position)))))

(defun inc0n/message-says-attachment-p ()
  "Return t if the message suggests there can be an attachment."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward "\\(attach\\|pdf\\|file\\|screen ?shot\\)" nil t))))

(defun inc0n/message-has-attachment-p ()
  "Return t if an attachment is already attached to the message."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward "<#part" nil t))))

(defun inc0n/message-pre-send-check-attachment ()
  "Check attachment before send mail."
  (when (and (inc0n/message-says-attachment-p)
             (not (inc0n/message-has-attachment-p)))
    (unless
		(y-or-n-p "The message suggests that you may want to attach something, but no attachment is found. Send anyway?")
      (error "It seems that an attachment is needed, but none was found. Aborting sending."))))
(add-hook 'message-send-hook #'inc0n/message-pre-send-check-attachment)
;; }}

(defun minibuffer-inactive-mode-hook-setup ()
  ;; Make `try-expand-dabbrev' from `hippie-expand' work in mini-buffer.
  ;; @see `he-dabbrev-beg', so we need re-define syntax for '/'.
  (set-syntax-table (let ((table (make-syntax-table)))
                      (modify-syntax-entry ?/ "." table)
                      table)))
(add-hook 'minibuffer-inactive-mode-hook #'minibuffer-inactive-mode-hook-setup)

;; {{ vc-msg
(defun vc-msg-hook-setup (vcs-type commit-info)
  ;; copy commit id to clipboard
  (util/set-clip (plist-get commit-info :id)))
(add-hook 'vc-msg-hook #'vc-msg-hook-setup)

(defun vc-msg-show-code-setup ()
  "Use `ffip-diff-mode' instead of `diff-mode'."
  (util/ensure 'find-file-in-project)
  (ffip-diff-mode))
(add-hook 'vc-msg-show-code-hook #'vc-msg-show-code-setup)
;; }}

;; {{ typewriter
;; (local-require 'typewriter-mode)
;; (add-hook 'after-init-hook #'typewriter-mode)
;; }}

(with-eval-after-load 'grep
  ;; eacl and other general grep (rgrep, grep ...) setup
  (dolist (v '("auto"
               "target"
               "node_modules"
               "bower_components"
               "*dist"
               ".sass_cache"
               ".cache"
               ".npm"
               "elpa"))
    (add-to-list 'grep-find-ignored-directories v))
  (dolist (v '("*.min.js"
               "*.map"
               "*.bundle.js"
               "*.min.css"
               "tags"
               "TAGS"
               "GTAGS"
               "GRTAGS"
               "GPATH"
               "cscope.files"
               "*.json"
               "*.log"))
    (add-to-list 'grep-find-ignored-files v))

  ;; wgrep and rgrep, inspired by http://oremacs.com/2015/01/27/inc0n/refactoring-workflow/
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

;; {{ https://www.emacswiki.org/emacs/EmacsSession better than "desktop.el" or "savehist".
(require-package 'session)
;; Any global variable matching `session-globals-regexp' is saved *automatically*.

(with-eval-after-load 'session
  (setq session-save-file (inc0n/emacs-d ".session"))
  (setq session-globals-max-size 512)
  ;; can store 4Mb string
  (setq session-globals-max-string (* 4 1024))
  (setq session-globals-include '(kill-ring
                                  (session-file-alist 100 t)
                                  inc0n/dired-commands-history
                                  file-name-history
                                  search-ring
                                  regexp-search-ring))
  (setq session-save-file-coding-system 'utf-8))
(add-hook 'after-init-hook #'session-initialize)
;; }}

;; {{
(require-package 'adoc-mode) ; asciidoc files

(defun adoc-imenu-index ()
  (let ((patterns '((nil "^=\\([= ]*[^=\n\r]+\\)" 1))))
    (save-excursion
      (imenu--generic-function patterns))))

(defun adoc-mode-hook-setup ()
  ;; Don't wrap lines because there is table in `adoc-mode'.
  (setq truncate-lines t)
  (setq imenu-create-index-function 'adoc-imenu-index))
(add-hook 'adoc-mode-hook #'adoc-mode-hook-setup)
;; }}

(with-eval-after-load 'compile
  (defun inc0n/compile-hack (orig-func &rest args)
    (if (member major-mode '(octave-mode))
        (octave-send-buffer)
      (apply orig-func args)))
  (advice-add 'compile :around #'inc0n/compile-hack)

  (add-to-list 'compilation-error-regexp-alist-alist
               (list 'mocha "at [^()]+ (\\([^:]+\\):\\([^:]+\\):\\([^:]+\\))" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'mocha))

(defun switch-to-builtin-shell ()
  "Switch to builtin shell.
If the shell is already opened in some buffer, switch to that buffer."
  (interactive)
  (if-let ((shell (get-buffer "*shell*")))
	  (let ((dir (file-name-directory (buffer-file-name))))
		(with-current-buffer (switch-to-buffer shell)
		  ;;TODO: change directory to current directory of the buffer
		  (setq default-directory dir)))
	(shell)))

;; {{ emms
(require-package 'emms)
(with-eval-after-load 'emms
  (emms-all)
  (setq emms-source-file-default-directory "~/Music"
        emms-info-asynchronously t
        emms-show-format "♩♪ %s")
  (setq emms-player-list '(emms-player-mplayer
						   emms-player-mplayer-playlist)))
;; }}

(add-hook 'after-init-hook 'transient-mark-mode) ;; wanted

(add-hook 'after-init-hook 'global-auto-revert-mode)
(with-eval-after-load 'autorevert
  (setq auto-revert-verbose nil
		global-auto-revert-non-file-buffers t))

(defun inc0n/insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%d %B %Y"))))
    (insert (format-time-string format))))

(defun inc0n/insert-timestamp ()
  "Insert time stamps at current position."
  (interactive)
  (let ((current-date-time-format "%a %b %d %H:%M %Z %Y"))
    (insert (format-time-string current-date-time-format (current-time)))))

;; show ascii table
(defun ascii-table ()
  "Print the ascii table."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (insert (format "ASCII characters up to number %d.\n" 254))
  (dotimes (i 255)
	(insert (format "%4d %c\n" i i)))
  (beginning-of-buffer)
  (read-only-mode 1))

;; unique lines
(defun uniq-lines (beg end)
  "Delete duplicate lines in region or buffer."
  (interactive (if (region-active-p)
				   (list (region-beginning) (region-end))
				 (list (point-min) (point-max))))
  (save-excursion
    (while (progn
			 (goto-char bed)
			 (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
      (replace-match "\\1\n\\2"))))

;; from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun vc-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (if (vc-backend filename)
            (vc-rename-file filename new-name)
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun vc-copy-file-and-rename-buffer ()
  "Copy the current buffer and file it is visiting.
If the old file is under version control, the new file is added into
version control automatically."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (copy-file filename new-name t)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)
        (when (vc-backend filename)
          (vc-register))))))

(defun toggle-env-http-proxy ()
  "Set/unset the environment variable http_proxy used by w3m."
  (interactive)
  (let ((proxy "http://127.0.0.1:8000"))
    (cond
     ((string= (getenv "http_proxy") proxy)
      (setenv "http_proxy" "")
      (message "env http_proxy is empty now"))
     (t
      (setenv "http_proxy" proxy)
      (message "env http_proxy is %s now" proxy)))))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun cleanup-buffer ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

(defun cleanup-buffer-and-indent ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer)
  (indent-region (point-min) (point-max)))

;; {{ easygpg setup
;; @see http://www.emacswiki.org/emacs/EasyPG#toc4
(with-eval-after-load 'epg
  (defun inc0n/epg--start-hack (orig-func &rest args)
    "Make `epg--start' not able to find gpg-agent."
    (let ((agent (getenv "GPG_AGENT_INFO")))
      (setenv "GPG_AGENT_INFO" nil)
      (apply orig-func args)
      (setenv "GPG_AGENT_INFO" agent)))
  (advice-add 'epg--start :around #'inc0n/epg--start-hack)

  (unless (string-match-p
           "^gpg (GnuPG) 1.4"
           (shell-command-to-string (format "%s --version" epg-gpg-program)))

    ;; "apt-get install pinentry-tty" if using emacs-nox
    ;; Create `~/.gnupg/gpg-agent.conf' which has one line
    ;; "pinentry-program /usr/bin/pinentry-curses"
    (setq epa-pinentry-mode 'loopback)))
;; }}

;; {{ show current function name in `mode-line'
;; (defun inc0n/which-func-update-hack (orig-func &rest args)
;;   "`which-function-mode' scanning makes Emacs unresponsive in big buffer."
;;   (unless (buffer-too-big-p)
;;     (apply orig-func args)))
;; (advice-add 'which-func-update :around #'inc0n/which-func-update-hack)

(autoload 'which-function "which-func")
;; (with-eval-after-load 'which-function
;;   (add-to-list 'which-func-modes 'org-mode))
;; (which-function-mode 1)
;; }}

;; {{ pomodoro
(require-package 'pomodoro)
(with-eval-after-load 'pomodoro
  (setq pomodoro-play-sounds nil		; *.wav is not installed
		pomodoro-break-time 2
		pomodoro-long-break-time 5
		pomodoro-work-time 15)
  (push '(pomodoro-mode-line-string pomodoro-mode-line-string) mode-line-format))

;; (unless (featurep 'omodoro)
;;   (require 'pomodoro))
;; }}

;; {{ epub setup
(require-package 'nov)

(with-eval-after-load 'nov
  (setq nov-text-width t)
  (add-to-list 'evil-emacs-state-modes 'nov-mode)
  ;;
  (general-define-key
   :keymaps 'nov-mode-map
   "j" 'next-line
   "k" 'previous-line
   "w" 'mybigword-pronounce-word
   ";" 'avy-goto-char-2
   "d" (lambda ()
		 (interactive)
		 ;; go to end of word to workaround `nov-mode' bug
		 (forward-word)
		 (forward-char -1)
		 (sdcv-search-input (thing-at-point 'word))))
  (add-hook 'nov-mode-hook (lambda ()
							 "Set up of `nov-mode'."
							 (display-line-numbers-mode -1))))
;; }}

;; {{ octave
(defun octave-mode-hook-setup ()
  "Set up of `octave-mode'."
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (when (eq window-system 'x)
    (font-lock-mode 1))
  (setq-local comment-start "%"
			  comment-add 0))
(add-hook 'octave-mode-hook #'octave-mode-hook-setup)
;; }}

;; {{ wgrep setup
(require-package 'wgrep)
(with-eval-after-load 'wgrep
  (define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit)
  ;; save the change after wgrep finishes the job
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-too-many-file-length 2024))
;; }}

;; {{ edit-server
(require-package 'edit-server)
(defun edit-server-start-hook-setup ()
  "Some web sites actually pass html to edit server."
  (let ((url (buffer-name)))
    (cond ((string-match "github.com" url)
		   (markdown-mode))
		  ((string-match "zhihu.com" url)
		   ;; `web-mode' plus `sgml-pretty-print' get best result
		   (web-mode)
		   ;; format html
		   (util/ensure 'sgml)
		   (sgml-pretty-print (point-min) (point-max))
		   (goto-char (point-min))
		   ;; insert text after removing br tag, that's required by zhihu.com
		   ;; unfortunately, after submit comment once, page need be refreshed.
		   (replace-regexp "<br data-text=\"true\">" "")))))
(add-hook 'edit-server-start-hook #'edit-server-start-hook-setup)

(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (add-hook 'after-init-hook 'edit-server-start))
;; }}

;; {{ which-key-mode
(require-package 'which-key)
(setq which-key-allow-imprecise-window-fit t) ; performance
(setq which-key-idle-delay 0.5)
(setq which-key-separator ":"
	  which-key-add-column-padding 1)
(setq which-key-min-display-lines 2)
(add-hook 'after-init-hook #'which-key-mode)
;; }}

;; {{ eldoc
(with-eval-after-load 'eldoc
  ;; multi-line message should not display too soon
  (setq eldoc-idle-delay 0.5
		eldoc-echo-area-use-multiline-p t))
;; }}

(defun inc0n/browse-file ()
  "Open the a file, ask for a file path via `find-file', points to the
current file, as a URL via `browse-url'."
  (interactive)
  (browse-url-generic
   (read-file-name "New name: "
				   (or (buffer-file-name)
					   default-directory))))

;; {{ `browse-url' setup
(setq-default browse-url-generic-program "firefox")
(setq-default browse-url-generic-args '("--private-window"))
(setq-default browse-url-firefox-arguments '("--private-window"))
;; }}

;; {{ cache files
(cl-flet ((inc0n/emacs.d/cache (path)
							   (inc0n/emacs-d (concat "var/" path))))
  (unless (file-directory-p (inc0n/emacs-d "var"))
	(make-directory (inc0n/emacs-d "var")))
  (setq amx-save-file (inc0n/emacs.d/cache "amx-items"))
  (setq ido-save-directory-list-file (inc0n/emacs.d/cache "ido.last"))
  (setq company-statistics-file (inc0n/emacs.d/cache "company-statistics-cache.el"))

  (setq eshell-aliases-file (inc0n/emacs.d/cache "eshell/alias")
		eshell-history-file (inc0n/emacs.d/cache "eshell/history")
		eshell-last-dir-ring-file-name (inc0n/emacs.d/cache "eshell/lastdir"))
  (setq pyim-dcache-directory (inc0n/emacs.d/cache "pyim/dcache/"))
  (setq save-place-file (inc0n/emacs.d/cache "places"))
  (setq recentf-save-file (inc0n/emacs.d/cache "recentf"))
  (setq nov-save-place-file (inc0n/emacs.d/cache "nov-places"))
  (setq auto-save-list-file-prefix (inc0n/emacs.d/cache "auto-save-list/.saves-"))
  (setq keyfreq-file (inc0n/emacs.d/cache "keyfreq")
		keyfreq-file-lock (inc0n/emacs.d/cache "keyfreq.lock")))
;; }}

;; {{ ligature
(local-require 'ligature)
(setq ligature-composition-table nil)
(ligature-set-ligatures 'prog-mode '("::" ":::" "->" "=>" "==" "!="
									 "++" "<-" "/=" ">=" "<=" ".."
									 "..." "&&" "||" "//"))
(add-hook 'after-init-hook
		  (lambda ()
			(global-ligature-mode 0)))
;; }}

;; {{
;; (require-package 'highlight-thing)
;; (global-highlight-thing-mode nil)
;; (setq highlight-thing-delay-seconds 1.0
;; 	  highlight-thing-limit-to-region-in-large-buffers-p t)
;; }}

(require-package 'highlight-symbol)
(with-eval-after-load 'highlight-symbol
  (setq highlight-symbol-colors
		(delete "SpringGreen1"
				(delete "yellow" highlight-symbol-colors))
		highlight-symbol-idle-delay 1.0))

(with-eval-after-load 'rainbow-delimiters
  (setq rainbow-delimiters-max-face-count 1))

(defun clean-backup-dir ()
  "delete the files in the backup dir that are not in the list of
`recentf-list'"
  (cl-labels ((aux (x)
				   (let ((x (subst-char-in-string ?! ?/ x)))
					 (substring (subst-char-in-string ?! ?/ x)
								0 (- (length x) 5)))))
	(mapcar (lambda (dir-pair)
			  (let* ((dir (cdr dir-pair))
					 (files-to-delete
					  (cl-set-difference
					   (mapcar #'aux (butlast (directory-files dir) 2))
					   recentf-list
					   :test 'string=)))
				(dolist (f files-to-delete)
				  (dolist (f (directory-files
							  dir
							  t
							  (concat "^" (file-name-nondirectory f) ".*")))
					(delete-file f)))
				(cons (car dir-pair)
					  (length files-to-delete))))
			backup-directory-alist)))

(provide 'init-misc)