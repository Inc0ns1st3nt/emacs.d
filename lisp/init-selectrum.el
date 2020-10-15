;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary
;; selectrum, the incremental search package, setup
;;

;;; Code

;; @see https://github.com/raxod502/selectrum
(require-package 'selectrum)

;; (add-hook 'after-init-hook 'selectrum-mode)
(selectrum-mode 1)

(setq amx-backend 'selectrum)
(setq-default selectrum-should-sort-p nil)

;; @see https://github.com/raxod502/prescient.el
(when (maybe-require-package 'selectrum-prescient)
  (selectrum-prescient-mode 1)
  (prescient-persist-mode 1))

;; optional completion package
;; (when (maybe-require-package 'orderless)
;;   (setq selectrum-refine-candidates-function #'orderless-filter)
;;   (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

(defun selectrum-yank-search (regex-str)
  "set search item as str"
  (cond ((and (boundp 'evil-mode)
			  evil-mode)
		 (evil-search regex-str t t))
		(t
		 (re-search-forward regex-str nil t)
		 (isearch-mode t)
		 (isearch-yank-string regex-str))))

;; @see https://github.com/raxod502/selectrum/wiki/Useful-Commands#swiper-like-jumping-to-matching-lines
(defvar selectrum-swiper-history nil
  "Submission history for `selectrum-swiper'.")

(defun selectrum-swiper-candidates ()
  (let ((inhibit-field-text-motion t))
	(save-excursion
	  (goto-char (point-min))
	  (cl-loop
	   with n-lines = (count-lines (point-min) (point-max))
	   with number-format = (concat
							 "%"
							 (number-to-string
							  (length (number-to-string n-lines)))
							 "d ")
	   repeat n-lines
	   for num from 1
	   for buffer-line = (buffer-substring (point) (line-end-position))
	   if (string-empty-p buffer-line)	; Just skip empty lines.
	   collect buffer-line
	   else collect
	   (propertize buffer-line
				   'selectrum-candidate-display-prefix
				   (format number-format num)
				   ;; (propertize
				   ;;  (format number-format num)
				   ;;  'face 'completions-annotations)
				   'line-num num)
	   do (forward-line 1)))))
(add-hook 'after-init-hook (lambda () (byte-compile 'selectrum-swiper-candidates)))

;; TODO - custom mode and mode-map for `M-q' query-replace support
(defun selectrum-swiper (&optional initial-input)
  "Search for a matching line and jump to the beginning of its text.  Obeys narrowing."
  (interactive (list (util/thing-at-point)))
  (let* ((current-line-number (line-number-at-pos (point) t))
         (cands (selectrum-swiper-candidates))
		 (chosen-line
		  (selectrum-read "Selectrum Swiper: "
						  cands
						  :default-candidate (nth (1- current-line-number) cands)
						  :initial-input initial-input
						  :history 'selectrum-swiper-history
						  :may-modify-candidates t
						  :require-match t
						  :no-move-default-candidate t))
         (chosen-line-number
		  (get-text-property 0 'line-num chosen-line)))
    (push-mark (point) t)
    (forward-line (- chosen-line-number current-line-number))
	;; (beginning-of-line-text 1)
	(selectrum-yank-search selectrum--previous-input-string)))

(global-set-key (kbd "C-s") #'selectrum-swiper)

(defun selectrum-find-file ()
  ;; TODO - complete selectrum-find-file
  ;; (interactive)
  (let ((cand
		 (selectrum-read "Find file: " nil
						 :initial-input default-directory
						 ;; :may-modify-candidates t
						 :history 'file-name-history
						 :require-match 'confirm-after-completion)))
	(find-file file-name)))

;; @see https://github.com/raxod502/selectrum/wiki/Useful-Commands#search-with-ripgrep-like-selectrum-rg
(defvar selectrum-rg-base-cmd
  "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"
  "selectrum rg base cmd, can be used to set to use different command to grep")

(autoload 'grep-expand-template "grep" "")
(autoload 'counsel--elisp-to-pcre "counsel" "")

(defun selectrum-rg (&optional initial-input)
  (interactive (list (util/thing-at-point)))
  (let* ((command selectrum-rg-base-cmd)
		 (prop (lambda (c)
				 (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c)
                   (add-face-text-property ;; file name
					(match-beginning 1)
					(match-end 1)
					'compilation-info nil c)
                   (add-face-text-property ;; line number
					(match-beginning 2)
					(match-end 2)
					'(:underline t :inherit compilation-line-number) nil c))
				 c))
		 (cands (lambda (in)
				  (if (< (length in) 3)
					  (list "Input should more than 3 characters.")
					(let* ((counsel--regex-look-around "--pcre2")
						   (regex
							(counsel--elisp-to-pcre in counsel--regex-look-around)))
					  ;; `((candidates . ,)
					  ;;   (input . ,regex))
					  (mapcar prop
							  (split-string (shell-command-to-string
											 (grep-expand-template command regex))
											"\n"))))))
		 (cand (selectrum-read "rg: " cands
							   :initial-input initial-input
							   ;; :may-modify-candidates t
							   :history 'selectrum-search-rg-history
							   :require-match nil)))
	(when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
	  (let ((file-name (match-string-no-properties 1 cand))
			(line-number (match-string-no-properties 2 cand))
			(input selectrum--previous-input-string))
		(find-file file-name)
		(goto-line (string-to-number line-number))
		(selectrum-yank-search input)
		(recenter)))))

;; selectrum find file in dir

(defvar selectrum--search-file-max-depth 3
  "the maximum depth selectrum-search-file-list will reach into, default is 3")

(defun inc0n/ffip-project-root ()
  "Return project root or `default-directory'."
  (if-let ((project-root
			;; (cond ((listp ffip-project-file))
			;; 		(t (locate-dominating-file default-directory
			;; 								 ffip-project-file)))
			(cl-some (apply-partially #'locate-dominating-file
                                      default-directory)
					 ffip-project-file)))
      (file-name-as-directory project-root)
    default-directory))

(defun selectrum-search-file-list (root-path)
  "generate a list of files recursively starting from dir-path
as deep as selectrum--search-file-max-depth"
  (cl-labels
      ((aux
        (dir-path depth)
        (cond ((< depth selectrum--search-file-max-depth)
			   (if (file-directory-p dir-path)
				   (cl-reduce (lambda (acc f)
								(let ((f (format "%s/%s" dir-path f)))
								  (if (file-directory-p f)
									  (append (aux f (1+ depth))
											  acc)
									(cons f acc))))
							  (directory-files dir-path nil "[^.]")
							  :initial-value nil)
				 (list dir-path)))
			  ((file-directory-p dir-path) (list (file-name-as-directory dir-path)))
              (t (list dir-path)))))
    (let ((default-directory root-path))
	  (aux "." 0))))

(defun selectrum-ffip ()
  (interactive)
  (let* ((collection (selectrum-search-file-list (inc0n/ffip-project-root)))
		 (cand (selectrum-read "Search files:" collection
							   :require-match t)))
    (find-file cand)))

;;

(defun selectrum-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (util/ensure 'recentf)
  (let* ((files (mapcar 'abbreviate-file-name recentf-list))
		 (cand (selectrum-read "Find recent file: " files
							   :require-match t
							   :initial-input (and (region-active-p)
												   (util/selected-str)))))
	(find-file cand)))

(defun selectrum-git-recentf ()
  (find-file (selectrum-read "Find recent file (git): "
							 (inc0n/git-recent-files)
							 :require-match t
							 :initial-input (and (region-active-p)
												 (util/selected-str)))))

(defun selectrum--imenu-candidates ()
  (require 'imenu)
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items)))
	(when (eq major-mode 'emacs-lisp-mode)
      (when-let ((fns (cl-remove-if #'listp items :key #'cdr)))
		(setq items (nconc (cl-remove-if #'nlistp items :key #'cdr)
						   `(("Function" ,@fns))))))
	(cl-labels ((get-candidates
				 (alist &optional prefix)
                 (cl-mapcan
                  (lambda (elm)
                    (if (imenu--subalist-p elm)
                        (get-candidates
                         (cl-loop for (e . v) in (cdr elm)
                                  collect (cons e (if (integerp v) (copy-marker v) v)))
                         (concat prefix
								 (and prefix ".")
								 (car elm)))
                      (let ((key (concat (if prefix
											 (concat
											  (propertize prefix
														  'face 'font-lock-keyword-face)
											  ": "))
                                         (car elm))))
                        `((,key . ,(if (overlayp (cdr elm))
									   (overlay-start (cdr elm))
									 (cdr elm)))))))
                  alist)))
      (get-candidates items))))

(defun selectrum-imenu ()
  "`imenu' interfacing with `selectrum'"
  (interactive)
  (let* ((cands (selectrum--imenu-candidates))
		 (cand (selectrum-read
				"imenu items: " (mapcar #'car cands)
				:initial-input (thing-at-point 'symbol)
				:require-match t
				:history 'counsel-imenu-history)))
	(imenu (cl-find cand cands :test #'string= :key #'car))))

(defun selectrum-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (let ((imenu-create-index-function
		 (lambda ()
		   (save-excursion
			 (goto-char (point-min))
			 (cl-loop with comment-end-fn = (if (string= comment-end "")
												#'line-end-position
											  (lambda ()
												(search-forward comment-end nil t)))
					  with comment-start = (string-trim comment-start)
					  with file-end = (point-max)
					  for start = (search-forward comment-start file-end t)
					  while start
					  for end = (funcall comment-end-fn)
					  collect (cons (format
									 "%d:%s"
									 (line-number-at-pos start)
									 (replace-regexp-in-string
									  (concat "^" comment-start "+[ ]*")
									  ""
									  (buffer-substring-no-properties (1- start) end)))
									(point-marker))
					  do (goto-char end))))))
	(selectrum-imenu)))

(defun selectrum-org-agenda-headlines ()
  "Choose from headers of `org-mode' files in the agenda."
  (interactive)
  ;; (require 'org)
  (let ((minibuffer-allow-text-properties t))
    (funcall counsel-org-agenda-headlines-action-goto
			 (selectrum-read "Org headline: "
							 (counsel-org-agenda-headlines--candidates)
							 :history 'counsel-org-agenda-headlines-history))))

(defun inc0n/imenu-or-list-tag-in-current-file ()
  "Combine the power of counsel-etags and imenu."
  (interactive)
  ;; (counsel-etags-push-marker-stack)
  (cond
   ((inc0n/use-tags-as-imenu-function-p)
    ;; see code of `inc0n/use-tags-as-imenu-function-p'.
	;; Currently we only use ctags for imenu in typescript
	;; because `lsp-mode' is too damn slow
    (let ((imenu-create-index-function
		   #'counsel-etags-imenu-default-create-index-function))
      (selectrum-imenu)))
   (t
    (selectrum-imenu))))

;; evil mark

(defun selectrum-evil-marks ()
  "Jump to a marker in `evil-marker-alist', signified by a highlighted \"|\".
Currently truncates line if longer than window body width."
  (interactive)
  (if-let ((placed-markers
            (sort (cl-remove-if (lambda (elem)
                                  (not (markerp (cdr-safe elem))))
                                evil-markers-alist)
                  #'car-less-than-car)))
      (let* ((formatted-candidates
              (save-excursion
				(cl-loop
				 with window-width = (window-body-width (minibuffer-window))
                 for (char-key . marker) in placed-markers
                 for pos          = (goto-char (marker-position marker))
                 for line-beg-pos = (line-beginning-position)
                 for str-pos      = (- pos line-beg-pos)
                 for line-string  = (buffer-substring line-beg-pos
													  (line-end-position))
                 for highlighted-candidate = (concat
											  (substring line-string 0 str-pos)
											  (propertize "|" 'face 'highlight)
											  (substring line-string str-pos))
                 ;; Final formatting.
                 collect  char-key              into char-keys
                 collect  pos                   into marker-positions
                 collect  highlighted-candidate into highlighted-candidates
                 for      line-number           =    (line-number-at-pos pos t)
                 collect  line-number           into line-numbers
                 maximize line-number           into max-line-number
                 collect  str-pos               into column-numbers
                 maximize str-pos               into max-col-number
                 finally return
                 (cl-loop with form = (concat "%0"   (number-to-string (length (number-to-string max-line-number)))
                                              "d,%0" (number-to-string (length (number-to-string max-col-number)))
                                              "d: %s")
                          for marker-pos in marker-positions
                          for line-num   in line-numbers
                          for col-num    in column-numbers
                          for cand       in highlighted-candidates
                          for str        =  (format form line-num col-num cand)
                          for key        in char-keys
                          collect (cons (propertize
                                         (if (> (length str) window-width)
                                             (concat
											  (substring
											   str 0 (- window-width 10))
											  "...")
                                           str)
                                         'selectrum-candidate-display-prefix
										 (format "%c: " key))
										marker-pos)))))
             (chosen-cand (completing-read "Go to position: "
										   formatted-candidates nil t)))
        (goto-char (cdr (assoc chosen-cand formatted-candidates))))
    (user-error "selectrum-evil-marks: No Evil marks placed.")))

(defun selectrum-bash-history ()
  "Yank one command from the bash history."
  (interactive)
  (shell-command "history -r")          ; reload history
  (let* ((collection
          (nreverse
           (util/read-lines (file-truename "~/.bash_history"))))
		 (cmd
		  (selectrum-read "Bash history: " collection)))
	;; (kill-new val)
	;; (message "%s => kill-ring" val)
	;; TODO - use region selection instead of kill
	(save-excursion
	  (insert cmd))))

(provide 'init-selectrum)