;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary
;; selectrum, the incremental search package, setup
;;

;;; Code

;; @see https://github.com/raxod502/selectrum
(require-package 'selectrum)

(add-hook 'after-init-hook 'selectrum-mode)

;; @see https://github.com/raxod502/prescient.el
(when (maybe-require-package 'selectrum-prescient)
  (add-hook 'after-init-hook
			(lambda ()
			  (selectrum-prescient-mode 1)
			  (prescient-persist-mode 1)))
  (with-eval-after-load 'selectrum-prescient
	(setq prescient-filter-method '(literal regexp fuzzy))))

(require 'selectsel)

(with-eval-after-load 'selectrum
  (setq amx-backend 'selectrum)
  (setq-default selectrum-should-sort-p nil)
  (global-set-key (kbd "C-s") #'selectrum-swiper))

;;

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
  (let ((imenu-create-index-function
		 (if (inc0n/use-tags-as-imenu-function-p)
			 ;; see code of `inc0n/use-tags-as-imenu-function-p'.
			 ;; Currently we only use ctags for imenu in typescript
			 ;; because `lsp-mode' is too damn slow
			 #'counsel-etags-imenu-default-create-index-function
		   imenu-create-index-function)))
	(selectrum-imenu)))

;; selectrum evil mark
;; @see https://github.com/raxod502/selectrum/wiki/Useful-Commands#evil-marks

(defun selectrum-bash-history ()
  "Yank one command from the bash history."
  (interactive)
  (shell-command "history -r")          ; reload history
  (let* ((collection
          (nreverse
           (util/read-lines (file-truename "~/.bash_history"))))
		 (cmd
		  (selectrum-read "Bash history: " collection)))
	;; TODO - use region selection instead of kill
	(save-excursion
	  (insert cmd))))

;; (defun selectrum-switch-tabs ()
;;   (interactive)
;;   (let ((selectrum-minibuffer-map))
;; 	(read-buffer-to-switch "Switch to buffer: ")))

(provide 'init-selectrum)