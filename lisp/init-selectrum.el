;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary
;; selectrum, the incremental search package, setup
;;

;;; Code

;; @see https://github.com/raxod502/selectrum
(require-package 'selectrum)
(local-require 'selectsel)

(add-hook 'after-init-hook 'selectrum-mode)

;; @see https://github.com/raxod502/prescient.el
(when (require-package 'selectrum-prescient)
  (add-hook 'after-init-hook
			(lambda ()
			  (selectrum-prescient-mode 1)
			  (prescient-persist-mode 1)))
  (with-eval-after-load 'selectrum-prescient
	(setq prescient-filter-method '(literal fuzzy regexp))))

(with-eval-after-load 'selectrum
  (setq amx-backend 'selectrum)
  (setq-default selectrum-should-sort-p nil)
  (setq selectrum-count-style 'matches
		selectrum-extend-current-candidate-highlight t))

(global-set-key (kbd "C-s")
				(lambda ()
				  (interactive)
				  (let ((prescient-filter-method '(regexp)))
					(selectrum-swiper (util/thing-at-point/deselect)))))
(global-set-key (kbd "C-x C-z") 'selectrum-quick-repeat)

(defun selectrum-quick-repeat ()
  "Quick navigation variant of `selectrum-repeat'."
  (interactive)
  (let ((selectrum-minibuffer-map
         (let ((map (make-sparse-keymap)))
           (set-keymap-parent map selectrum-minibuffer-map)
           (define-key map "n" #'selectrum-next-candidate)
           (define-key map "p" #'selectrum-previous-candidate)
           (define-key map "q" #'abort-recursive-edit)
           map)))
    (minibuffer-message "quick navigate n/p/q")
    (selectrum-repeat)))

;;

(defun selectrum-git-recentf ()
  (find-file (selectrum-read "Find recent file (git): "
							 (inc0n/git-recent-files)
							 :require-match t
							 :initial-input (and (region-active-p)
												 (util/selected-str)))))

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

(defun selectrum-org-agenda-headlines-candidates ()
  (org-map-entries
   (lambda ()
	 (cl-destructuring-bind (level1 level2 todo priority text tags)
		 (org-heading-components)
	   (let ((headline
			  (mapconcat
			   'identity
			   (cl-remove-if
				'null
				(list (make-string level1 ?*)
					  ;; (and priority (format "[#%c]" priority))
					  (mapconcat 'identity
								 (append
								  (org-get-outline-path)
								  (list ""))
								 "/")
					  todo
					  text
					  tags))
			   " ")))
		 (propertize headline
					 'selectrum-candidate-display-prefix
					 (propertize
					  (format "%s:" (file-name-nondirectory buffer-file-name))
					  'face 'completions-annotations)
					 'marker (point-marker)))))
   nil
   'agenda))

(defun selectrum-org-agenda-headlines ()
  "Choose from headers of `org-mode' files in the agenda."
  (interactive)
  (when-let* ((headline (selectrum-read "Org headline: "
										(selectrum-org-agenda-headlines-candidates)
										;; :history
										;; 'counsel-org-agenda-headlines-history
										)))
	(let ((m (get-text-property 0 'marker headline)))
	  (switch-to-buffer (marker-buffer m))
	  (goto-char (marker-position m)))))

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

;; (defun selectrum-switch-tabs ()
;;   (interactive)
;;   (let ((selectrum-minibuffer-map))
;; 	(read-buffer-to-switch "Switch to buffer: ")))

(defun ask-action-on (prompt actions-list &optional target)
  "`ask-action-on' takes `actions-list' a list of (prompt char
action), that could optionally act on `target' with the
corresponding action"
  (let ((key (read-char-from-minibuffer
			  (concat "actions for: " prompt "\n"
					  (mapconcat (lambda (p)
								   (cl-destructuring-bind (prompt key action) p
									 (declare (ignore action))
									 (format "[%c] %s" key prompt)))
								 actions-list
								 "\n"))
			  (cons ?q (mapcar 'cadr actions-list)))))
	(unless (= key ?q)
	  (cl-destructuring-bind (prompt key action)
		  (cl-find key actions-list :key 'cadr)
		(declare (ignore prompt key))
		(when action
          (if target
              (funcall action target)
            (funcall action)))))))

;; test
(lambda () (interactive)
  (ask-action-on '(("delete file" ?d delete-file)
				   ("rename file" ?r rename-file)
				   ("find file" ?f find-file))))

(provide 'init-selectrum)