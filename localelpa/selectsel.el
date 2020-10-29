;;; selectsel.el --- the counsel for selectrum

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Danny He <o28c14@gmail.com>
;; Package-Requires: ((emacs "24.1"))
;; Version: 1.0.0
;; Keywords: extensions elisp
;; Prefix: selectrum
;; Separator: -

;;; Commentary:

;; The counsel package for selectrum
;;
;;; Code:

;; utils

(defun selectrum--replace-search (regex-str cands)
  "replace the regex-str with read-string"
  (let ((cands (cl-remove-if-not (lambda (x) (string-match-p regex-str x))
								 cands))
		(cands-fn (lambda (in)
					(mapcar (lambda (c)
							  (let ((str (replace-regexp-in-string regex-str in c)))
								(string-match in)
								(add-face-text-property
								 (match-beginning 0)
								 (match-end 0)
								 'compilation-mode-line-fail
								 nil c)
								c))
							cands))))
	(selectrum-read (format "Replace \"%s\" with: " regex-str)
					cands-fn)
	(query-replace regex-str selectrum--previous-input-string)))

(defun selectrum--yank-search (regex-str)
  "set search item as str"
  (cond ((and (boundp 'evil-mode)
			  evil-mode)
		 (evil-search regex-str t t))
		(t
		 (re-search-forward regex-str nil t)
		 (isearch-mode t)
		 (isearch-yank-string regex-str))))

;; selectrum-swiper

(defun selectrum--swiper-candidates ()
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

(defvar selectrum--swiper-history nil
  "Submission history for `selectrum-swiper'.")

(defun selectrum-swiper (&optional initial-input)
  "Search for a matching line and jump to the beginning of its text.  Obeys narrowing."
  (interactive (list (util/thing-at-point/deselect)))
  (let* ((current-line-number (line-number-at-pos (point) t))
         (cands (selectrum--swiper-candidates))
		 (selectrum-minibuffer-map
		  (let ((map (make-sparse-keymap)))
			(set-keymap-parent map selectrum-minibuffer-map)
			(define-key map (kbd "M-q")
			  (lambda () (interactive)
				(selectrum--replace-search
				 selectrum--previous-input-string
				 cands)))
			map))
		 (chosen-line
		  (selectrum-read "Selectrum Swiper: "
						  cands
						  :default-candidate (nth (1- current-line-number) cands)
						  :initial-input initial-input
						  :history 'selectrum--swiper-history
						  :may-modify-candidates t
						  :require-match t
						  :no-move-default-candidate t))
         (chosen-line-number
		  (get-text-property 0 'line-num chosen-line)))
    (push-mark (point) t)
    (forward-line (- chosen-line-number current-line-number))
	;; (beginning-of-line-text 1)
	(selectrum--yank-search selectrum--previous-input-string)))

;; selectrum-rg

(defvar selectrum--rg-history nil
  "history for `selectrum-rg'")

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
					  (mapcar prop
							  (split-string (shell-command-to-string
											 (grep-expand-template command regex))
											"\n"))))))
		 (cand (selectrum-read "rg: " cands
							   :initial-input initial-input
							   ;; :may-modify-candidates t
							   :history 'selectrum--rg-history
							   :require-match nil)))
	(when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
	  (let ((file-name (match-string-no-properties 1 cand))
			(line-number (match-string-no-properties 2 cand))
			(input selectrum--previous-input-string))
		(find-file file-name)
		(goto-line (string-to-number line-number))
		(selectrum--yank-search input)
		(recenter)))))

;; selectrum-ffip

(defvar selectrum-search-file-max-depth 3
  "the maximum depth selectrum-search-file-list will reach into, default is 3")

(defun selectrum--ffip-project-root ()
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

(defun selectrum--dir-tree-list (root-path)
  "generate a list of files recursively starting from dir-path
as deep as selectrum--search-file-max-depth"
  (cl-labels
      ((aux
        (dir-path depth)
        (cond ((< depth selectrum-search-file-max-depth)
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
  "Find a file in project."
  (interactive)
  (let* ((collection (selectrum--dir-tree-list (selectsel--ffip-project-root)))
		 (cand (selectrum-read "Search files:" collection
							   :require-match t)))
    (find-file cand)))

(defun selectrum-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (if recentf-mode
	  (let* ((files (mapcar 'abbreviate-file-name recentf-list))
			 (cand (selectrum-read "Find recent file: " files
								   :require-match t
								   :initial-input (and (region-active-p)
													   (util/selected-str)))))
		(find-file cand))
	(message "turn on recentf-mode first")))

;;;; ChangeLog:

;; 2015-12-01  Artur Malabarba  <bruce.connor.am@gmail.com>
;;
;; 	packages/let-alist: Define it as a :core package
;;
;; 2020-10-15  Danny He <o28c14@gmail.com>
;;
;; 	selectsel: New package
;;

(provide 'selectsel)
;;; selectsel.el ends here
