;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary
;; selectrum, the incremental search package, setup
;;

;;; Code:

;; @see https://github.com/raxod502/selectrum
(use-package selectrum
  :ensure t
  :config
  (setq amx-backend 'selectrum)
  (setq selectrum-should-sort nil)
  (setq selectrum-count-style 'current/matches
		selectrum-extend-current-candidate-highlight t)
  (setq-default selectrum-move-default-candidate nil)
  (define-key selectrum-minibuffer-map (kbd "C-<return>")
    #'selectrum-select-current-candidate)
  :init (add-hook 'after-init-hook 'selectrum-mode))

(local-require 'selectsel)

(require-package 'consult)
;; (require-package 'icomplete-vertical)


;; (add-hook 'after-init-hook 'icomplete-mode)
;; (add-hook 'after-init-hook 'icomplete-vertical-mode)
;; (add-hook 'after-init-hook 'fido-mode)
(global-set-key (kbd "C-c C-k") 'which-key-show-major-mode)

;; (grep-apply-setting
;;  'grep-find-command
;;  '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))

;; @see https://github.com/raxod502/prescient.el
(when (require-package 'selectrum-prescient)
  (add-hook 'after-init-hook
			(lambda ()
			  (selectrum-prescient-mode 1)
			  (prescient-persist-mode 1)))
  (with-eval-after-load 'selectrum-prescient
	(setq prescient-filter-method '(literal fuzzy regexp))))

(defun inc0n/selectsel-swiper ()
  "My version of swiper."
  (interactive)
  ;; (selectsel-swiper (util/thing-at-point/deselect))
  (consult-line (util/thing-at-point/deselect))
  (selectsel--yank-search (car consult--line-history)))

(defun inc0n/selectsel-rg ()
  "My version of ripgrep."
  (interactive)
  (consult-ripgrep default-directory (util/thing-at-point/deselect))
  (selectsel--yank-search (car consult--grep-history)))

(defvar selectsel-quick-repeat-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map selectrum-minibuffer-map)
    (define-key map "n" #'selectrum-next-candidate)
    (define-key map "p" #'selectrum-previous-candidate)
    (define-key map "q" #'abort-recursive-edit)
    map))

(defun selectsel-quick-repeat ()
  "Quick navigation variant of `selectrum-repeat'."
  (interactive)
  (let ((selectrum-minibuffer-map selectsel-quick-repeat-map)
        (minibuffer-message-timeout 0.3))
    (minibuffer-message "quick navigate n/p/q")
    (selectrum-repeat)))

(global-set-key (kbd "C-s") 'inc0n/selectsel-swiper)
(global-set-key (kbd "C-x C-z") 'selectsel-quick-repeat)

;;

(defun inc0n/git-root-dir ()
  "Git root directory."
  ;; (project-try-vc default-directory)
  (locate-dominating-file default-directory ".git"))

(defun inc0n/git-recent-files ()
  "Get files in my recent git commits."
  (let* ((default-directory (inc0n/git-root-dir))
         ;; two weeks is a sprint, minus weekend and days for sprint review and test
         (cmd (format "git --no-pager log --name-only --since=\"10 days ago\" --pretty=format:"))
         (lines (util/shell-command-to-lines cmd)))
    (when lines
      (cl-reduce (lambda (acc file)
                   (let ((file (abbreviate-file-name (string-trim file))))
                     (if (and (file-exists-p file)
                              (not (member file acc)))
                         (cons file acc)
                       ;; (acons file (file-truename file) acc)
                       acc)))
                 lines
                 :initial-value nil))))

(defun selectsel-git-recentf (&optional initial-input)
  (interactive (list (and (region-active-p)
		                  (util/selected-str))))
  (find-file
   (completing-read
    "Find recent file (git): "
	(inc0n/git-recent-files)
    nil t initial-input)))

(defun selectsel-recentf-directories (&optional initial-input)
  "Completion interface for recent directories."
  (interactive (list (and (region-active-p)
		                  (util/selected-str))))
  (let* ((directories
          (cl-remove-duplicates
           (mapcar (lambda (x)
                     (abbreviate-file-name (file-name-directory x)))
                   recentf-list)
           :test #'string=))
         (default-directory
           (completing-read
            "Find recent directories: "
	        directories
            nil t initial-input)))
    (call-interactively 'find-file)))

(defun selectsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (let ((imenu-generic-expression
         `(("Comments" ,(format "^%s+\\(.*\\)$" comment-start) 1))))
    ;; (consult-imenu)
	(selectsel-imenu)))

(defun selectsel-imenu ()
   (interactive)
   (consult-imenu))

(defun selectrum-org-agenda-headlines-candidates ()
  (org-map-entries
   (lambda ()
	 (cl-destructuring-bind (level1 level2 todo priority text tags)
		 (org-heading-components)
	   (let ((headline
			  (mapconcat
			   'identity
			   (delq ;; remove nil
                nil
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
  (when-let* ((headline (selectrum--read
                         "Org headline: "
						 (selectrum-org-agenda-headlines-candidates))))
	(let ((m (get-text-property 0 'marker headline)))
	  (switch-to-buffer (marker-buffer m))
	  (goto-char (marker-position m)))))

;; selectrum evil mark
;; @see https://github.com/raxod502/selectrum/wiki/Useful-Commands#evil-marks

(provide 'init-selectrum)