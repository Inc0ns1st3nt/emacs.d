;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary
;; selectrum, the incremental search package, setup
;;

;;; Code:

;; @see https://github.com/raxod502/selectrum
(require-package 'selectrum)
(require-package 'consult)
;; (require-package 'icomplete-vertical)
(local-require 'selectsel)

(add-hook 'after-init-hook 'selectrum-mode)
;; (add-hook 'after-init-hook 'icomplete-mode)
;; (add-hook 'after-init-hook 'icomplete-vertical-mode)
;; (add-hook 'after-init-hook 'fido-mode)
(global-set-key (kbd "C-c C-k") 'which-key-show-top-level)

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

(with-eval-after-load 'selectrum
  (setq amx-backend 'selectrum)
  (setq-default selectrum-should-sort-p nil)
  (setq selectrum-count-style 'matches
		selectrum-extend-current-candidate-highlight t))

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

(defun selectsel-quick-repeat ()
  "Quick navigation variant of `selectrum-repeat'."
  (interactive)
  (let* ((selectrum-minibuffer-map
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map selectrum-minibuffer-map)
            (define-key map "n" #'selectrum-next-candidate)
            (define-key map "p" #'selectrum-previous-candidate)
            (define-key map "q" #'abort-recursive-edit)
            map)))
    (let ((minibuffer-message-timeout 0.3))
      (minibuffer-message "quick navigate n/p/q"))
    (selectrum-repeat)))

(global-set-key (kbd "C-s") 'inc0n/selectsel-swiper)
(global-set-key (kbd "C-x C-z") 'selectsel-quick-repeat)

;;

(defun selectrum-git-recentf ()
  (find-file (selectrum-read "Find recent file (git): "
							 (inc0n/git-recent-files)
							 :require-match t
							 :initial-input (and (region-active-p)
												 (util/selected-str)))))


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
	(selectsel-imenu)))

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
    ;; (selectsel-imenu)
    (consult-imenu)))

;; selectrum evil mark
;; @see https://github.com/raxod502/selectrum/wiki/Useful-Commands#evil-marks

;; buffer style popup window

(defvar my-key--buffer nil)
(defvar my-key-buffer-name "*my-key*")

(defun my-key--init-buffer ()
  (unless (buffer-live-p my-key--buffer)
    (setq my-key--buffer (get-buffer-create my-key-buffer-name))
    (with-current-buffer my-key--buffer
      ;; suppress confusing minibuffer message
      (let (message-log-max)
        (toggle-truncate-lines 1)
        (message ""))
      (display-line-numbers-mode -1)
      (setq-local cursor-type nil
                  cursor-in-non-selected-windows nil
                  mode-line-format nil
                  word-wrap nil
                  show-trailing-whitespace nil)
      ;; (run-hooks 'my-key-init-buffer-hook)
      )))

(defun my-key--show-popup (height width)
  "Using dimension of HEIGHT and WIDTH to show the popup."
  (when (and (> height 0) (> width 0))
    (let ((alist
           `((window-width .  ,(which-key--text-width-to-total width))
             (window-height . ,height)
             (side . bottom)
             (slot . 0))))
      ;; Previously used `display-buffer-in-major-side-window' here, but
      ;; apparently that is meant to be an internal function. See emacs bug #24828
      ;; and advice given there.
      (cond ((get-buffer-window my-key--buffer)
             (display-buffer-reuse-window my-key--buffer alist))
            (t
             (display-buffer-in-side-window my-key--buffer alist))))))

(defun my-key--show-page (msg)
  "Show current page filled with MSG."
  (my-key--init-buffer) ;; in case it was killed
  (let (;; (page-echo (which-key--process-page which-key--pages-obj))
        (height 10)
        (width (window-size (frame-root-window) t))
        ;; disable golden-ratio for messing with the size configuration
        (golden-ratio-mode nil))
    (with-current-buffer my-key--buffer
      (erase-buffer)
      (insert msg)
      (goto-char (point-min)))
      ;; (when (cdr page-echo) (funcall (cdr page-echo)))
      ;; which-key--show-popup
    (my-key--show-popup height width)))

(defun my-key--hide-popup ()
  "Hide which-key buffer when side-window popup is used."
  (when (buffer-live-p my-key--buffer)
    ;; in case which-key buffer was shown in an existing window, `quit-window'
    ;; will re-show the previous buffer, instead of closing the window
    (quit-windows-on my-key--buffer)))

(cl-defun ask-action-on (prompt actions-list &key target on-exit)
  "`ask-action-on' takes ACTIONS-LIST a list of (char PROMPT action), that could optionally act on TARGET with the corresponding action."
  (let ((echo-keystrokes nil)
        (menu-prompt
         (format " Actions for: %s\n\n%s\n" prompt
                 (mapconcat (lambda (p)
		    		          (cl-destructuring-bind (key prompt action) p
                                (unless action
                                  (error "No action specified for %s" p))
                                (unless (characterp key)
                                  (error "Key is not a character %s" p))
		    			        (format "  [%c] %s" key prompt)))
		    		        actions-list "\n")))
        (on-exit (if (and on-exit (functionp on-exit))
                     (lambda ()
                       (funcall on-exit)
                       (my-key--hide-popup))
                   'my-key--hide-popup)))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map [?\C-g] 'abort-recursive-edit)
       (dolist (actions actions-list)
         (define-key
           map
           (vector (car actions))
           (caddr actions)))
       map)
     t on-exit)
    (my-key--show-page menu-prompt)))

;; test
(lambda () (interactive)
  (ask-action-on "Prompt"
                 '((?d "delete file" delete-file)
				   (?r "rename file" rename-file)
				   (?f "find file"  find-file)
                   (?q "quit"       quit))))

(provide 'init-selectrum)