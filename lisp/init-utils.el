;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

(defun util/ensure (feature)
  "Make sure FEATURE is required."
  (unless (require feature nil t)
	(warn "util/ensure - feature %s cannot be required" feature)))

(defun inc0n/git-root-dir ()
  "Git root directory."
  (locate-dominating-file default-directory ".git"))

(defun inc0n/git-files-in-rev-command (rev level)
  "Return git command line to show files in REV and LEVEL."
  (unless level
    (setq level 0))
  (concat "git diff-tree --no-commit-id --name-only -r "
          rev
          (make-string level ?^)))

(defun util/async-shell-command (command)
  "Util function for running a shell COMMAND asynchronously."
  (let ((process (start-process "Shell"
                                nil
                                shell-file-name
                                shell-command-switch
                                command)))
    (set-process-sentinel
     process
     (lambda (process signal)
       (let ((status (process-status process)))
         (when (memq status '(exit signal))
           (if (string= (substring signal 0 -1) "finished")
               (message "done: %s" command)
             (message "failed to run: %s" command))))))))

(defun util/shell-command-to-lines (command)
  "Return lines of COMMAND output."
  (split-string (shell-command-to-string command)
				"[\r\n]+" t))

(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or whole buffer and replace it with cli output."
  (let ((orig-point (point))
        (begin (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region begin end cmd nil t)
    (goto-char orig-point)))

(defun inc0n/use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function."
  (and (locate-dominating-file default-directory "TAGS")
       ;; ctags needs extra setup to extract typescript tags
       ;; (file-exists-p counsel-etags-ctags-options-file)
       (memq major-mode '(typescript-mode js-mode))))

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
;; (defun util/read-file-content (file)
;;   "Return FILE's content."
;;   (with-temp-buffer
;;     (insert-file-contents file)
;;     (buffer-string)))

(defun util/read-lines (file)
  "Return a list of lines of FILE."
  (split-string (with-temp-buffer
				  (insert-file-contents file)
				  (buffer-string))
				"\n" t))
;; }}

(defun util/make-file (file-path &optional init-content)
  "Create the file at FILE-PATH if not exist.
And insert INIT-CONTENT if non-nil."
  (unless (file-exists-p file-path)
    (with-temp-buffer
      (when (stringp init-content)
		(insert init-content))
      (write-file (file-truename file-path)))))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-to-list/s (list-var elements &optional append)
  "Add multiple ELEMENTS to the `LIST-VAR'."
  (dolist (elm elements)
    (add-to-list list-var elm append))
  (symbol-value list-var))

(defalias 'add-to-list-multi 'add-to-list/s)

(defun font-belongs-to (pos fonts)
  "Current font at POS belongs to FONTS."
  (let ((fontfaces (get-text-property pos 'face)))
    (unless (listp fontfaces)
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  fontfaces))))

;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
;; (defun string-all-matches (regex str &optional group)
;;   "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
;;   (let ((result nil)
;;         (pos 0)
;;         (group (or group 0)))
;;     (while (string-match regex str pos)
;;       (push (match-string group str) result)
;;       (setq pos (match-end group)))
;;     result))

;; Find the directory containing a given library
;; (defun directory-of-library (library-name)
;;   "Return the directory in which the `LIBRARY-NAME' load file is found."
;;   (file-name-as-directory
;;    (file-name-directory
;;     (find-library-name library-name))))

;; (defun util/prepare-candidate-fit-into-screen (s)
;;   (let* ((w (frame-width))
;;          ;; display kill ring item in one line
;;          (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s))
;;          ;; strip the whitespace
;;          (key (string-trim-left key "[ \t]+")))
;;     ;; fit to the minibuffer width
;;     (cons (if (> (length key) w)
;;               (concat (substring key 0 (- w 4)) "...")
;;             key)
;;           s)))

(defun inc0n/select-from-kill-ring (n)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `completing-read' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates
		  (cl-remove-if
           (lambda (s)
             (or (< (length s) 5)
                 (string-match-p "\\`[\n[:blank:]]+\\'" s)))
           (delete-dups kill-ring)))
         (cand (if (null n)
	               (completing-read "Browse `kill-ring':" candidates)
                 (nth n candidates))))
    (util/insert-str cand)
    (util/set-clip cand)))

(defun util/insert-str (str)
  "Insert STR into current buffer."
  (when (and (functionp 'evil-normal-state-p)
             (evil-normal-state-p)
             (bound-and-true-p evil-move-cursor-back)
             (not (eolp))
             (not (eobp)))
    (forward-char))

  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert str))

(defun util/looking-at-line-p (regexp)
  "Check if the current line match REGEXP."
  (save-excursion
    (goto-char (line-beginning-position))
    (looking-at-p regexp)))

(defun util/line-str (&optional n)
  "Get string of N lines from the current line."
  (buffer-substring-no-properties
   (line-beginning-position)
   (save-excursion
	 (forward-line n)
	 (point))))

(defun util/in-one-line-p (b e)
  "Check if B and E positions are in the same line."
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) e)
         (<= e (line-end-position)))))

(defun util/buffer-str ()
  "Get string of the whole buffer."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun util/selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun util/use-selected-string-or-ask (hint &optional default-string)
  "Use selected region or ask for input.
If HINT is empty, throw an error.
Optional argument DEFAULT-STRING default string to return from `read-string'."
  (cond ((or (not (stringp hint))
             (string-empty-p hint))
         (error "Empty prompt, %s" hint))
        ((region-active-p)
         (util/selected-str))
        ((stringp default-string)
         (read-string (concat hint " (" default-string "): ")
                      default-string
                      nil))
        (t (read-string (concat hint ": ") "" nil))))

(defun util/thing-at-point ()
  "Get thing at point.  Gotten from ivy-thing-at-point.
If region is active get region string.
Else use `thing-at-point' to get current string 'symbol."
  ;; (if (and (not (= (point-max) (point)))
  ;; 		   (char-equal ?\  (char-after)))
  ;; 	  "")
  (substring-no-properties
   (cond
	((use-region-p)
	 ;; (let* ((beg (region-beginning))
	 ;;    	(end (region-end))
	 ;;    	(eol (save-excursion (goto-char beg) (line-end-position))))
	 ;;   (buffer-substring-no-properties beg end))
     (buffer-substring-no-properties (region-beginning) (region-end)))
    ((and (not (= (point-max) (point)))
		  (char-equal ?\  (char-after)))
	 "")
	((thing-at-point 'url))
	((let ((s (thing-at-point 'symbol)))
	   (and (stringp s)
			(if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
				(match-string 1 s)
			  s))))
	((looking-at "(+\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
	 (match-string-no-properties 1))
	(t ""))))

(defun util/thing-at-point/deselect ()
  "Get thing at point.
If region is active get region string and deactivate."
  (prog1 (util/thing-at-point)
	(when (region-active-p)
	  (deactivate-mark))))

(defun delete-this-buffer-and-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (when (y-or-n-p (format "Really delete file and buffer '%s'? "
                                (file-name-nondirectory file-name)))
          ;; (if (vc-backend file-name)
          ;;     (vc-delete-file file-name))
          (delete-file file-name)
          (kill-this-buffer))
      (message "No file is currently being edited"))))
;; (defalias 'delete-file-and-buffer 'delete-this-buffer-and-file)

(defun rename-this-file-and-buffer ()
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        (let ((new-name (read-file-name "New name: " filename)))
		  (rename-file filename new-name 1) ;; will ask for confirmation
		  (when (file-directory-p new-name)
            (rename-buffer (buffer-name) t))
		  (set-visited-file-name new-name)
		  (set-buffer-modified-p nil))
      (message "Buffer '%s' is not visiting a file!" name))))
(defalias 'rename-this-buffer-and-file 'rename-this-file-and-buffer)

;; (defun file-too-big-p (file)
;;   (> (nth 7 (file-attributes file))
;;      (* 5000 64)))

(defvar force-buffer-file-temp-p nil
  "When non-nil buffer file will be treated as temp file.")

(defun buffer-file-temp-p ()
  "If variable `buffer-file-name' is nil or a temp file or HTML file converted from org file."
  (and (not scratch-buffer)       ; don't treat scratch-buffer as temp
       (let ((f buffer-file-name))
         (or (null f) ;; file does not exist at all
		     ;; org-babel edit inline code block need calling hook
		     ;; file is create from temp directory
		     ;; (string-match (concat "^" temporary-file-directory) f)
		     ;; file is a html file exported from org-mode
		     (and (string-match "\.html$" f)
			      (file-exists-p
                   (replace-regexp-in-string "\.html$" ".org" f)))
		     force-buffer-file-temp-p))))

(defvar inc0n/mplayer-extra-opts ""
  "Extra options for mplayer (ao or vo setup).
For example, you can '(setq inc0n/mplayer-extra-opts \"-ao alsa
-vo vdpau\")'.")

(defun inc0n/guess-mplayer-path ()
  (concat "mplayer -quiet -stop-xscreensaver " inc0n/mplayer-extra-opts))

(defun util/get-clip ()
  "Get clipboard content."
  (if kill-ring
	  (car kill-ring)
	""))

(defalias 'util/set-clip #'kill-new "Put STR-VAL into kill ring.")
;; }}

(defun should-use-minimum-resource ()
  "Some files should use minimum resource (no syntax highlight, no line number display)."
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

;; reply y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/
;; Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
;; For example
;; - "English" and 'utf-16-le
;; - "Chinese-GBK" and 'gbk
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; }}

(defun util/skip-white-space (begin step)
  "Skip white spaces within the line from BEGIN.
Return position of first non-space character.  If STEP is 1,
search in forward direction, or else in backward direction."
  (let* ((regex (rx (not (or ?\t ?\s))))
         (searcher
	      (if (> step 0)
              (lambda () (re-search-forward regex (line-end-position) t 1))
		    (lambda () (re-search-backward regex (line-beginning-position) t 1)))))
	(save-excursion
      (goto-char begin)
      ;; return begin if not match found
      (or (funcall searcher)
          begin))))
	  ;; "[^[:space:]]"
;;

(defun util/comint-operate-on-input-region (fn)
  "Let FN operate on region of current shell input."
  (funcall fn
		   (process-mark (get-buffer-process (current-buffer)))
           (line-end-position)))

(defun util/comint-kill-current-input ()
  "Kill current input in shell."
  (util/comint-operate-on-input-region 'kill-region))

(defun util/comint-current-input ()
  "Get current input in shell."
  (string-trim
   (util/comint-operate-on-input-region 'buffer-substring-no-properties)))

;;

(defun custom/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (interactive (list (intern (read-string "Symbol: "))))
  (set symbl (eval (car (get symbl 'standard-value)))))

;;

(defmacro define-hook-setup (hook-name &rest body)
  "Macro helper for `add-hook' to HOOK-NAME.
It will defun setup hook function with BODY.
The setup hook function will have the name `HOOK-NAME'-setup"
  (declare (debug (&define name
                           [&optional additional-name]
                           [&optional lambda-doc]
                           [&rest def-body]))
           (indent defun)
           (doc-string 2))
  (unless (or (symbolp hook-name)
              (and (listp hook-name) (eq (car hook-name) 'quote)))
    (warn "bad define-hook-setup name %s" hook-name))
  (let* ((hook-name (if (listp hook-name)
                        (cadr hook-name)
                      hook-name))
         (hook-setup-fn-name
          (intern (concat (symbol-name hook-name)
                          (if (keywordp (car body))
                              (concat "-"
                                      (substring (symbol-name (car body)) 1))
                            "")
                          "-setup"))))
    `(add-hook ',hook-name
               (defun ,hook-setup-fn-name ()
                 ,@(if (keywordp (car body))
                       (cdr body)
                     body)))))

(provide 'init-utils)
;;; init-utils ends here
