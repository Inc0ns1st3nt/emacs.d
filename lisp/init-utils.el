;; -*- coding: utf-8; lexical-binding: t; -*-

(defun util/ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (unless (require feature nil t)
	  (warn "util/ensure - feature %s cannot be required" feature))))

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
  "Can use tags file to build imenu function"
  (util/ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; ctags needs extra setup to extract typescript tags
       (file-exists-p counsel-etags-ctags-options-file)
       (memq major-mode '(typescript-mode
                          js-mode))))

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
  (unless (file-exists-p file-path)
    (with-temp-buffer
      (insert (if (stringp init-content)
				  init-content
				""))
      (write-file (file-truename file-path)))))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun font-belongs-to (pos fonts)
  "Current font at POS belongs to FONTS."
  (let ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
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

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let ((pattern (concat "^" (file-name-as-directory directory))))
    (when (string-match-p pattern file)
      file)))

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
If N is nil, use `selectrum-mode' to browse `kill-ring'."
  (interactive "P")
  (let* ((candidates
		  (cl-remove-if
           (lambda (s)
             (or (< (length s) 5)
                 (string-match-p "\\`[\n[:blank:]]+\\'" s)))
           (delete-dups kill-ring)))
		 (cand (selectrum-read "Browse `kill-ring':" candidates)))
    (util/set-clip s)
    (message "%s => clipboard" s)))

(defun util/insert-str (str)
  "Insert STR into current buffer."
  ;; evil-mode?
  (when (and (functionp 'evil-normal-state-p)
             (boundp 'evil-move-cursor-back)
             (evil-normal-state-p)
             (not (eolp))
             (not (eobp)))
    (forward-char))

  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert str))

(defun util/line-str (&optional n)
  (buffer-substring-no-properties (line-beginning-position)
                                  (save-excursion
									(forward-line n)
									(point))))

(defun util/in-one-line-p (b e)
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) e)
         (<= e (line-end-position)))))

(defun util/buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun util/selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun util/use-selected-string-or-ask (&optional hint default-string)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (cond ((or (not (stringp hint))
             (string-empty-p hint))
         (util/thing-at-point))
        ((stringp default-string)
         (read-string (concat hint " (" default-string "): ")
                      ""
                      nil))
        (t (read-string (concat hint ": ") "" nil))))

(defun util/thing-at-point ()
  "get thing at point.
If region is active get region string.
Else use thing-at-point to get current string 'symbol."
  ;; (if (and (not (= (point-max) (point)))
  ;; 		   (char-equal ?\  (char-after)))
  ;; 	  "")
  (substring-no-properties
   (cond
	((and (not (= (point-max) (point)))
		  (char-equal ?\  (char-after)))
	 "")
	((use-region-p)
	 (let* ((beg (region-beginning))
			(end (region-end))
			(eol (save-excursion (goto-char beg) (line-end-position))))
	   (buffer-substring-no-properties beg (min end eol))))
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
  "get thing at point.
If region is active get region string and deactivate."
  (prog1 (util/thing-at-point)
	(when (region-active-p)
	  (deactivate-mark))))

(defun delete-this-buffer-and-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name)
      (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (cond ((not filename)
           (message "Buffer '%s' is not visiting a file!" name))
          ((get-buffer new-name)
           (message "A buffer named '%s' already exists!" new-name))
          (t (rename-file filename new-name 1)
             (rename-buffer new-name)
             (set-visited-file-name new-name)
             (set-buffer-modified-p nil)))))

(defvar load-user-customized-major-mode-hook t)

(defun buffer-too-big-p ()
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

;; (defun file-too-big-p (file)
;;   (> (nth 7 (file-attributes file))
;;      (* 5000 64)))

(defvar force-buffer-file-temp-p nil
  "When non-nil buffer file will be treated as temp file")

(defun buffer-file-temp-p ()
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file."
  (and (not scratch-buffer) ;; treat scratch-buffer not as temp
	   (let ((f (buffer-file-name)))
		 (or
		  ;; file does not exist at all
		  ;; org-babel edit inline code block need calling hook
		  (null f)
		  ;; file is create from temp directory
		  (string-match (concat "^" temporary-file-directory) f)
		  ;; file is a html file exported from org-mode
		  (and (string-match "\.html$" f)
			   (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
		  force-buffer-file-temp-p))))

(defvar inc0n/mplayer-extra-opts ""
  "Extra options for mplayer (ao or vo setup).  For example,
you can '(setq inc0n/mplayer-extra-opts \"-ao alsa -vo vdpau\")'.")

(defun inc0n/guess-mplayer-path ()
  (format "mplayer -quiet -stop-xscreensaver %s" inc0n/mplayer-extra-opts))

(defun inc0n/guess-image-viewer-path (file &optional is-stream)
  (if is-stream
      (format "curl -L %s | feh -F - &" file)
    (format "feh -F %s &" file)))

(defun util/get-clip ()
  "Get clipboard content."
  (if kill-ring
	  (car kill-ring)
	""))

(defalias 'util/set-clip #'kill-new
  "Put STR-VAL into clipboard.")
;; }}

(defun should-use-minimum-resource ()
  "Some files should use minimum resource (no syntax highlight, no line number display)."
  (and buffer-file-name
       (string-match-p "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

(defun inc0n/async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let ((proc (start-process "Shell"
                             nil
                             shell-file-name
                             shell-command-switch command)))
    (set-process-sentinel proc
                          (lambda (process signal)
                            (let* ((status (process-status process)))
                              (when (memq status '(exit signal))
                                (unless (string= (substring signal 0 -1) "finished")
                                  (message "Failed to run \"%s\"." command))))))))

;; reply y/n instead of yes/no
;; (fset 'yes-or-no-p 'y-or-n-p)

;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/
;; Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
;; For example
;; - "English" and 'utf-16-le
;; - "Chinese-GBK" and 'gbk
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; }}

(defun util/skip-white-space (begin step)
  "Skip white spaces from BEGIN, return position of first non-space character.
If STEP is 1,  search in forward direction, or else in backward direction."
  (multiple-value-bind (compare-fn end)
	  (if (> step 0)
		  (values '> (line-end-position))
		(values '< (line-beginning-position)))
	(save-excursion
      (goto-char begin)
      (while (and (not (funcall compare-fn (point) end))
				  ;; if encounter tabs or space
				  (memq (following-char) '(?\t ?\s)))
		(forward-char step))
      (point))))
	  ;; "[^[:space:]]"
	  ;; (re-search-forward (rx (not (or ?\t ?\s))) nil t 1)
;;

(defun util/comint-current-input-region ()
  "Region of current shell input."
  (cons (process-mark (get-buffer-process (current-buffer)))
        (line-end-position)))

(defun util/comint-kill-current-input ()
  "Kill current input in shell."
  (interactive)
  (let ((region (util/comint-current-input-region)))
    (kill-region (car region) (cdr region))))

(defun util/comint-current-input ()
  "Get current input in shell."
  (let ((region (util/comint-current-input-region)))
    (string-trim (buffer-substring-no-properties (car region) (cdr region)))))

;;

(defun custom/reset-var (symbl)
  "Reset SYMBL to its standard value."
  (set symbl (eval (car (get symbl 'standard-value)))))

(provide 'init-utils)
