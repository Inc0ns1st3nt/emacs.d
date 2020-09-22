;; -*- coding: utf-8; lexical-binding: t; -*-

(defmacro util/ensure (feature)
  "Make sure FEATURE is required."
  `(unless (featurep ,feature)
     (condition-case nil
         (require ,feature)
       (error nil))))

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

(defun nonempty-lines (s)
  (split-string s "[\r\n]+" t))

(defun util/lines-from-command-output (command)
  "Return lines of COMMAND output."
  (let* ((output (string-trim (shell-command-to-string command)))
         (cands (nonempty-lines output)))
    (delq nil (delete-dups cands))))

(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or whole buffer and replace it with cli output."
  (let ((orig-point (point))
        (b (if (region-active-p) (region-beginning) (point-min)))
        (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))

(defun inc0n/use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function"
  (util/ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; ctags needs extra setup to extract typescript tags
       (file-exists-p counsel-etags-ctags-options-file)
       (memq major-mode '(typescript-mode
                          js-mode))))

(defun inc0n/add-subdirs-to-load-path (inc0n/lisp-dir)
  "Add sub-directories under INC0N/LISP-DIR into `load-path'."
  (let ((default-directory inc0n/lisp-dir))
    (setq load-path
          (append
           (delq nil
                 (mapcar (lambda (dir)
                           (unless (string-match-p "^\\." dir)
                             (expand-file-name dir)))
                         (directory-files inc0n/site-lisp-dir)))
           load-path))))

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun util/get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun util/read-lines (file)
  "Return a list of lines of FILE."
  (split-string (util/get-string-from-file file) "\n" t))
;; }}

(defun util/write-to-file (str file)
  "Write STR to FILE."
  (with-temp-buffer
    (insert str)
    (write-file (file-truename file))))

(defun util/write-to-missing-file (str file)
  "Write STR to FILE if it's missing."
  (unless (file-exists-p file)
    (util/write-to-file str file)))

;; Handier way to add modes to auto-mode-alist
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

(defun add-interpreter-mode (mode &rest patterns)
  "Add entries to `interpreter-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'interpreter-mode-alist (cons pattern mode))))

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
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

;; Find the directory containing a given library
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory
   (file-name-directory
    (find-library-name library-name))))

(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let ((pattern (concat "^" (file-name-as-directory directory))))
    (when (string-match-p pattern file)
      file)))

(defun util/prepare-candidate-fit-into-screen (s)
  (let* ((w (frame-width))
         ;; display kill ring item in one line
         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s))
         ;; strip the whitespace
         (key (string-trim-left key "[ \t]+")))
    ;; fit to the minibuffer width
    (cons (if (> (length key) w)
              (concat (substring key 0 (- w 4)) "...")
            key)
          s)))

(defun inc0n/select-from-kill-ring (fn)
  "If N > 1, yank the Nth item in `kill-ring'.
If N is nil, use `ivy-mode' to browse `kill-ring'."
  (interactive "P")
  (let ((candidates (cl-remove-if
                     (lambda (s)
                       (or (< (length s) 5)
                           (string-match-p "\\`[\n[:blank:]]+\\'" s)))
                     (delete-dups kill-ring)))
        (ivy-height (/ (frame-height) 2)))
    (ivy-read "Browse `kill-ring':"
              (mapcar #'util/prepare-candidate-fit-into-screen
                      candidates)
              :action fn)))

(defun util/delete-selected-region ()
  "Delete selected region."
  (when (region-active-p)
    (delete-region (region-beginning) (region-end))))

(defun util/insert-str (str)
  "Insert STR into current buffer."
  ;; ivy8 or ivy9
  (when (consp str)
    (setq str (cdr str)))
  ;; evil-mode?
  (when (and (functionp 'evil-normal-state-p)
             (boundp 'evil-move-cursor-back)
             (evil-normal-state-p)
             (not (eolp))
             (not (eobp)))
    (forward-char))

  (util/delete-selected-region)

  ;; insert now
  (insert str)
  str)

(defun inc0n/line-str (&optional line-end)
  (buffer-substring-no-properties (line-beginning-position)
                                  (if line-end line-end (line-end-position))))

(defun util/in-one-line-p (b e)
  (save-excursion
    (goto-char b)
    (and (<= (line-beginning-position) b)
         (<= e (line-end-position)))))

(defun util/buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun util/selected-str ()
  "Get string of selected region."
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun utils/selected-str/deactivate ()
  "Get string of selected region. And deactivate it"
  (prog1 (util/selected-str)
    (deactivate-mark)))

(defun util/use-selected-string-or-ask (&optional hint default-string)
  "Use selected region or ask for input.
If HINT is empty, use symbol at point."
  (if (or (not (stringp hint))
          (string-empty-p hint))
      (util/thing-at-point)
    (read-string hint "" nil (if (stringp default-string)
                                 default-string
                                 ""))))

(defun util/thing-at-point ()
  "get thing at point.
If region is active get region string.
Else use thing-at-point to get current string 'symbol."
  (cond ((char-equal ?\  (char-after)) "")
        (t (ivy-thing-at-point))))

(defun delete-this-file ()
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
(defvar cached-normal-file-full-path nil)

(defun buffer-too-big-p ()
  ;; 5000 lines
  (> (buffer-size) (* 5000 80)))

(defun file-too-big-p (file)
  (> (nth 7 (file-attributes file))
     (* 5000 64)))

(defvar force-buffer-file-temp-p nil
  "When non-nil buffer file will be treated as temp file")

(defun buffer-file-temp-p ()
  "If (buffer-file-name) is nil or a temp file or HTML file converted from org file."
  (interactive)
  (let ((f (buffer-file-name)))
    (or (not load-user-customized-major-mode-hook)
        ;; file does not exist at all
        ;; org-babel edit inline code block need calling hook
        (null f)
        ;; (string= f cached-normal-file-full-path)
        (or
         ;; file is create from temp directory
         (string-match (concat "^" temporary-file-directory) f)
         ;; file is a html file exported from org-mode
         (and (string-match "\.html$" f)
              (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
         force-buffer-file-temp-p)
        ;; (progn (setq cached-normal-file-full-path f)
        ;;        nil)
        )))

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
  (xclip-get-selection 'clipboard))

(defun util/set-clip (str-val)
  "Put STR-VAL into clipboard."
  (xclip-set-selection 'clipboard str-val))
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
(fset 'yes-or-no-p 'y-or-n-p)
;; {{ code is copied from https://liu233w.github.io/2016/09/29/org-python-windows.org/

(defun util/setup-language-and-encode (language-name coding-system)
  "Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
For example,
- \"English\" and 'utf-16-le
- \"Chinese-GBK\" and 'gbk"
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8))
;; }}

(defun util/skip-white-space (start step)
  "Skip white spaces from START, return position of first non-space character.
If STEP is 1,  search in forward direction, or else in backward direction."
  (let ((b start)
        (e (if (> step 0) (line-end-position) (line-beginning-position))))
    (save-excursion
      (goto-char b)
      (while (and (not (eq b e)) (memq (following-char) '(9 32)))
        (forward-char step))
      (point))))

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

(defun util/warp-interactive-search (proc)
  "warp proc with argumented procedure
If X is 1, get init-input from clipboard.
If X is 2, get init-input from kill-ring'.
Else get init-input from `ivy-thing-at-point'"
  (lambda (&optional x)
    (interactive "P")
    (let ((input (cond
                  ((eq 1 x) (cliphist-select-item))
                  ((eq 2 x) (inc0n/select-from-kill-ring 'identity))
                  (t (util/selected-str)))))
      (funcall proc input))))


(provide 'init-utils)
