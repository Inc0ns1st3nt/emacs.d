;;; init-evil.el --- evil setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'expand-region) ; I prefer stable version
(require-package 'evil)
;; (local-require 'evil-mark-replace)
(autoload 'evilmr-replace-in-defun "evil-mark-replace")
(autoload 'evilmr-replace-in-buffer "evil-mark-replace")

;; enable evil-mode
(evil-mode 1)

;; {{ replace undo-tree with undo-fu
;; @see https://github.com/emacs-evil/evil/issues/1074
(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist
        '(("." . "/home/linkedptr/.emacs.d/undo-history/")))
  ;; (setq undo-tree-auto-save-history t)
  (advice-add 'undo-tree-make-history-save-file-name :around
              (defun inc0n/undo-tree-history-compress (orig-func &rest args)
                "Compress the undo-tree history.
ORIG-FUNC and ARGS are the advice of undo-tree-make-history-save-file-name."
                (let ((save-file-name (apply orig-func args)))
                  (concat save-file-name ".gz"))))
  :init
  (global-undo-tree-mode 1)
  (evil-set-undo-system 'undo-tree)
  (setq evil-undo-system 'undo-tree))


(use-package undo-fu
  :disabled ;; using in favor of undo-tree
  :init
  ;; copied from doom-emacs
  (define-minor-mode global-undo-fu-mode
    "Enables `undo-fu' for the current session."
    :keymap (let ((map (make-sparse-keymap)))
              (define-key map [remap undo] #'undo-fu-only-undo)
              (define-key map [remap redo] #'undo-fu-only-redo-all)
              (define-key map [?\C-_]     #'undo-fu-only-undo)
              (define-key map [?\M-_]     #'undo-fu-only-redo)
              (define-key map [?\C-\M-_] #'undo-fu-only-redo-all)
              (define-key map [?\C-x ?r ?u] #'undo-fu-session-save)
              (define-key map [?\C-x ?r ?U] #'undo-fu-session-recover)
              map)
    :init-value nil
    :global t)
  (global-undo-fu-mode 1)
  (evil-set-undo-system 'undo-fu)
  ;; Store more undo history to prevent loss of data
  (setq undo-limit 8000000
        undo-strong-limit 8000000
        undo-outer-limit 8000000))

;; @see https://github.com/timcharper/evil-surround
(use-package evil-surround
  :defer t
  :init (add-hook 'after-init-hook 'global-evil-surround-mode)
  :config
  (define-hook-setup 'prog-mode-hook :evil-surround
    "Set up surround shortcuts."
    (util/ensure 'evil-surround)
    (push (if (memq major-mode '(sh-mode))
              '(?$ . ("$(" . ")"))
            '(?$ . ("${" . "}")))
          evil-surround-pairs-alist)

    (push '(?\( . ("(" . ")")) evil-surround-pairs-alist)
    (when (memq major-mode '(emacs-lisp-mode))
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))

    (when (memq major-mode '(js-mode js2-mode))
	  (push '(?j . ("JSON.stringify(" . ")")) evil-surround-pairs-alist)
      (push '(?> . ("(e) => " . "(e)")) evil-surround-pairs-alist))))
;; }}

;; ffip-diff-mode (read only) evil setup
(define-hook-setup 'ffip-diff-mode-hook
  (evil-define-key 'normal 'local
    "q" (lambda () (interactive) (quit-window t))
    (kbd "RET") #'ffip-diff-find-file
    ;; "C-c C-a" is binding to `diff-apply-hunk' in `diff-mode'
    "a" #'ffip-diff-apply-hunk
    "o" #'ffip-diff-find-file))

;; {{ define my own text objects, works on evil v1.0.9 using older method
;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro inc0n/evil-define-and-bind-text-object (key start-regex end-regex)
  `(progn
     (evil-define-text-object inner-name (count &optional beg end type)
       (evil-select-paren ,start-regex ,end-regex beg end type count nil))
     (evil-define-text-object outer-name (count &optional beg end type)
       (evil-select-paren ,start-regex ,end-regex beg end type count t))
     (define-key evil-inner-text-objects-map ,key 'inner-name)
     (define-key evil-outer-text-objects-map ,key 'outer-name)))

;; between equal signs
(inc0n/evil-define-and-bind-text-object "=" "=" "=")
;; between pipe characters:
(inc0n/evil-define-and-bind-text-object "|" "|" "|")
;; regular expression
(inc0n/evil-define-and-bind-text-object "/" "/" "/")
;; trimmed line
(inc0n/evil-define-and-bind-text-object "l" "^ *" " *$")
;; angular template
(inc0n/evil-define-and-bind-text-object "r" "\{\{" "\}\}")
;; }}

;; {{ nearby file path as text object,
;;      - "vif" to select base name
;;      - "vaf" to select full path
;;
;;  example:
;;    "/hello/world"
;;    "/test/back.exe"
;;    "C:hello\\hello\\world\\test.exe"
;;    "D:blah\\hello\\world\\base.exe"
(defun inc0n/evil-path-is-separator-char (ch)
  "Check ascii table that CH is slash characters.
If the character before and after CH is space or tab, CH is NOT slash"
  (let ((prefix-ch (preceding-char))
		(postfix-ch (char-after (1+ (point)))))
    (and (not (or (= prefix-ch ?\s) (= postfix-ch ?\s)))
         (or (= ch ?/) (= ch ?\\)))))

(defun inc0n/evil-path-not-path-char (ch)
  "Check ascii table for character CH."
  (or (and (<= 0 ch) (<= ch 32))
      (memq ch
            '(?\"
              ?'
              ?\(
              ?\)
              ?<
              ?>
              ?\[
              ?\]
              ?`
              ?{
              ?}
              127))))

(defun inc0n/evil-path-calculate-path (b e)
  (when (and b e)
    (setq b (+ 1 b))
    (save-excursion
      (goto-char e)
      (when-let ((f
				  (inc0n/evil-path-search-forward-char
				   #'inc0n/evil-path-is-separator-char t)))
        (and (>= f b)
             (list b (+ 1 f) (- e 1)))))))

(defun inc0n/evil-path-get-path-already-inside ()
  (let ((b (inc0n/evil-path-search-forward-char 'inc0n/evil-path-not-path-char t))
        (e (when-let ((e (inc0n/evil-path-search-forward-char
						  'inc0n/evil-path-not-path-char)))
             ;; example: hello/world,
             (and (memq (char-after (- e 1)) '(?, ?.))
				  (- e 1)))))
    (inc0n/evil-path-calculate-path b e)))

(defun inc0n/evil-path-search-forward-char (fn &optional backward)
  (let ((limit
		 (if backward (point-min) (point-max))))
    (save-excursion
	  (while (= (point) limit)
		(if (funcall fn (following-char))
			(return (point))
		  (if backward
			  (backward-char)
			(forward-char -1)))))))

(defun inc0n/evil-path-extract-region ()
  "Find the closest file path."
  (or
   ;; maybe (point) is in the middle of the path
   (and (not (inc0n/evil-path-not-path-char (following-char)))
        (inc0n/evil-path-get-path-already-inside))
   ;; need search forward AND backward to find the right path
   (cl-flet ((aux
			  ()
              (save-excursion
                ;; path in backward direction
                (when-let ((b (inc0n/evil-path-search-forward-char
                               #'inc0n/evil-path-is-separator-char t)))
                  (goto-char b)
                  (inc0n/evil-path-get-path-already-inside)))))
     (if-let ((f1 (aux))
			  (f2 (aux)))
		 ;; pick one path as the final result
		 (if (and f1 f2)
			 (if (> (- (point) (nth 2 f1))
					(- (nth 0 f2) (point)))
				 f2
               f1)
           (or f1 f2))))))

(evil-define-text-object inc0n/evil-path-inner-text-object (&optional count begin end type)
  "File name of nearby path"
  (when-let ((selected-region (inc0n/evil-path-extract-region)))
    (evil-range (nth 1 selected-region)
                (nth 2 selected-region)
                :expanded t)))

(evil-define-text-object inc0n/evil-path-outer-text-object (&optional count begin end type)
  "Nearby path."
  (when-let ((selected-region (inc0n/evil-path-extract-region)))
    (evil-range (car selected-region)
                (+ 1 (nth 2 selected-region))
                type
                :expanded t)))

(define-key evil-inner-text-objects-map "f" 'inc0n/evil-path-inner-text-object)
(define-key evil-outer-text-objects-map "f" 'inc0n/evil-path-outer-text-object)
;; }}

;; @see https://github.com/syl20bnr/evil-escape
(use-package evil-escape
  ;; evil-escape will be disabled when input method is on
  :defer t
  :config
  (setq evil-escape-excluded-major-modes '(dired-mode))
  :init
  ;; evil-escape uses pre-command-hook, which may slow down emacs??
  ;; (add-hook 'after-init-hook 'evil-escape-mode)
  (setq-default evil-escape-key-sequence "fd")
  (setq-default evil-escape-delay 0.2))

(defun handle-error (fn handler)
  "Handle error from FN and call HANDLER instead."
  (lambda ()
	(interactive)
	(condition-case nil
        (funcall fn)
      (error (and (functionp handler)
				  (funcall handler))))))

(defun util/delim-p (pos)
  "Check if character at POS is a parenthesis."
  (let* ((c (char-after pos))
         (syntax (char-syntax c)))
    (if (= syntax ?\")
        (let ((string-start-pos (nth 8 (syntax-ppss))))
          (or (null string-start-pos) ;; means this is the string-starting-pos
              (< pos string-start-pos)))
      (memq c '(?\( ?\))))))

;; TODO: mark region if char syntax is ?\)
(defun delim-or-normal (paredit-fn normal-fn)
  "If current point on delim and no prefix args call PAREDIT-FN.
Otherwise call NORMAL-FN.
Check `util/delim-p' for the definition of delim."
  (if (or (not (functionp normal-fn))
          (not (commandp normal-fn)))
      (warn "normal-fn is not an interactive function, %s" normal-fn)
    (lambda (&optional arg)
      (interactive "P")
      (if (and (null arg)
               (or (util/delim-p (point))
		           (region-active-p)))
	      (funcall paredit-fn)
	    (and (functionp normal-fn)
		     (call-interactively normal-fn))))))

;; (popup-tip (documentation 'paredit-copy-as-kill))

(evil-define-key '(normal visual) paredit-mode-map
  "C" (delim-or-normal #'paredit-copy-as-kill #'evil-change-line)
  "R" (delim-or-normal #'paredit-raise-sexp   #'evil-replace-state)
  "X" #'kill-sexp
  "(" #'paredit-wrap-round
  "[" (handle-error #'backward-sexp #'backward-paragraph)
  "]" (handle-error #'forward-sexp  #'forward-paragraph)
  "\"" #'paredit-meta-doublequote
  ;; (kbd "C-<") #'paredit-forward-barf-sexp
  ;; (kbd "C->") #'paredit-forward-slurp-sexp
  "<" #'paredit-forward-barf-sexp
  ">" #'paredit-forward-slurp-sexp
  "+" #'paredit-join-sexps
  "-" #'paredit-split-sexp)

(evil-define-key '(insert normal) paredit-mode-map
  [?\C-<] #'paredit-forward-barf-sexp
  [?\C->] #'paredit-forward-slurp-sexp)

;; As a general rule, mode specific evil leader keys started
;; with upper cased character or 'g' or special character except "=" and "-"
(evil-define-key 'normal org-mode-map
  [?\C-\M-u] 'outline-up-heading
  "gh" 'org-up-element
  "gj" 'org-next-visible-element
  "gk" 'org-previous-visible-element
  "gl" nil ;; 'org-forward-heading-same-level

  "gJ" 'org-move-subtree-down
  "gK" 'org-move-subtree-up

  "$" 'org-end-of-line           ; smarter behaviour on headlines etc.
  "^" 'org-beginning-of-line     ; ditto
  "<" 'org-shiftmetaleft
  ">" 'org-shiftmetaright
  [?\C->] 'org-do-demote
  [?\C-<] 'org-do-promote
  ;; (kbd "RET") #'newline-and-indent
  [tab] 'org-cycle)

(evil-define-key 'normal markdown-mode-map
  "gh" #'outline-up-heading
  [tab] #'markdown-cycle)

;; I prefer Emacs way after pressing ":" in evil-mode
(evil-define-key nil evil-ex-completion-map
  [?\C-a] #'move-beginning-of-line
  [?\C-b] #'backward-char
  [?\M-p] #'previous-complete-history-element
  [?\M-n] #'next-complete-history-element)

;; (evil-define-key '(normal visual) 'global
;;   [tab] 'indent-for-tab-command)

(evil-define-key 'normal 'global
  [?\C-e] 'evil-scroll-up
  "Y" #'evil-yank-line                  ; "y$"
  "U" #'join-line
  ;;
  [return] #'newline-and-indent
  "n" 'evil-search-next
  ;; evil re-assign "M-." to `evil-repeat-pop-next' which I don't use actually.
  ;; Restore "M-." to original binding command
  [?\M-.] 'xref-find-definitions
  ;; hard bind N in normal state
  "N" 'evil-search-previous)

;; evil g leader key
(evil-define-key '(normal motion) 'global
  ;;
  "ga" #'selectsel-quick-repeat
  "gs" #'selectsel-recentf
  "gj" #'avy-goto-line-below
  "gk" #'avy-goto-line-above
  "gb" #'switch-to-buffer
  [?g ?\C-b] (lambda () (interactive)   ; to previous buffer
               (switch-to-buffer nil))
  [?g ?\C-k] #'kill-buffer
  ;;
  "g " #'just-one-space
  "gc" #'comment-operator               ; same as doom-emacs
  "gy" #'comment-and-copy-line
  "gr" #'copy-and-paste    ; [?Y ?p]                       ; copy-line
  "go" #'endless/capitalize
  "gl" #'endless/downcase
  "gu" #'endless/upcase)

(evil-define-key 'motion 'global
  "]" nil                               ; disable ] prefix
  "n" nil                               ; disable evil-search-next
  "N" 'evil-search-next                 ; rebinds
  "P" 'evil-search-previous)

(evil-define-key 'insert 'global
  [?\C-x ?\C-n] #'evil-complete-next-line
  [?\C-x ?\C-p] #'evil-complete-previous-line
  [?\C-\]] #'forward-word
  ;; #'aya-expand
  [?\C-e]  #'move-end-of-line
  [?\C-\;] #'company-kill-ring ; replaces fly spell-auto-correct-previous-word
  [?\C-k]  #'kill-sexp)

(with-eval-after-load 'expand-region
  ;; press "v" to expand region
  ;; then press "c" to contract, "x" to expand
  (setq expand-region-contract-fast-key "c"))

;; I learn this trick from ReneFroger, need latest expand-region
;; @see https://github.com/redguardtoo/evil-matchit/issues/38
(define-key evil-visual-state-map "v" #'er/expand-region)
(define-key evil-visual-state-map (kbd "C-]") #'counsel-etags-find-tag-at-point)

(defun inc0n/search-defun-from-pos (search pos)
  (evil-search search t t pos)
  ;; ignore this.f1 = this.fn.bind(this) code
  (when (and (memq major-mode '(js-mode js2-mode rjsx-mode))
             (util/looking-at-line-p
              "^[ \t]*this\.[a-zA-Z0-9]+[ \t]*=[ \t]*this\.[a-zA-Z0-9]*\.bind(this);"))
    (forward-line 1)
    (evil-search search t t (point))))

;; "gd" or `evil-goto-definition' now use `imenu', `xref' first,
;; BEFORE searching string from `point-min'.
;; xref part is annoying because I already use `counsel-etags' to search tag.
(evil-define-motion inc0n/evil-goto-definition ()
  "Go to definition or first occurrence of symbol under point in current buffer."
  :jump t
  :type exclusive
  (let* ((string (evil-find-symbol t))
         (search (format "\\_<%s\\_>" (regexp-quote string)))
         ientry
         ipos)
    ;; load imenu if available
    (util/ensure 'imenu)

    (if (null string)
        (user-error "No symbol under cursor")
      (setq isearch-forward t)
      ;; if imenu is available, try it
      (cond
       ((and (derived-mode-p 'js2-mode)
             (or (null (get-text-property (point) 'face))
                 (font-belongs-to (point) '(rjsx-tag))))
        (js2-jump-to-definition))
       ((fboundp 'imenu--make-index-alist)
        (condition-case nil
            (setq ientry (imenu--make-index-alist))
          (error nil))
        (setq ientry (assoc string ientry))
        (setq ipos (cdr ientry))
        (when (and (markerp ipos)
                   (eq (marker-buffer ipos) (current-buffer)))
          (setq ipos (marker-position ipos)))
        ;; imenu found a position, so go there and
        ;; highlight the occurrence
        (inc0n/search-defun-from-pos search (if (numberp ipos) ipos (point-min))))
       ;; otherwise just go to first occurrence in buffer
       (t
        (inc0n/search-defun-from-pos search (point-min)))))))

;; {{
;; (advice-add 'evil-set-marker :before #'inc0n/evil-set-marker-hack)
;; (advice-add 'evil-goto-mark-line :around #'inc0n/evil-goto-mark-line-hack)
;; }}

;; (general-evil-setup t) ;; this is for vim like helper macro
;; }}

;; {{
(evil-define-text-object inc0n/evil-a-statement (count &optional beg end type)
  "Select a statement."
  (list (util/skip-white-space (line-beginning-position) 1)
        (line-end-position)))

(evil-define-text-object inc0n/evil-inner-statement (count &optional beg end type)
  "Select inner statement."
  (let ((b (util/skip-white-space (line-beginning-position) 1))
        (e (line-end-position)))
    (list (save-excursion
            (goto-char b)
            (while (and (< (point) e)
                        (not (= (following-char) ?=)))
              (forward-char))
            (if (= (point) e)
                b
              ;; skip '=' at point
              (goto-char (util/skip-white-space (1+ (point)) 1))
              (point)))
          (if (= (char-before e) ?\;)
              (util/skip-white-space (1- e) -1)
            e))))

(define-key evil-outer-text-objects-map "v" #'inc0n/evil-a-statement)
(define-key evil-inner-text-objects-map "v" #'inc0n/evil-inner-statement)
;; }}

;; {{ I select string inside single quote frequently
(defun inc0n/single-or-double-quote-range (count beg end type inclusive)
  "Get maximum range of single or double quote text object.
If INCLUSIVE is t, the text object is inclusive."
  (let* ((s-range (evil-select-quote ?' beg end type count inclusive))
         (d-range (evil-select-quote ?\" beg end type count inclusive))
         (beg (min (nth 0 s-range) (nth 0 d-range)))
         (end (max (nth 1 s-range) (nth 1 d-range))))
    (list beg end)))

(evil-define-text-object inc0n/evil-a-single-or-double-quote
  (count &optional beg end type)
  "Select a single-quoted expression."
  :extend-selection t
  (inc0n/single-or-double-quote-range count beg end type t))

(evil-define-text-object inc0n/evil-inner-single-or-double-quote
  (count &optional beg end type)
  "Select 'inner' single-quoted expression."
  :extend-selection nil
  (inc0n/single-or-double-quote-range count beg end type nil))

(define-key evil-outer-text-objects-map "i" #'inc0n/evil-a-single-or-double-quote)
(define-key evil-inner-text-objects-map "i" #'inc0n/evil-inner-single-or-double-quote)
;; }}

(defun inc0n/rename-thing-at-point ()
  "Rename thing at point."
  (interactive)
  (if (derived-mode-p 'js2-mode)
      ;; use `js2-mode' parser, much smarter and works in any scope
      (js2hl-rename-thing-at-point)
    ;; simple string search/replace in function scope
    (evilmr-replace-in-defun)))

(defun inc0n/evil-transient-jump (&optional backward)
  "Transient interface for `evil-jump'.
Argument BACKWARD non-nil will jump backwards initially, otherwise jump forwards."
  (interactive)
  (let ((echo-keystrokes nil))
	(if backward
	    (evil-jump-backward)
      (evil-jump-forward))
	(message "evil-jump: [f]orward [b]ackward [q]uit")
	(set-transient-map
	 (let ((map (make-sparse-keymap)))
	   (define-key map "f" #'evil-jump-forward)
	   (define-key map "b" #'evil-jump-backward)
	   map)
	 t)))

;; {{ Use `SPC` as leader key
;; all keywords arguments are still supported

;; prevent space leader key overriden
;; @see https://github.com/noctuid/evil-guide#further-integrating-evil-and-emacs
;; (with-eval-after-load 'dired-mode
;;   (general-override-mode t))
;; (add-hook 'after-init-hook 'general-override-mode)

(general-create-definer inc0n/space-leader-def
  :prefix "SPC"
  :states '(normal motion visual)
  :keymaps 'override)

;; Please check "init-ediff.el" which contains `inc0n/space-leader-def' code too
(inc0n/space-leader-def
  "0" 'winum-select-window-0
  "1" 'winum-select-window-1
  "2" 'winum-select-window-2
  "3" 'winum-select-window-3
  "4" 'winum-select-window-4
  "5" 'winum-select-window-5
  "6" 'winum-select-window-6
  "7" 'winum-select-window-7
  "8" 'winum-select-window-8

  "ac" 'aya-create
  "aw" 'ace-window
  "ar" 'align-regexp
  ;;
  "bb" (lambda () (interactive) (switch-to-buffer nil)) ; to previous buffer
  "bf" 'beginning-of-defun
  "bd" 'paredit-backward-down
  "bp" 'browse-url-at-point
  ;; "bu" 'paredit-backward-up
  ;; comment
  "ci" 'comment-operator
  "cl" 'comment-and-copy-line
  "cc" 'copy-to-clipboard
  "cp" 'comment-or-uncomment-paragraph
  ;; org
  "c$" 'org-archive-subtree             ; `C-c $'
  "cam" 'org-tags-view ; `C-c a m': search items in org-file-apps by tag
  "cxi" 'org-clock-in  ; `C-c C-x C-i'
  "cxo" 'org-clock-out ; `C-c C-x C-o'
  "cxr" 'org-clock-report               ; `C-c C-x C-r'
  ;;
  ;; "cf" 'counsel-grep           ; grep current buffer
  "cg" 'counsel-git                     ; find file
  ;;
  "da" 'diff-region-tag-selected-as-a
  "db" 'diff-region-compare-with-b
  "di" 'evilmi-delete-items

  "dc" 'inc0n/dired-redo-from-commands-history
  "dl" 'inc0n/dired-redo-last-command
  "dt" 'delete-this-buffer-and-file
  "rt" 'rename-this-file-and-buffer

  "eb" 'eval-buffer
  "ed" 'checkdoc-eval-defun
  "ee" 'eval-expression
  "ef" 'end-of-defun

  "cn" 'cp-filename-of-current-buffer
  "fp" 'cp-fullpath-of-current-buffer
  ;; find file in project
  "fp" 'find-file-in-project-at-point
  "fc" 'find-file-with-similar-name     ; ffip v5.3.1
  "fi" 'selectsel-ffip
  ;; "fi" 'find-file-in-project
  ;; "kk" 'find-file-in-project-by-selected
  "fd" 'find-directory-in-project-by-selected
  "fm" 'dired-jump ;; open the dired from current file, naming with fm for file manager

  "fa" 'flyspell-auto-correct-word
  "fb" 'flyspell-buffer
  "fc" 'flyspell-correct-word-before-point
  ;; "be" 'flyspell-goto-previous-error
  "fs" 'inc0n/transient-flyspell
  "fe" 'inc0n/transient-flycheck
  ;; "ft" 'counsel-etags-find-tag-at-point

  "gg" 'consult-git-grep       ; quickest grep should be easy to press
  "gd" 'ffip-show-diff-by-description   ;find-file-in-project 5.3.0+
  "gt" 'inc0n/evil-goto-definition      ; "gt" is occupied by evil
  ;; git
  "gl" 'inc0n/git-log-trace-definition ; find history of a function or range
  "ga" 'git-add-current-file
  "gc" 'git-checkout-current-file
  "g=" 'git-gutter:popup-hunk

  "hd" 'describe-function
  "hf" 'find-function
  "hc" 'describe-key-briefly
  "hk" 'describe-key
  "hv" 'describe-variable

  "ih" 'inc0n/goto-git-gutter           ; use ivy-mode
  "ii" 'inc0n/imenu-or-list-tag-in-current-file

  "jb" 'inc0n/evil-transient-jump
  ;;
  ;; "jj" 'scroll-other-window
  "kb" 'kill-buffer-and-window ;; "k" is preserved to replace "C-g"

  "ls" 'inc0n/transient-highlight-symbol
  ;; "ln" 'highlight-symbol-nav-mode ; use M-n/M-p to navigation between symbols

  "mm" 'lookup-doc-in-man
  "mf" 'mark-defun
  "mp" 'pop-to-mark-command ;; 'avy-pop-mark

  "op" 'compile
  "oa" 'selectrum-org-agenda-headlines
  "oc" 'org-capture
  "og" 'org-agenda
  "on" 'org-agenda-show-agenda-and-todo
  "otl" 'org-toggle-link-display
  "ou" 'org-update-statistics-cookies

  "nh" 'inc0n/goto-next-hunk

  "pd" 'pwd
  "ph" 'inc0n/goto-previous-hunk
  "pp" 'paste-from-clipboard

  "rb" 'evilmr-replace-in-buffer
  "rn" 'evilmr-replace-in-defun
  "re" 'counsel-etags-recent-tag
  "rjs" 'run-js

  "sr" 'scratch
  ;; "ss" 'wg-create-workgroup ; save windows layout
  ;; "sc" 'shell-command
  "sc" 'selectrum-imenu-comments
  ;; "sm" 'selectrum-evil-marks
  "ss" 'inc0n/selectsel-rg              ; 'selectsel-rg

  ;; "ti" 'inc0n/toggle-indentation
  ;; @see https://github.com/pidu/git-timemachine
  ;; p: previous; n: next; w:hash; W:complete hash; g:nth version; q:quit
  "tm" 'inc0n/git-timemachine
  "tt" 'inc0n/toggle-day/night
  "ti" 'toggle-indent-tabs-mode

  "vf" 'vc-rename-file-and-buffer
  "vc" 'vc-copy-file-and-rename-buffer
  "vg" 'vc-annotate                     ; 'C-x v g' in original
  "vv" 'vc-msg-show

  "ycr" 'inc0n/yas-reload-all

  "xv" 'vc-next-action                  ; 'C-x v v' in original
  "xe" 'eval-last-sexp
  "xh" 'mark-whole-buffer
  "xc" 'save-buffers-kill-emacs
  "xm" 'execute-extended-command
  "xs" 'save-buffer
  ;; {{ @see http://ergoemacs.org/emacs/emacs_pinky_2020.html
  ;; `keyfreq-show' proved sub-window operations happen most.
  "x0" 'delete-window
  "x1" 'delete-other-windows
  "x2" (lambda () (interactive) (split-window-vertically) (other-window 1))
  "x3" (lambda () (interactive) (split-window-horizontally) (other-window 1))
  ;; }}
  "uu" 'inc0n/transient-winner

  ;; {{ window move
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wk" 'evil-window-up
  "wj" 'evil-window-down
  ;; }}
  "wf" 'popup-which-function
  "wo" 'ace-window
  "ws" 'ace-swap-window
  "wr" 'rotate-two-split-window
  "ww" 'narrow-or-widen-dim
  ;;
  "SPC" 'just-one-space)
;; }}

(defun copy-and-paste (beg end)
  (interactive (if (region-active-p)
				   (list (region-beginning) (region-end))
			     (let ((beg (line-beginning-position))
                       (end (1+ (line-end-position))))
                   (goto-char end)
                   (list beg end))))
  (let ((x (buffer-substring beg end)))
    (insert x)))

(defun comment-and-copy-line (n)
  (interactive "p")
  (let ((str (if (region-active-p)
				 (concat (util/selected-str) "\n")
			   (util/line-str n))))
	(save-excursion
	  (beginning-of-line)
	  (insert str)
	  (comment-line (- n)))))

(defun comment-or-uncomment-paragraph (n)
  "Comment out a paragraph starting from the beginning of line text.
Argument N the number of paragraph to operate on."
  (interactive "p")
  (comment-or-uncomment-region
   (line-beginning-position)
   (progn (forward-paragraph (or n 1))
		  (point))))

(defun comment-operator (n)
  "My implementation of a `comment-operator'.
Argument N the number of lines to operate on."
  (interactive "P")
  (if (region-active-p)
	  (comment-or-uncomment-region
       (region-beginning) (region-end))
    (comment-line n)))


;; {{ Use `;` as leader key, for searching something
(general-create-definer inc0n/semicolon-leader-def
  :prefix ";"
  :states '(normal visual))

(inc0n/semicolon-leader-def
  ;; Search character(s) at the beginning of word
  ;; See https://github.com/abo-abo/avy/issues/70
  ;; You can change the avy font-face in ~/.custom.el:
  ;;  (with-eval-after-load 'avy
  ;;    (set-face-attribute 'avy-lead-face-0 nil :foreground "black")
  ;;    (set-face-attribute 'avy-lead-face-0 nil :background "#f86bf3"))
  ;; ";" 'ace-pinyin-jump-char-2
  ";" 'avy-goto-char-timer
  "w" 'avy-goto-word-or-subword-1
  "a" 'avy-goto-char-timer
  "db" 'sdcv-search-input               ; details
  "dt" 'sdcv-search-input+              ; summary
  "dd" 'inc0n/lookup-dict-org
  "mm" 'lookup-doc-in-man
  "ge" 'inc0n/eww-search
  "gs" 'w3m-search
  "gg" 'w3m-google-search
  "gd" 'w3m-search-financial-dictionary
  "gq" 'w3m-stackoverflow-search)
;; }}

(evil-set-initial-state 'xref--xref-buffer-mode 'motion)

(use-package evil-matchit
  :config
  (setq evilmi-shortcut "m"
	    evilmi-may-jump-by-percentage nil)
  :init
  (add-hook 'after-init-hook 'global-evil-matchit-mode))

;; evil-exchange
(use-package evil-exchange
  :defer 1
  :config
  (evil-exchange-install)
  ;; press `evil-exchange-key' twice to exchange, gX to cancel
  (setq evil-exchange-key (kbd "zx")))

;; {{ @see https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org#replacing-text-with-iedit
;; same keybindings as spacemacs:
;;  - Start `iedit-mode' by `evil-iedit-state/iedit-mode'
;;  - "TAB" to toggle current occurrence
;;  - "n" next, "N" previous (obviously we use "p" for yank)
;;  - "gg" the first occurrence, "G" the last occurrence
;;  - Please note ";;" or `avy-goto-char-timer' is also useful
;; }}

;; Evil’s f/F/t/T command can search PinYin
(use-package evil-find-char-pinyin
  :defer 2
  :config (evil-find-char-pinyin-mode))

;; {{ evil-args
(require-package 'evil-args)
;; bind evil-args text objects
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)

;; bind evil-forward/backward-args
(evil-define-key '(normal motion) 'global
  "L" 'evil-forward-arg
  "H" 'evil-backward-arg
  ;; Move the cursor out of the nearest enclosing matching pairs.
  "K" 'evil-jump-out-args)
;; }}

(with-eval-after-load 'evil
  ;; @see https://bitbucket.org/lyro/evil/issue/360/possible-evil-search-symbol-forward
  ;; evil 1.0.8 search word instead of symbol
  (setq evil-symbol-word-search t)

  ;; don't add replaced text to `kill-ring'
  (setq evil-kill-on-visual-paste nil)
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; @see https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  ;; uncomment below line to make "dw" has exact same behaviour in evil as as in vim
  ;; (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
  (with-eval-after-load 'git-timemachine
    (evil-make-overriding-map 'git-timemachine-mode-map 'normal)
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

  ;; Move the cursor one character backward when exiting insert mode
  (setq evil-move-cursor-back t)
  (setq evil-ex-search-direction 'forward)

  ;; @see https://bitbucket.org/lyro/evil/issue/342/evil-default-cursor-setting-should-default
  ;; Cursor is always black because of evil.
  ;; Here is the workaround
  (setq evil-default-cursor t
        evil-auto-indent t
        evil-want-fine-undo t
        evil-buffer-regexps nil
        evil-want-C-i-jump t))

(provide 'init-evil)
