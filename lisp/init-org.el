;; -*- coding: utf-8; lexical-binding: t; -*-

;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

(require-package 'org-re-reveal)
(require-package 'org-appear)
(require-package 'org-superstar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org clock
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'org-clock
  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook #'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook #'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook #'sanityinc/hide-org-clock-from-header-line)

  (define-key org-clock-mode-line-map [header-line mouse-2] #'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] #'org-clock-menu))


;; {{ @see http://orgmode.org/worg/org-contrib/org-mime.html
(with-eval-after-load 'org-mime
  (setq org-mime-export-options
		'(:section-numbers nil :with-author nil :with-toc nil))
  (define-hook-setup org-mime-html-hook
    (org-mime-change-element-style
     "pre"
     "color:#E6E1DC; background-color:#232323; padding:0.5em;")
    (org-mime-change-element-style
     "blockquote"
     "border-left: 2px solid gray; padding-left: 4px;")))
;; }}

(define-hook-setup org-mode-hook
  (unless (buffer-file-temp-p)

    ;; org-mime setup, run this command in org-file, than
    ;; yank in `message-mode'
    (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)
    (local-set-key (kbd "M-+") 'org-count-words)

    ;; don't spell check double words
    (setq inc0n/flyspell-check-doublon nil)

    ;; create updated table of contents of org file
    ;; @see https://github.com/snosov1/toc-org
    (toc-org-enable)

    ;; display wrapped lines instead of truncated lines
    (setq truncate-lines nil)
    (setq word-wrap t)
    (setq prettify-symbols-alist
          (append prettify-symbols-alist
                  `(("[ ]" . ?☐)          ; checkbox
                    ("[-]" . ?◼)          ; pending
                    ("[X]" . ?☑)          ; checked box
                    ("::" . ?∷)           ; list property
                    ("---" . "—")         ; em dash
                    ("..." . ?…)          ; ellipsis
                    ("->" . ?→)
                    ("<-" . ?←)
                    ;; ("#+title:" . ?𝙏)
                    ;; ("#+subtitle:"      ?𝙩)
                    ;; ("#+author:" . ?𝘼)
                    ;; ("#+date:" . ?𝘿)
                    ;; ("#+property" . ?⚙)
                    ;; ("#+options:" . ?⌥)
                    ("#+header:" . ?›)
                    ("#+latex_header:" . ?⇥)
                    ("#+beamer_header" . ?↠)
                    ;; ("#+caption" . ?☰)
                    ("#+begin_quote" . ?❝)
                    ("#+end_quote" . ?❞)
                    ("#+begin_export" . ?⏩)
                    ("#+end_export" . ?⏪)
                    ("[#A]" . ,(propertize "⚑" 'face 'all-the-icons-red))
                    ("[#B]" . ,(propertize "⬆" 'face 'all-the-icons-orange))
                    ("[#C]" . ,(propertize "■" 'face 'all-the-icons-yellow))
                    ("[#D]" . ,(propertize "⬇" 'face 'all-the-icons-green))
                    ("[#E]" . ,(propertize "❓" 'face 'all-the-icons-blue))
                    (":PROPERTIES:" . ?☸)
                    (":END" . ?∎))))
    (prettify-symbols-mode 1)))

(with-eval-after-load 'org
  ;; {{
  (defvar inc0n/org-src--saved-temp-window-config nil
    "Window layout before edit special element.")

  (defun inc0n/org-edit-special (&optional arg)
    "Save current window layout before `org-edit' buffer is open.
ARG is ignored."
    (setq inc0n/org-src--saved-temp-window-config
          (current-window-configuration)))

  (defun inc0n/org-edit-src-exit ()
    "Restore the window layout that was saved in `inc0n/org-edit-special'."
    (when inc0n/org-src--saved-temp-window-config
      (set-window-configuration inc0n/org-src--saved-temp-window-config)))

  ;; org 9.3 do not restore windows layout when editing special element
  (advice-add 'org-edit-special :before #'inc0n/org-edit-special)
  (advice-add 'org-edit-src-exit :after #'inc0n/org-edit-src-exit)
  ;; }}

  (util/ensure 'org-clock)

  ;; odt export
  (add-to-list 'org-export-backends 'odt)

  ;; markdown export
  (util/ensure 'ox-md)
  (add-to-list 'org-export-backends 'md)

  (defun inc0n/org-publish-hack (orig-func &rest args)
    "Stop running `major-mode' hook when `org-publish'."
    (let ((load-user-customized-major-mode-hook nil))
      (apply orig-func args)))
  (advice-add 'org-publish :around #'inc0n/org-publish-hack)

  ;; {{ NO spell check for embedded snippets
  (defun inc0n/org-mode-code-snippet-p ()
    "Code snippet embedded in org file?"
    (let ((begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\)")
          (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\)")
          (case-fold-search t))
      (save-excursion
        (and (re-search-backward begin-regexp nil t)
             (< (point)
                (re-search-forward end-regexp nil t))))))

  (defun inc0n/org-mode-flyspell-verify-hack (orig-func &rest args)
    "flyspell only uses `ispell-word'."
    (let ((run-spellcheck (apply orig-func args)))
      (and run-spellcheck
           ;; will not run if any of the following it `t'
           (not (or
                 ;; skip checking in certain faces
                 (font-belongs-to (point) '(org-verbatim org-code))
                 ;; skip checking property lines
                 (save-excursion
                   (goto-char (line-beginning-position))
                   (looking-at-p "^[ \t]+:[A-Z]+:[ \t]+"))
                 ;; skipping checking in code snippet
                 ;; slow test should be placed at last
                 (inc0n/org-mode-code-snippet-p))))))
  (advice-add 'org-mode-flyspell-verify
              :around #'inc0n/org-mode-flyspell-verify-hack)
  ;; }}

  (defun inc0n/org-refile-hack (orig-func &rest args)
    "When `org-refile' scans org files, skip user's own code in `org-mode-hook'."
    (let ((force-buffer-file-temp-p t))
      (apply orig-func args)))
  (advice-add 'org-refile :around #'inc0n/org-refile-hack)

  (define-hook-setup org-after-todo-state-change-hook
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))

  ;; {{ export org-mode in Chinese into PDF
  ;; @see http://freizl.github.io/posts/tech/2012-04-06-export-orgmode-file-in-Chinese.html
  ;; and you need install texlive-xetex on different platforms
  ;; To install texlive-xetex:
  ;;    `sudo USE="cjk" emerge texlive-xetex` on Gentoo Linux
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f")) ;; org v8
  ;; }}

  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
        org-list-allow-alphabetical t  ; have a. A. a) A) list bullets
        )
  (setq org-export-in-background nil ; run export processes in external emacs process
        org-catch-invisible-edits 'smart ; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{} ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
        )

  ;; misc
  (setq org-startup-with-latex-preview t
        org-startup-indented t
        org-indent-mode-turns-on-hiding-stars nil
		org-hide-leading-stars nil
        org-log-done 'note
        org-edit-src-content-indentation 0
        org-edit-timestamp-down-means-later t
        org-fast-tag-selection-single-key 'expert
        ;; org v8
        org-odt-preferred-output-format "doc"
        org-tags-column 80

        ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
        org-refile-targets '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
							 ("agenda.org" :regexp . "Past"))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-todo-keywords
		'((sequence "TODO(t)" "STARTED(s@)" "NEXT(n)" "HOLD(h@/!)" "|" "DONE(d!/!)")
          (sequence "PROJECT(P@)" "|" "CANCELLED(c@/!)"))
        org-imenu-depth 5
        ;; @see http://irreal.org/blog/1
        org-return-follows-link t)
  ;; org agenda
  (setq org-agenda-start-on-weekday nil
        org-agenda-span 14
        ;; org-agenda-include-diary t
        org-agenda-window-setup 'only-window
        ;; {{ org 8.2.6 has some performance issue. Here is the workaround.
        ;; @see http://punchagan.muse-amuse.in/posts/how-i-learnt-to-use-emacs-profiler.html
        org-agenda-inhibit-startup t       ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        ;; }}
        org-agenda-tags-column 80)

  (define-hook-setup org-insert-heading-hook
    (evil-insert-state 1))

  (general-define-key
   :keymaps 'org-mode-map
   [C-return] 'inc0n/org-insert
   [return] 'org-return-indent)

  (setq org-directory "~/sources/org/agenda/")
  (setq org-agenda-files (list (concat org-directory "agenda.org")
                               (concat org-directory "analysis.org")
                               (concat org-directory "refile.org")
                               (concat org-directory "projects.org")
                               (concat org-directory "notes.org")
                               (concat org-directory "todo.org")))
  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs"
           ((tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "Completed today")))
            (agenda ""
                    ((org-agenda-skip-function
					  'org-agenda-skip-if-past-schedule)
                     (org-deadline-warning-days 7)))
            ;; (tags-todo "TODO")
            (todo "TODO"
                  (;; (org-agenda-format-date "")
                   (org-agenda-prefix-format " %-10:c %s %l")
                   (org-agenda-overriding-header "Todo")))
            (todo "PROJECT"
                  ((org-agenda-overriding-header "Projects")))
            (todo "HOLD"
                  ((org-agenda-overriding-header "Maybe")))))
		  ("b" "buffer summary"
		   ((agenda "" ((org-agenda-files (list (buffer-file-name)))))))))
  (setq org-capture-templates
		`(("t" "Todo" entry  (file "todo.org")
		   ,(concat "* TODO %?\n"
					"/Entered on/ %U"))
          ("a" "Analysis" entry (file "analysis.org")
		   "* TODO %? [%<%Y-%m-%d %a>]\n")
          ("e" "Event" entry (file+headline "agenda.org" "Future")
		   ,(concat "* %? :event:\n"
					"SCHEDULED: <%<%Y-%m-%d %a %H:00>>"))
          ;; ("r" "Respond" entry (file "agenda.org")
		  ;;  ,(concat "* NEXT Respond to %:from on %:subject\n"
		  ;;   		"SCHEDULED: %t\n"
		  ;;   		"%U\n"
		  ;;   		"%a\n"))
		  ("s" "Schedule" entry (file+headline "agenda.org" "Future")
		   ,(concat "* %?\n"
					"SCHEDULED: %t")
           :time-prompt t)
          ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
		   ,(concat "* %? :meeting:\n"
					"<%<%Y-%m-%d %a %H:00>>"))
		  ("d" "Deadline" entry (file+headline "agenda.org" "Deadline")
		   ,(concat "* %?\n"
					"DEADLINE: %t"))
		  ("n" "Note" entry  (file "notes.org")
		   ,(concat "* Note (%a)\n"
					"/Entered on/ %U\n" "\n" "%?"))
		  ;; ("n" "note" entry (file "note.org")
		  ;;  "* %? :NOTE:\n%U\n%a\n")
		  ("r" "Rant" entry  (file "notes.org")
		   "* %T %? :rant:\n" :jump-to-captured t)
		  ;; ("d" "Diary" entry (file+datetree "diary.org")
		  ;;  "* %?\n%U\n")
		  ;; ("p" "Phone call" entry (file "refile.org")
		  ;;  "* PHONE %? :PHONE:\n%U")
		  ;; ("h" "Habit" entry (file "refile.org")
		  ;;  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
		  ("p" "Project" entry (file "projects.org")
		   ,(concat "* PROJECT [%<%Y-%m-%d %a>] %?"))))
  (setq org-pretty-entities t ;; render entity
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-link-descriptive t
        org-hide-emphasis-markers t)
  ;; org-babel for gnuplot
  ;; @see https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-gnuplot.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))
  ;; (require 'org-protocol)
  (custom-set-faces
   '(org-document-title ((t (:height 1.2))))
   '(outline-1 ((t (:weight black :height 1.25))))
   '(outline-2 ((t (:weight bold :height 1.15))))
   '(outline-3 ((t (:weight bold :height 1.12))))
   '(outline-4 ((t (:weight semi-bold :height 1.09))))
   '(outline-5 ((t (:weight semi-bold :height 1.06))))
   '(outline-6 ((t (:weight semi-bold :height 1.03))))
   '(outline-8 ((t (:weight semi-bold))))
   '(outline-9 ((t (:weight semi-bold))))))

(defun inc0n/org-insert (&optional arg)
    "Insert item or heading depending on context.
Insert before if ARG is non-nil"
    (interactive "P")
    (if (and (null arg)
             (org-in-item-p))
        (org-insert-item)
      (org-insert-heading-after-current)))

;;; org-appear
(add-hook 'org-mode-hook 'org-appear-mode)
(with-eval-after-load 'org-appear
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(defun org-agenda-show-agenda-and-todo (&optional arg)
  "Better `org-mode' agenda view.  ARG is passed in."
  (interactive "P")
  (org-agenda arg "n"))

(defun org-agenda-skip-if-past-schedule ()
  "If this function return nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (when-let ((subtree-end (save-excursion (org-end-of-subtree t)))
			 (schedule (org-entry-get nil "SCHEDULED"))
             (now (time-to-seconds (current-time))))
    (when-let ((scheduled-seconds
			    (time-to-seconds
				 (org-time-string-to-time schedule))))
      (and (not (string= (org-get-todo-state) "NEXT")) ;; never skip todo NEXT state
           (< scheduled-seconds now)
           subtree-end))))

(define-hook-setup org-agenda-mode-hook
  (evil-mode 1)
  (evil-normal-state))

(with-eval-after-load 'org-superstar
  ;; (setq org-superstar-headline-bullets-list '(?◉ ?◈ ?✸ ?▣)
  ;;   	org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➤) (?- . ?-)))
  ;;    org-superstar-item-bullet-alist '((?* . ?▶) (?+ . ?⬘) (?- . ?⬙))
  (setq-default org-superstar-prettify-item-bullets t
                ;; org-superstar-headline-bullets-list '(?⬘)
                org-superstar-leading-bullet "."
                org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ"))
  (setq org-superstar-cycle-headline-bullets nil
		org-superstar-special-todo-items nil)
  (set-face-attribute 'org-superstar-leading nil :foreground "dark gray")
  (org-superstar-restart))
(add-hook 'org-mode-hook 'org-superstar-mode)

;; org capture
(global-set-key (kbd "C-c C-c") 'org-capture)

(define-hook-setup org-ctrl-c-ctrl-c-final-hook :capture
  (org-capture)
  t)

(defun org-goto-item-between-region (start forward)
  "Find the item between START and END, direction is controlled by FORWARD."
  (call-interactively
   (if forward 'next-line 'previous-line))
  (let ((item (org-in-item-p)))
    (or (if (and item start)
            (and (/= start item) item)
          item)
        (and (org-at-heading-p) (point))
        ;; (unless (if forward (> (point) end) (< (point) end)))
        (org-goto-item-between-region start forward))))

;; (evil-declare-key 'normal org-mode-map
;;   "g=" nil)

(defun org-goto-visible-element (arg forward)
  "Move cursor to the previous visible item or heading.
ARG will repeat the operation ARG number of times.
FORWARD will go forward unless nil"
  ;; (setq forward (if forward 1 -1))
  (when (> arg 0)
    (let* ((item (org-in-item-p))
           (next-pos
            (if (or (null item)
                    (= item (point)))
                (org-goto-item-between-region item forward)
              item)))
      ;; (message "item %s %s" next-pos arg)
      (goto-char next-pos)
      (org-goto-visible-element (1- arg) forward))))

(defun org-next-visible-element (arg)
  "Move cursor to the previous visible item or heading.
ARG will repeat this function ARG number of times."
  (interactive "P")
  ;; (message "arg %s" format-args)
  (if (consp arg)
      ;; (org-back-to-heading)
      (org-forward-element)
    (org-goto-visible-element (or arg 1) t)))

(defun org-previous-visible-element (arg)
  "Move cursor to the previous visible item or heading.
ARG will repeat this function ARG number of times."
  (interactive "P")
  (if (consp arg)
      (org-backward-heading-same-level 1)
    (org-goto-visible-element (or arg 1) nil)))

(defun org-count-words (start end)
  "This is the count words version that skips comments.
It will operate between the region from START to END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (setq start (point-min)
            end (point-max))
      (goto-char start)
      (cl-loop with org-line-commentp =
               (rx line-start (* space) "#" (any " " "+") (* any))
               ;; "^[ \t]*#[+ ].*"
               for line-beg = (line-beginning-position)
               for line-end = (line-end-position)
               until (= line-end end)
               unless (search-forward-regexp org-line-commentp line-end t)
               sum (count-words line-beg line-end) into words-count
               and sum 1 into lines-count
               and sum (- line-end line-beg) into chars-count
               do (goto-char (1+ line-end))
               finally (message "region has %d lines, %d words, %d characters"
                                lines-count words-count chars-count)))))


(defun latex-auto-ref-link-export (path _desc backend)
  "Exporting link using autoref of PATH for latex BACKEND."
  (cond ((eq 'latex backend)
         (format "\\autoref{%s}" path))))

(defun latex-set-org-link-parameters (&rest types)
  "Link parameter of TYPES will be using `latex-auto-ref-link-export' for latex."
  (mapc (lambda (type)
          (org-link-set-parameters type :export 'latex-auto-ref-link-export))
        types))

(with-eval-after-load 'ox-latex
  (latex-set-org-link-parameters "lst" "table")
  ;;
  (defun inc0n/org-latex-link (orig-func link desc info)
    "Advice function that will find a default description, i.e. the caption to pass on into the original `org-latex-link' function"
    (let ((type (org-element-property :type link)))
      (when-let* ((link-params (assoc type org-link-parameters #'string=))
                  (export (plist-get (cdr link-params) :export)))
        (when (eq export 'latex-auto-ref-link-export)
          ;; store the `raw-link' back into the `path' of link
          (org-element-put-property link :path (org-element-property :raw-link link))
          ;; TEST - the correct reference can be found now
          ;;        this line  can be removed
          (org-export-resolve-fuzzy-link link info)))
      (funcall orig-func link nil info)))
  (advice-add 'org-latex-link :around 'inc0n/org-latex-link)
  (add-to-list 'org-latex-listings-langs '(javascript "Javascript"))
  (setq org-latex-caption-above '(table src-block)
        ;; Our hack for using auto ref to generate our nice labels
        org-latex-listings t
        org-latex-prefer-user-labels t))

;; org-emphasis-alist
(provide 'init-org)
;;; init-org.el ends here
