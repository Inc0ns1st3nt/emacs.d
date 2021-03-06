;; -*- coding: utf-8; lexical-binding: t; -*-

;; some cool org tricks
;; @see http://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode

(require-package 'org-re-reveal)
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

(defun org-op-on-tree-and-subtree (procedure)
  "Call procedure on the tree and its subtree or tree in region"
  (lambda ()
    (interactive)
    ;; fix edge case when heading at EOF and only has spaces as heading title
    (beginning-of-line)
    (save-excursion
      (unless (or (region-active-p)
                  (let ((line (thing-at-point 'line t)))
                    (and (string-match-p "^\\*+ $" line) ;; is node only one spaced
                         (= (point) (- (point-max) (length line))) ;; is line at EOF
                         )))
        (org-mark-subtree)))
    (funcall procedure)))

;; {{ @see http://orgmode.org/worg/org-contrib/org-mime.html
(with-eval-after-load 'org-mime
  (setq org-mime-export-options
		  '(:section-numbers nil :with-author nil :with-toc nil))
  (defun org-mime-html-hook-setup ()
    (org-mime-change-element-style
     "pre"
     "color:#E6E1DC; background-color:#232323; padding:0.5em;")
    (org-mime-change-element-style "blockquote"
                                   "border-left: 2px solid gray; padding-left: 4px;"))
  (add-hook 'org-mime-html-hook #'org-mime-html-hook-setup))

(autoload 'org-mime-edit-mail-in-org-mode "org-mime" nil t)
(autoload 'org-mime-revert-to-plain-text-mail "org-mime" nil t)
;; }}

(defun org-mode-hook-setup ()
  (unless (buffer-file-temp-p)

    ;; org-mime setup, run this command in org-file, than yank in `message-mode'
    (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)
    (local-set-key (kbd "M-+") 'org-count-words)

    ;; don't spell check double words
    (setq inc0n/flyspell-check-doublon nil)

    ;; create updated table of contents of org file
    ;; @see https://github.com/snosov1/toc-org
    (toc-org-enable)

    ;; display wrapped lines instead of truncated lines
    (setq truncate-lines nil)
    (setq word-wrap t)))
(add-hook 'org-mode-hook #'org-mode-hook-setup)

(with-eval-after-load 'org
  ;; {{
  (defvar inc0n/org-src--saved-temp-window-config nil
    "Window layout before edit special element.")
  (defun inc0n/org-edit-special (&optional arg)
    "Save current window layout before `org-edit' buffer is open.
ARG is ignored."
    (setq inc0n/org-src--saved-temp-window-config (current-window-configuration)))

  (defun inc0n/org-edit-src-exit ()
    "Restore the window layout that was saved before `org-edit-special' is called."
    (when inc0n/org-src--saved-temp-window-config
      (set-window-configuration inc0n/org-src--saved-temp-window-config)
      ;; (setq inc0n/org-src--saved-temp-window-config nil)
      ))

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

  (defun org-agenda-show-agenda-and-todo (&optional arg)
    "Better org-mode agenda view."
    (interactive "P")
    (org-agenda arg "n"))

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
    (let ((run-spellcheck
           (apply orig-func args)))
      (and run-spellcheck
           ;; will not run if any of the following it `t'
           (not (or
                 ;; skip checking in below fonts
                 (font-belongs-to (point) '(org-verbatim org-code))
                 ;; skip checking property lines
                 (string-match "^[ \t]+:[A-Z]+:[ \t]+" (util/line-str))
                 ;; skipping checking in code snippet
                 ;; slow test should be placed at last
                 (inc0n/org-mode-code-snippet-p))))))
  (advice-add 'org-mode-flyspell-verify :around #'inc0n/org-mode-flyspell-verify-hack)
  ;; }}

  (defun inc0n/org-refile-hack (orig-func &rest args)
    "When `org-refile' scans org files, skip user's own code in `org-mode-hook'."
    (let ((force-buffer-file-temp-p t))
      (apply orig-func args)))
  (advice-add 'org-refile :around #'inc0n/org-refile-hack)

  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook
			#'log-todo-next-creation-date)

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

  (setq org-latex-listings t)

  ;; misc
  (setq org-startup-with-latex-preview t
        org-startup-indented t
        org-indent-mode-turns-on-hiding-stars nil
		org-hide-leading-stars nil
		org-pretty-entities t ;; render entity
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
        org-src-fontify-natively t
        org-return-follows-link t)
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

  (defun org-insert-heading-hooker ()
    (evil-insert-state 1))
  (add-hook 'org-insert-heading-hook 'org-insert-heading-hooker)

  (defun inc0n/org-insert (&optional arg)
    "insert item or heading depending on context, insert before
if arg is non-nil"
    (interactive "P")
    (if (and (null arg)
             (org-in-item-p))
        (org-insert-item)
      (org-insert-heading-respect-content)))
  (define-key org-mode-map [C-return] 'inc0n/org-insert)
  (define-key org-mode-map [return] 'org-return-indent)

  (global-set-key (kbd "C-c C-C") 'org-capture)

  (setq org-directory "~/sources/org/agenda/")
  (setq org-agenda-files (list (concat org-directory "agenda.org")
                               (concat org-directory "analysis.org")
                               (concat org-directory "refile.org")
                               (concat org-directory "projects.org")
                               (concat org-directory "notes.org")
                               (concat org-directory "todo.org")))
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
					"SCHEDULED: %t"))
          ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
		   ,(concat "* %? :meeting:\n"
					"<%<%Y-%m-%d %a %H:00>>"))
		  ("d" "Deadline" entry (file+headline "agenda.org" "Deadline")
		   ,(concat "* %?\n"
					"DEADLINE: %t"))
		  ("n" "Note" entry  (file "notes.org")
		   ,(concat "* Note (%a)\n"
					"/Entered on/ %U\n" "\n" "%?"))
		  ("r" "Rant" entry  (file "notes.org")
		   "* %T %? :rant:\n" :jump-to-captured t)
		  ;; ("d" "Diary" entry (file+datetree "diary.org")
		  ;;  "* %?\n%U\n")
		  ("p" "Project" entry (file "projects.org")
		   ,(concat "* PROJECT [%<%Y-%m-%d %a>] %?"))
		  ;; ("n" "note" entry (file "note.org")
		  ;;  "* %? :NOTE:\n%U\n%a\n")
		  ;; ("p" "Phone call" entry (file "refile.org")
		  ;;  "* PHONE %? :PHONE:\n%U")
		  ;; ("h" "Habit" entry (file "refile.org")
		  ;;  "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
		  ))
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
  ;; org-babel for gnuplot
  ;; @see https://www.orgmode.org/worg/org-contrib/babel/languages/ob-doc-gnuplot.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))
  ;; (require 'org-protocol)
  )

(defun org-agenda-skip-if-past-schedule ()
  "If this function return nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (when-let* ((subtree-end (save-excursion (org-end-of-subtree t)))
			  (schedule (org-entry-get nil "SCHEDULED"))
			  (scheduled-seconds
			   (time-to-seconds
				(org-time-string-to-time schedule)))
			  (now (time-to-seconds (current-time))))
    (and (not (string= (org-get-todo-state) "NEXT")) ;; never skip todo NEXT state
		 scheduled-seconds
         (<= scheduled-seconds now)
         subtree-end)))

(defun org-agenda-mode-setup ()
  (evil-mode 1)
  (evil-normal-state))
(add-hook 'org-agenda-mode-hook #'org-agenda-mode-setup)

(with-eval-after-load 'org-superstar
  ;; (setq org-superstar-headline-bullets-list '(?◉ ?◈ ?✸ ?▣)
  ;;   	org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?➤) (?- . ?-)))
  (setq org-superstar-headline-bullets-list '(?•)
        org-superstar-item-bullet-alist '((?* . ?▶) (?+ . ?⬘) (?- . ?⬙)))
  (setq org-superstar-cycle-headline-bullets t
		org-superstar-special-todo-items nil)
  (set-face-attribute 'org-superstar-leading
                      nil
                      :foreground "dark gray")
  (org-superstar-restart))
(add-hook 'org-mode-hook 'org-superstar-mode)

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

(defvar org-latex-link-prefix-alist nil)
(defun latex-set-org-link-parameters (type name)
  (add-to-list 'org-latex-link-prefix-alist (cons type name))
  (org-link-set-parameters
   type
   :export (lambda (path desc backend)
             (cond ((eq 'latex backend)
                    (format "\\autoref{%s}" path))))))

(with-eval-after-load 'ol
  (latex-set-org-link-parameters "lst" "Listing")
  (latex-set-org-link-parameters "table" "Table")
  (setq org-link-descriptive nil))

(with-eval-after-load 'ox-latex
  (defun inc0n/org-latex-link (orig-func link desc info)
    "Advice function that will find a default description, i.e. the caption to pass on into the original `org-latex-link' function"
    (let ((type (org-element-property :type link)))
      (when (assoc type org-latex-link-prefix-alist)
        ;; store the type back into the `path' of link
        (org-element-put-property link :path (org-element-property :raw-link link))
        ;; the correct reference can be found now
        (org-export-resolve-fuzzy-link link info))
      (funcall orig-func link nil info)))
  (advice-add 'org-latex-link :around 'inc0n/org-latex-link)
  (add-to-list 'org-latex-listings-langs '(javascript "Javascript"))
  (setq org-latex-caption-above '(table src-block)
        org-latex-prefer-user-labels t))

;; org-emphasis-alist
(provide 'init-org)
;;; init-org.el ends here