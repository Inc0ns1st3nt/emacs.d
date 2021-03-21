;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

(defun inc0n/enable-yas-minor-mode ()
  "Enable `yas-minor-mode'."
  (unless (buffer-file-temp-p)
    (yas-minor-mode 1)))

(dolist (hook '(prog-mode-hook
                text-mode-hook
                ;; {{ modes that are NOT inherit from prog-mode
                cmake-mode-hook
                web-mode-hook
                scss-mode-hook
                ;; }}
                ))
  (add-hook hook #'inc0n/enable-yas-minor-mode))

(defun inc0n/yas-expand-from-trigger-key-hack (orig-func &rest args)
  "Tab key won't trigger yasnippet expand in org heading."
  (if (and (eq major-mode 'org-mode)
           (string-match "^org-level-"
                         (format "%S" (get-text-property (point) 'face))))
      ;; skip yas expand in org heading
      (org-cycle)
    (apply orig-func args)))
(advice-add 'yas-expand-from-trigger-key
			:around #'inc0n/yas-expand-from-trigger-key-hack)

(defun inc0n/yas-reload-all ()
  "Compile and reload snippets.  Run the command after adding new snippets."
  (interactive)
  (yas-compile-directory (inc0n/emacs-d "snippets"))
  (yas-reload-all)
  (inc0n/enable-yas-minor-mode))

(defun inc0n/yas-field-to-statement (str sep)
  "If STR=='a.b.c' and SEP=' && ', 'a.b.c' => 'a && a.b && a.b.c'"
  (mapconcat 'identity
             (cl-reduce (lambda (acc elm)
						  (if acc
							  (concat acc "." elm)
							elm))
						(split-string str "\\."))
             sep))

(defun inc0n/yas-get-first-name-from-to-field ()
  (let ((str (save-excursion
               (goto-char (point-min))
               ;; first line in email could be some hidden line containing NO to field
               (util/buffer-str))))
    ;; (message "str=%s" str)
    (if (string-match "^To: \"?\\([a-zA-Z]+\\)" str)
        (capitalize (match-string 1 str))
      "AGENT_NAME")))

(defun inc0n/yas-camelcase-to-string-list (str)
  "Convert camelcase STR into string list."
  (let ((case-fold-search nil))
	(setq str (replace-regexp-in-string "\\([A-Z]+\\)" " \\1" str t))
	(setq str (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]+\\)" "\\1 \\2"
										str t))
    (split-string str " ")))

(defun inc0n/yas-camelcase-to-downcase (str)
  (let ((l (inc0n/yas-camelcase-to-string-list str))
        (case-fold-search nil))
    (mapconcat #'identity
               (mapcar (lambda (elem)
                         (if (string-match "^[A-Z]+$" elem)
                             elem
                           (downcase elem)))
                       l)
               " ")))

(defun inc0n/yas-escape-string (s)
  (replace-regexp-in-string
   "\"" "\\\\\""
   (replace-regexp-in-string "'" "\\\\'" s)))

(defun inc0n/read-n-from-kill-ring ()
  (let ((cands (subseq kill-ring 0 (min (read-number "fetch N `kill-ring'?" 1)
                                        (length kill-ring)))))
    (mapc (lambda (txt)
            (set-text-properties 0 (length txt) nil txt))
          cands)))

(defun inc0n/yas-get-var-list-from-kill-ring ()
  "Variable name is among the `kill-ring'.  Multiple major modes supported."
  (let ((top-kill-ring (inc0n/read-n-from-kill-ring)))
    (cond
     ((memq major-mode '(js-mode javascript-mode js2-mode js3-mode rjsx-mode web-mode))
      (mapconcat (lambda (i)
                   (format "'%s=', %s" (inc0n/yas-escape-string i) i))
                 top-kill-ring
                 ", "))
     ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
      (concat (mapconcat (lambda (i) (format "%s=%%s" i)) top-kill-ring ", ")
              "\" "
              (mapconcat (lambda (i) (format "%s" i)) top-kill-ring " ")))
     ((memq major-mode '(c-mode c++-mode))
      (concat (mapconcat (lambda (i) (format "%s=%%s" i)) top-kill-ring ", ")
              "\\n\", "
              (mapconcat (lambda (i) (format "%s" i)) top-kill-ring ", ")))
     (t ""))))

(with-eval-after-load 'yasnippet
  ;; http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
  (setq-default yas/prompt-functions
				(delete 'yas-dropdown-prompt yas/prompt-functions))
  (setq-default mode-require-final-newline nil)
  ;; Use `yas-dropdown-prompt' if possible. It requires `dropdown-list'.
  (local-require 'dropdown-list)
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-completing-prompt
							   yas-maybe-ido-prompt))
  ;; yas fallback when no expansion found
  (setq yas-fallback-behavior 'return-nil)

  ;; Use `yas-completing-prompt' when ONLY when "M-x yas-insert-snippet"
  ;; Thanks to capitaomorte for providing the trick.
  (defun inc0n/yas-insert-snippet-hack (orig-func &rest args)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let ((yas-prompt-functions '(yas-completing-prompt)))
      (apply orig-func args)))
  ;; (advice-add 'yas-insert-snippet :around #'inc0n/yas-insert-snippet-hack)

  ;; how to add custom yasnippet directory
  ;; (add-to-list 'yas-snippet-dirs inc0n/yasnippets)
  (yas-reload-all))
(global-set-key [C-tab] 'yas-expand)


(provide 'init-yasnippet)
;;; init-yasnippet ends here
