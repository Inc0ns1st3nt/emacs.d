;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'yasnippet)
(require-package 'yasnippet-snippets)

(defun inc0n/enable-yas-minor-mode ()
  "Enable `yas-minor-mode'."
  (unless (buffer-file-temp-p)
    (require 'yasnippet)
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

(advice-add 'yas-expand-from-trigger-key :around
            (defun inc0n/yas-expand-from-trigger-key-hack (orig-func &rest args)
              "Tab key won't trigger yasnippet expand in org heading.
Argument ORIG-FUNC the original function.
Optional argument ARGS the arguements that the original function was called with."
              (if (and (eq major-mode 'org-mode)
                       (org-at-heading-p))
                  ;; skip yas expand in org heading
                  (org-cycle)
                (apply orig-func args))))

(defun inc0n/yas-reload-all ()
  "Compile and reload snippets.  Run the command after adding new snippets."
  (interactive)
  (yas-compile-directory (inc0n/emacs-d "snippets"))
  (yas-reload-all))

(defun inc0n/yas-get-first-name-from-to-field ()
  (let ((str (util/buffer-str)))
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

(with-eval-after-load 'yasnippet
  ;; http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
  (setq-default mode-require-final-newline nil)
  ;; Use `yas-dropdown-prompt' if possible. It requires `dropdown-list'.
  ;; (local-require 'dropdown-list)
  (add-to-list 'yas/prompt-functions 'yas-dropdown-prompt)
  ;; (setq-default yas/prompt-functions (delete 'yas-dropdown-prompt yas/prompt-functions))
  ;; (setq yas-prompt-functions '(yas-dropdown-prompt
  ;;                              yas-completing-prompt
  ;;   						   yas-maybe-ido-prompt))
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
