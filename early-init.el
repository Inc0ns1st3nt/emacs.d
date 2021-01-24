;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)

;; Show a marker in the left fringe for lines not in the buffer
(setq indicate-empty-lines t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(menu-bar-mode -1)

;;;;
;; Font
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
;; "Bitstream Vera Sans Mono"
;; "TerminessTTFNerdFont"
;; "DejaVu Sans Mono"
;; "Source Code Pro"
;; "Fira Code", "monaco"
;; (set-face-attribute 'default nil :font "Fira Code" :height 120)

;; https://emacs.stackexchange.com/questions/29289/my-change-to-the-default-font-size-reverts-at-startup
(add-to-list 'default-frame-alist
             '(font . "Fira Code-12"))

;; transparency setup
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(defun show-scratch-buffer-message ()
  (if-let ((fortune-prog (or (executable-find "fortune-zh")
                             (executable-find "fortune"))))
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; "                    ; comment each line
        (replace-regexp-in-string
         "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9]m *\\)" "" ; remove trailing linebreak
         (shell-command-to-string fortune-prog))))
    (concat ";; Happy hacking "
            (or user-login-name "")
            " - Emacs loves you!\n\n")))

(setq initial-scratch-message nil
      ;; (show-scratch-buffer-message)
      ;; (concat ";; Please wait "
      ;;         (or user-login-name "")
      ;;         " org agenda is being prepared for you")
	  )

(setq initial-buffer-choice
	  (lambda ()
		(let ((org-agenda-window-setup 'only-window))
		  (org-agenda nil "n"))
		(current-buffer)))
;; (fringe-mode '(3 . 0))

(provide 'early-init)
;;; early-init.el ends here