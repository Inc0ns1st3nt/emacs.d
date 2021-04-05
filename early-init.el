;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

;;;;
;; Font
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
;; "Bitstream Vera Sans Mono"
;; "TerminessTTFNerdFont"
;; "DejaVu Sans Mono"
;; "Source Code Pro"
;; "Fira Code", "monaco"
;; (set-face-attribute 'default nil :font "Fira Code" :height 100)
;; (set-face-attribute 'default nil :height 130 :font "AR PL New Kai")
;; (set-face-attribute 'default nil :height 135)

;; https://emacs.stackexchange.com/questions/29289/my-change-to-the-default-font-size-reverts-at-startup
;; (add-to-list 'default-frame-alist ')
(setq default-frame-alist
      `((tool-bar-lines . 0)
        (menu-bar-lines . ,(if (display-graphic-p) 1 0))
        (font . "Liberation Mono-14")))

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

;; transparency setup
;; (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

(defun show-scratch-buffer-message ()
  (let ((fortune-prog (executable-find "fortune")))
    (if fortune-prog
        (format
         ";; %s\n\n"
         (replace-regexp-in-string
          "\n" "\n;; "                  ; comment each line
          (replace-regexp-in-string
           "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9]m *\\)" "" ; remove trailing linebreak
           (shell-command-to-string
            (concat fortune-prog " ~/arch/fortunes/data")))))
      ;; (concat ";; Happy hacking "
      ;;         (or user-login-name "")
      ;;         " - Emacs loves you!\n\n")
      (concat ";; Please wait "
              (or user-login-name "")
              " org agenda is being prepared for you"))))

(setq initial-scratch-message
      (show-scratch-buffer-message))

(setq initial-buffer-choice
      (lambda ()
        (let ((org-agenda-window-setup
			   'Only-window))
		  (org-agenda nil "n")
          (current-buffer))))
;; (run-with-idle-timer 0.1 nil
;;  (lambda ()))
;; (current-buffer)
;; (fringe-mode '(3 . 0))

(provide 'early-init)
;;; early-init.el ends here