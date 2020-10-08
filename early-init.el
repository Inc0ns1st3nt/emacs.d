
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
;; (set-face-attribute 'default nil :font "monaco" :height 120)

;; https://emacs.stackexchange.com/questions/29289/my-change-to-the-default-font-size-reverts-at-startup
(add-to-list 'default-frame-alist
             '(font . "monaco-12"))
;; (fringe-mode '(3 . 0))