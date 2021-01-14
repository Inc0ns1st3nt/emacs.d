;; -*- coding: utf-8; lexical-binding: t; -*-

;; {{ use pdf-tools to view pdf
;; run "M-x pdf-tool-install" at debian and open pdf in GUI Emacs
(require-package 'pdf-tools)

(with-eval-after-load 'pdf-tools
  (when (display-graphic-p)
    (pdf-loader-install)))

(with-eval-after-load 'pdf-view
  (general-define-key
   :keymaps 'pdf-view-mode-map
   "k" #'pdf-view-previous-line-or-previous-page
   "j" #'pdf-view-next-line-or-next-page))
;; }}

(with-eval-after-load 'doc-view
  (general-define-key
   :keymaps 'doc-view-mode-map
   "k" 'previous-line
   "j" 'next-line))

(provide 'init-pdf)