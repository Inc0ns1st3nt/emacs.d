;; -*- coding: utf-8; lexical-binding: t; -*-

(with-eval-after-load 'evil
  (defun pdf-view-mode-hook-setup ()
    (local-set-key "k" #'pdf-view-previous-line-or-previous-page)
    (local-set-key "j" #'pdf-view-next-line-or-next-page)
    (evil-local-set-key 'normal "k" #'pdf-view-previous-line-or-previous-page)
    (evil-local-set-key 'normal "j" #'pdf-view-next-line-or-next-page))
  (add-hook 'pdf-view-mode-hook #'pdf-view-mode-hook-setup))

(provide 'init-pdf)