;;; init-flycheck.el --- Configure Flycheck -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require-package 'elpy)

(use-package python
  :mode "\\.py\\'"
  :interpreter "python"
  :config
  ;; run command `pip install jedi flake8 importmagic` in shell,
  ;; or just check https://github.com/jorgenschaefer/elpy
  (elpy-enable)
  ;; (setq elpy-shell-command-prefix-key "C-c C-f")
  ;; If you don't like any hint or error report from elpy,
  ;; set `elpy-disable-backend-error-display' to t.
  (setq elpy-disable-backend-error-display nil)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  ;; http://emacs.stackexchange.com/questions/3322/python-auto-indent-problem/3338#3338
  ;; emacs 24.4+
  (setq electric-indent-chars (delq ?: electric-indent-chars)))

(provide 'init-python)
