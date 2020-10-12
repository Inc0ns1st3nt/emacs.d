;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'exec-path-from-shell)
(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var)))

(provide 'init-exec-path)
