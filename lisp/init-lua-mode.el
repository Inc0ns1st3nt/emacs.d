;; -*- coding: utf-8; lexical-binding: t; -*-

(defun inc0n/lua-mode-setup ()
  "Set up lua script."
  (unless (buffer-file-temp-p)
    (setq-local imenu-generic-expression
                '(("Variable" "^ *\\([a-zA-Z0-9_.]+\\) *= *{ *[^ ]*$" 1)
                  ("Function" "function +\\([^ (]+\\).*$" 1)
                  ("Module" "^ *module +\\([^ ]+\\) *$" 1)
                  ("Variable" "^ *local +\\([^ ]+\\).*$" 1)))))

;; @see http://lua-users.org/wiki/LuaStyleGuide
;; indent 2 spaces by default
(setq-default lua-indent-level 2)

(add-hook 'lua-mode-hook 'inc0n/lua-mode-setup)
(provide 'init-lua-mode)
