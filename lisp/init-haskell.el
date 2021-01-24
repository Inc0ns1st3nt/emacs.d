;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'haskell-mode)

(custom-set-variables
 ;; '(haskell-process-type 'ghci)
 ;; '(haskell-interactive-types-for-show-ambiguous nil)
 '(haskell-interactive-mode-hide-multi-line-errors nil)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-svg-render-images t))

;; (setq haskell-interactive-mode-eval-mode 'haskell-mode)

(defun haskell-interactive-toggle-print-mode ()
  (interactive)
  (setq haskell-interactive-mode-eval-mode
        (intern
         (completing-read "Eval result mode: "
                          '("fundamental-mode"
                            "haskell-mode"
                            "espresso-mode"
                            "ghc-core-mode"
                            "org-mode")))))

(add-hook
 'haskell-mode-hook
 (lambda ()
   ;; Haskell smarter completion
   ;; @see http://haskell.github.io/haskell-mode/manual/latest/Completion-support.html#Completion-support
   (setq-local company-backends
			   (append '((company-capf company-dabbrev-code))
					   company-backends))))

;; Haskell Unicode support
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
;; (add-hook 'haskell-interactive-mode-hook 'turn-on-haskell-unicode-input-method)
;; Haskell module auto insert template
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)
;; Haskell interactive mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Haskell declaration scanning like 'beginning-of-defun' 'end-of-defun'
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

(general-create-definer inc0n/haskell-space-leader-def
  :prefix "SPC h"
  :states '(normal visual)
  :keymaps 'haskell-mode-map)

(with-eval-after-load 'haskell-mode
  ;; (inc0n/haskell-space-leader-def
  ;;  "t" 'haskell-process-do-type
  ;;  "i" 'haskell-process-do-info
  ;;  "l" 'haskell-process-load-file)
  (general-define-key
   :keymaps 'haskell-mode-map
   ;; @see http://haskell.github.io/haskell-mode/manual/latest/Compilation.html#Compilation
   "C-c C-c" 'haskell-compile

   "M-." 'haskell-mode-jump-to-def)

  (general-define-key
   :keymaps 'haskell-mode-map
   :states '(normal visual)
   :prefix "SPC h"
   "t" 'haskell-process-do-type
   "i" 'haskell-process-do-info
   "l" 'haskell-process-load-or-reload))

;; interactive Haskell
;; start the repl 'haskell-interactive-bring
;; load the file 'haskell-process-load-or-reload

;; @see http://haskell.github.io/haskell-mode/manual/latest/REPL.html#REPL
;; 'run-haskell
;; 'switch-to-haskell

;; @see http://haskell.github.io/haskell-mode/manual/latest/Aligning-code.html#Aligning-code


;;; clean
;; (require-package 'clean-mode)

(provide 'init-haskell)