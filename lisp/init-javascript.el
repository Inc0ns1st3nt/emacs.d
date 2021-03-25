;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

(require-package 'js-doc)
(require-package 'js2-mode)
(require-package 'rjsx-mode)
(require-package 'typescript-mode)

;; javascript
(add-auto-mode 'js-mode
               "\\.ja?son$"
               "\\.pac$"
               "\\.jshintrc$")

(add-auto-mode 'js2-mode "\\.js\\(\\.erb\\)?\\'")

;; JSX
(add-auto-mode 'rjsx-mode
               "\\.tsx\\'"
               "\\.jsx\\'"
               "components\\/.*\\.js\\'")

;; mock file
(add-auto-mode 'js-mode "\\.mock.js\\'")

(add-auto-mode 'typescript-mode "\\.ts$")

(add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

;; don't waste time on angular patterns, it's updated too frequently
(setq javascript-common-imenu-regex-list
      '(("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
        ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
        ;; {{ es6 beginning
        ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*([a-zA-Z0-9, ]*) *\{ *$" 1) ;; es6 fn1 () { }
        ("Function" "^[ \t]*\\([A-Za-z_$][A-Za-z0-9_$]+\\)[ \t]*=[ \t]*(?[a-zA-Z0-9, ]*)?[ \t]*=>" 1) ;; es6 fn1 = (e) =>
        ;; }}
        ))

;; js-mode imenu enhancement
;; @see http://stackoverflow.com/questions/20863386/idomenu-not-working-in-javascript-mode
(defun mo-js-imenu-make-index ()
  (save-excursion
    (imenu--generic-function javascript-common-imenu-regex-list)))

(defun common-js-node-repl-setup ()
  "Setup function for node-repl for MODE-MAP."
  (general-define-key
   :keymaps '(js-mode-map js2-mode-map)
   "C-x C-e" 'nodejs-repl-send-last-expression
   "C-c C-j" 'nodejs-repl-send-line
   "C-c C-r" 'nodejs-repl-send-region
   "C-c C-c" 'nodejs-repl-send-buffer
   "C-c C-l" 'nodejs-repl-load-file
   "C-c C-z" 'nodejs-repl-switch-to-repl))

(defun inc0n/common-js-setup ()
  (local-require 'js-comint)
  ;; (common-js-node-repl-setup)
  (subword-mode))

(define-hook-setup 'js-mode-hook :mo
  (when (and (not (buffer-file-temp-p))
			 (not (derived-mode-p 'js2-mode)))
    (inc0n/common-js-setup)
    (setq imenu-create-index-function 'mo-js-imenu-make-index)))

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

;; {{ patching imenu in js2-mode
(defvar js2-imenu-original-item-lines nil
  "List of line information of original imenu items.")

(defun js2-imenu--get-line-start-end (pos)
  (save-excursion
    (goto-char pos)
	(list (line-beginning-position) (line-end-position))))

(defun js2-imenu--get-pos (item)
  (cond ((integerp item) item)
        ((markerp item) (marker-position item))))

(defun js2-imenu--get-extra-item-pos (item)
  (cond ((integerp item) item)
        ((markerp item) (marker-position item))
        ;; plist
        ((and (listp item) (listp (cdr item)))
         (js2-imenu--get-extra-item-pos (cadr item)))
        ;; alist
        ((and (listp item) (not (listp (cdr item))))
         (js2-imenu--get-extra-item-pos (cdr item)))))

(defun js2-imenu--extract-line-info (item)
  "Recursively parse the original imenu ITEMs created by `js2-mode'.
The line numbers of items will be extracted."
  (when item
    (let ((pos (js2-imenu--get-pos item)))
	  (cond
	   ;; Marker or line number
	   (pos (push (js2-imenu--get-line-start-end pos)
			      js2-imenu-original-item-lines))

	   ;; The item is Alist, example: (hello . 163)
	   ((and (listp item)
             (not (listp (cdr item))))
		(setq pos (js2-imenu--get-pos (cdr item)))
		(when pos
          (push (js2-imenu--get-line-start-end pos)
				js2-imenu-original-item-lines)))

	   ;; The item is a Plist
	   ((and (listp item) (listp (cdr item)))
		(js2-imenu--extract-line-info (cadr item))
		(js2-imenu--extract-line-info (cdr item)))
	   ;; Error handling
	   (t (message "unexpected line info! %s" item))))))

(defun js2-imenu--item-exist (pos lines)
  "Try to detect does POS belong to some LINES."
  (cl-loop for (beg end) in lines
		   when (and (< pos end) (>= pos beg))
		   return t))

(defun js2-imenu--check-single-item (r)
  (and (if (listp (cdr r))
		   (when-let ((new-types
                       (seq-remove (lambda (item)
                                     (js2-imenu--item-exist
                                      (js2-imenu--get-extra-item-pos item)
                                      js2-imenu-original-item-lines))
                                   (cdr r))))
			 (setf (cdr r) new-types))
		 (js2-imenu--item-exist (js2-imenu--get-extra-item-pos r)
								js2-imenu-original-item-lines))
	   r))

(defun inc0n/js2-imenu--merge-imenu-items (rlt extra-rlt)
  "RLT contain imenu items created from AST.
EXTRA-RLT contains items parsed with simple regex.
Merge RLT and EXTRA-RLT, items in RLT has *higher* priority."
  ;; Clear the lines.
  (setq-local js2-imenu-original-item-lines nil)
  ;; Analyze the original imenu items created from AST,
  ;; I only care about line number.
  (mapc 'js2-imenu--extract-line-info rlt)

  ;; @see https://gist.github.com/redguardtoo/558ea0133daa72010b73#file-hello-js
  ;; EXTRA-RLT sample:
  ;; ((function ("hello" . #<marker 63>) ("bye" . #<marker 128>))
  ;;  (controller ("MyController" . #<marker 128))
  ;;  (hellworld . #<marker 161>))
  (append rlt
          (delq nil (mapcar 'js2-imenu--check-single-item extra-rlt))))

(with-eval-after-load 'js2-mode
  ;; {{ disable hot keys for elements hiding/showing
  (define-key js2-mode-map (kbd "C-c C-e") nil)
  (define-key js2-mode-map (kbd "C-c C-s") nil)
  (define-key js2-mode-map (kbd "C-c C-f") nil)
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  (define-key js2-mode-map (kbd "C-c C-o") nil)
  (define-key js2-mode-map (kbd "C-c C-w") nil)
  ;; }}
  (setq-default ;; js2-use-font-lock-faces t
   ;; js2-mode-must-byte-compile nil
   ;; {{ comment indention in modern frontend development
   js-indent-level 2
   typescript-indent-level 2
   ;; }}
   js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
   js2-idle-timer-delay 0.5   ; NOT too big for real time syntax check
   js2-skip-preprocessor-directives t
   js2-strict-inconsistent-return-warning nil ; return <=> return null
   js2-bounce-indent-p t)
  (setq-default js2-additional-externs
                '("$"
                  "$A"                ; salesforce lightning component
                  "$LightningApp"     ; salesforce
                  "AccessifyHTML5"
                  "Blob"
                  "FormData"
                  "KeyEvent"
                  "Raphael"
                  "React"
                  "URLSearchParams"
                  "__dirname"           ; Node
                  ;; "_content" ; Keysnail
                  "after"
                  "afterEach"
                  "angular"
                  "app"
                  "assert"
                  "assign"
                  "before"
                  "beforeEach"
                  "browser"
                  "by"
                  "clearInterval"
                  "clearTimeout"
                  ;; "command" ; Keysnail
                  ;; "content" ; Keysnail
                  "decodeURI"
                  "define"
                  "describe"
                  ;; "display" ; Keysnail
                  "documentRef"
                  "element"
                  "encodeURI"
                  "expect"
                  ;; "ext" ; Keysnail
                  "fetch"
                  ;; "gBrowser" ; Keysnail
                  "global"
                  ;; "goDoCommand" ; Keysnail
                  ;; "hook" ; Keysnail
                  "inject"
                  "isDev"
                  "it"
                  "jQuery"
                  "jasmine"
                  ;; "key" ; Keysnail
                  "ko"
                  "log"
                  "mockStore"
                  "module"
                  "mountWithTheme"
                  ;; "plugins" ; Keysnail
                  "process"
                  "require"
                  "setInterval"
                  "setTimeout"
                  ;; "shell" ; Keysnail
                  ;; "tileTabs" ; Firefox addon
                  ;; "util" ; Keysnail
                  "utag"))
  (defun inc0n/js2-mode-create-imenu-index-hack (orig-func &rest args)
    (let ((extra-items
		   (save-excursion
             (imenu--generic-function javascript-common-imenu-regex-list))))
      (inc0n/js2-imenu--merge-imenu-items
       (apply orig-func args)
       extra-items)))
  (advice-add 'js2-mode-create-imenu-index :around #'inc0n/js2-mode-create-imenu-index-hack)
  (define-hook-setup 'js2-mode-hook
    (unless (buffer-file-temp-p)
      (inc0n/common-js-setup)
      ;; if use node.js we need nice output
      (js2-imenu-extras-mode)
      ;; (setq mode-name "JS2")
      ;; counsel/ivy is more generic and powerful for refactoring
      ;; js2-mode has its own syntax linter

	  ;; call js-doc commands through `counsel-M-x'!

      ;; @see https://github.com/mooz/js2-mode/issues/350
      ;; (setq forward-sexp-function nil)
      )))
;; }}

;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
(with-eval-after-load 'rjsx-mode
  ;; (define-key rjsx-mode-map "<" nil)
  )

;; {{ js-beautify
(defun js-beautify (&optional indent-size)
  "Beautify selected region or whole buffer with js-beautify.
INDENT-SIZE decide the indentation level.
`sudo pip install jsbeautifier` to install js-beautify.'"
  (interactive "P")
  (if (not (executable-find "js-beautify"))
	  (message "js-beautify needs to be installed!")
    ;; detect indentation level
    (unless indent-size
      (setq indent-size
			(cond
             ((memq major-mode '(js-mode javascript-mode)) js-indent-level)
             ((memq major-mode '(web-mode)) web-mode-code-indent-offset)
             ((memq major-mode '(typescript-mode)) typescript-indent-level)
             (t 2))))
    ;; do it!
    (run-cmd-and-replace-region
     (concat "js-beautify"
             " --stdin "
             " --jslint-happy --brace-style=end-expand --keep-array-indentation "
             (format " --indent-size=%d " indent-size)))))
;; }}

;; {{ js-comint
(defun js-clear-send-buffer ()
  (interactive)
  (js-clear)
  (js-send-buffer))
;; }}

;; Latest rjsx-mode does not have indentation issue
;; @see https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs

(with-eval-after-load 'typescript
  (define-hook-setup 'typescript-mode-hoo
    (setq imenu-create-index-function 'mo-js-imenu-make-index)))

(with-eval-after-load 'nodejs-repl
  (setq nodejs-repl-prompt "nodejs> "
        nodejs-repl-command "node"
        nodejs-repl-arguments '("--experimental-modules")))
(provide 'init-javascript)
;;; init-javascript.el ends here