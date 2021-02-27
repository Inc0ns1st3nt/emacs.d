;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'typescript-mode)

(setq-default js2-use-font-lock-faces t
              js2-mode-must-byte-compile nil
              ;; {{ comment indention in modern frontend development
              javascript-indent-level 2
              js-indent-level 2
              css-indent-offset 2
              typescript-indent-level 2
              ;; }}
              js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
              js2-idle-timer-delay 0.5 ; NOT too big for real time syntax check
              js2-auto-indent-p nil
              js2-indent-on-enter-key nil ; annoying instead useful
              js2-skip-preprocessor-directives t
              js2-strict-inconsistent-return-warning nil ; return <=> return null
              js2-enter-indents-newline nil
              js2-bounce-indent-p t)

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

(defun inc0n/common-js-setup ()
  (local-require 'js-comint)
  (subword-mode))

(defun mo-js-mode-hook ()
  (when (and (not (buffer-file-temp-p))
			 (not (derived-mode-p 'js2-mode)))
    (inc0n/common-js-setup)
    (setq imenu-create-index-function 'mo-js-imenu-make-index)))
(add-hook 'js-mode-hook 'mo-js-mode-hook)

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
  (cond
   ((integerp item) item)
   ((markerp item) (marker-position item))))

(defun js2-imenu--get-extra-item-pos (item)
  (cond
   ((integerp item) item)
   ((markerp item) (marker-position item))

   ;; plist
   ((and (listp item) (listp (cdr item)))
    (js2-imenu--get-extra-item-pos (cadr item)))
   ;; alist
   ((and (listp item) (not (listp (cdr item))))
    (js2-imenu--get-extra-item-pos (cdr item)))))

(defun js2-imenu--extract-line-info (item)
  "Recursively parse the original imenu items created by js2-mode.
The line numbers of items will be extracted."
  (when item
    (let ((val (js2-imenu--get-pos item)))
	  (cond
	   ;; Marker or line number
	   (val
		(push (js2-imenu--get-line-start-end val)
			  js2-imenu-original-item-lines))

	   ;; The item is Alist, example: (hello . 163)
	   ((and (listp item) (not (listp (cdr item))))
		(setq val (js2-imenu--get-pos (cdr item)))
		(if val (push (js2-imenu--get-line-start-end val)
					  js2-imenu-original-item-lines)))

	   ;; The item is a Plist
	   ((and (listp item) (listp (cdr item)))
		(js2-imenu--extract-line-info (cadr item))
		(js2-imenu--extract-line-info (cdr item)))

	   ;;Error handling
	   (t (message "Impossible to here! item=%s" item))))))

(defun js2-imenu--item-exist (pos lines)
  "Try to detect does POS belong to some LINE"
  (cl-loop for line in lines
		   when (and (< pos (cadr line)) (>= pos (car line)))
		   return t))

(defun js2-imenu--is-item-already-created (item)
  (unless (js2-imenu--item-exist
           (js2-imenu--get-extra-item-pos item)
           js2-imenu-original-item-lines)
    item))

(defun js2-imenu--check-single-item (r)
  (and (if (and (listp (cdr r)))
		   (when-let
			   ((new-types
				 (delq nil (mapcar 'js2-imenu--is-item-already-created (cdr r)))))
			 (setcdr r (delq nil new-types)))
		 (js2-imenu--item-exist (js2-imenu--get-extra-item-pos r)
								js2-imenu-original-item-lines))
	   r))

(defun inc0n/validate-json-or-js-expression (&optional not-json-p)
  "Validate buffer or select region as JSON.
If NOT-JSON-P is not nil, validate as Javascript expression instead of JSON."
  (interactive "P")
  (let ((json-exp (if (region-active-p) (util/selected-str)
                    (util/buffer-str)))
        (jsbuf-offet (if not-json-p 0 (length "var a=")))
        errs
        first-err
        (first-err-pos (if (region-active-p) (region-beginning) 0)))
    (unless not-json-p
      (setq json-exp (format "var a=%s;"  json-exp)))
    (with-temp-buffer
      (insert json-exp)
      (util/ensure 'js2-mode)
      (js2-parse)
      (setq errs (js2-errors))
      (if (not errs)
          (message "NO error found. Good job!")
        ;; yes, first error in buffer is the last element in errs
        (setq first-err (car (last errs)))
        (setq first-err-pos (+ first-err-pos (- (cadr first-err) jsbuf-offet)))
        (message "%d error(s), first at buffer position %d: %s"
                 (length errs)
                 first-err-pos
                 (js2-get-msg (caar first-err)))))
    (if first-err (goto-char first-err-pos))))

(defun inc0n/print-json-path (&optional hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
  (interactive "P")
  (if (memq major-mode '(js2-mode))
      (js2-print-json-path hardcoded-array-index)
	(let ((cur-pos (point))
		  (str (util/buffer-str)))
	  (when (string= "json" (file-name-extension buffer-file-name))
        (setq str (format "var a=%s;" str))
        (setq cur-pos (+ cur-pos (length "var a="))))
	  (util/ensure 'js2-mode)
	  (with-temp-buffer
        (insert str)
        (js2-init-scanner)
        (js2-do-parse)
        (goto-char cur-pos)
        (js2-print-json-path)))))

(defun js2-imenu--remove-duplicate-items (extra-rlt)
  (delq nil (mapcar 'js2-imenu--check-single-item extra-rlt)))

(defun inc0n/js2-imenu--merge-imenu-items (rlt extra-rlt)
  "RLT contains imenu items created from AST.
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
  (append rlt (js2-imenu--remove-duplicate-items extra-rlt)))

(with-eval-after-load 'js2-mode
  ;; {{ I hate the hotkeys to hide things
  ;; (define-key js2-mode-map (kbd "C-c C-e") nil)
  ;; (define-key js2-mode-map (kbd "C-c C-s") nil)
  ;; (define-key js2-mode-map (kbd "C-c C-f") nil)
  ;; (define-key js2-mode-map (kbd "C-c C-t") nil)
  ;; (define-key js2-mode-map (kbd "C-c C-o") nil)
  ;; (define-key js2-mode-map (kbd "C-c C-w") nil)
  ;; }}
  (defun inc0n/js2-mode-create-imenu-index-hack (orig-func &rest args)
    (let ((extra-items
		   (save-excursion
             (imenu--generic-function javascript-common-imenu-regex-list))))
      (inc0n/js2-imenu--merge-imenu-items (apply orig-func args) extra-items)))
  (advice-add 'js2-mode-create-imenu-index :around #'inc0n/js2-mode-create-imenu-index-hack))
;; }}

(defun inc0n/js2-mode-setup()
  (unless (buffer-file-temp-p)
    (inc0n/common-js-setup)
    ;; if use node.js we need nice output
    (js2-imenu-extras-mode)
    (setq mode-name "JS2")
    ;; counsel/ivy is more generic and powerful for refactoring
    ;; js2-mode has its own syntax linter

	;; call js-doc commands through `counsel-M-x'!

    ;; @see https://github.com/mooz/js2-mode/issues/350
    (setq forward-sexp-function nil)))

(add-hook 'js2-mode-hook 'inc0n/js2-mode-setup)

;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil))

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
             ((memq major-mode '(js-mode javascript-mode))
              js-indent-level)
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

(defun typescript-mode-hook-setup ()
  (setq imenu-create-index-function 'mo-js-imenu-make-index))
(add-hook 'typescript-mode-hook 'typescript-mode-hook-setup)

(setq-default js2-additional-externs
              '("$"
                "$A" ; salesforce lightning component
                "$LightningApp" ; salesforce
                "AccessifyHTML5"
                "Blob"
                "FormData"
                "KeyEvent"
                "Raphael"
                "React"
                "URLSearchParams"
                "__dirname" ; Node
                "_content" ; Keysnail
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
                "command" ; Keysnail
                "content" ; Keysnail
                "decodeURI"
                "define"
                "describe"
                "display" ; Keysnail
                "documentRef"
                "element"
                "encodeURI"
                "expect"
                "ext" ; Keysnail
                "fetch"
                "gBrowser" ; Keysnail
                "global"
                "goDoCommand" ; Keysnail
                "hook" ; Keysnail
                "inject"
                "isDev"
                "it"
                "jQuery"
                "jasmine"
                "key" ; Keysnail
                "ko"
                "log"
                "mockStore"
                "module"
                "mountWithTheme"
                "plugins" ; Keysnail
                "process"
                "require"
                "setInterval"
                "setTimeout"
                "shell" ; Keysnail
                "tileTabs" ; Firefox addon
                "util" ; Keysnail
                "utag"))

(provide 'init-javascript)
