;; -*- coding: utf-8; lexical-binding: t; -*-

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes

(setq-default
 mode-line-format
 (list
  ;; the buffer name; the file name as a tool tip
  '(:eval
	(concat (propertize "%b " 'face nil 'help-echo (buffer-file-name))
			(cond ((buffer-modified-p) "◆ ")
				  (buffer-read-only "RO "))))

  ;; line and column
  "(" ;; '%02' to set to 2 chars at least; prevents flickering
  ;; "%02l"
  ","
  "%01c"
  ") "

  ;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Echo.html
  "["
  ;; the current major mode for the buffer.
  '(:eval (propertize "%m" 'face nil 'help-echo buffer-file-coding-system))
  " "
  ;; insert vs overwrite mode, input-method in a tooltip
  '(:eval (when (and (boundp 'evil-input-method)
					 evil-input-method)
			(concat
			 (propertize evil-input-method
						 'face nil
						 'help-echo "Input method for Buffer is enabled")
			 " ")))
  ;; buffer file encoding
  '(:eval (let ((sys (coding-system-plist buffer-file-coding-system)))
            (if (memq (plist-get sys :category)
                      '(coding-category-undecided coding-category-utf-8))
                "UTF-8"
              (upcase (symbol-name (plist-get sys :name))))))
  "] "

  ;; global-mode-string, org-timer-set-timer in org-mode need this
  (propertize "%M" 'face nil)

  ;; " --"
  ;; Don't show `minor-mode'
  ;; minor-mode-alist  ;; list of minor modes
  ;; "%-" ;; fill with '-'
  ))

(provide 'init-modeline)
