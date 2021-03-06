;; -*- coding: utf-8; lexical-binding: t; -*-

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes

(setq-default
 mode-line-format
 (list
  '(:eval ;; the buffer name, the file name
	(concat (propertize "%b " 'face nil 'help-echo buffer-file-name)
			(cond ((and buffer-file-name
                        (file-exists-p buffer-file-name)
                        (not (file-directory-p buffer-file-name))
                        (buffer-modified-p))
                   "◆ ")
				  (buffer-read-only
                   "RO "))))

  ;; line and column
  ;; '%02' to set to 2 chars at least; prevents flickering
  "(%02l,%01c) "
  ;; insert vs overwrite mode
  '(:eval (if (and (boundp 'evil-mode) evil-mode)
    		  (propertize "§ " 'help-echo "evil indicator"
                          'face
                          (list :foreground
                                (cond ((evil-insert-state-p) "red1")
                                      ((evil-normal-state-p) "deep sky blue")
                                      ((or (evil-motion-state-p)
                                           (evil-visual-state-p)) "SeaGreen2")
                                      (t "magenta4"))))))
  ;; @see https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Echo.html
  "["
  ;; the current major mode for the buffer.
  '(:eval (propertize "%m" 'help-echo buffer-file-coding-system))
  " "
  ;; input-method
  '(:eval (when (and (boundp 'evil-input-method)
					 evil-input-method)
			(concat
			 (propertize evil-input-method
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

;; doom-modeline
;; (require 'doom-modeline)

(defun mode-line-setup ()
  (local-require 'simple-modeline)
  (simple-modeline-set-segments
   '((winum popper evil-modal modified buffer-name position)
     (input-method vc major-mode misc-info process eol encoding)))
  (simple-modeline-mode 1))

(defun simple-modeline-segment-popper ()
  "Simple-modeline-segment popper."
  (if (popper-popup-p (current-buffer))
      (propertize " POP" 'face 'bold)
    ""))

(with-eval-after-load 'simple-modeline
  (setq simple-modeline-box-height 3)
  (simple-modeline--update-modeline))

(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'after-init-hook 'mode-line-setup)

(provide 'init-modeline)
;;; init-modeline.el ends here
