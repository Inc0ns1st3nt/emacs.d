;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Code:

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes

(setq
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
  '(:eval (if (bound-and-true-p evil-mode)
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
  '(:eval (when (bound-and-true-p evil-input-method)
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

;; simple-modeline

(defun simple-modeline-segment-popper ()
  "Simple-modeline-segment popper."
  (if (and (fboundp 'popper-popup-p)
           (popper-popup-p (current-buffer)))
      (propertize " POP" 'face 'bold)
    ""))

(defun simple-modeline-segment-nov-info ()
  "Simple-modeline-segment nov epub."
  (concat " "
          (propertize
           (or (cdr (assoc 'creator nov-metadata))
               "unknown")
           'face 'simple-modeline-unimportant)
          " "
          (cdr (assoc 'title nov-metadata))
          " "
          (propertize
           (format "%d/%d"
                   (1+ nov-documents-index)
                   (length nov-documents))
           'face 'simple-modeline-status-info)))


(defvar simple-modeline-evil-modal-alist
  '((insert . " 恶")
    (normal . " 常")
    (visual . " 选")
    (operator . " 动")
    (motion . " 水")
    (emacs  . " 本")
    (replace . " 换")))

(defun simple-modeline-segment-evil-modal ()
  "Displays a color-coded evil state modal indicator in the mode-line."
  (propertize
   ;; (if ((selected-window)))
   (if-let ((modal
             (assoc evil-state simple-modeline-evil-modal-alist)))
       (cdr modal)
     (symbol-name evil-state))
   'face `(:inherit
           ,(pcase evil-state
              ('insert 'simple-modeline-status-modified)
              ('normal 'simple-modeline-unimportant)
              ('emacs  'simple-modeline-status-error)
              (_ 'simple-modeline-important))
           :height 1.2)
   'display '(raise -0.1)))

(with-eval-after-load 'simple-modeline
  (setq simple-modeline-box-height 3)
  (simple-modeline--update-modeline))

(autoload 'simple-modeline-get-segments "simple-modeline")

(defun simple-modeline-seutp ()
  "Simple-modeline seutp."
  ;; (local-require 'simple-modeline)
  (let ((segments (simple-modeline-get-segments
                   '((winum popper evil-modal modified buffer-name position)
                     (input-method vc major-mode eol encoding)))))
    (setq-default simple-modeline-segments segments))
  (simple-modeline-mode 1))

(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'after-init-hook 'simple-modeline-seutp)

(provide 'init-modeline)
;;; init-modeline.el ends here
