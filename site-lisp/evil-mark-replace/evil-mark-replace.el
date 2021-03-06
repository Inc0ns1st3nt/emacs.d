;;; evil-mark-replace.el --- replace the thing in marked area -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Chen Bin

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: http://github.com/redguardtoo/evil-mark-replace
;; Package-Version: 20200630.940
;; Package-Commit: d4fec7b10e93cca149163324cd2b2b2dcc211047
;; Keywords: convenience
;; Version: 0.0.5
;; Package-Requires: ((evil "1.14.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Install:
;;  (require 'evil-mark-replace)
;;
;; Usage:
;;  1, "M-x evilmr-replace-in-defun"
;;  2, "M-x evilmr-replace-in-buffer"
;;

;; This file is free software (GPLv3 License)

;;; Code:

(require 'evil nil t)

(defun evilmr-replace (mark-fn)
  "Mark region with MARK-FN and replace in marked area."
  (let* ((old (or (if (region-active-p)
					  (prog1 (buffer-substring-no-properties
							  (region-beginning) (region-end))
						;; quit the active region
						(set-mark nil))
					(thing-at-point 'symbol))
				  (read-string "String to be replaced:")))
         (escaped-old (replace-regexp-in-string "\\$" "\\\\$" old)))
    (save-excursion
	  (funcall mark-fn)
      ;; (evil-visual-state 1)
      (evil-ex (concat "'<,'>s/\\<\\(" escaped-old "\\)\\>/")))))

;;;###autoload
(defun evilmr-replace-in-buffer ()
  "Mark buffer and replace the thing."
  (interactive)
  (evilmr-replace #'mark-whole-buffer))

;;;###autoload
(defun evilmr-replace-in-defun ()
  "Mark defun and replace the thing."
  (interactive)
  (evilmr-replace #'mark-defun))

(provide 'evil-mark-replace)
;;; evil-mark-replace.el ends here
