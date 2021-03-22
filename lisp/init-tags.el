;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
;; (setq large-file-warning-threshold nil)


(defvar generic-tag-major-modes nil
  "The `major-mode's to try to find TAGS.")

(setq generic-tag-major-modes '((lisp-mode . ".lisp")
                                ;; (c-mode . ".c")
                                (js-mode . ".js")
                                (js2-mode . ".js")
                                (c++-mode . ".cpp")
                                (haskell-mode . nil)))

(add-hook 'before-save-hook 'generic-tag-before-save-update)

(defvar tag-shell-format-string
  "ctags -e -f TAGS -R *%s"
  ;; "fd %s | etags -"
  "The format string of tag shell command to run to generate the tags.")

(defun generic-tag-before-save-update ()
  "The TAGS update function."
  (if tags-file-name
      (message "tags %s updated %s"
               tags-file-name
               (when-let* ((pair (assoc major-mode generic-tag-major-modes))
                           (ext (cdr pair)))
                 (shell-command-to-string
                  (format (concat "PWD=%s " tag-shell-format-string)
                          (file-name-directory tags-file-name)
                          ext))))
    (message "tags not updated for %s" major-mode)))

(defun generic-prog-mode-tag-setup ()
  "My generic tag file setup for `prog-mode'."
  (when (and (assoc major-mode generic-tag-major-modes)
             buffer-file-name)
    (util/ensure 'etags)
    (when (and (null tags-file-name)
               (null tags-completion-table)
               ;; (not (y-or-n-p "Keep current tags? "))
               )
      (when-let ((tag (or (locate-dominating-file default-directory "TAGS")
                          (when (y-or-n-p "Find (create) TAGS? ")
                            (read-file-name "TAGS: ")))))
        (util/make-file tag)
        (setq-local tags-completion-table nil)
        ;; Set the local value of tags-file-name.
        (setq-local tags-file-name tag)
        (message "%s found tag: %s" major-mode tag)))))

(add-hook 'prog-mode-hook #'generic-prog-mode-tag-setup)

(provide 'init-tags)
;;; init-tags ends here