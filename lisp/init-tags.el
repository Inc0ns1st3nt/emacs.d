;;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Code:

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)
;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive
;; Don't warn when TAGS files are large
;; (setq large-file-warning-threshold nil)


(defvar generic-tag-major-modes
  '((lisp-mode ".lisp")
    ;; (c-mode    ".c")
    (js-mode   ".js" :ignore "node_modules")
    (js2-mode  ".js" :ignore "node_modules")
    (c++-mode  ".cpp")
    ;; (haskell-mode nil)
    )
  "The `major-mode's to try to find TAGS.")

(add-hook 'after-save-hook 'generic-tag-after-save-update)

(defvar tag-shell-format-string
  "ctags -e -f TAGS -R *%s"
  ;; "fd %s | etags -"
  "The format string of tag shell command to run to generate the tags.")

(defvar generic-tag-enable nil
  "Enable generic-tag if non-nil.")

(defun generic-tag-after-save-update ()
  "The TAGS update function."
  (when (and generic-tag-enable tags-file-name)
    (message "tags %s updated %s"
             tags-file-name
             (when-let* ((pair (assoc major-mode generic-tag-major-modes))
                         (ext (cdr pair)))
               (shell-command-to-string
                (format (concat "PWD=%s " tag-shell-format-string)
                        (file-name-directory tags-file-name)
                        ext))))))

(defun generic-prog-mode-tag-setup ()
  "My generic tag file setup for `prog-mode'."
  (when (and (assoc major-mode generic-tag-major-modes)
             buffer-file-name)
    (when generic-tag-enable
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
          (message "%s found tag: %s" major-mode tag))))
    ;; gtags setup
    (require 'gtags)
    (when-let ((root-dir (gtags-visit-project-rootdir)))
      (gtags-setup-in-project-rootdir root-dir))
    ;; (ggtags-mode 1)
    (gtags-mode 1)))

(add-hook 'prog-mode-hook #'generic-prog-mode-tag-setup)

;;; gtags init

(with-eval-after-load 'gtags
  (setq gtags-auto-update t))

(defvar fd-command-format
  "fd --extension %s --type f --color never"
  "Fd-search command format.")
(defvar generic-tag-command
  "gtags --gtagslabel pygments --compact"
  "Tag command format.")

(defun gtags-setup-in-project-rootdir (root-dir)
  "Setup gtags in ROOT-DIR."
  (pcase-let* ((tag-info (assoc major-mode generic-tag-major-modes))
               (`(,_major-mode ,ext . ,info-plist) tag-info)
               (command (format (concat "PWD=%S " fd-command-format)
                                root-dir ext)))
    ;; append ignore
    (when-let ((ignore (plist-get info-plist :ignore)))
      (setq command (concat command " --exclude " ignore)))
    (shell-command
     (concat command ;; finds all files
             (format " | %s -f -" generic-tag-command)))))

(defun gtags-visit-project-rootdir ()
  "Find and set the root directory of source tree for gtags."
  (when-let ((project (project-current)))
    (setq gtags-rootdir (expand-file-name (car (project-roots project))))
    (setenv "GTAGSROOT" gtags-rootdir)
    gtags-rootdir))

;; @see http://emacs-fu.blogspot.com.au/2008/01/navigating-through-source-code-using.html
;; (defun gtags-ext-create-or-update ()
;;   "Create or update the gnu global tag file."
;;   (interactive)
;;   (gtags-ext-produce-tags-if-needed (read-directory-name
;;                             "gtags: top of source tree:" default-directory)))

(provide 'init-tags)
;;; init-tags ends here