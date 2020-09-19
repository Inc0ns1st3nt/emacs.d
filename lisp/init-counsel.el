;; -*- coding: utf-8; lexical-binding: t; -*-

(defcustom counsel--search-file-max-depth 3
  "the maximum depth counsel-search-file-list will reach into, default is 3")

(defun counsel-search-file-list (dir-path)
  "generate a list of files recursively starting from dir-path
as deep as counsel--search-file-max-depth"
  (cl-labels
      ((aux
        (dir-path depth)
        (cond ((< depth counsel--search-file-max-depth)
               (seq-reduce (lambda (acc f)
                             (cond ((file-directory-p f)
                                    (append (aux (format "%s/%s" dir-path f)
                                                 (1+ depth))
                                            acc))
                                   (t
                                    (cons f acc))))
                           (directory-files "." nil "[^.]")
                           nil))
              ((listp dir-path) dir-path)
              (t (list dir-path)))))
    (if (file-directory-p dir-path)
        (aux dir-path 0)
      (list dir-path))))

(defun counsel-search-file-action (x)
  "wrapper for find-file with non-string-file arg safe guard"
  (if (file-directory-p x)
      (message "counsel-search-file-action: x is path to directory not a file %s" x)
    (find-file x)))

(defun counsel-search-files ()
  (interactive)
  (if (fboundp 'ivy-read)
      (let ((collection (counsel-search-file-list ".")))
        (ivy-read "Search files:" collection
                  :action #'counsel-find-file-action
                  ;; :require-match 'confirm-after-completion
                  ;; :history 'file-name-history
                  ;; :keymap counsel-find-file-map
                  :caller 'isearch-files))
    (message "%s" "mssing ivy")))

(provide 'init-counsel)
;;; init-counsel.el ends here