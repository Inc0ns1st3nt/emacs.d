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
      (let ((collection
             (counsel-search-file-list ".")))
        (ivy-read "Search files:" collection
                  :action #'counsel-find-file-action
                  ;; :require-match 'confirm-after-completion
                  ;; :history 'file-name-history
                  ;; :keymap counsel-find-file-map
                  :caller 'isearch-files))
    (message "%s" "mssing ivy")))


(defun util/string-multi-line-p (str)
  (cl-loop for c across str
           for i from 0
           when (char-equal ?\n c)
           return (> (length str) i)))

(defun incon/evilnc-imenu-create-index-function ()
  (cl-labels ((search-all-comments
               (cands)
               (if-let ((beg (search-forward comment-start (point-max) t)))
                   (let* ((beg (1+ beg))
                          (linenum (line-number-at-pos beg))
                          (end (if (string= comment-end "")
                                   (line-end-position)
                                 (search-forward comment-end (point-max) t)))
                          (str (if (and end (> end beg))
                                   (let ((str (string-trim-left
                                               (buffer-substring-no-properties beg end))))
                                     (if (> (length str) 77)
                                         (concat (cl-subseq str 0 (80))
                                                 "...")
                                       (cl-subseq str 0 (length str))))
                                 nil)))
                     (if (and (not (string-match-p "^[ \t\n\r]*$" str))
                              (> (length str) evilnc-min-comment-length-for-imenu))
                         (let ((m (make-marker)))
                           (set-marker m beg)
                           (goto-char (min (1+ end) (point-max)))
                           (search-all-comments
                            (acons (evilnc-frame-wide-string
                                    (format "%d:%s" linenum str))
                                   m
                                   cands)))
                       (goto-char (min (1+ end) (point-max)))
                       (search-all-comments
                        cands)))
                 (nreverse cands))))
    (search-all-comments '())))

(defun counsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (when (and (fboundp 'evilnc-imenu-create-index-function)
             (fboundp 'counsel-imenu))
    (let ((imenu-create-index-function #'evilnc-imenu-create-index-function))
      (counsel-imenu))))


(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-m") 'counsel-M-x)

(provide 'init-counsel)
;;; init-counsel.el ends here