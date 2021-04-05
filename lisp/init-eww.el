;;; -*- coding: utf-8; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

; {{ Search using external browser
(setq browse-url-generic-program
      (or (executable-find "firefox")
          (executable-find "google-chrome")))
;; (setq browse-url-browser-function #'browse-url-generic)
(setq browse-url-browser-function 'eww)
;; }}

;; ewww download rss stream

(with-eval-after-load 'evil
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-set-initial-state 'eww-buffer-mode 'emacs))


(define-hook-setup 'eww-after-render-hook
  "My eww hook setup."
  (let* ((title (plist-get eww-data :title))
         (title (if (string-empty-p title)
                    "untitled"
                  (if (> (length title) 45)
                      (concat (substring title 0 45) "...")
                    title))))
    (rename-buffer (format "*eww: %s*" title)
                   t)))

(define-hook-setup 'eww-mode-hook
  (setq-local truncate-lines t)
  (setq-local shr-width 90)
  (setq-local fill-column 90
              visual-fill-column-center-text nil)
  (visual-line-mode 1)
  (visual-fill-column-mode 1))

(defun eww-edit-url-and-goto ()
  (interactive)
  (eww (read-from-minibuffer "URL: " (plist-get eww-data :url))))

(defvar eww-search-engine-alist
  '(("stackoverflow" "https://www.stackoverflow.com/search?q=%s" utf-8)
    ("duckduckgo"    "https://lite.duckduckgo.com/lite?q=%s")
    ("financial"     "https://financial-dictionary.thefreedictionary.com/%s" utf-8)
    ("dictionary"    "https://dictionary.reference.com/search?q=%s" utf-8))
  "My eww search engine alist similar to w3m.")

(defun inc0n/eww-search (&optional arg)
  "My eww that can search with search engines.
Non-nil ARG will allow search engine selection."
  (interactive "P")
  (require 'eww)
  (let ((engine (if arg
                    (completing-read "Engine: " (mapcar 'car eww-search-engine-alist))
                  eww-search-default-engine)))
    (if-let* ((pair (assoc engine eww-search-engine-alist))
              (url (cadr pair)))
        (eww
         (format url
                 (read-from-minibuffer
                  (format "Search with %s: " engine))))
      (message "cannot find engine %s" engine))))

(defun inc0n/eww-readable ()
  (interactive)
  (eww-readable)
  (setq-local visual-fill-column-center-text t))

(with-eval-after-load 'shr
  (setq shr-max-image-proportion 0.3))

(with-eval-after-load 'eww
  (general-define-key
   :keymaps 'eww-mode-map
   ;; "C-c b" 'w3m-external-view-this-url
   ;; "f" 'inc0n/eww-search
   "j" 'next-line
   "h" 'eww-list-histories
   "R" 'inc0n/eww-readable
   "H" 'eww-back-url
   "L" 'eww-forward-url
   "l" 'inc0n/eww-search
   "k" 'previous-line
   "J" 'scroll-up-command
   "K" 'scroll-down-command
   "U" 'eww)
  (defvar eww-home-page "https://lite.duckduckgo.com/")
  (defvar eww-search-default-engine "duckduckgo"))

(provide 'init-eww)