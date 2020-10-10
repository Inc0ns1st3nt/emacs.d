;; -*- coding: utf-8; lexical-binding: t; -*-

;; someone mentioned that blink cursor could slow Emacs24.4
;; I couldn't care less about cursor, so turn it off explicitly
;; https://github.com/redguardtoo/emacs.d/issues/208
;; but somebody mentioned that blink cursor is needed in dark theme
;; so it should not be turned off by default
;; (blink-cursor-mode -1)

(defvar theme/night 'atom-one-dark)
(defvar theme/day 'doom-one-light)

(defun load-theme-only (theme)
  "unload all other theme before loading `theme'"
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (load-theme theme t))

(cl-labels ((time-abs (num)
                      (if (< num 0)
                          (+ num (* 24 60 60))
                        num))
            (string-time-diff
             (h m)
             (time-abs
              (+ (* 60 60 ;; convert hour to seconds
                    (- h (string-to-number (format-time-string "%H"))))
                 (* 60 ;; convert minutes to seconds
                    (- m (string-to-number (format-time-string "%M"))))))))
  (let ((one-day-secs (* 24 60 60))
        (current-time (format-time-string "%H %M")))
    (run-with-timer (string-time-diff 8 00)
                    one-day-secs
                    (lambda () (load-theme-only theme/day)))
    (run-with-timer (string-time-diff 16 00)
                    one-day-secs
                    (lambda () (load-theme-only theme/night)))
    (if (or (string> current-time "16 00")
            (string< current-time "08 00"))
        (load-theme-only theme/night)
      (load-theme-only theme/day))))

(defun inc0n/toggle-day/night ()
  (interactive)
  (if (and custom-enabled-themes
           (null (cdr custom-enabled-themes)))
      (if (equal (car custom-enabled-themes) theme/night)
          (load-theme-only theme/day)
        (load-theme-only theme/night))
    (message "theme cannot be toggled")))

;; random color theme
;; (defun inc0n/random-color-theme (themes)
;;   "Pickup random color theme from themes."
;;   (interactive (custom-available-themes))
;;   (let ((theme
;;          (nth (random (length available-themes)) themes)))
;;     (load-theme-only theme)
;;     (message "Color theme [%s] loaded." theme)))

(defun inc0n/theme-packages (packages)
  "Get themes from PACKAGES."
  (cl-loop with top-num = 110
           with i = 0
           for p in (sort packages
                          (lambda (a b)
                            (> (cdr a) (cdr b))))
           for name = (symbol-name (car p))
           when (and (string-match-p "-themes?$" name)
                     (not (memq name
                                '(color-theme smart-mode-line-powerline-theme))))
           do (setq i (1+ i))
           and collect p into themes
           when (= i top-num)
           return themes))

(defun inc0n/get-popular-theme-name ()
  "Insert names of popular theme."
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously "http://melpa.org/download_counts.json" t t 30)
    (goto-char (point-min))
    (search-forward "{")
    (backward-char)                  ; move cursor just before the "{"

    (when-let* ((pkgs (json-read))
                (names (inc0n/theme-packages pkgs)))
      ;; insert theme package names
      (kill-buffer)
      (ivy-read "Select theme to install: "
                (mapcar (lambda (x) (cons (format "%s:%s" (cdr x) (car x))
                                          (car x)))
                        names)
                :action (lambda (x) (package-install (cdr x)))))))

(provide 'init-theme)
;;; init-theme.el ends here
