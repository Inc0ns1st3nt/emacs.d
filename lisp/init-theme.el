;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; themes also some selectrum specific theme hacks

;;; Code:

(require-package 'color-theme-modern)
(require-package 'atom-one-dark-theme)
(require-package 'doom-themes)

(defvar theme/night 'atom-one-dark)
(defvar theme/day 'doom-homage-white) ;;doom-one-light
;; (defvar themes/day '(doom-homage-white doom-one-light))

(defun load-theme-only (theme)
  "unload all other theme before loading `theme'"
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (load-theme theme t)
  ;; update colour for correct evil state mode line face colour
  (setq inc0n/default-color (cons (face-background 'mode-line)
                                  (face-foreground 'mode-line)))
  (when (boundp 'inc0n/update-modeline-face)
    (inc0n/update-modeline-face)))

(defun load-day-theme ()
  ;; selectrum is okay with this
  (load-theme-only theme/day))

(defun load-night-theme ()
  ;; selectrum quick fix
  (load-theme-only theme/night)
  ;; (let ((class t))
  ;; 	(custom-theme-set-faces
  ;; 	 'atom-one-dark
  ;; 	 `(selectrum-current-candidate
  ;; 	   ((,class (:background "#3E4451"))))
  ;; 	 `(selectrum-primary-highlight
  ;; 	   ((,class (:foreground "#C678DD" :background "#2C323C" :underline t
  ;; 							 :weight semi-bold))))
  ;; 	 `(selectrum-secondary-highlight
  ;; 	   ((,class (:inherit selectrum-primary-highlight))))))
  ;; (enable-theme 'atom-one-dark)
  )

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
                    #'load-day-theme)
    (run-with-timer (string-time-diff 16 00)
                    one-day-secs
                    #'load-night-theme)
    (if (or (string> current-time "16 00")
            (string< current-time "08 00"))
        (load-night-theme)
      (load-day-theme))))

(defun inc0n/toggle-day/night ()
  (interactive)
  (cond ((equal (car custom-enabled-themes)
		        theme/night)
	     (load-day-theme))
        (t
	     (load-night-theme))))

(defun inc0n/theme-packages (packages)
  "Filter themes from PACKAGES."
  (let* ((themes (seq-remove (lambda (p)
                               (let ((name (symbol-name (car p))))
                                 (not (string-match-p "-themes?$" name))))
                             packages))
         (sorted (sort themes
                       (lambda (a b)
                         (> (cdr a) (cdr b))))))
    (subseq sorted 0 (min (length sorted) 100))))

(defun inc0n/get-popular-theme-name ()
  "Insert names of popular theme."
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously "http://melpa.org/download_counts.json" t t 30)
    (goto-char (point-min))
    (search-forward "{")
    (backward-char)                  ; move cursor just before the "{"
    (when-let* ((pkgs (json-read))   ; now read the json at point
                (names (inc0n/theme-packages pkgs)))
      (let* ((selectrum-should-sort-p nil)
			 (cand (selectrum-read
				    "Select theme to install: "
				    (mapcar (lambda (x)
                              (propertize (propertize (symbol-name (car x))
                                                      'face 'package-name)
                                          'selectrum-candidate-display-prefix
                                          (concat (number-to-string (cdr x)) " ")))
						    names))))
		(package-install (intern cand))))))

(provide 'init-theme)
;;; init-theme.el ends here
