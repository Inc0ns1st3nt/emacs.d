;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; themes also some selectrum specific theme hacks

;;; Code:

(require-package 'color-theme-modern)
(require-package 'atom-one-dark-theme)
(require-package 'doom-themes)

(defvar theme/night 'doom-moonlight) ;; atom-one-dark
(defvar theme/day 'doom-homage-white) ;;doom-one-light
;; (defvar themes/day '(doom-homage-white doom-one-light))

;; timers
(defvar theme/day-time '(8 . 00))
(defvar theme/night-time '(16 . 00))

(defvar theme/day-timer nil)
(defvar theme/night-timer nil)

(defun inc0n/faces-setup ()
  "My faces setup."
  (set-face-attribute 'fixed-pitch-serif nil
                      :height 130
                      :font "Liberation Sans")
  (set-face-attribute 'font-lock-doc-face nil :slant 'italic)
  (when (facep 'org-done)
    (set-face-attribute 'org-done nil :underline t :bold t)))

(defun load-theme-only (theme)
  "Unload all other theme before loading `THEME'."
  (dolist (i custom-enabled-themes)
    (disable-theme i))
  (load-theme theme t)
  ;; update colour for correct evil state mode line face colour
  (inc0n/faces-setup))

(defun load-day-theme ()
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

(defun theme/update-day-night-theme-timers ()
  "Setup the day night timer."
  (interactive)
  (let ((one-day-secs (* 24 60 60))
        (current-time (let ((decoded-time (decode-time)))
                        (cons (caddr decoded-time) (cadr decoded-time)))))
    (cl-labels ((time-abs
                 (time)
                 (if (< time 0)
                     (+ time one-day-secs)
                   time))
                (time-diff
                 (time1 time2)
                 (+ (* 60 (- (car time1) (car time2)))
                    (* 3600 (- (cdr time1) (cdr time2))))))
      (when theme/day-timer (cancel-timer theme/day-timer))
      (when theme/night-timer (cancel-timer theme/night-timer))
      (setq theme/day-timer
            (run-with-timer (time-abs (time-diff theme/day-time current-time))
                            one-day-secs
                            #'load-day-theme)
            theme/night-timer
            (run-with-timer (time-abs (time-diff theme/night-time current-time))
                            one-day-secs
                            #'load-night-theme))
      (if (or (> (time-diff theme/night-time current-time) 0) ; past night
              (< (time-diff theme/day-time current-time) 0)) ; before day
          (load-night-theme)
        (load-day-theme)))))

(theme/update-day-night-theme-timers)

(defun inc0n/toggle-day/night ()
  "Toggle between day and night themes."
  (interactive)
  (if (equal (car custom-enabled-themes) theme/night)
	  (load-day-theme)
    (load-night-theme)))

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
