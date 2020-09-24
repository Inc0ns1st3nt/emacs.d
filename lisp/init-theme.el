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
              (+ (* 60 60
                    (- h (string-to-number (format-time-string "%H"))))
                 (* 60
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

;;

(defvar inc0n/favourite-color-themes nil
  "Color themes to use by `random-color-theme'.")

;; random color theme
(defun inc0n/pickup-random-color-theme (themes)
  "Pickup random color theme from themes."
  (interactive (or inc0n/favourite-color-themes
                   (custom-available-themes)))
  (let*
      ((theme (nth (random (length available-themes)) themes))
       (theme (symbol-name theme)))
    (load-theme-only theme)
    (message "Color theme [%s] loaded." theme)))

(defun random-healthy-color-theme (&optional join-dark-side)
  "Random healthy color theme.  If JOIN-DARK-SIDE is t, use dark theme only."
  (interactive "P")
  (let* (themes
         (hour (string-to-number (format-time-string "%H" (current-time))))
         (prefer-light-p (and (not join-dark-side) (>= hour 9) (<= hour 19)) ))
    (dolist (theme (custom-available-themes))
      (let* ((light-theme-p
              (or (and (string-match-p "light\\|bright\\|white" (symbol-name theme))
                       (not (string-match-p "^base16-\\|^airline-\\|^doom=\\|^alect-" (symbol-name theme)))
                       (not (member theme '(twilight
                                            avk-darkblue-white
                                            sanityinc-tomorrow-bright))))
                  (member theme '(adwaita
                                  aliceblue
                                  bharadwaj
                                  black-on-gray
                                  blippblopp
                                  emacs-21
                                  emacs-nw
                                  fischmeister
                                  github
                                  greiner
                                  gtk-ide
                                  high-contrast
                                  jb-simple
                                  kaolin-breeze
                                  katester
                                  leuven
                                  marquardt
                                  mccarthy
                                  montz
                                  occidental
                                  oldlace
                                  scintilla
                                  sitaramv-nt
                                  snowish
                                  soft-stone
                                  standard
                                  tango
                                  tango-plus
                                  tangotango
                                  tao-yang
                                  vim-colors
                                  whateveryouwant
                                  wheat
                                  xemacs
                                  xp)))))
        (when (if prefer-light-p light-theme-p (not light-theme-p))
          (push theme themes))))
    (inc0n/pickup-random-color-theme themes)))

(defun inc0n/theme-packages (packages)
  "Get themes from PACKAGES."
  (cl-loop with top-num = 110
           with i = 0
           for p in (sort packages
                          (lambda (a b)
                            (> (cdr a) (cdr b))))
           for name = (symbol-name (car p))
           when (and (string-match-p "-themes?$" name)
                     (< i top-num)
                     (not (member name
                                  '(color-theme smart-mode-line-powerline-theme))))
           do (setq i (1+ i))
           and collect p))

(defun inc0n/get-popular-theme-name ()
  "Insert names of popular theme."
  (interactive)
  (when-let
      ((buf
        (url-retrieve-synchronously "http://melpa.org/download_counts.json" t t 30)))
    (pop-to-buffer buf)
    (goto-char (point-min))
    (search-forward "{")
    (backward-char)                  ; move cursor just before the "{"

    (when-let* ((pkgs (json-read))
                (names (inc0n/theme-packages pkgs)))
      ;; insert theme package names
      (erase-buffer)
      (insert "{\n")
      (insert (mapconcat (lambda (x) (format "  %s: %s" (car x) (cdr x)))
                         names "\n"))
      (insert "\n}"))))

(provide 'init-theme)
;;; init-theme.el ends here
