;; -*- coding: utf-8; lexical-binding: t; -*-

(defvar best-gc-cons-threshold
  4000000
  "Best default gc threshold value.  Should NOT be too big!")

(defvar inc0n/debug nil "Enable debug mode.")

;; don't GC during startup to save time
(setq gc-cons-threshold most-positive-fixnum)

(setq emacs-load-start-time (current-time))

;; {{ emergency security fix
;; https://bugs.debian.org/766397
(with-eval-after-load 'enriched
  (defun enriched-decode-display-prop (start end &optional param)
    (list start end)))
;; }}
;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(defvar *linux* (memq system-type '(gnu/linux linux)))
(defvar *unix* (or *linux* (memq system-type '(usg-unix-v berkeley-unix))))
(defvar *emacs24* (>= emacs-major-version 24))
(defvar *emacs25* (>= emacs-major-version 25))
(defvar *emacs26* (>= emacs-major-version 26))
(defvar *no-memory* nil)

(defconst inc0n/emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d")

(defconst inc0n/site-lisp-dir (concat inc0n/emacs-d "site-lisp")
  "Directory of site-lisp")

(defconst inc0n/lisp-dir (concat inc0n/emacs-d "lisp")
  "Directory of lisp")

(defun inc0n/vc-merge-p ()
  "Use Emacs for git merge only?"
  (boundp 'startup-now))

(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not (inc0n/vc-merge-p)))
    (load (file-truename (format "%s/%s" inc0n/lisp-dir pkg)) t t)))

(defun local-require (pkg)
  "Require PKG in site-lisp directory."
  (unless (featurep pkg)
    (load (expand-file-name
           (cond
            ((eq pkg 'go-mode-load)
             (format "%s/go-mode/%s" inc0n/site-lisp-dir pkg))
            (t
             (format "%s/%s/%s" inc0n/site-lisp-dir pkg pkg))))
          t t)))

(when *emacs25*
  ;; (setq garbage-collection-messages t) ; for debug
  (setq best-gc-cons-threshold (* 64 1024 1024))
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
(let* ((file-name-handler-alist nil))

  ;; ;; {{
  ;; (require 'benchmark-init-modes)
  ;; (require 'benchmark-init)
  ;; (benchmark-init/activate)
  ;; ;; `benchmark-init/show-durations-tree' to show benchmark result
  ;; ;; }}

  (require-init 'init-autoload)
  ;; `package-initialize' takes 35% of startup time
  ;; need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
  (require-init 'init-modeline)
  (require-init 'init-utils)
  (require-init 'init-file-type)
  (require-init 'init-elpa)
  (require-init 'init-exec-path t) ;; Set up $PATH
  ;; Any file use flyspell should be initialized after init-spelling.el
  (require-init 'init-spelling t)
  (require-init 'init-uniquify t)
  (require-init 'init-ibuffer t)
  (require-init 'init-ivy)
  (require-init 'init-hippie-expand)
  (require-init 'init-windows)
  (require-init 'init-markdown t)
  (require-init 'init-javascript t)
  (require-init 'init-org t)
  (require-init 'init-css t)
  (require-init 'init-python t)
  (require-init 'init-lisp t)
  (require-init 'init-yasnippet t)
  (require-init 'init-cc-mode t)
  (require-init 'init-linum-mode)
  (require-init 'init-git t)
  (require-init 'init-gtags t)
  (require-init 'init-clipboard)
  (require-init 'init-ctags t)
  (require-init 'init-bbdb t)
  (require-init 'init-gnus t)
  (require-init 'init-lua-mode t)
  (require-init 'init-workgroups2 t) ; use native API in lightweight mode
  (require-init 'init-term-mode t)
  (require-init 'init-web-mode t)
  (require-init 'init-company t)
  (require-init 'init-chinese t) ;; cannot be idle-required
  (require-init 'init-counsel)
  ;; need statistics of keyfreq asap
  (require-init 'init-keyfreq t)
  (require-init 'init-httpd t)

  ;; projectile costs 7% startup time

  ;; don't play with color-theme in light weight mode
  ;; color themes are already installed in `init-elpa.el'
  (require-init 'init-theme)

  ;; misc has some crucial tools I need immediately
  (require-init 'init-essential)
  ;; handy tools though not must have
  (require-init 'init-misc t)

  (require-init 'init-emacs-w3m t)
  (require-init 'init-shackle t)
  (require-init 'init-dired t)
  (require-init 'init-writting t)
  (require-init 'init-hydra) ; hotkey is required everywhere
  ;; use evil mode (vi key binding)
  (require-init 'init-evil) ; init-evil dependent on init-clipboard

  ;; ediff configuration should be last so it can override
  ;; the key bindings in previous configuration
  (require-init 'init-ediff)

  ;; @see https://github.com/hlissner/doom-emacs/wiki/FAQ
  ;; Adding directories under "site-lisp/" to `load-path' slows
  ;; down all `require' statement. So we do this at the end of startup
  ;; NO ELPA package is dependent on "site-lisp/".
  (setq load-path (cdr load-path))
  (inc0n/add-subdirs-to-load-path (file-name-as-directory inc0n/site-lisp-dir))
  (require-init 'init-flymake t)

  (unless (inc0n/vc-merge-p)
    ;; @see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
    ;; See `custom-file' for details.
    (setq custom-file (expand-file-name (concat inc0n/emacs-d "custom-set-variables.el")))
    (when (file-exists-p custom-file)
      (load custom-file t t))

    ;; my personal setup, other major-mode specific setup need it.
    ;; It's dependent on *.el in `inc0n/site-lisp-dir'
    (require-init 'init-custom)))
(setq gc-cons-threshold best-gc-cons-threshold)

(when (require 'time-date nil t)
  (message "Emacs startup time: %s milliseconds."
           (format-time-string "%3N" (time-since emacs-load-start-time))))

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled nil)
