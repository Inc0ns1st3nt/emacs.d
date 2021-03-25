;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; (defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

(defvar normal-gc-cons-threshold (* 48 1024 1024)) ;; 40mb
;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((init-gc-cons-threshold (* 128 1024 1024))) ;; 128mb
  (setq garbage-collection-messages t) ; for debug
  (setq gc-cons-threshold init-gc-cons-threshold)
  (setq gc-cons-percentage 0.5))
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "startup time: %s" (emacs-init-time))
            (setq gc-cons-threshold normal-gc-cons-threshold)
            (setq gc-cons-percentage 0.1)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------

(defun inc0n/vc-merge-p ()
  "Use Emacs for git merge only?"
  (boundp 'startup-now))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defvar inc0n/lisp-dir (expand-file-name "lisp" user-emacs-directory))
(defvar inc0n/site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory)
  "My site directory.")
(defun local-require (pkg)
  "Require PKG in site-lisp directory."
  (or (featurep pkg)
	  (let* ((pkg (symbol-name pkg))
             (path (expand-file-name pkg inc0n/site-lisp-dir))
             (load-path (cons path load-path)))
	    (load (if (file-exists-p path)
				  (expand-file-name pkg path)
			    (file-truename path))
			  t nil))))
(defalias 'local-package 'local-require)

(defun add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory (file-name-as-directory parent-dir)))
    (normal-top-level-add-subdirs-to-load-path)))

;; @see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.

(let ((file-name-handler-alist nil)
      (load-path (cons inc0n/lisp-dir load-path)))
  (require 'init-autoload)
  ;; `package-initialize' takes 35% of startup time
  ;; need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
  (require 'init-modeline)
  (require 'init-utils)
  (require 'init-file-type)
  (require 'init-elpa)
  (require 'init-exec-path) ;; Set up $PATH
  ;; Any file use flyspell should be initialized after init-spelling.el
  (require 'init-spelling)
  (require 'init-ibuffer)
  (require 'init-selectrum)
  ;; (require 'init-ivy)
  (require 'init-windows)
  (require 'init-org)
  (require 'init-yasnippet)
  (require 'init-linum-mode)
  (require 'init-company)

  (require 'init-markdown)
  (require 'init-javascript)
  (require 'init-css)
  (require 'init-python)
  (require 'init-lisp)
  (require 'init-cc-mode)
  (require 'init-git)
  (require 'init-lua-mode)
  (require 'init-term-mode)
  (require 'init-web-mode)
  (require 'init-haskell)
  (require 'init-latex)
  (require 'init-pdf)
  (require 'init-emacs-w3m)
  (require 'init-eww)
  (require 'init-tags)

  ;; (require 'init-bbdb)
  ;; (require 'init-gnus)
  (require 'init-chinese) ;; cannot be idle-required
  ;; (require 'init-counsel)
  (require 'init-keyfreq) ;; need statistics of keyfreq asap
  ;; (require 'init-httpd)
  ;; projectile costs 7% startup time

  (require 'init-flycheck)
  (require 'init-theme)     ;; don't play with color-theme in light weight mode
  (require 'init-essential) ;; essential has some crucial tools I need immediately
  (require 'init-misc)      ;; misc, handy tools though not must have
  (require 'init-shackle)
  (require 'init-dired)
  (require 'init-writting)
  ;; (require 'init-hydra)  ; hotkey is required everywhere
  (require 'init-clipboard)
  (require 'init-evil)
  (require 'init-transient)
  ;; ediff configuration should be last so it can override
  ;; the key bindings in previous configuration
  (require 'init-ediff)
  ;; @see https://github.com/hlissner/doom-emacs/wiki/FAQ
  ;; Adding directories under "site-lisp/" to `load-path' slows
  ;; down all `require' statement. So we do this at the end of startup
  ;; NO ELPA package is dependent on "site-lisp/".
  (add-subdirs-to-load-path inc0n/site-lisp-dir)

  (when (file-exists-p custom-file)
    (load custom-file)))

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'erase-buffer 'disabled t)
(put 'list-timers 'disabled nil)
