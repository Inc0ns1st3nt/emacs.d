;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
;; (setq package-user-dir
;;       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
;;                         user-emacs-directory))
;; (package-initialize)
(add-to-list-multi
 'package-archives
 '(("melpa" . "https://melpa.org/packages/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("gnu" . "http://elpa.gnu.org/packages/"))
 t)

;; '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
;;   ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
;;   ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;;   ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/"))

(defun inc0n/emacs-d (path)
  "get the expanded path under .emacs.d"
  (expand-file-name path user-emacs-directory))

(when (and (null noninteractive)         ; no popup in batch mode
           (not (file-exists-p (file-truename package-user-dir)))
           (yes-or-no-p "Switch to faster package repositories in China temporarily?
You still need modify `package-archives' in \"init-elpa.el\" to PERMANENTLY use this ELPA mirror."))
  (setq package-archives
        '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
          ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/"))))

;; my local repository is always needed.
;; (push (cons "localelpa" (inc0n/emacs-d "localelpa/"))
;;       package-archives)

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (or (package-installed-p package min-version)
      (if (or (assoc package package-archive-contents)
              no-refresh)
          (progn (package-install package)
                 t)
        (package-refresh-contents)
        (require-package package min-version t))))

;; List of visible packages from melpa-unstable (http://melpa.org).
;; Please add the package name into `melpa-include-packages'
;; if it's not visible after  `list-packages'.
(defvar inc0n/melpa-include-packages
  '(ace-window ; lastest stable is released on year 2014
	ace-pinyin
    auto-package-update
    nov
    bbdb
    native-complete
    company-native-complete
    js2-mode ; need new features
    git-timemachine ; stable version is broken when git rename file
    undo-fu
    command-log-mode
    ;; lsp-mode ; stable version has performance issue, but unstable version sends too many warnings
    edit-server ; use Emacs to edit textarea in browser, need browser addon
    vimrc-mode
    rjsx-mode ; fixed the indent issue in jsx
    package-lint ; for melpa pull request only
    auto-yasnippet
    typescript-mode ; the stable version lacks important feature (highlight function names)
    evil-exchange
    evil-find-char-pinyin

	auctex
	which-key

    iedit
    undo-tree
    js-doc
    wgrep

    groovy-mode
    company ; I won't wait another 2 years for stable
    simple-httpd
    dsvn
    ;; findr
    mwe-log-commands
    db
    creole
    web
    buffer-move
    regex-tool
    legalese
    htmlize
    pyim-basedict
    pyim-wbdict
    scratch
    session
    inflections
    lua-mode
    pomodoro
    packed
    keyfreq
    gitconfig-mode
    textile-mode
    w3m
    zoutline
    company-c-headers
    company-statistics
    ;;
    face-up ;; for racket mode
    racket-mode)
  "Packages to install from melpa-unstable.")

;; (defvar inc0n/melpa-stable-banned-packages nil
;;   "Banned packages from melpa-stable.")

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(require-package 'async)

(require-package 'amx)
(require-package 'avy)
(require-package 'popup) ; some old package need it
(require-package 'fringe-helper)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(require-package 'request)
;; (require-package 'jump)
;; (require-package 'findr)
(require-package 'diredfl) ; font lock for `dired-mode'
(require-package 'pinyinlib)
(require-package 'find-by-pinyin-dired)
(require-package 'nvm)
(require-package 'writeroom-mode)
(require-package 'link)
(require-package 'connection)
(require-package 'dictionary) ; dictionary requires 'link and 'connection
(require-package 'htmlize)
(require-package 'scratch)
(require-package 'textile-mode)
(require-package 'dsvn)
(require-package 'git-timemachine)

(require-package 'yaml-mode)
(require-package 'vimrc-mode)
(require-package 'csv-mode)
(require-package 'rust-mode)

(require-package 'command-log-mode)
(require-package 'regex-tool)
(require-package 'groovy-mode)
(require-package 'emmet-mode)

(require-package 'unfill)
(require-package 'counsel-bbdb)
(require-package 'counsel-gtags)

(require-package 'cmake-mode)
(require-package 'cpputils-cmake)
(require-package 'bbdb)

;; rvm-open-gem to get gem's code
(require-package 'rvm)
;; C-x r l to list bookmarks
(require-package 'tagedit)
(require-package 'git-link)

(require-package 'lsp-mode)
(require-package 'legalese)
;; (require-package 'git-gutter) ; use my patched version
(require-package 'neotree)
;; (require-package 'hydra)
;; (require-package 'ivy-hydra) ; @see https://oremacs.com/2015/07/23/ivy-multiaction/
(require-package 'web-mode)
(require-package 'iedit)
(require-package 'websocket) ; for debug debugging of browsers
(require-package 'counsel-css)
(require-package 'auto-package-update)
(require-package 'toc-org)
(require-package 'elpa-mirror)
;; {{ @see https://pawelbx.github.io/emacs-theme-gallery/
(require-package 'visual-regexp) ;; Press "M-x vr-*"

;; {{ Fixed expiring GNU ELPA keys
;; GNU ELPA GPG key will expire on Sep-2019. So we need install this package to
;; update key or else users can't install packages from GNU ELPA.
;; @see https://www.reddit.com/r/emacs/comments/bn6k1y/updating_gnu_elpa_keys/
;; BTW, this setup uses MELPA only. So GNU ELPA GPG key is not used.
(require-package 'gnu-elpa-keyring-update)
;; }}

(require-package 'rainbow-mode)
(require-package 'magit)

;; kill buffer without my confirmation
;; (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(provide 'init-elpa)
