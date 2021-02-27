;; -*- coding: utf-8; lexical-binding: t; -*-

;; avoid default "gnu" style, use more popular one
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))

(setq-default c-basic-offset 4)

(defun inc0n/common-cc-mode-setup ()
  "setup shared by all languages (java/groovy/c++ ...)"
  ;; give me NO newline automatically after electric expressions are entered
  (setq c-auto-newline nil)

  ;; syntax-highlight aggressively
  ;; (setq font-lock-support-mode 'lazy-lock-mode)
  (setq lazy-lock-defer-contextually t)
  (setq lazy-lock-defer-time 0)

  ;; make DEL take all previous whitespace with it
  (c-toggle-hungry-state 1)

  ;; indent
  ;; google "C/C++/Java code indentation in Emacs" for more advanced skills
  ;; C code:
  ;;   if(1) // press ENTER here, indent with 4 spaces
  (c-set-offset 'substatement 4)
  ;;   void fn() // press ENTER here, zero means no indentation
  (c-set-offset 'func-decl-cont 0))

(defun inc0n/c-mode-setup ()
  "C/C++ only setup."
  ;; @see http://stackoverflow.com/questions/3509919/ \
  ;; emacs-c-opening-corresponding-header-file
  (local-set-key (kbd "C-x C-o") 'ff-find-other-file)

  (setq cc-search-directories
        '("." "/usr/include" "/usr/local/include/*" "../*/include" "$WXWIN/include"))

  ;; {{ @see https://github.com/redguardtoo/cpputils-cmake
  ;; In theory, you can write your own Makefile for `flyamke-mode' without cmake.
  ;; Nobody actually does it in real world.

  ;; debugging Emacs c code
  (add-to-list 'imenu-generic-expression '(nil "^DEFUN *(\"\\([a-zA-Z0-9-]+\\)" 1))

  ;; make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft)))

;; don't use c-mode-common-hook or cc-mode-hook because many major-modes use this hook
(defun c-mode-common-hook-setup ()
  (unless (buffer-file-temp-p)
    (inc0n/common-cc-mode-setup)
    (unless (or (derived-mode-p 'java-mode)
                (derived-mode-p 'groovy-mode))
      (inc0n/c-mode-setup))
    (eldoc-mode 1)))
(add-hook 'c-mode-common-hook 'c-mode-common-hook-setup)

(provide 'init-cc-mode)
