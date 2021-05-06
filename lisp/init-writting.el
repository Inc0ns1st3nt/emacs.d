;; -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; themes also some selectrum specific theme hacks

;;; Code:

(require-package 'writeroom-mode)
(local-require 'mixed-pitch)

;;; mixed-pitch

(autoload #'mixed-pitch-serif-mode "mixed-pitch"
  "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch." t)

(with-eval-after-load 'mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "Merriweather")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (face-remap-add-relative 'variable-pitch :family "Merriweather")
  ;; (face-remap-add-relative 'fixed-pitch-serif :family "Merriweather")
  (setq mixed-pitch-set-height t))

(defun mixed-pitch-serif-mode (&optional arg)
  "Change the default face of the current buffer to a serifed variable pitch.
ARG is passed in."
  (interactive)
  (let (;; (mixed-pitch-fixed-pitch-faces nil)
        (mixed-pitch-face 'variable-pitch-serif))
    (mixed-pitch-mode (or arg 'toggle))))

(setq text-scale-mode-step 1.1)
;; (text-scale-set 0)

;;; writeroom

(defalias 'readroom-mode 'writeroom-mode)

(with-eval-after-load 'writeroom-mode
  (add-to-list-multi 'writeroom--local-variables
                     '(mixed-pitch-mode
                       org-indent-mode
                       org-adapt-indentation
                       display-line-numbers-mode))
  (setq writeroom-width 0.7
        ;; writetoom-
        writeroom-window-maximized nil
        writeroom-fullscreen-effect 'maximized
        writeroom-extra-line-spacing nil
        writeroom-mode-line t))

(define-hook-setup 'writeroom-mode-enable-hook :zen
  "Reformat the current Org buffer appearance for prose."
  (when (eq major-mode 'org-mode)
    ;; (setq-local visual-fill-column-center-text t)
    (setq-local visual-fill-column-width (max visual-fill-column-width 65))
    (message "%s" visual-fill-column-width)
    ;; (setq-local visual-fill-column-extra-text-width '(0 . 0))
    ;; (when (fboundp 'org-pretty-table-mode)
    ;;   (org-pretty-table-mode 1))
    (setq-local org-adapt-indentation t)
    ;; (text-scale-increase 1.0)
    (org-indent-mode -1)
    (display-line-numbers-mode -1)
    (mixed-pitch-serif-mode 1)))

(define-hook-setup 'writeroom-mode-disable-hook :zen
  (when (eq major-mode 'org-mode)
    (mapc (lambda (val)
            (if (symbolp val)
                (kill-local-variable val)
              (if (fboundp (car val))
                  (funcall (car val) (cdr val))
                (set (car val) (cdr val)))))
          writeroom--saved-data)
    ;; (text-scale-decrease 1.0)
    ))


;;; writing

;; @see http://endlessparentheses.com/super-smart-capitalization.html
(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to `text-mode'."
  (let* ((f "\\(%s\\)\\(%s\\)")
         (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (when (and (derived-mode-p 'text-mode)
               (or (looking-at (format f space rg))
                   (looking-back (format f rg space))))
      (replace-match rp nil nil nil 1))))

(defun endless/capitalize ()
  "Capitalise region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (call-interactively 'subword-capitalize)))

(defun endless/downcase ()
  "Down-case region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'subword-downcase)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'subword-upcase)))

;; these bindings are fine
(global-set-key (kbd "M-c") 'endless/capitalize)
(global-set-key (kbd "M-l") 'endless/downcase)
(global-set-key (kbd "M-u") 'endless/upcase)

(provide 'init-writting)