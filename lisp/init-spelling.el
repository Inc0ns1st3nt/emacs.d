;; -*- coding: utf-8; lexical-binding: t; -*-

;; avoid spell-checking doublon (double word) in certain major modes
(defvar inc0n/flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")
 (make-variable-buffer-local 'inc0n/flyspell-check-doublon)

(defvar inc0n/default-spell-check-language "en_US"
  "Language used by aspell and hunspell CLI.")

(with-eval-after-load 'flyspell
  ;; {{ flyspell setup for web-mode
  (defun inc0n/web-mode-flyspell-verify ()
    (let* ((f (get-text-property (- (point) 1) 'face))
           rlt)
      (cond
       ;; Check the words whose font face is NOT in below *blacklist*
       ((not (memq f '(web-mode-html-attr-value-face
                       web-mode-html-tag-face
                       web-mode-html-attr-name-face
                       web-mode-constant-face
                       web-mode-doctype-face
                       web-mode-keyword-face
                       web-mode-comment-face ;; focus on get html label right
                       web-mode-function-name-face
                       web-mode-variable-name-face
                       web-mode-css-property-name-face
                       web-mode-css-selector-face
                       web-mode-css-color-face
                       web-mode-type-face
                       web-mode-block-control-face)))
        (setq rlt t))
       ;; check attribute value under certain conditions
       ((memq f '(web-mode-html-attr-value-face))
        (save-excursion
          (search-backward-regexp "=['\"]" (line-beginning-position) t)
          (backward-char)
          (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                  (or (thing-at-point 'symbol) "")))))
       ;; finalize the blacklist
       (t
        (setq rlt nil)))
      ;; If rlt is t, it's a typo. If nil, not a typo.
      rlt))
  (put 'web-mode 'flyspell-mode-predicate 'inc0n/web-mode-flyspell-verify)
  ;; }}

  ;; better performance
  (setq flyspell-issue-message-flag nil)

  ;; flyspell-lazy is outdated and conflicts with latest flyspell

  (defun inc0n/flyspell-highlight-incorrect-region-hack (orig-func &rest args)
    "Don't mark doublon (double words) as typo."
    (cl-destructuring-bind (beg end poss)
        args
      (when (or inc0n/flyspell-check-doublon (not (eq 'doublon poss)))
        (apply orig-func args))))
  (advice-add 'flyspell-highlight-incorrect-region :around #'inc0n/flyspell-highlight-incorrect-region-hack))

;; Basic Logic Summary:
;; If (aspell is installed) { use aspell}
;; else if (hunspell is installed) { use hunspell }
;; English dictionary is used.
;;
;; I prefer aspell because:
;; - aspell is very stable and easy to install
;; - looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun inc0n/detect-ispell-args (&optional run-together)
  "If RUN-TOGETHER is true, spell check the CamelCase words.
Please note RUN-TOGETHER makes aspell less capable.  So it should be used in `prog-mode-hook' only."
  (let* (args)
    (when ispell-program-name
      (cond
       ;; use aspell
       ((string-match "aspell" ispell-program-name)
        ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
        ;; For aspell's option "--lang", "two letter ISO 3166 country code after a underscore" is OPTIONAL.
        (setq args (list "--sug-mode=ultra" (format "--lang=%s" inc0n/default-spell-check-language)))
        ;; "--run-together-min" could not be 3, see `check` in "speller_impl.cpp".
        ;; The algorithm is not precise.
        ;; Run `echo tasteTableConfig | aspell --lang=en_US -C --run-together-limit=16  --encoding=utf-8 -a` in shell.
        (when run-together
          (cond
           ;; Kevin Atkinson said now aspell supports camel case directly
           ;; https://github.com/redguardtoo/emacs.d/issues/796
           ((string-match-p "--.*camel-case"
                            (shell-command-to-string (concat ispell-program-name " --help")))
            (setq args (append args '("--camel-case"))))

           ;; old aspell uses "--run-together". Please note we are not dependent on this option
           ;; to check camel case word. wucuo is the final solution. This aspell options is just
           ;; some extra check to speed up the whole process.
           (t
            (setq args (append args '("--run-together" "--run-together-limit=16")))))))

       ;; use hunspell
       (t (setq args nil))))
    args))

(setq ispell-program-name "aspell")

(defun inc0n/ispell-word-hack (orig-func &rest args)
  "Use Emacs original arguments when calling `ispell-word'.
When fixing a typo, avoid pass camel case option to cli program."
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (inc0n/detect-ispell-args))
    (apply orig-func args)
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)))
(advice-add 'ispell-word :around #'inc0n/ispell-word-hack)
(advice-add 'flyspell-auto-correct-word :around #'inc0n/ispell-word-hack)

;; (defun text-mode-hook-setup ()
;;   ;; Turn off RUN-TOGETHER option when spell check text-mode
;;   (setq-local ispell-extra-args (inc0n/detect-ispell-args))
;;   (util/ensure 'wucuo)
;;   (wucuo-start))
;; (add-hook 'text-mode-hook 'text-mode-hook-setup)
;;
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'text-mode-hook 'flyspell-prog-mode)

;; You can also use "M-x ispell-word" or hotkey "M-$". It pop up a multiple choice
;; @see http://frequal.com/Perspectives/EmacsTip03-FlyspellAutoCorrectWord.html
(global-set-key (kbd "C-c s") 'flyspell-auto-correct-word)

(defun inc0n/clean-aspell-dict ()
  "Clean ~/.aspell.pws (dictionary used by aspell)."
  (interactive)
  (let* ((dict (file-truename "~/.aspell.en.pws"))
         (lines (util/read-lines dict))
         ;; sort words
         (aspell-words (sort (cdr lines) 'string<)))
    (with-temp-file dict
      (insert (format "%s %d\n%s"
                        "personal_ws-1.1 en"
                        (length aspell-words)
                        (mapconcat 'identity aspell-words "\n"))))))

;; {{ langtool setup
(with-eval-after-load 'langtool
  (setq langtool-generic-check-predicate
        (lambda (start end)
          ;; set up for `org-mode'
          (let ((begin-regexp "^[ \t]*#\\+begin_\\(src\\|html\\|latex\\|example\\|quote\\)")
                (end-regexp "^[ \t]*#\\+end_\\(src\\|html\\|latex\\|example\\|quote\\)")
                (case-fold-search t)
                (ignored-font-faces '(org-verbatim
                                      org-block-begin-line
                                      org-meta-line
                                      org-special-keyword
                                      org-property-value
                                      org-tag
                                      org-link
                                      org-table
                                      org-level-1
                                      org-document-info))
                ff)
            (save-excursion
              (goto-char start)

              ;; get current font face
              (setq ff (get-text-property start 'face))
              (when (listp ff)
                (setq ff (car ff)))

              ;; ignore certain errors by set rlt to nil
              (if (or (memq ff ignored-font-faces) ;; check current font face
                      (string-match "^ *- $"
                                    (buffer-substring (line-beginning-position)
                                                      (+ start 2)))
                      (string-match "^ *- $"
                                    (buffer-substring (line-beginning-position)
                                                      (+ end 2))))
                  ;; dash character of " - list item 1"
                  nil
                (let ((th (thing-at-point 'evil-WORD)))
                  (if (and th
                           (or (string-match "^=[^=]*=[,.]?$" th)
                               (string-match "^\\[\\[" th)
                               (string-match "^=(" th)
                               (string-match ")=$" th)
                               (string= "w3m" th)))
                      ;; embedded code like =code= or org-link [[http://google.com][google]] or [[www.google.com]]
                      ;; langtool could finish checking before major mode prepare font face for all texts
                      nil
                    ;; inside source block?
                    (if (re-search-backward begin-regexp nil t)
                        (let ((e (re-search-forward end-regexp nil t)))
                          (if e
                              (not (< start e))
                            t))
                      t)))))))))
;; }}

(with-eval-after-load 'wucuo
  ;; {{ wucuo is used to check camel cased code and plain text.  Code is usually written
  ;; in English. If your code uses other language (Spanish?),
  ;; Un-comment and modify below two lines:

  ;; (setq wucuo-aspell-language-to-use "en")
  ;; (setq wucuo-hunspell-dictionary-base-name "en_US")

  ;; }}

  ;; do NOT turn on `flyspell-mode' automatically.
  ;; check buffer or visible region only
  ;; spell check buffer every 30 seconds
  (setq wucuo-update-interval 2))

(provide 'init-spelling)
