;;; init-transient --- my transient setup for serveal packages

;;; Commentary:
;; should load after the packages making transient interface for

;;; Code:

(require-package 'transient)
(require 'transient)
;; inc0n/define-transient-command

;; transient-define-prefix

(fset 'inc0n/transient-suffix 'transient--do-stay)
(fset 'inc0n/transient-non-suffix 'transient--do-stay)

(defmacro inc0n/define-transient-command (name arg-list &rest args)
  "My `define-transient-command' setup for a transient stay interface.
NAME and ARG-LIST and ARGS check `define-transient-command'."
  (declare (debug (&define name
                           [&optional arg-list]
                           [&optional lambda-doc]
                           [&rest def-body]))
           (indent defun)
           (doc-string 3))
  (let ((doc-str (and (stringp (car args)) (car args)))
        (rest-args (if (stringp (car args)) (cdr args) args)))
    `(progn
       (define-transient-command ,name ,arg-list
         ,doc-str
         :transient-suffix 'inc0n/transient-suffix
         :transient-non-suffix 'inc0n/transient-non-suffix
         ,@rest-args
         (transient-setup ',name))
       ',name)))

;;;###autoload
(inc0n/define-transient-command inc0n/transient-flyspell ()
  "Transient interface for `flyspell'"
  ["Flyspell"
   ("n" "next error" flyspell-goto-next-error)
   ("c" "correct error or next correction" flyspell-auto-correct-word)
   ("q" "quit" transient-quit-one)]
  (interactive)
  (flyspell-goto-next-error))

;;;###autoload
(inc0n/define-transient-command inc0n/transient-flycheck (&optional arg)
  "Transient interface for `flycheck'"
  ["Flycheck"
   ("n" "next error" flycheck-next-error)
   ("p" "previous error" flycheck-previous-error)
   ("d" "display error" flycheck-display-error-at-point)
   ("e" "explain error" flycheck-explain-error-at-point)
   ("q" "quit" transient-quit-one)]
  (interactive "P")
  (if arg (flycheck-previous-error)
    (flycheck-next-error)))

;;;###autoload
(inc0n/define-transient-command inc0n/transient-winner (&optional arg)
  "Transient interface for `winner-undo'."
  ["Winner do"
   ("u" "undo" winner-undo)
   ("r" "redo" winner-redo)
   ("q" "quit" transient-quit-one)]
  (interactive "P")
  (if arg (winner-redo)
    (winner-undo)))

(defun transient-highlight-symbol-exit ()
  "Transient highlight symbol exit handler."
  (highlight-symbol--remove-symbol (highlight-symbol--get-symbol))
  (transient--do-exit))

;;;###autoload
(define-transient-command inc0n/transient-highlight-symbol ()
  "The transient version of `highlight-symbol'.
Check for `highlight-symbol' for more information such as for SYMBOL."
  :transient-suffix 'inc0n/transient-suffix
  :transient-non-suffix 'transient-highlight-symbol-exit
  ["Highlight symbol"
   ("n" "next" highlight-symbol-next-in-defun)
   ("p" "previous" highlight-symbol-prev-in-defun)
   ("q" "query replace" highlight-symbol-query-replace)]
  (interactive)
  (let ((symbol (or (highlight-symbol--get-symbol)
                    (error "No symbol at point"))))
    (unless (highlight-symbol--symbol-highlighted-p symbol)
      (highlight-symbol symbol)))
  (transient-setup 'inc0n/transient-highlight-symbol))

;; (defun inc0n/transient-highlight-symbol ()
;;   "The transient version of command `highlight-symbol'."
;;   (interactive)
;;   (let ((symbol (or (highlight-symbol--get-symbol)
;;                     (error "No symbol at point"))))
;;     (unless (highlight-symbol--symbol-highlighted-p symbol)
;;       (highlight-symbol symbol)))
;;   (ask-action-on "Prompt"
;;                  '((?n "next" highlight-symbol-next-in-defun)
;;                    (?p "previous" highlight-symbol-prev-in-defun)
;;                    (?q "query replace" highlight-symbol-query-replace))
;;                  ;; :target symbol
;;                  :on-exit 'transient-highlight-symbol-exit))

;; (global-set-key (kbd "M-o") nil)

(provide 'init-transient)