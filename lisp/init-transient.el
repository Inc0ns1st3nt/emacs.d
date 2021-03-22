;;; init-transient --- my transient setup for serveal packages

;;; Commentary:
;; should load after the packages making transient interface for

;;; Code:

(require-package 'transient)
(require 'transient)
;; inc0n/define-transient-command

(defun transient-post-suffix-quit ()
  "My transient post suffix do.
stay transient for all other suffix other than this suffix."
  (interactive)
  (when transient--prefix
    (if (eq (oref transient--prefix transient-suffix) this-command)
        (transient--do-exit)
      (transient--do-stay))))

;; transient-define-prefix

(defvar inc0n/transient-suffix 'transient--do-stay)
(defvar inc0n/transient-non-suffix 'transient--do-exit)

(defmacro inc0n/define-transient-command (name arg-list &rest args)
  "My `define-transient-command' setup for a transient stay interface.
NAME and ARG-LIST and ARGS check `define-transient-command'."
  (let ((doc-str (and (stringp (car args)) (car args)))
        (rest-args (if (stringp (car args)) (cdr args) args)))
    `(progn
       (define-transient-command ,name ,arg-list
         ,doc-str
         :transient-suffix inc0n/transient-suffix
         :transient-non-suffix inc0n/transient-non-suffix
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

;; (global-set-key (kbd "M-o") nil)
(provide 'init-transient)
;;; init-transient ends here