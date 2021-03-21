;;; init-shackel.el --- shackle setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; TODO

;;; Code:

(require-package 'shackle)
(require-package 'popper)

(defvar current-pop-window nil
  "Last or current window for pop.")

(defun inc0n/shackle-display-buffer-frame (buffer alist plist)
  "Display BUFFER in a popped up frame.
ALIST is passed to `shackle--window-display-buffer' internally.
If PLIST contains the :other key with t as value, reuse the next
available frame if possible, otherwise pop up a new frame."
  (unless (and current-pop-window
               (window-valid-p current-pop-window))
    (setq current-pop-window
          (split-window-vertically
           (round
            (* (- 1 (or (plist-get plist :size)
                        shackle-default-size))
               (window-size (frame-root-window) nil))))))
  (let (;; (frame (shackle--splittable-frame))
        (window current-pop-window))
    (prog1 (shackle--window-display-buffer buffer window 'window alist)
      (when window
        (setq shackle-last-window window
              shackle-last-buffer buffer))
      ;; (unless (cdr (assq 'inhibit-switch-frame alist))
      ;;   (window--maybe-raise-frame frame))
      )))

(with-eval-after-load 'shackle
  ;; @see https://github.com/wasamasa/shackle
  (setq shackle-select-reused-windows nil ; default nil
        shackle-default-alignment 'below  ; default below
        shackle-default-size 0.4)
  ;; :same if non-nil open in current window
  ;; :select if non-nil select upon open
  ;; :inhibit-window-quit if non-nil prevent window quiting on q
  (setq shackle-rules
        ;; CONDITION(:regexp) :select :inhibit-window-quit :size+:align|:other :same|:popup
        `((compilation-mode :select nil)
          ("*undo-tree*" :size 0.25 :align right)
          ("*eshell*" :select t :other t)
          ("*Shell Command Output*" :select nil)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          (occur-mode :select nil :align t)
          (,(rx "*" (or "eww history" "Help") "*")
           :regexp t :select t :inhibit-window-quit t :other t
           :custom inc0n/shackle-display-buffer-frame)
          ("*Completions*" :size 0.3 :align t)
          ("*Messages*" :select nil :inhibit-window-quit t :other t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)
          ("\\*poporg.*\\*" :regexp t :select t :other t)
          ("*Calendar*" :select t :size 0.3 :align below)
          ("*info*" :select t :inhibit-window-quit t :same nil)
          (magit-status-mode :select t :inhibit-window-quit t :same t)
          (magit-log-mode :select t :inhibit-window-quit t :same t)
          ("*Flycheck errors*" :select nil :size 0.3 :align below)
          ;; (" \\*which-key\\*" :size 0.3 :align below)
          ("TAGS" :select t :other t))))

(defun inc0n/popper-select-popup-at-bottom (buffer &optional _alist)
  "Display and switch to popup-buffer BUFFER at the bottom of the screen."
  (let ((window (split-window-vertically
                 (round
                  (* (- 1 shackle-default-size)
                     (max (window-size (frame-root-window) nil)
                          (window-size (selected-window) nil)))))))
    (prog1
        (window--display-buffer buffer window 'window)
      (select-window window))))

(with-eval-after-load 'popper
  (require 'project)
  (setq popper-mode-line '(:eval (propertize " POP" 'face 'bold))
        popper-mode-line-position 2
        popper-display-function 'inc0n/popper-select-popup-at-bottom)
  (setq popper-reference-buffers
        '(Custom-mode
          compilation-mode
          messages-mode
          help-mode
          occur-mode
          eww-history-mode
          flycheck-error-list-mode
          debugger-mode
          xref--xref-buffer-mode
          Info-mode
          "^\\*Warning\\*"
          "^\\*Messages-Log\\*"
          "^\\*Completions\\*"
          "^\\*Shell Command Output\\*")
        popper-group-function 'popper-group-by-project
        popper-display-control t)
  (defun popper-group-by-project ()
    "Return an identifier (project root) to group popups."
    (let ((project (project-current)))
      (or (and project
               (car (project-roots project)))
          default-directory))))
(util/add-to-timed-init-hook 1 'popper-mode)

(global-set-key [C-iso-lefttab] 'popper-cycle) ;; ctrl-shift-tab
(global-set-key (kbd "C-S-L") 'popper-toggle-latest)


(provide 'init-shackle)
;;; init-shackle.el ends here