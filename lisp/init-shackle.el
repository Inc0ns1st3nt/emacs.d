;;; init-shackel.el --- shackle setup -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; TODO

;;; Code:

;; @see https://github.com/wasamasa/shackle
(use-package shackle
  :ensure t
  :defer t
  :config
  (setq shackle-select-reused-windows nil ; default nil
        shackle-default-alignment 'below  ; default below
        shackle-default-size 0.4)
  ;; :same if non-nil open in current window
  ;; :select if non-nil select upon open
  ;; :inhibit-window-quit if non-nil prevent window quiting on q

  ;; CONDITION(:regexp) :select :inhibit-window-quit :size+:align|:other :same|:popup
  (setq shackle-rules
        `((compilation-mode :select nil)
          ("*undo-tree*" :size 0.25 :align right)
          ("*eshell*" :select t :other t)
          ("*Shell Command Output*" :select nil)
          ("\\*Async Shell.*\\*" :regexp t :ignore t)
          (occur-mode :select nil :align t)
          ;; (,(rx "*" (or "eww history" "Help") "*")
          ;;  :regexp t :select nil :inhibit-window-quit t)
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

(defvar inc0n/popper-last-window nil)
(defvar inc0n/popper-window-size 0.4)

(defun inc0n/popper-select-popup-at-bottom (buffer &optional _alist)
  "Display and switch to popup-buffer BUFFER at the bottom of the screen."
  (when (or (null inc0n/popper-last-window)
            (not (window-live-p inc0n/popper-last-window)))
    (setq inc0n/popper-last-window
          (split-window-vertically
           (round
            (* (- 1 inc0n/popper-window-size)
               (window-size (frame-root-window) nil))))))
  (let ((window inc0n/popper-last-window))
    (prog1 (window--display-buffer buffer window 'window)
      (select-window window))))

(use-package popper
  :ensure t
  :defer 1
  :init
  (message "popper initialized")
  (popper-mode 1)
  (bind-key [C-iso-lefttab] 'popper-cycle) ;; ctrl-shift-tab
  (bind-key "C-S-l" 'popper-toggle-latest))

(global-set-key (kbd "C-S-o")
                (lambda () (interactive)
                  (if inc0n/popper-last-window
                      (select-window
                       (if (eq (selected-window) inc0n/popper-last-window)
                           (frame-root-window)
                         inc0n/popper-last-window))
                    (message "no popper window opened!"))))

(with-eval-after-load 'popper
  (setq popper-mode-line '(:eval (propertize " POP" 'face 'bold))
        popper-mode-line-position 2
        popper-display-function 'inc0n/popper-select-popup-at-bottom)
  (setq popper-reference-buffers
        '(;; Custom-mode
          compilation-mode
          messages-mode
          help-mode
          occur-mode
          eww-history-mode
          flycheck-error-list-mode
          debugger-mode
          xref--xref-buffer-mode gtags-select-mode
          Info-mode
          "^\*Compile-log\*"
          "^\*Warning\*"
          "^\\*Messages-Log\\*"
          "^\\*Completions\\*"
          "^\\*Shell Command Output\\*")
        popper-group-function (defun inc0n/popper-group-by-project ()
                                "Return an identifier (project root) to group popups."
                                (let ((project (project-current)))
                                  (or (and project
                                           (car (project-roots project)))
                                      default-directory)))
        popper-display-control t))

;; (autoload #'popper-group-by-project "project"
;;   "Setups the gtags project root-dir." nil)

(provide 'init-shackle)