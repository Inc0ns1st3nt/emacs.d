Content-Type: text/enriched
Text-Width: 70

;;; <x-color><param>#556b2f</param>init.el --- Load the full configuration -*- lexical-binding: t -*-

</x-color>
;;; <x-color><param>#556b2f</param>Commentary:

</x-color>
;; <x-color><param>#556b2f</param>This file bootstraps the configuration, which is divided into
</x-color>;; <x-color><param>#556b2f</param>a number of other files.
</x-color>
;;; <x-color><param>#556b2f</param>Code:
</x-color>
;; <x-color><param>#556b2f</param>Produce backtraces when errors occur
</x-color><x-color><param>#014980</param>(</x-color>setq debug-on-error t<x-color><param>#014980</param>)</x-color>


;; <x-color><param>#556b2f</param>(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

</x-color>

<x-color><param>#014980</param>(</x-color>defvar <x-color><param>#383a42</param>normal-gc-cons-threshold</x-color> <x-color><param>#014980</param>(</x-color>* 40 1024 1024<x-color><param>#014980</param>))</x-color> ;; <x-color><param>#556b2f</param>40mb
</x-color>;;<x-color><param>#556b2f</param>----------------------------------------------------------------------------
</x-color>;; <x-color><param>#556b2f</param>Adjust garbage collection thresholds during startup, and thereafter
</x-color>;;<x-color><param>#556b2f</param>----------------------------------------------------------------------------
</x-color><x-color><param>#014980</param>(</x-color>let <x-color><param>#014980</param>((</x-color>init-gc-cons-threshold <x-color><param>#014980</param>(</x-color>* 128 1024 1024<x-color><param>#014980</param>)))</x-color> ;; <x-color><param>#556b2f</param>128mb
</x-color>  <x-color><param>#014980</param>(</x-color>setq garbage-collection-messages t<x-color><param>#014980</param>)</x-color> ; <x-color><param>#556b2f</param>for debug
</x-color>  <x-color><param>#014980</param>(</x-color>setq gc-cons-threshold init-gc-cons-threshold<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>setq gc-cons-percentage 0.20<x-color><param>#014980</param>))</x-color>
<x-color><param>#014980</param>(</x-color>add-hook 'emacs-startup-hook
          <x-color><param>#014980</param>(</x-color>lambda <x-color><param>#014980</param>()</x-color>

            <x-color><param>#014980</param>(</x-color>setq gc-cons-threshold normal-gc-cons-threshold<x-color><param>#014980</param>)</x-color>

            <x-color><param>#014980</param>(</x-color>message <x-color><param>#8a3b3c</param>"startup time: %s"</x-color> <x-color><param>#014980</param>(</x-color>emacs-init-time<x-color><param>#014980</param>))))</x-color>


;;<x-color><param>#556b2f</param>----------------------------------------------------------------------------
</x-color>;; <x-color><param>#556b2f</param>Bootstrap config
</x-color>;;<x-color><param>#556b2f</param>----------------------------------------------------------------------------
</x-color>

<x-color><param>#014980</param>(</x-color>defun inc0n/vc-merge-p <x-color><param>#014980</param>()</x-color>
  <x-color><param>#485a27</param>"Use Emacs for git merge only?"</x-color>
  <x-color><param>#014980</param>(</x-color>boundp 'startup-now<x-color><param>#014980</param>))</x-color>


<x-color><param>#014980</param>(</x-color>setq custom-file <x-color><param>#014980</param>(</x-color>expand-file-name <x-color><param>#8a3b3c</param>"custom.el"</x-color> user-emacs-directory<x-color><param>#014980</param>))</x-color>

<x-color><param>#014980</param>(</x-color>defvar <x-color><param>#383a42</param>inc0n/lisp-dir</x-color> <x-color><param>#014980</param>(</x-color>expand-file-name <x-color><param>#8a3b3c</param>"lisp"</x-color> user-emacs-directory<x-color><param>#014980</param>))</x-color>


<x-color><param>#014980</param>(</x-color>defun require-init <x-color><param>#014980</param>(</x-color>pkg<x-color><param>#014980</param>)</x-color>

  <x-color><param>#485a27</param>"Require the init PKG files in init Lisp directory."</x-color>
  <x-color><param>#014980</param>(</x-color>load <x-color><param>#014980</param>(</x-color>file-truename <x-color><param>#014980</param>(</x-color>format <x-color><param>#8a3b3c</param>"%s/%s"</x-color> inc0n/lisp-dir pkg<x-color><param>#014980</param>))</x-color>

        t t<x-color><param>#014980</param>))</x-color>


<x-color><param>#014980</param>(</x-color>defun inc0n/add-subdirs-to-load-path <x-color><param>#014980</param>(</x-color>parent-dir<x-color><param>#014980</param>)</x-color>
  <x-color><param>#485a27</param>"Add every non-hidden subdir of PARENT-DIR to `</x-color><x-color><param>#383a42</param><x-color><param>#485a27</param>load-path</x-color></x-color><x-color><param>#485a27</param>'."</x-color>
  <x-color><param>#014980</param>(</x-color>let <x-color><param>#014980</param>((</x-color>default-directory parent-dir<x-color><param>#014980</param>))</x-color>
    <x-color><param>#014980</param>(</x-color>setq load-path
          <x-color><param>#014980</param>(</x-color>append
           <x-color><param>#014980</param>(</x-color>cl-remove-if-not
            #'file-directory-p
            <x-color><param>#014980</param>(</x-color>directory-files <x-color><param>#014980</param>(</x-color>expand-file-name parent-dir<x-color><param>#014980</param>)</x-color> t <x-color><param>#8a3b3c</param>"^[</x-color><x-color><param>#383a42</param><x-color><param>#8a3b3c</param>^</x-color></x-color><x-color><param>#8a3b3c</param>\\.]"</x-color><x-color><param>#014980</param>))</x-color>
           load-path<x-color><param>#014980</param>))))</x-color>


;; <x-color><param>#556b2f</param>@see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
</x-color>;; <x-color><param>#556b2f</param>Normally file-name-handler-alist is set to
</x-color>;; <x-color><param>#556b2f</param>(("\\`/[</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>^</x-color></x-color><x-color><param>#556b2f</param>/]*\\'" . tramp-completion-file-name-handler)
</x-color>;; <x-color><param>#556b2f</param>("\\`/[</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>^</x-color></x-color><x-color><param>#556b2f</param>/|:][</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>^</x-color></x-color><x-color><param>#556b2f</param>/|]*:" . tramp-file-name-handler)
</x-color>;; <x-color><param>#556b2f</param>("\\`/:" . file-name-non-special))
</x-color>;; <x-color><param>#556b2f</param>Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
</x-color><x-color><param>#014980</param>(</x-color>let <x-color><param>#014980</param>((</x-color>file-name-handler-alist nil<x-color><param>#014980</param>))</x-color>

  <x-color><param>#014980</param>(</x-color>require-init 'init-autoload<x-color><param>#014980</param>)</x-color>
  ;; <x-color><param>#556b2f</param>`</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>package-initialize</x-color></x-color><x-color><param>#556b2f</param>' takes 35% of startup time
</x-color>  ;; <x-color><param>#556b2f</param>need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-modeline<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-utils<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-file-type<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-elpa<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-exec-path<x-color><param>#014980</param>)</x-color> ;; <x-color><param>#556b2f</param>Set up $PATH
</x-color>  ;; <x-color><param>#556b2f</param>Any file use flyspell should be initialized after init-spelling.el
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-spelling<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-ibuffer<x-color><param>#014980</param>)</x-color>

  <x-color><param>#014980</param>(</x-color>require-init 'init-selectrum<x-color><param>#014980</param>)</x-color>
  ;; <x-color><param>#556b2f</param>(require-init 'init-ivy)
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-windows<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-markdown<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-javascript<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-org<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-css<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-python<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-lisp<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-yasnippet<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-cc-mode<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-linum-mode<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-git<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-gtags<x-color><param>#014980</param>)</x-color>


  <x-color><param>#014980</param>(</x-color>require-init 'init-ctags<x-color><param>#014980</param>)</x-color>
  ;; <x-color><param>#556b2f</param>(require-init 'init-bbdb)
</x-color>  ;; <x-color><param>#556b2f</param>(require-init 'init-gnus)
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-lua-mode<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-term-mode<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-web-mode<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-haskell<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-latex<x-color><param>#014980</param>)</x-color>

  <x-color><param>#014980</param>(</x-color>require-init 'init-company<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-chinese<x-color><param>#014980</param>)</x-color> ;; <x-color><param>#556b2f</param>cannot be idle-require-initd
</x-color>  ;; <x-color><param>#556b2f</param>(require-init 'init-counsel)
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-keyfreq<x-color><param>#014980</param>)</x-color> ;; <x-color><param>#556b2f</param>need statistics of keyfreq asap
</x-color>  ;; <x-color><param>#556b2f</param>(require-init 'init-httpd)

</x-color>  ;; <x-color><param>#556b2f</param>projectile costs 7% startup time

</x-color>

  <x-color><param>#014980</param>(</x-color>require-init 'init-flycheck<x-color><param>#014980</param>)</x-color>

  <x-color><param>#014980</param>(</x-color>require-init 'init-pdf<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-theme<x-color><param>#014980</param>)</x-color>     ;; <x-color><param>#556b2f</param>don't play with color-theme in light weight mode
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-essential<x-color><param>#014980</param>)</x-color> ;; <x-color><param>#556b2f</param>essential has some crucial tools I need immediately
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-misc<x-color><param>#014980</param>)</x-color>      ;; <x-color><param>#556b2f</param>misc, handy tools though not must have

</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-emacs-w3m<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-shackle<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-dired<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-writting<x-color><param>#014980</param>)</x-color>
  ;; <x-color><param>#556b2f</param>(require-init 'init-hydra)  ; hotkey is require-initd everywhere
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-clipboard<x-color><param>#014980</param>)</x-color>
  <x-color><param>#014980</param>(</x-color>require-init 'init-evil<x-color><param>#014980</param>)</x-color>

  ;; <x-color><param>#556b2f</param>ediff configuration should be last so it can override
</x-color>  ;; <x-color><param>#556b2f</param>the key bindings in previous configuration
</x-color>  <x-color><param>#014980</param>(</x-color>require-init 'init-ediff<x-color><param>#014980</param>)</x-color>

  ;; <x-color><param>#556b2f</param>@see https://github.com/hlissner/doom-emacs/wiki/FAQ
</x-color>  ;; <x-color><param>#556b2f</param>Adding directories under "site-lisp/" to `</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>load-path</x-color></x-color><x-color><param>#556b2f</param>' slows
</x-color>  ;; <x-color><param>#556b2f</param>down all `</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>require-init</x-color></x-color><x-color><param>#556b2f</param>' statement. So we do this at the end of startup
</x-color>  ;; <x-color><param>#556b2f</param>NO ELPA package is dependent on "site-lisp/".
</x-color>  ;; <x-color><param>#556b2f</param>(inc0n/add-subdirs-to-load-path (file-name-as-directory inc0n/site-lisp-dir))

</x-color>

  <x-color><param>#014980</param>(</x-color>when <x-color><param>#014980</param>(</x-color>file-exists-p custom-file<x-color><param>#014980</param>)</x-color>
	<x-color><param>#014980</param>(</x-color>load custom-file<x-color><param>#014980</param>)))</x-color>


;;; <x-color><param>#556b2f</param>Local Variables:
</x-color>;;; <x-color><param>#556b2f</param>no-byte-compile: t
</x-color>;;; <x-color><param>#556b2f</param>End:
</x-color><x-color><param>#014980</param>(</x-color>put 'erase-buffer 'disabled t<x-color><param>#014980</param>)</x-color>
