Content-Type: text/enriched
Text-Width: 70

;;; <x-color><param>#9ca0a4</param>init.el --- Load the full configuration -*- lexical-binding: t -*-

</x-color>
;;; <x-color><param>#9ca0a4</param>Commentary:

</x-color>
;; <x-color><param>#9ca0a4</param>This file bootstraps the configuration, which is divided into
</x-color>;; <x-color><param>#9ca0a4</param>a number of other files.
</x-color>
;;; <x-color><param>#9ca0a4</param>Code:
</x-color>
;; <x-color><param>#9ca0a4</param>Produce backtraces when errors occur
</x-color><x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>setq</x-color> debug-on-error t<x-color><param>#4078f2</param>)</x-color>


;; <x-color><param>#9ca0a4</param>(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defvar</x-color> <x-color><param>#6a1868</param>normal-gc-cons-threshold</x-color> <x-color><param>#a626a4</param>(</x-color>* 36 1024 1024<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color> ;; <x-color><param>#9ca0a4</param>36mb
</x-color>;;<x-color><param>#9ca0a4</param>----------------------------------------------------------------------------
</x-color>;; <x-color><param>#9ca0a4</param>Adjust garbage collection thresholds during startup, and thereafter
</x-color>;;<x-color><param>#9ca0a4</param>----------------------------------------------------------------------------
</x-color><x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>let</x-color> <x-color><param>#a626a4</param>(</x-color><x-color><param>#50a14f</param>(</x-color>init-gc-cons-threshold <x-color><param>#da8548</param>(</x-color>* 128 1024 1024<x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color> ;; <x-color><param>#9ca0a4</param>128mb
</x-color>  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>setq</x-color> garbage-collection-messages t<x-color><param>#a626a4</param>)</x-color> ; <x-color><param>#9ca0a4</param>for debug
</x-color>  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>setq</x-color> gc-cons-threshold init-gc-cons-threshold<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>setq</x-color> gc-cons-percentage 0.5<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>
<x-color><param>#4078f2</param>(</x-color>add-hook 'emacs-startup-hook
          <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>lambda</x-color> <x-color><param>#50a14f</param>()</x-color>

            <x-color><param>#50a14f</param>(</x-color><x-color><param>#e45649</param>setq</x-color> gc-cons-threshold normal-gc-cons-threshold<x-color><param>#50a14f</param>)</x-color>

            <x-color><param>#50a14f</param>(</x-color>message <x-color><param>#50a14f</param>"startup time: %s"</x-color> <x-color><param>#da8548</param>(</x-color>emacs-init-time<x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>


;;<x-color><param>#9ca0a4</param>----------------------------------------------------------------------------
</x-color>;; <x-color><param>#9ca0a4</param>Bootstrap config
</x-color>;;<x-color><param>#9ca0a4</param>----------------------------------------------------------------------------
</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defun</x-color> <x-color><param>#a626a4</param>inc0n/vc-merge-p</x-color> <x-color><param>#a626a4</param>()</x-color>
  <x-color><param>#84888b</param>"Use Emacs for git merge only?"</x-color>
  <x-color><param>#a626a4</param>(</x-color>boundp 'startup-now<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>


<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>setq</x-color> custom-file <x-color><param>#a626a4</param>(</x-color>expand-file-name <x-color><param>#50a14f</param>"custom.el"</x-color> user-emacs-directory<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defvar</x-color> <x-color><param>#6a1868</param>inc0n/lisp-dir</x-color> <x-color><param>#a626a4</param>(</x-color>expand-file-name <x-color><param>#50a14f</param>"lisp"</x-color> user-emacs-directory<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>


<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defun</x-color> <x-color><param>#a626a4</param>require-init</x-color> <x-color><param>#a626a4</param>(</x-color>pkg<x-color><param>#a626a4</param>)</x-color>

  <x-color><param>#84888b</param>"require the init pkg files in init lisp directory"</x-color>
  <x-color><param>#a626a4</param>(</x-color>load <x-color><param>#50a14f</param>(</x-color>file-truename <x-color><param>#da8548</param>(</x-color>format <x-color><param>#50a14f</param>"%s/%s"</x-color> inc0n/lisp-dir pkg<x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color>

        t t<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>


<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defun</x-color> <x-color><param>#a626a4</param>inc0n/add-subdirs-to-load-path</x-color> <x-color><param>#a626a4</param>(</x-color>parent-dir<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#84888b</param>"Adds every non-hidden subdir of PARENT-DIR to `</x-color><x-color><param>#b751b6</param><x-color><param>#84888b</param>load-path</x-color></x-color><x-color><param>#84888b</param>'."</x-color>
  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>let</x-color> <x-color><param>#50a14f</param>(</x-color><x-color><param>#da8548</param>(</x-color>default-directory parent-dir<x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color>
    <x-color><param>#50a14f</param>(</x-color><x-color><param>#e45649</param>setq</x-color> load-path
          <x-color><param>#da8548</param>(</x-color>append
           <x-color><param>#b751b6</param>(</x-color>cl-remove-if-not
            #'file-directory-p
            <x-color><param>#986801</param>(</x-color>directory-files <x-color><param>#4db5bd</param>(</x-color>expand-file-name parent-dir<x-color><param>#4db5bd</param>)</x-color> t <x-color><param>#50a14f</param>"^[</x-color><x-color><param>#4078f2</param><x-color><param>#50a14f</param>^</x-color></x-color><x-color><param>#50a14f</param>\\.]"</x-color><x-color><param>#986801</param>)</x-color><x-color><param>#b751b6</param>)</x-color>
           load-path<x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>


;; <x-color><param>#9ca0a4</param>@see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
</x-color>;; <x-color><param>#9ca0a4</param>Normally file-name-handler-alist is set to
</x-color>;; <x-color><param>#9ca0a4</param>(("\\`/[</x-color><x-color><param>#4078f2</param><x-color><param>#9ca0a4</param>^</x-color></x-color><x-color><param>#9ca0a4</param>/]*\\'" . tramp-completion-file-name-handler)
</x-color>;; <x-color><param>#9ca0a4</param>("\\`/[<x-color><param>#4078f2</param>^</x-color>/|:][<x-color><param>#4078f2</param>^</x-color>/|]*:" . tramp-file-name-handler)
</x-color>;; <x-color><param>#9ca0a4</param>("\\`/:" . file-name-non-special))
</x-color>;; <x-color><param>#9ca0a4</param>Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
</x-color><x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>let</x-color>

	<x-color><param>#a626a4</param>(</x-color><x-color><param>#50a14f</param>(</x-color>file-name-handler-alist nil<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color>

  <x-color><param>#a626a4</param>(</x-color>require-init 'init-autoload<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>`<x-color><param>#b751b6</param>package-initialize</x-color>' takes 35% of startup time
</x-color>  ;; <x-color><param>#9ca0a4</param>need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-modeline<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-utils<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-file-type<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-elpa<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-exec-path<x-color><param>#a626a4</param>)</x-color> ;; <x-color><param>#9ca0a4</param>Set up $PATH
</x-color>  ;; <x-color><param>#9ca0a4</param>Any file use flyspell should be initialized after init-spelling.el
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-spelling<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-ibuffer<x-color><param>#a626a4</param>)</x-color>

  <x-color><param>#a626a4</param>(</x-color>require-init 'init-selectrum<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>(require-init 'init-ivy)
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-windows<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-markdown<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-javascript<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-org<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-css<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-python<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-lisp<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-yasnippet<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-cc-mode<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-linum-mode<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-git<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-gtags<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-clipboard<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-ctags<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>(require-init 'init-bbdb)
</x-color>  ;; <x-color><param>#9ca0a4</param>(require-init 'init-gnus)
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-lua-mode<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-term-mode<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-web-mode<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-haskell<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-latex<x-color><param>#a626a4</param>)</x-color>

  <x-color><param>#a626a4</param>(</x-color>require-init 'init-company<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-chinese<x-color><param>#a626a4</param>)</x-color> ;; <x-color><param>#9ca0a4</param>cannot be idle-require-initd
</x-color>  ;; <x-color><param>#9ca0a4</param>(require-init 'init-counsel)
</x-color>  ;; <x-color><param>#9ca0a4</param>need statistics of keyfreq asap
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-keyfreq<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>(require-init 'init-httpd)

</x-color>  ;; <x-color><param>#9ca0a4</param>projectile costs 7% startup time

</x-color>

  <x-color><param>#a626a4</param>(</x-color>require-init 'init-pdf<x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>don't play with color-theme in light weight mode
</x-color>  ;; <x-color><param>#9ca0a4</param>color themes are already installed in `<x-color><param>#b751b6</param>init-elpa.el</x-color>'
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-theme<x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>essential has some crucial tools I need immediately
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-essential<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>misc, handy tools though not must have
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-misc<x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>(require-init 'init-emacs-w3m)
</x-color>  ;; <x-color><param>#9ca0a4</param>(require-init 'init-shackle)
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-dired<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-writting<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>(require-init 'init-hydra)  ; hotkey is require-initd everywhere
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-evil<x-color><param>#a626a4</param>)</x-color>    ; <x-color><param>#9ca0a4</param>init-evil dependent on init-clipboard
</x-color>
  ;; <x-color><param>#9ca0a4</param>ediff configuration should be last so it can override
</x-color>  ;; <x-color><param>#9ca0a4</param>the key bindings in previous configuration
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-ediff<x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>@see https://github.com/hlissner/doom-emacs/wiki/FAQ
</x-color>  ;; <x-color><param>#9ca0a4</param>Adding directories under "site-lisp/" to `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>load-path</x-color></x-color><x-color><param>#9ca0a4</param>' slows
</x-color>  ;; <x-color><param>#9ca0a4</param>down all `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>require-init</x-color></x-color><x-color><param>#9ca0a4</param>' statement. So we do this at the end of startup
</x-color>  ;; <x-color><param>#9ca0a4</param>NO ELPA package is dependent on "site-lisp/".

</x-color>  <x-color><param>#a626a4</param>(</x-color>inc0n/add-subdirs-to-load-path <x-color><param>#50a14f</param>(</x-color>file-name-as-directory inc0n/site-lisp-dir<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-flymake<x-color><param>#a626a4</param>)</x-color>


  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>when</x-color> <x-color><param>#50a14f</param>(</x-color>file-exists-p custom-file<x-color><param>#50a14f</param>)</x-color>
	<x-color><param>#50a14f</param>(</x-color>load custom-file<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>


;;; <x-color><param>#9ca0a4</param>Local Variables:
</x-color>;;; <x-color><param>#9ca0a4</param>no-byte-compile: t
</x-color>;;; <x-color><param>#9ca0a4</param>End:
</x-color><x-color><param>#4078f2</param>(</x-color>put 'erase-buffer 'disabled nil<x-color><param>#4078f2</param>)</x-color>
