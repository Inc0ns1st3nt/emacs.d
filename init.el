Content-Type: text/enriched
Text-Width: 70

;;; <x-color><param>Firebrick</param>init.el --- Load the full configuration -*- lexical-binding: t -*-

</x-color>
;;; <x-color><param>Firebrick</param>Commentary:

</x-color>
;; <x-color><param>Firebrick</param>This file bootstraps the configuration, which is divided into
</x-color>;; <x-color><param>Firebrick</param>a number of other files.
</x-color>
;;; <x-color><param>Firebrick</param>Code:
</x-color>
;; <x-color><param>Firebrick</param>Produce backtraces when errors occur
</x-color><x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>setq</x-color> debug-on-error t<x-color><param>#707183</param>)</x-color>


<x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>defconst</x-color> <x-color><param>sienna</param>*spell-check-support-enabled*</x-color> nil<x-color><param>#707183</param>)</x-color> ;; <x-color><param>Firebrick</param>Enable with t if you prefer

</x-color>
;;<x-color><param>Firebrick</param>----------------------------------------------------------------------------
</x-color>;; <x-color><param>Firebrick</param>Adjust garbage collection thresholds during startup, and thereafter
</x-color>;;<x-color><param>Firebrick</param>----------------------------------------------------------------------------
</x-color><x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>let</x-color> <x-color><param>#7388d6</param>(</x-color><x-color><param>#909183</param>(</x-color>normal-gc-cons-threshold <x-color><param>#709870</param>(</x-color>* 24 1024 1024<x-color><param>#709870</param>)</x-color><x-color><param>#909183</param>)</x-color> ;; <x-color><param>Firebrick</param>20mb
</x-color>      <x-color><param>#909183</param>(</x-color>init-gc-cons-threshold <x-color><param>#709870</param>(</x-color>* 128 1024 1024<x-color><param>#709870</param>)</x-color><x-color><param>#909183</param>)</x-color><x-color><param>#7388d6</param>)</x-color> ;; <x-color><param>Firebrick</param>128mb
</x-color>  <x-color><param>#7388d6</param>(</x-color><x-color><param>Purple</param>setq</x-color> gc-cons-threshold init-gc-cons-threshold<x-color><param>#7388d6</param>)</x-color>
  ;; <x-color><param>Firebrick</param>(setq garbage-collection-messages t) ; for debug
</x-color>  <x-color><param>#7388d6</param>(</x-color><x-color><param>Purple</param>setq</x-color> gc-cons-percentage 0.5<x-color><param>#7388d6</param>)</x-color>
  ;; <x-color><param>Firebrick</param>(run-with-idle-timer 5 t #'garbage-collect)
</x-color>  <x-color><param>#7388d6</param>(</x-color>add-hook 'emacs-startup-hook
            <x-color><param>#909183</param>(</x-color><x-color><param>Purple</param>lambda</x-color> <x-color><param>#709870</param>()</x-color>

              <x-color><param>#709870</param>(</x-color><x-color><param>Purple</param>setq</x-color> gc-cons-threshold normal-gc-cons-threshold<x-color><param>#709870</param>)</x-color>

              <x-color><param>#709870</param>(</x-color>message <x-color><param>VioletRed4</param>"startup time: %s"</x-color> <x-color><param>#907373</param>(</x-color>emacs-init-time<x-color><param>#907373</param>)</x-color><x-color><param>#709870</param>)</x-color><x-color><param>#909183</param>)</x-color><x-color><param>#7388d6</param>)</x-color><x-color><param>#707183</param>)</x-color>


;;<x-color><param>Firebrick</param>----------------------------------------------------------------------------
</x-color>;; <x-color><param>Firebrick</param>Bootstrap config
</x-color>;;<x-color><param>Firebrick</param>----------------------------------------------------------------------------
</x-color><x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>setq</x-color> custom-file <x-color><param>#7388d6</param>(</x-color>expand-file-name <x-color><param>VioletRed4</param>"custom.el"</x-color> user-emacs-directory<x-color><param>#7388d6</param>)</x-color><x-color><param>#707183</param>)</x-color>


<x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>defun</x-color> <x-color><param>Blue1</param>inc0n/vc-merge-p</x-color> <x-color><param>#7388d6</param>()</x-color>
  "Use Emacs for git merge only?"
  <x-color><param>#7388d6</param>(</x-color>boundp 'startup-now<x-color><param>#7388d6</param>)</x-color><x-color><param>#707183</param>)</x-color>


;; <x-color><param>Firebrick</param>(defalias 'require-init

</x-color>;;   <x-color><param>Firebrick</param>(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))

</x-color>;; <x-color><param>Firebrick</param>	(lambda (pkg &optional maybe-disabled)

</x-color>;; <x-color><param>Firebrick</param>	  (unless maybe-disabled
</x-color>;; <x-color><param>Firebrick</param>		(load (file-truename (format "%s/%s" lisp-dir pkg))
</x-color>;; <x-color><param>Firebrick</param>			  t t))))

</x-color>;;   <x-color><param>Firebrick</param>"Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly.")

</x-color>

<x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>defvar</x-color> <x-color><param>sienna</param>inc0n/lisp-dir</x-color> <x-color><param>#7388d6</param>(</x-color>expand-file-name <x-color><param>VioletRed4</param>"lisp"</x-color> user-emacs-directory<x-color><param>#7388d6</param>)</x-color><x-color><param>#707183</param>)</x-color>

<x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>defun</x-color> <x-color><param>Blue1</param>require-init</x-color> <x-color><param>#7388d6</param>(</x-color>pkg <x-color><param>ForestGreen</param>&optional</x-color> maybe-disabled<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color><x-color><param>Purple</param>unless</x-color> maybe-disabled
    <x-color><param>#909183</param>(</x-color>load <x-color><param>#709870</param>(</x-color>file-truename <x-color><param>#907373</param>(</x-color>format <x-color><param>VioletRed4</param>"%s/%s"</x-color> inc0n/lisp-dir pkg<x-color><param>#907373</param>)</x-color><x-color><param>#709870</param>)</x-color>

          t t<x-color><param>#909183</param>)</x-color><x-color><param>#7388d6</param>)</x-color><x-color><param>#707183</param>)</x-color>


;; <x-color><param>Firebrick</param>@see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
</x-color>;; <x-color><param>Firebrick</param>Normally file-name-handler-alist is set to
</x-color>;; <x-color><param>Firebrick</param>(("\\`/[</x-color><x-color><param>Firebrick</param>^</x-color><x-color><param>Firebrick</param>/]*\\'" . tramp-completion-file-name-handler)
</x-color>;; <x-color><param>Firebrick</param>("\\`/[</x-color><x-color><param>Firebrick</param>^</x-color><x-color><param>Firebrick</param>/|:][</x-color><x-color><param>Firebrick</param>^</x-color><x-color><param>Firebrick</param>/|]*:" . tramp-file-name-handler)
</x-color>;; <x-color><param>Firebrick</param>("\\`/:" . file-name-non-special))
</x-color>;; <x-color><param>Firebrick</param>Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
</x-color><x-color><param>#707183</param>(</x-color><x-color><param>Purple</param>let</x-color>

	<x-color><param>#7388d6</param>(</x-color><x-color><param>#909183</param>(</x-color>file-name-handler-alist nil<x-color><param>#909183</param>)</x-color><x-color><param>#7388d6</param>)</x-color>

  <x-color><param>#7388d6</param>(</x-color>require-init 'init-autoload<x-color><param>#7388d6</param>)</x-color>
  ;; <x-color><param>Firebrick</param>`</x-color><x-color><param>dark cyan</param><x-color><param>Firebrick</param>package-initialize</x-color></x-color><x-color><param>Firebrick</param>' takes 35% of startup time
</x-color>  ;; <x-color><param>Firebrick</param>need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
</x-color>  <x-color><param>#7388d6</param>(</x-color>require-init 'init-modeline<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-utils<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-file-type<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-elpa<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-exec-path<x-color><param>#7388d6</param>)</x-color> ;; <x-color><param>Firebrick</param>Set up $PATH
</x-color>  ;; <x-color><param>Firebrick</param>Any file use flyspell should be initialized after init-spelling.el
</x-color>  <x-color><param>#7388d6</param>(</x-color>require-init 'init-spelling<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-uniquify<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-ibuffer<x-color><param>#7388d6</param>)</x-color>

  <x-color><param>#7388d6</param>(</x-color>require-init 'init-selectrum<x-color><param>#7388d6</param>)</x-color>
  ;; <x-color><param>Firebrick</param>(require-init 'init-ivy)
</x-color>  ;; <x-color><param>Firebrick</param>(require-init 'init-hippie-expand)
</x-color>  <x-color><param>#7388d6</param>(</x-color>require-init 'init-windows<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-markdown<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-javascript<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-org<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-css<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-python<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-lisp<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-yasnippet<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-cc-mode<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-linum-mode<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-git<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-gtags<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-clipboard<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-ctags<x-color><param>#7388d6</param>)</x-color>
  ;; <x-color><param>Firebrick</param>(require-init 'init-bbdb)
</x-color>  <x-color><param>#7388d6</param>(</x-color>require-init 'init-gnus<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-lua-mode<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-term-mode<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-web-mode<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-company<x-color><param>#7388d6</param>)</x-color>
  <x-color><param>#7388d6</param>(</x-color>require-init 'init-chinese<x-color><param>#7388d6</param>)</x-color> ;; <x-color><param>Firebrick</param>cannot be idle-require-initd
</x-color>  ;; <x-color><param>Firebrick</param>(require-init 'init-counsel)
</x-color>  ;; <x-color><param>#5C6370</param>need statistics of keyfreq asap
</x-color>  <x-color><param>#98C379</param>(</x-color>require-init 'init-keyfreq<x-color><param>#98C379</param>)</x-color>
  ;; <x-color><param>#5C6370</param>(require-init 'init-httpd)

</x-color>  ;; <x-color><param>#5C6370</param>projectile costs 7% startup time

</x-color>

  <x-color><param>#98C379</param>(</x-color>require-init 'init-pdf<x-color><param>#98C379</param>)</x-color>

  ;; <x-color><param>#5C6370</param>don't play with color-theme in light weight mode
</x-color>  ;; <x-color><param>#5C6370</param>color themes are already installed in `<x-color><param>#56B6C2</param>init-elpa.el</x-color>'
</x-color>  <x-color><param>#98C379</param>(</x-color>require-init 'init-theme<x-color><param>#98C379</param>)</x-color>

  ;; <x-color><param>#5C6370</param>misc has some crucial tools I need immediately
</x-color>  <x-color><param>#98C379</param>(</x-color>require-init 'init-essential<x-color><param>#98C379</param>)</x-color>
  ;; <x-color><param>#5C6370</param>handy tools though not must have
</x-color>  <x-color><param>#98C379</param>(</x-color>require-init 'init-misc<x-color><param>#98C379</param>)</x-color>

  ;; <x-color><param>#5C6370</param>(require-init 'init-emacs-w3m)
</x-color>  <x-color><param>#98C379</param>(</x-color>require-init 'init-shackle<x-color><param>#98C379</param>)</x-color>
  <x-color><param>#98C379</param>(</x-color>require-init 'init-dired<x-color><param>#98C379</param>)</x-color>
  <x-color><param>#98C379</param>(</x-color>require-init 'init-writting<x-color><param>#98C379</param>)</x-color>
  ;; <x-color><param>#5C6370</param>(require-init 'init-hydra)  ; hotkey is require-initd everywhere
</x-color>  ;; <x-color><param>#5C6370</param>use evil mode (vi key binding)
</x-color>  <x-color><param>#98C379</param>(</x-color>require-init 'init-evil<x-color><param>#98C379</param>)</x-color>    ; <x-color><param>#5C6370</param>init-evil dependent on init-clipboard
</x-color>
  ;; <x-color><param>#5C6370</param>ediff configuration should be last so it can override
</x-color>  ;; <x-color><param>#5C6370</param>the key bindings in previous configuration
</x-color>  <x-color><param>#98C379</param>(</x-color>require-init 'init-ediff<x-color><param>#98C379</param>)</x-color>

  ;; <x-color><param>#5C6370</param>@see https://github.com/hlissner/doom-emacs/wiki/FAQ
</x-color>  ;; <x-color><param>#5C6370</param>Adding directories under "site-lisp/" to `<x-color><param>#56B6C2</param>load-path</x-color>' slows
</x-color>  ;; <x-color><param>#5C6370</param>down all `<x-color><param>#56B6C2</param>require-init</x-color>' statement. So we do this at the end of startup
</x-color>  ;; <x-color><param>#5C6370</param>NO ELPA package is dependent on "site-lisp/".
</x-color>  <x-color><param>#98C379</param>(</x-color><x-color><param>#C678DD</param>setq</x-color> load-path <x-color><param>#D19A66</param>(</x-color>cdr load-path<x-color><param>#D19A66</param>)</x-color><x-color><param>#98C379</param>)</x-color>
  <x-color><param>#98C379</param>(</x-color>inc0n/add-subdirs-to-load-path <x-color><param>#D19A66</param>(</x-color>file-name-as-directory inc0n/site-lisp-dir<x-color><param>#D19A66</param>)</x-color><x-color><param>#98C379</param>)</x-color>
  <x-color><param>#98C379</param>(</x-color>require-init 'init-flymake<x-color><param>#98C379</param>)</x-color>


  <x-color><param>#98C379</param>(</x-color><x-color><param>#C678DD</param>when</x-color> <x-color><param>#D19A66</param>(</x-color>file-exists-p custom-file<x-color><param>#D19A66</param>)</x-color>
	<x-color><param>#D19A66</param>(</x-color>load custom-file<x-color><param>#D19A66</param>)</x-color><x-color><param>#98C379</param>)</x-color><x-color><param>#61AFEF</param>)</x-color>


;;; <x-color><param>#5C6370</param>Local Variables:
</x-color>;;; <x-color><param>#5C6370</param>no-byte-compile: t
</x-color>;;; <x-color><param>#5C6370</param>End:
</x-color><x-color><param>#61AFEF</param>(</x-color>put 'erase-buffer 'disabled nil<x-color><param>#61AFEF</param>)</x-color>
