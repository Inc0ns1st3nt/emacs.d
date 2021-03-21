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

<x-color><param>#014980</param>(</x-color>defvar <x-color><param>#383a42</param>normal-gc-cons-threshold</x-color> <x-color><param>#014980</param>(</x-color>* 48 1024 1024<x-color><param>#014980</param>))</x-color> ;; <x-color><param>#556b2f</param>40mb
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
<x-color><param>#014980</param>(</x-color>defvar <x-color><param>#383a42</param>inc0n/site-lisp-dir</x-color> <x-color><param>#014980</param>(</x-color>expand-file-name <x-color><param>#8a3b3c</param>"site-lisp"</x-color> user-emacs-directory<x-color><param>#014980</param>)</x-color>

  <x-color><param>#485a27</param>"My site directory."</x-color><x-color><param>#014980</param>)</x-color>

<x-color><param>#014980</param>(</x-color>defun local-require <x-color><param>#014980</param>(</x-color>pkg<x-color><param>#014980</param>)</x-color>
  <x-color><param>#485a27</param>"Require PKG in site-lisp directory."</x-color>
  <x-color><param>#014980</param>(</x-color>unless <x-color><param>#014980</param>(</x-color>featurep <x-color><param>#383a42</param>pkg</x-color><x-color><param>#014980</param>)</x-color>
	<x-color><param>#014980</param>(</x-color>let* <x-color><param>#014980</param>((</x-color>pkg <x-color><param>#014980</param>(</x-color>symbol-name pkg<x-color><param>#014980</param>))</x-color>
           <x-color><param>#014980</param>(</x-color>path <x-color><param>#014980</param>(</x-color>expand-file-name pkg inc0n/site-lisp-dir<x-color><param>#014980</param>))</x-color>

           <x-color><param>#014980</param>(</x-color>load-path <x-color><param>#014980</param>(</x-color>cons path load-path<x-color><param>#014980</param>)))</x-color>
	  <x-color><param>#014980</param>(</x-color>load <x-color><param>#014980</param>(</x-color>if <x-color><param>#014980</param>(</x-color>file-exists-p path<x-color><param>#014980</param>)</x-color>
				<x-color><param>#014980</param>(</x-color>expand-file-name pkg path<x-color><param>#014980</param>)</x-color>
			  <x-color><param>#014980</param>(</x-color>file-truename path<x-color><param>#014980</param>))</x-color>
			t nil<x-color><param>#014980</param>))))</x-color>


<x-color><param>#014980</param>(</x-color>defun inc0n/add-subdirs-to-load-path <x-color><param>#014980</param>(</x-color>parent-dir<x-color><param>#014980</param>)</x-color>
  <x-color><param>#485a27</param>"Add every non-hidden subdir of PARENT-DIR to `</x-color><x-color><param>#383a42</param><x-color><param>#485a27</param>load-path</x-color></x-color><x-color><param>#485a27</param>'."</x-color>
  <x-color><param>#014980</param>(</x-color>setq load-path
        <x-color><param>#014980</param>(</x-color>append
         <x-color><param>#014980</param>(</x-color>cl-remove-if-not
          'file-directory-p
          <x-color><param>#014980</param>(</x-color>directory-files <x-color><param>#014980</param>(</x-color>expand-file-name parent-dir<x-color><param>#014980</param>)</x-color> t <x-color><param>#8a3b3c</param>"^[</x-color><x-color><param>#383a42</param><x-color><param>#8a3b3c</param>^</x-color></x-color><x-color><param>#8a3b3c</param>\\.]"</x-color><x-color><param>#014980</param>))</x-color>
         load-path<x-color><param>#014980</param>)))</x-color>


;; <x-color><param>#556b2f</param>@see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
</x-color>;; <x-color><param>#556b2f</param>Normally file-name-handler-alist is set to
</x-color>;; <x-color><param>#556b2f</param>(("\\`/[</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>^</x-color></x-color><x-color><param>#556b2f</param>/]*\\'" . tramp-completion-file-name-handler)
</x-color>;; <x-color><param>#556b2f</param>("\\`/[</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>^</x-color></x-color><x-color><param>#556b2f</param>/|:][</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>^</x-color></x-color><x-color><param>#556b2f</param>/|]*:" . tramp-file-name-handler)
</x-color>;; <x-color><param>#556b2f</param>("\\`/:" . file-name-non-special))
</x-color>;; <x-color><param>#556b2f</param>Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.

</x-color>

<x-color><param>#014980</param>(</x-color>let <x-color><param>#014980</param>((</x-color>file-name-handler-alist nil<x-color><param>#014980</param>)</x-color>

      <x-color><param>#014980</param>(</x-color>load-path <x-color><param>#014980</param>(</x-color>cons inc0n/lisp-dir load-path<x-color><param>#014980</param>)))</x-color>

  <x-color><param>#014980</param>(</x-color>require '<x-color><param>#383a42</param>init-autoload</x-color><x-color><param>#014980</param>)</x-color>
  ;; <x-color><param>#556b2f</param>`</x-color><x-color><param>#383a42</param><x-color><param>#556b2f</param>package-initialize</x-color></x-color><x-color><param>#556b2f</param>' takes 35% of startup time
</x-color>  ;; <x-color><param>#556b2f</param>need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-modeline</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-utils</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-file-type</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-elpa</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-exec-path</x-color><x-color><param>#c099ff</param>)</x-color> ;; <x-color><param>#7a88cf</param>Set up $PATH
</x-color>  ;; <x-color><param>#7a88cf</param>Any file use flyspell should be initialized after init-spelling.el
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-spelling</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-ibuffer</x-color><x-color><param>#c099ff</param>)</x-color>

  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-selectrum</x-color><x-color><param>#c099ff</param>)</x-color>
  ;; <x-color><param>#7a88cf</param>(require 'init-ivy)
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-windows</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-org</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-yasnippet</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-linum-mode</x-color><x-color><param>#c099ff</param>)</x-color>

  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-company</x-color><x-color><param>#c099ff</param>)</x-color>


  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-markdown</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-javascript</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-css</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-python</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-lisp</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-cc-mode</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-git</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-gtags</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-lua-mode</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-term-mode</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-web-mode</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-haskell</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-latex</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-pdf</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-emacs-w3m</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#61AFEF</param>(</x-color><x-color><param>#C678DD</param>require</x-color> '<x-color><param>#56B6C2</param>init-eww</x-color><x-color><param>#61AFEF</param>)</x-color>


  <x-color><param>#61AFEF</param>(</x-color><x-color><param>#C678DD</param>require</x-color> '<x-color><param>#56B6C2</param>init-tags</x-color><x-color><param>#61AFEF</param>)</x-color>
  ;; <x-color><param>#5C6370</param>(require 'init-bbdb)
</x-color>  ;; <x-color><param>#7a88cf</param>(require 'init-gnus)
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-chinese</x-color><x-color><param>#c099ff</param>)</x-color> ;; <x-color><param>#7a88cf</param>cannot be idle-required
</x-color>  ;; <x-color><param>#7a88cf</param>(require 'init-counsel)
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-keyfreq</x-color><x-color><param>#c099ff</param>)</x-color> ;; <x-color><param>#7a88cf</param>need statistics of keyfreq asap
</x-color>  ;; <x-color><param>#7a88cf</param>(require 'init-httpd)

</x-color>  ;; <x-color><param>#7a88cf</param>projectile costs 7% startup time

</x-color>

  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-flycheck</x-color><x-color><param>#c099ff</param>)</x-color>

  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-theme</x-color><x-color><param>#c099ff</param>)</x-color>     ;; <x-color><param>#7a88cf</param>don't play with color-theme in light weight mode
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-essential</x-color><x-color><param>#c099ff</param>)</x-color> ;; <x-color><param>#7a88cf</param>essential has some crucial tools I need immediately
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-misc</x-color><x-color><param>#c099ff</param>)</x-color>      ;; <x-color><param>#7a88cf</param>misc, handy tools though not must have

</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-shackle</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-dired</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-writting</x-color><x-color><param>#c099ff</param>)</x-color>
  ;; <x-color><param>#7a88cf</param>(require 'init-hydra)  ; hotkey is required everywhere
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-clipboard</x-color><x-color><param>#c099ff</param>)</x-color>
  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-evil</x-color><x-color><param>#c099ff</param>)</x-color>

  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-transient</x-color><x-color><param>#c099ff</param>)</x-color>

  ;; <x-color><param>#7a88cf</param>ediff configuration should be last so it can override
</x-color>  ;; <x-color><param>#7a88cf</param>the key bindings in previous configuration
</x-color>  <x-color><param>#c099ff</param>(require</x-color> '<x-color><param>#ff995e</param>init-ediff</x-color><x-color><param>#c099ff</param>)</x-color>

  ;; <x-color><param>#7a88cf</param>@see https://github.com/hlissner/doom-emacs/wiki/FAQ
</x-color>  ;; <x-color><param>#7a88cf</param>Adding directories under "site-lisp/" to `<x-color><param>#ff995e</param>load-path</x-color>' slows
</x-color>  ;; <x-color><param>#7a88cf</param>down all `<x-color><param>#ff995e</param>require</x-color>' statement. So we do this at the end of startup
</x-color>  ;; <x-color><param>#7a88cf</param>NO ELPA package is dependent on "site-lisp/".
</x-color>  ;; <x-color><param>#7a88cf</param>(inc0n/add-subdirs-to-load-path (file-name-as-directory inc0n/site-lisp-dir))

</x-color>

  <x-color><param>#c099ff</param>(when</x-color> <x-color><param>#c099ff</param>(</x-color>file-exists-p custom-file<x-color><param>#c099ff</param>)</x-color>
    <x-color><param>#c099ff</param>(</x-color>load custom-file<x-color><param>#c099ff</param>)))</x-color>


;;; <x-color><param>#7a88cf</param>Local Variables:
</x-color>;;; <x-color><param>#7a88cf</param>no-byte-compile: t
</x-color>;;; <x-color><param>#7a88cf</param>End:
</x-color><x-color><param>#c099ff</param>(</x-color>put 'erase-buffer 'disabled t<x-color><param>#c099ff</param>)</x-color>
