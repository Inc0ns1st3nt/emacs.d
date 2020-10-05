Content-Type: text/enriched
Text-Width: 70

;; <x-color><param>#9ca0a4</param>-*- coding: utf-8; lexical-binding: t; -*-
</x-color>
<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defvar</x-color> <x-color><param>#6a1868</param>best-gc-cons-threshold</x-color> 4000000
  <x-color><param>#84888b</param>"Best default gc threshold value.  Should NOT be too big!"</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defvar</x-color> <x-color><param>#6a1868</param>inc0n/debug</x-color> nil
  <x-color><param>#84888b</param>"Enable debug mode."</x-color><x-color><param>#4078f2</param>)</x-color>

;; <x-color><param>#9ca0a4</param>don't GC during startup to save time
</x-color><x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>setq</x-color> gc-cons-threshold most-positive-fixnum<x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defvar</x-color> <x-color><param>#6a1868</param>inc0n/emacs-load-start-time</x-color> <x-color><param>#a626a4</param>(</x-color>current-time<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#84888b</param>"The time that emacs started as time object"</x-color><x-color><param>#4078f2</param>)</x-color>

;;<x-color><param>#9ca0a4</param>----------------------------------------------------------------------------
</x-color>;; <x-color><param>#9ca0a4</param>Which functionality to enable (use t or nil for true and false)
</x-color>;;<x-color><param>#9ca0a4</param>----------------------------------------------------------------------------
</x-color><x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defvar</x-color> <x-color><param>#6a1868</param>*no-memory*</x-color> nil<x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defconst</x-color> <x-color><param>#6a1868</param>inc0n/emacs-d</x-color> <x-color><param>#a626a4</param>(</x-color>file-name-as-directory user-emacs-directory<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#84888b</param>"Directory of emacs.d"</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defconst</x-color> <x-color><param>#6a1868</param>inc0n/site-lisp-dir</x-color> <x-color><param>#a626a4</param>(</x-color>concat inc0n/emacs-d <x-color><param>#50a14f</param>"site-lisp"</x-color><x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#84888b</param>"Directory of site-lisp"</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defconst</x-color> <x-color><param>#6a1868</param>inc0n/lisp-dir</x-color> <x-color><param>#a626a4</param>(</x-color>concat inc0n/emacs-d <x-color><param>#50a14f</param>"lisp"</x-color><x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#84888b</param>"Directory of lisp"</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defun</x-color> <x-color><param>#a626a4</param>inc0n/vc-merge-p</x-color> <x-color><param>#a626a4</param>()</x-color>
  <x-color><param>#84888b</param>"Use Emacs for git merge only?"</x-color>
  <x-color><param>#a626a4</param>(</x-color>boundp 'startup-now<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defun</x-color> <x-color><param>#a626a4</param>require-init</x-color> <x-color><param>#a626a4</param>(</x-color>pkg <x-color><param>#986801</param>&optional</x-color> maybe-disabled<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#84888b</param>"Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."</x-color>
  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>when</x-color> <x-color><param>#50a14f</param>(</x-color><x-color><param>#e45649</param>or</x-color> <x-color><param>#da8548</param>(</x-color>not maybe-disabled<x-color><param>#da8548</param>)</x-color>

            <x-color><param>#da8548</param>(</x-color>not <x-color><param>#b751b6</param>(</x-color>inc0n/vc-merge-p<x-color><param>#b751b6</param>)</x-color><x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color>
    <x-color><param>#50a14f</param>(</x-color>load <x-color><param>#da8548</param>(</x-color>file-truename <x-color><param>#b751b6</param>(</x-color>format <x-color><param>#50a14f</param>"%s/%s"</x-color> inc0n/lisp-dir pkg<x-color><param>#b751b6</param>)</x-color><x-color><param>#da8548</param>)</x-color>

          t t<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>defun</x-color> <x-color><param>#a626a4</param>local-require</x-color> <x-color><param>#a626a4</param>(</x-color>pkg<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#84888b</param>"Require PKG in site-lisp directory."</x-color>
  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>unless</x-color> <x-color><param>#50a14f</param>(</x-color><x-color><param>#e45649</param>featurep</x-color> <x-color><param>#b751b6</param>pkg</x-color><x-color><param>#50a14f</param>)</x-color>
    <x-color><param>#50a14f</param>(</x-color>load <x-color><param>#da8548</param>(</x-color>expand-file-name
           <x-color><param>#b751b6</param>(</x-color><x-color><param>#e45649</param>if</x-color> <x-color><param>#986801</param>(</x-color>eq pkg 'go-mode-load<x-color><param>#986801</param>)</x-color>
               <x-color><param>#986801</param>(</x-color>format <x-color><param>#50a14f</param>"%s/go-mode/%s"</x-color> inc0n/site-lisp-dir pkg<x-color><param>#986801</param>)</x-color>
             <x-color><param>#986801</param>(</x-color>format <x-color><param>#50a14f</param>"%s/%s/%s"</x-color> inc0n/site-lisp-dir pkg pkg<x-color><param>#986801</param>)</x-color><x-color><param>#b751b6</param>)</x-color><x-color><param>#da8548</param>)</x-color>
          t t<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>

;; <x-color><param>#9ca0a4</param>(setq garbage-collection-messages t) ; for debug
</x-color><x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>setq</x-color> best-gc-cons-threshold <x-color><param>#a626a4</param>(</x-color>* 64 1024 1024<x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>
<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>setq</x-color> gc-cons-percentage 0.5<x-color><param>#4078f2</param>)</x-color>
<x-color><param>#4078f2</param>(</x-color>run-with-idle-timer 5 t #'garbage-collect<x-color><param>#4078f2</param>)</x-color>

;; <x-color><param>#9ca0a4</param>@see https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
</x-color>;; <x-color><param>#9ca0a4</param>Normally file-name-handler-alist is set to
</x-color>;; <x-color><param>#9ca0a4</param>(("\\`/[</x-color><x-color><param>#4078f2</param><x-color><param>#9ca0a4</param>^</x-color></x-color><x-color><param>#9ca0a4</param>/]*\\'" . tramp-completion-file-name-handler)
</x-color>;; <x-color><param>#9ca0a4</param>("\\`/[</x-color><x-color><param>#4078f2</param><x-color><param>#9ca0a4</param>^</x-color></x-color><x-color><param>#9ca0a4</param>/|:][</x-color><x-color><param>#4078f2</param><x-color><param>#9ca0a4</param>^</x-color></x-color><x-color><param>#9ca0a4</param>/|]*:" . tramp-file-name-handler)
</x-color>;; <x-color><param>#9ca0a4</param>("\\`/:" . file-name-non-special))
</x-color>;; <x-color><param>#9ca0a4</param>Which means on every .el and .elc file loaded during start up, it has to runs those regexps against the filename.
</x-color><x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>let</x-color> <x-color><param>#a626a4</param>(</x-color><x-color><param>#50a14f</param>(</x-color>file-name-handler-alist nil<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>;; {{
</x-color>  ;; <x-color><param>#9ca0a4</param>(require 'benchmark-init-modes)
</x-color>  ;; <x-color><param>#9ca0a4</param>(require 'benchmark-init)
</x-color>  ;; <x-color><param>#9ca0a4</param>(benchmark-init/activate)
</x-color>  ;; <x-color><param>#9ca0a4</param>;; `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>benchmark-init/show-durations-tree</x-color></x-color><x-color><param>#9ca0a4</param>' to show benchmark result
</x-color>  ;; <x-color><param>#9ca0a4</param>;; }}
</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-autoload<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>`</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>package-initialize</x-color></x-color><x-color><param>#9ca0a4</param>' takes 35% of startup time
</x-color>  ;; <x-color><param>#9ca0a4</param>need check https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast for solution
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-modeline<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-utils<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-file-type<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-elpa<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-exec-path t<x-color><param>#a626a4</param>)</x-color> ;; <x-color><param>#9ca0a4</param>Set up $PATH
</x-color>  ;; <x-color><param>#9ca0a4</param>Any file use flyspell should be initialized after init-spelling.el
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-spelling t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-uniquify t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-ibuffer t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-ivy<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-hippie-expand<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-windows<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-markdown t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-javascript t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-org t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-css t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-python t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-lisp t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-yasnippet t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-cc-mode t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-linum-mode<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-git t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-gtags t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-clipboard<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-ctags t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-bbdb t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-gnus t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-lua-mode t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-workgroups2 t<x-color><param>#a626a4</param>)</x-color> ; <x-color><param>#9ca0a4</param>use native API in lightweight mode
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-term-mode t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-web-mode t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-company t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-chinese t<x-color><param>#a626a4</param>)</x-color> ;; <x-color><param>#9ca0a4</param>cannot be idle-required
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-counsel<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>need statistics of keyfreq asap
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-keyfreq t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-httpd t<x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>projectile costs 7% startup time
</x-color>
  ;; <x-color><param>#9ca0a4</param>don't play with color-theme in light weight mode
</x-color>  ;; <x-color><param>#9ca0a4</param>color themes are already installed in `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>init-elpa.el</x-color></x-color><x-color><param>#9ca0a4</param>'
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-theme<x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>misc has some crucial tools I need immediately
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-essential<x-color><param>#a626a4</param>)</x-color>
  ;; <x-color><param>#9ca0a4</param>handy tools though not must have
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-misc t<x-color><param>#a626a4</param>)</x-color>

  <x-color><param>#a626a4</param>(</x-color>require-init 'init-emacs-w3m t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-shackle t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-dired t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-writting t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-hydra<x-color><param>#a626a4</param>)</x-color>   ; <x-color><param>#9ca0a4</param>hotkey is required everywhere
</x-color>  ;; <x-color><param>#9ca0a4</param>use evil mode (vi key binding)
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-evil<x-color><param>#a626a4</param>)</x-color>    ; <x-color><param>#9ca0a4</param>init-evil dependent on init-clipboard
</x-color>
  ;; <x-color><param>#9ca0a4</param>ediff configuration should be last so it can override
</x-color>  ;; <x-color><param>#9ca0a4</param>the key bindings in previous configuration
</x-color>  <x-color><param>#a626a4</param>(</x-color>require-init 'init-ediff<x-color><param>#a626a4</param>)</x-color>

  ;; <x-color><param>#9ca0a4</param>@see https://github.com/hlissner/doom-emacs/wiki/FAQ
</x-color>  ;; <x-color><param>#9ca0a4</param>Adding directories under "site-lisp/" to `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>load-path</x-color></x-color><x-color><param>#9ca0a4</param>' slows
</x-color>  ;; <x-color><param>#9ca0a4</param>down all `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>require</x-color></x-color><x-color><param>#9ca0a4</param>' statement. So we do this at the end of startup
</x-color>  ;; <x-color><param>#9ca0a4</param>NO ELPA package is dependent on "site-lisp/".
</x-color>  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>setq</x-color> load-path <x-color><param>#50a14f</param>(</x-color>cdr load-path<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>inc0n/add-subdirs-to-load-path <x-color><param>#50a14f</param>(</x-color>file-name-as-directory inc0n/site-lisp-dir<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>require-init 'init-flymake t<x-color><param>#a626a4</param>)</x-color>

  <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>unless</x-color> <x-color><param>#50a14f</param>(</x-color>inc0n/vc-merge-p<x-color><param>#50a14f</param>)</x-color>
    ;; <x-color><param>#9ca0a4</param>@see https://www.reddit.com/r/emacs/comments/4q4ixw/how_to_forbid_emacs_to_touch_configuration_files/
</x-color>    ;; <x-color><param>#9ca0a4</param>See `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>custom-file</x-color></x-color><x-color><param>#9ca0a4</param>' for details.
</x-color>    <x-color><param>#50a14f</param>(</x-color><x-color><param>#e45649</param>setq</x-color> custom-file <x-color><param>#da8548</param>(</x-color>expand-file-name <x-color><param>#b751b6</param>(</x-color>concat inc0n/emacs-d <x-color><param>#50a14f</param>"custom-set-variables.el"</x-color><x-color><param>#b751b6</param>)</x-color><x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color>
    <x-color><param>#50a14f</param>(</x-color><x-color><param>#e45649</param>when</x-color> <x-color><param>#da8548</param>(</x-color>file-exists-p custom-file<x-color><param>#da8548</param>)</x-color>
      <x-color><param>#da8548</param>(</x-color>load custom-file t t<x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color>

    ;; <x-color><param>#9ca0a4</param>my personal setup, other major-mode specific setup need it.
</x-color>    ;; <x-color><param>#9ca0a4</param>It's dependent on *.el in `</x-color><x-color><param>#b751b6</param><x-color><param>#9ca0a4</param>inc0n/site-lisp-dir</x-color></x-color><x-color><param>#9ca0a4</param>'
</x-color>    <x-color><param>#50a14f</param>(</x-color>require-init 'init-custom<x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>
<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>setq</x-color> gc-cons-threshold best-gc-cons-threshold<x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color><x-color><param>#e45649</param>when</x-color> <x-color><param>#a626a4</param>(</x-color><x-color><param>#e45649</param>require</x-color> '<x-color><param>#b751b6</param>time-date</x-color> nil t<x-color><param>#a626a4</param>)</x-color>
  <x-color><param>#a626a4</param>(</x-color>message <x-color><param>#50a14f</param>"Emacs startup time: %s milliseconds."</x-color>
           <x-color><param>#50a14f</param>(</x-color>format-time-string <x-color><param>#50a14f</param>"%3N"</x-color> <x-color><param>#da8548</param>(</x-color>time-since inc0n/emacs-load-start-time<x-color><param>#da8548</param>)</x-color><x-color><param>#50a14f</param>)</x-color><x-color><param>#a626a4</param>)</x-color><x-color><param>#4078f2</param>)</x-color>

<x-color><param>#4078f2</param>(</x-color>custom-set-variables
 ;; <x-color><param>#9ca0a4</param>custom-set-variables was added by Custom.
</x-color> ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
 ;;; <x-color><param>#5C6370</param>Local Variables:
</x-color>;;; <x-color><param>#5C6370</param>no-byte-compile: t
</x-color>;;; <x-color><param>#5C6370</param>End:
</x-color><x-color><param>#61AFEF</param>(</x-color>put 'erase-buffer 'disabled nil<x-color><param>#61AFEF</param>)</x-color>
