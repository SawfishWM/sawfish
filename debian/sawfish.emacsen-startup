;; -*-emacs-lisp-*-

(setq load-path (cons (concat "/usr/share/"
                              (symbol-name flavor)
			      "/site-lisp/sawfish") load-path))

(autoload 'sawfish-mode "sawfish" "sawfish-mode" t)

(setq auto-mode-alist (cons '("\\.sawfishrc$"  . sawfish-mode) auto-mode-alist)
auto-mode-alist (cons '("\\.jl$"         . sawfish-mode) auto-mode-alist)
auto-mode-alist (cons '("\\.sawfish/rc$" . sawfish-mode) auto-mode-alist))
