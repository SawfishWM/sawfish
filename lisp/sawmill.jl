;; sawmill.jl -- initialisation script
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(setq backtrace-on-error '(bad-arg missing-arg void-value invalid-function))

(provide 'sawmill)
(provide 'sawfish)
(require 'define)			;for old (broken?) reps

;; frame-style loaded if user hasn't set their own
(define fallback-frame-style 'microGUI)

;; quiet autoloading
(setq autoload-verbose nil)

;; so modularised rep knows where to inherit specials and load from
(define *user-structure* 'sawfish)
(define *root-structure* 'sawfish)

;; hack to load setenv etc before autoloads are defined
(load "environ")

;; load always-present session-manager stuff
(require 'sm-init)

;; set $DISPLAY so that any subprocesses inherit it
(setenv "DISPLAY" display-name)

;; load i18n support when necessary
(unless batch-mode
  (let
      ((lang (or (getenv "LANGUAGE") (getenv "LC_ALL")
		 (getenv "LC_MESSAGES") (getenv "LANG")))
       (disable-nls (get-command-line-option "--disable-nls")))

    (when (and lang (not disable-nls) (not (string= lang "C")))
      (require 'gettext)
      (bindtextdomain
       "sawfish" (expand-file-name "../locale" sawfish-lisp-lib-directory))
      (textdomain "sawfish"))))

;; add ~/.sawfish/lisp to load-path for user-code (canonicalize it
;; now to avoid calling tilde file-handler multiple times)
(setq load-path (cons (canonical-file-name "~/.sawfish/lisp") load-path))

;; load standard libraries
(require 'custom)
(require 'functions)
(require 'cursors)
(require 'keymaps)
(require 'configure)
(require 'workspace)
(require 'viewport)
(require 'focus)
(require 'place-window)
(require 'stacking)
(require 'frames)
(require 'group-funs)
(require 'mwm)
(require 'open-look)
(require 'transient)
(require 'iconify)
(require 'shading)
(require 'window-anim)
(require 'server)
(require 'compat)

(define (sawfish-load-all s)
  ;; ensure that files are loaded in the correct structure
  (load-all s (lambda (f) (load f nil t))))

;; all rep-based programs should do this
(sawfish-load-all "autoload")
(sawfish-load-all (concat "os-" (symbol-name operating-system)))

;; this will autoload, but it can be overridden
(define window-menu beos-window-menu)

(require 'sawmill-gaol)

;; Load site specific initialisation. Errors here are trapped since
;; they're probably not going to leave us in an unusable state
(unless (get-command-line-option "--no-rc")
  (condition-case error-data
      (progn
	;; try to rename ~/.sawmill to ~/.sawfish
	(when (and (file-directory-p "~/.sawmill")
		   (not (file-exists-p "~/.sawfish")))
	  (rename-file "~/.sawmill" "~/.sawfish")
	  (message "Renamed directory ~/.sawmill -> ~/.sawfish")
	  (make-symlink "~/.sawmill" ".sawfish")
	  (message "Created .sawmill symlink (delete if unwanted)"))

	;; First the site-wide stuff
	(sawfish-load-all "site-init")

	;; then the users rep configuration, or site-wide defaults
	(or (load (concat (user-home-directory) ".reprc") t t t)
	    (load "rep-defaults" t))

	(unless batch-mode
	  (let ((rc-file-exists-p (lambda (f)
				    (or (file-exists-p f)
					(file-exists-p (concat f ".jl"))
					(file-exists-p (concat f ".jlc"))))))
	    ;; load these before customized settings (but only if there's
	    ;; no .sawmillrc file)
	    (unless (or (rc-file-exists-p "~/.sawfishrc")
			(rc-file-exists-p "~/.sawmillrc"))
	      (load "sawmill-defaults" t))

	    ;; then the customized options
	    (custom-load-user-file)

	    ;; then the sawmill specific user configuration
	    (cond ((rc-file-exists-p "~/.sawfishrc")
		   (load "~/.sawfishrc" t t t))
		  ((rc-file-exists-p "~/.sawmillrc")
		   (load "~/.sawmillrc" t t t))))))
    (error
     (format (stderr-file) "error in local config--> %S\n" error-data))))

;; use a default theme if none given
(unless (or batch-mode default-frame-style)
  (set-frame-style fallback-frame-style))

;; might it be useful to load the GNOME support?
(unless batch-mode
  (catch 'out
    (mapc (lambda (prop)
	    (when (string-match "^GNOME_" (symbol-name prop))
	      (require 'gnome)
	      (throw 'out t)))
	  (list-x-properties 'root))))

;; now connect with the session manager; gsm requires that apps don't
;; connect until they're ready to handle the later priority levels
(let
    ((tem (or (get-command-line-option "--sm-client-id" t)
	      ;; may be passed through from the default GNOME session
	      (get-command-line-option "-clientId" t))))
  (when (and (not batch-mode) (getenv "SESSION_MANAGER"))
    (sm-init tem)))

;; Use all arguments which are left.
(let
    ((do-load (lambda (name)
		(cond ((file-exists-p name)
		       (load name nil t t))
		      ((string-match "\\.jlc?$" name)
		       (load name))
		      (t (require (intern name))))))
     arg)
  (while (setq arg (car command-line-args))
    (setq command-line-args (cdr command-line-args))
    (cond
      ((equal "-f" arg)
       (setq arg (car command-line-args))
       (setq command-line-args (cdr command-line-args))
       ((symbol-value (read-from-string arg))))
      ((equal "-l" arg)
       (setq arg (car command-line-args))
       (setq command-line-args (cdr command-line-args))
       (do-load arg))
      ((equal "-q" arg)
       (throw 'quit 0))
      (t
       (do-load arg)))))
