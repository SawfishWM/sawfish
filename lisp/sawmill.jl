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

(provide 'sawmill)

;; frame-style loaded if user hasn't set their own
(defvar fallback-frame-style 'microGUI)

;; quiet autoloading
(setq autoload-verbose nil)

;; XXX backwards compatibility, remove at some point
(unless (boundp 'define-value)
  (setq define-value set))

;; hack to load setenv etc before autoloads are defined
(load "environ")

;; load always-present session-manager stuff
(require 'sm-init)

;; set $DISPLAY so that any subprocesses inherit it
(setenv "DISPLAY" display-name)

;; load i18n support when necessary
(unless batch-mode
  (let
      ((lang (or (getenv "LC_ALL") (getenv "LC_MESSAGES") (getenv "LANG"))))
    (when (and (not (get-command-line-option "--disable-nls"))
	       lang (not (string= lang "C")))
      (require 'gettext)
      (bindtextdomain
       "sawmill" (expand-file-name "../locale" sawmill-lisp-lib-directory))
      (textdomain "sawmill"))))

;; load standard libraries
(require 'custom)
(require 'functions)
(require 'cursors)
(require 'keymaps)
(require 'configure)
(require 'sawmill-gaol)
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
(require 'server)
(require 'compat)

;; all rep-based programs should do this
(load-all "autoload.jl" t)
(load-all (concat "os-" (symbol-name operating-system)) t)

;; ensure that the things people usually like doing show up in the
;; customization interface
(mapc custom-add-required '(edge-flip match-window move-resize
                            tooltips auto-raise shade-hover))

;; Load site specific initialisation. Errors here are trapped since
;; they're probably not going to leave us in an unusable state
(unless (get-command-line-option "--no-rc")
  (condition-case error-data
      (progn
	;; First the site-wide stuff
	(load-all "site-init")

	;; then the users rep configuration, or site-wide defaults
	(or (load (concat (user-home-directory) ".reprc") t t)
	    (load "rep-defaults" t))

	(unless batch-mode
	  ;; load these before customized settings (but only if there's
	  ;; no .sawmillrc file)
	  (unless (or (file-exists-p "~/.sawmillrc")
		      (file-exists-p "~/.sawmillrc.jl")
		      (file-exists-p "~/.sawmillrc.jlc"))
	    (load "sawmill-defaults" t))

	  ;; then the customized options
	  (custom-load-user-file)

	  ;; then the sawmill specific user configuration
	  (load "~/.sawmillrc" t t)))
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
    (arg)
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
       (load arg))
      ((equal "-q" arg)
       (throw 'quit 0))
      (t
       (load arg)))))
