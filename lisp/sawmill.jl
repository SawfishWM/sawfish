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

;; load standard libraries
(require 'custom)
(require 'functions)
(require 'cursors)
(require 'focus)
(require 'keymaps)
(require 'transient)
(require 'frames)
(require 'workspace)
(require 'stacking)
(require 'place-window)
(require 'mwm)
(require 'open-look)
(require 'server)

(load-all "autoload.jl" t)
(load-all (concat "os-" (symbol-name operating-system)) t)

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
	  (load "~/.sawmillrc" t t)

	  ;; use a default theme if none given
	  (unless default-frame-style
	    (set-frame-style 'absolute-e))))
    (error
     (format (stderr-file) "error in local config--> %S\n" error-data))))

;; If we connected with a session manager, initialise our state
(require 'sm-init)

;; Use all arguments which are left.
(let
    (arg)
  (while (setq arg (car command-line-args))
    (setq command-line-args (cdr command-line-args))
    (cond
      ((equal "-f" arg)
       (setq arg (car command-line-args))
       (setq command-line-args (cdr command-line-args))
       (funcall (read-from-string arg)))
      ((equal "-l" arg)
       (setq arg (car command-line-args))
       (setq command-line-args (cdr command-line-args))
       (load arg))
      ((equal "-q" arg)
       (throw 'quit 0))
      (t
       (load arg)))))
