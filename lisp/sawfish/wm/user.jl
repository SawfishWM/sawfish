#| user.jl -- do user-local initialization

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;; Commentary:

;; The idea is that the `sawfish-user' module is where all user code
;; gets to play. By default it has a set of modules opened so that
;; it looks pretty much how the old non-modular wm looked.

;; This means that the `sawfish' structure isn't modified by user code,
;; this is a good thing, since it means that modules importing
;; `sawfish' know what they are getting (no random user bindings)

;; The downside is that it's harder for user extensions to redefine
;; existing code, IMHO this may also be a good thing..

(define-structure user ()

    ((open rep
	   rep.regexp
	   rep.system
	   rep.io.files
	   rep.io.processes
	   sawfish.wm
	   sawfish.wm.util.groups
	   sawfish.wm.util.display-window
	   sawfish.wm.util.compat)
     (set-binds))

  ;; frame-style loaded if user hasn't set their own
  (define fallback-frame-style 'microGUI)

  ;; initialize the special variable pointing at this structure
  (structure () (open rep rep.structures)
    (setq *user-module* (get-structure 'user)))

  (define (safe-load . args)
    (condition-case data
	(apply load args)
      (error
       (error-handler-function (car data) (cdr data)))))

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
	  (load-all "site-init" (lambda (f) (load f nil t)))

	  ;; then the users rep configuration, or site-wide defaults
	  (or (safe-load (concat (user-home-directory) ".reprc") t t t)
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
		(load "sawmill/wm/defaults" t))

	      ;; then the customized options
	      (custom-load-user-file)

	      ;; then the sawmill specific user configuration
	      (cond ((rc-file-exists-p "~/.sawfishrc")
		     (safe-load "~/.sawfishrc" t t t))
		    ((rc-file-exists-p "~/.sawmillrc")
		     (safe-load "~/.sawmillrc" t t t))))))
      (error
       (format (stderr-file) "error in local config--> %S\n" error-data))))

  ;; use a default theme if none given
  (unless (or batch-mode default-frame-style)
    (setq default-frame-style fallback-frame-style))

  (unless (and (boundp 'window-menu) window-menu)
    (require 'sawfish.wm.ext.beos-window-menu))

  ;; might it be useful to load the GNOME support?
  (unless batch-mode
    (catch 'out
      (mapc (lambda (prop)
	      (when (string-match "^GNOME_" (symbol-name prop))
		(load-module 'sawfish.wm.state.gnome)
		(throw 'out t)))
	    (list-x-properties 'root))))

  ;; Use all arguments which are left.
  (let ((do-load (lambda (name)
		   (cond ((file-exists-p name)
			  (load name nil t t))
			 ((string-match "\\.jlc?$" name)
			  (load name))
			 (t (require (intern name))))))
	arg)
    (while (setq arg (car command-line-args))
      (setq command-line-args (cdr command-line-args))
      (cond
       ((member arg '("-f" "--call"))
	(setq arg (car command-line-args))
	(setq command-line-args (cdr command-line-args))
	((symbol-value (read-from-string arg))))
       ((member arg '("-l" "--load"))
	(setq arg (car command-line-args))
	(setq command-line-args (cdr command-line-args))
	(do-load arg))
       ((member arg '("-q" "--quit"))
	(throw 'quit 0))
       (t (do-load arg))))))

;; prevent this file being loaded as a module
nil
