#| sawfish.wm bootstrap

   $Id$

   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawmill.

   sawmill is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawmill is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawmill; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(declare (in-module sawfish.wm))

(open-structures '(rep))

;;(setq debug-on-error '(bad-arg missing-arg void-value invalid-function))

;; so modularised rep knows where to inherit specials and load from
(setq *root-structure* 'sawfish.wm)
(setq *user-structure* 'user)

;; load always-present session-manager stuff
(require 'sawfish.wm.session.init)

;; set $DISPLAY so that any subprocesses inherit it
(setenv "DISPLAY" display-name)

;; load i18n support when necessary
(unless batch-mode
  (let ((lang (or (getenv "LANGUAGE") (getenv "LC_ALL")
		  (getenv "LC_MESSAGES") (getenv "LANG")))
	(disable-nls (get-command-line-option "--disable-nls")))

    (when (and lang (not disable-nls) (not (string= lang "C")))
      (require 'rep.i18n.gettext)
      (bindtextdomain
       "sawfish" (expand-file-name "../locale" sawfish-lisp-lib-directory))
      (textdomain "sawfish"))))

;; add ~/.sawfish/lisp to load-path for user-code (canonicalize it
;; now to avoid calling tilde file-handler multiple times)
(setq load-path (cons (canonical-file-name "~/.sawfish/lisp") load-path))

;; import libraries that may be needed by autoload files
(require 'sawfish.wm.commands)
(require 'sawfish.wm.custom)
(require 'sawfish.wm.focus)
(require 'sawfish.wm.placement)
(require 'sawfish.wm.window-anim)

;; load standard libraries (but don't import them)
(mapc intern-structure '(sawfish.wm.keymaps
			 sawfish.wm.workspace
			 sawfish.wm.viewport
			 sawfish.wm.stacking
			 sawfish.wm.frames
			 sawfish.wm.swapper
			 sawfish.wm.state.configure
			 sawfish.wm.state.mwm
			 sawfish.wm.state.open-look
			 sawfish.wm.state.transient
			 sawfish.wm.state.shading
			 sawfish.wm.state.iconify
			 sawfish.wm.server))

;; create the exports from sawfish.wm
(open-structures '(sawfish.wm.colors
		   sawfish.wm.commands
		   sawfish.wm.cursors
		   sawfish.wm.custom
		   sawfish.wm.events
		   sawfish.wm.focus
		   sawfish.wm.fonts
		   sawfish.wm.frames
		   sawfish.wm.gaol
		   sawfish.wm.images
		   sawfish.wm.misc
		   sawfish.wm.placement
		   sawfish.wm.session.init
		   sawfish.wm.server
		   sawfish.wm.stacking
		   sawfish.wm.viewport
		   sawfish.wm.window-anim
		   sawfish.wm.windows
		   sawfish.wm.workspace
		   sawfish.wm.state.iconify
		   sawfish.wm.state.shading
		   sawfish.wm.state.transient))

(export-bindings (parse-interface
		  '(compound-interface
		    (structure-interface sawfish.wm.colors)
		    (structure-interface sawfish.wm.commands)
		    (structure-interface sawfish.wm.cursors)
		    (structure-interface sawfish.wm.custom)
		    (structure-interface sawfish.wm.events)
		    (structure-interface sawfish.wm.focus)
		    (structure-interface sawfish.wm.fonts)
		    (structure-interface sawfish.wm.frames)
		    (structure-interface sawfish.wm.gaol)
		    (structure-interface sawfish.wm.images)
		    (structure-interface sawfish.wm.misc)
		    (structure-interface sawfish.wm.placement)
		    (structure-interface sawfish.wm.session.init)
		    (structure-interface sawfish.wm.server)
		    (structure-interface sawfish.wm.stacking)
		    (structure-interface sawfish.wm.viewport)
		    (structure-interface sawfish.wm.window-anim)
		    (structure-interface sawfish.wm.windows)
		    (structure-interface sawfish.wm.workspace)
		    (structure-interface sawfish.wm.state.iconify)
		    (structure-interface sawfish.wm.state.shading)
		    (structure-interface sawfish.wm.state.transient))))

(let ((sawfish-load-all (lambda (s)
			  ;; ensure files are loaded in the correct structure
			  (load-all s (lambda (f) (load f nil t))))))

  ;; all rep-based programs should do this
  (sawfish-load-all "sawfish/wm/autoload")
  (sawfish-load-all (concat "os-" (symbol-name operating-system))))

(let ((session-id (or (get-command-line-option "--sm-client-id" t)
		      ;; may be passed through from the default GNOME session
		      (get-command-line-option "-clientId" t))))

  ;; do user-level initialization
  (load "sawfish/wm/user")

  ;; now connect with the session manager; gsm requires that apps don't
  ;; connect until they're ready to handle the later priority levels
  (when (and (not batch-mode) (getenv "SESSION_MANAGER"))
    (sm-init session-id)))
