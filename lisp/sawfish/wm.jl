;; Initiates lisp part of Sawfish.
;;
;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(declare (in-module sawfish.wm))

(open-structures '(rep rep.system rep.io.files))

;;(setq debug-on-error '(bad-arg missing-arg void-value invalid-function))

;; so modularised rep knows where to inherit specials and load from
;; this will be changed to 'user later
(setq *user-structure* 'sawfish.wm)

;; Expand tilde for efficiency. Otherwise, tilde file-handler is called
;; everytime in path search.
(setq load-path (mapcar canonical-file-name load-path))

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
      (bindtextdomain "sawfish" sawfish-locale-directory)
      (when (boundp 'bindtextdomaincodeset)
	(bindtextdomaincodeset "sawfish" "UTF-8"))
      (textdomain "sawfish"))))

;; ignore file errors on stdio streams
(when (boundp 'set-file-ignore-errors)
  (set-file-ignore-errors (stdin-file) t)
  (set-file-ignore-errors (stdout-file) t)
  (set-file-ignore-errors (stderr-file) t))

;; used to mark variable declarations that need special attention
;; from the sawfish-xgettext script
(defmacro i18n-defvar args (cons 'defvar args))
(defmacro i18n-define args (cons 'define args))
(export-bindings '(i18n-defvar i18n-define))

;; import libraries needed by autoload
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
			 sawfish.wm.state.maximize
			 sawfish.wm.state.mwm
			 sawfish.wm.state.open-look
			 sawfish.wm.state.transient
			 sawfish.wm.state.shading
			 sawfish.wm.state.iconify
			 sawfish.wm.state.ignored
			 sawfish.wm.server
			 sawfish.wm.state.wm-spec
			 sawfish.wm.edge.actions))

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
		   sawfish.wm.state.transient
		   sawfish.wm.state.ignored
		   sawfish.wm.edge.actions))

;; Bindings in these modules are exported by sawfish.wm on behalf of
;; them. User scripts have to import only sawfish.wm for core functions.
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
		    (structure-interface sawfish.wm.state.transient)
		    (structure-interface sawfish.wm.state.ignored)
		    (structure-interface sawfish.wm.edge.actions))))

(let ((sawfish-load-all (lambda (s)
			  ;; ensure files are loaded in the correct structure
			  (load-all s (lambda (f) (load f nil t))))))

  ;; all rep-based programs should do this
  (sawfish-load-all "sawfish/wm/autoload")
  (sawfish-load-all (concat "os-" (symbol-name operating-system))))

(let ((session-id     (get-command-line-option "--sm-client-id" t))
      (session-prefix (get-command-line-option "--sm-prefix" t)))

  ;; Now connect with the session manager; gsm requires that apps don't
  ;; connect until they're ready to handle the later priority levels

  ;; NB: Even if you want to defer sm-init after sawfish.wm.user,
  ;; initialization of related command line arguments has to be done
  ;; here. Read also the comment below.
  (when (and (not batch-mode) (getenv "SESSION_MANAGER"))
    (sm-init session-id session-prefix)))

;; do user-level initialization
;; This swallows all command line arguments, including those starting
;; with "--".
(load "sawfish/wm/user")
