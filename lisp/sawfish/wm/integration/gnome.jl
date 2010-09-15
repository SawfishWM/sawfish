;; gnome.jl -- GNOME integration

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.integration.gnome

    (export detect-gnome)

    (open rep
	  rep.system
          sawfish.wm.menus
	  sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.commands.launcher)

  (define-structure-alias gnome-int sawfish.wm.integration.gnome)

  ;; Returns t or nil. If detected, do also gnome support init.
  (define (detect-gnome)
    (if (getenv "GNOME_DESKTOP_SESSION_ID")
	(let (menu)
	  (setq desktop-environment "gnome")
	  (setq want-poweroff-menu nil)

	  ;; invoke the GNOME terminal instead of xterm
	  (unless (variable-customized-p 'xterm-program)
	    (setq xterm-program "gnome-terminal.wrapper"))

	  ;; use the GNOME help browser and url launcher
	  (unless (variable-customized-p 'browser-program)
	    (setq browser-program "gnome-www-browser"))

	  ;; add some GNOME menu-entries
	  (when (setq menu (assoc (_ "_Help") root-menu))
	    (nconc menu `(()
			  (,(_ "_GNOME Help") (system "yelp &"))
			  (,(_ "GNOME _Website") (browser "http://www.gnome.org"))
			  (,(_ "_About GNOME") (system "gnome-about &")))))

	  ;; add gnome-logout and customize menu-entries
	  (when (setq menu (assoc (_ "Sessi_on") root-menu))
	    (nconc menu `(()
			  (,(_ "_Customize GNOME") (system "gnome-control-center &"))
			  ()
			  (,(_ "_Logout from GNOME")
			   (system "gnome-session-save --logout-dialog &"))
			  (,(_ "_Shutdown from GNOME")
			   (system "gnome-session-save --shutdown-dialog &")))))
	  t)
      ;; no, gnome not found
      nil)))
