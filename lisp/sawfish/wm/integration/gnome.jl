;; gnome.jl -- GNOME integration

;; Copyright (C) 2010 Christopher Roy Bratusek <nano@jpberlin.de>

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

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

  (define (init)
    (let (menu)
      (setq desktop-environment "gnome")
      (setq want-poweroff-menu nil)

      ;; invoke the GNOME terminal instead of xterm
      (unless (variable-customized-p 'xterm-program)
	(setq xterm-program "gnome-terminal.wrapper"))

      ;; use the GNOME help browser and url launcher
      (unless (variable-customized-p 'browser-program)
	(setq browser-program "gnome-www-browser"))

      ;; use the GNOME filemanager
      (unless (variable-customized-p 'filemanager-program)
	(setq filemanager-program "nautilus"))

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
		      (,(_ "_Edit GNOME menu") (system "alacarte &"))
		      ()
		      (,(_ "L_ock screen from GNOME")
			(system "gnome-screensaver-command -l &"))
		      (,(_ "_Logout from GNOME")
		       (system "gnome-session-save --logout-dialog &"))
		      (,(_ "_Shutdown from GNOME")
		       (system "gnome-session-save --shutdown-dialog &")))))))

  ;; Returns nil if gnome is not found.
  ;; If detected, returns t, and do also gnome support init.
  (define (detect-gnome)
    (when (or (equal (getenv "XDG_CURRENT_DESKTOP") "GNOME")
              (getenv "GNOME_DESKTOP_SESSION_ID"))
      (init)
      t)))
