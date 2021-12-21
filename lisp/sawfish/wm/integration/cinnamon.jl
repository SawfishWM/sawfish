;; cinnamon.jl -- Cinnamon integration

;; Copyright (C) 2021 Christopher Roy Bratusek <nano@jpberlin.de>

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

(define-structure sawfish.wm.integration.cinnamon

    (export detect-cinnamon)

    (open rep
	  rep.system
          sawfish.wm.menus
	  sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.commands.launcher)

  (define-structure-alias cinnamon-int sawfish.wm.integration.cinnamon)

  (define (init)
    (let (menu)
      (setq desktop-environment "cinnamon")
      (setq want-poweroff-menu nil)

      ;; invoke the Cinnamon terminal instead of xterm
      (unless (variable-customized-p 'xterm-program)
	(setq xterm-program "gnome-terminal"))

      ;; use the Cinnamon help browser and url launcher
      (unless (variable-customized-p 'browser-program)
	(setq browser-program "x-www-browser"))

      ;; use the Cinnamon filemanager
      (unless (variable-customized-p 'filemanager-program)
	(setq filemanager-program "nemo"))

      ;; add some Cinnamon menu-entries
      (when (setq menu (assoc (_ "_Help") root-menu))
	(nconc menu `(()
		      (,(_ "_Cinnamon Help") (system "yelp &"))
		      (,(_ "Cinnamon _Website") (browser "https://projects.linuxmint.com/cinnamon/"))
		      (,(_ "_About Cinnamon") (system "cinnamon-about &")))))

      ;; add mate-logout and customize menu-entries
      (when (setq menu (assoc (_ "Sessi_on") root-menu))
	(nconc menu `(()
		      (,(_ "_Customize Cinnamon") (system "cinnamon-settings &"))
		      (,(_ "_Edit Cinnamon menu") (system "cinnamon-menu-editor &"))
		      ()
		      (,(_ "L_ock screen from Cinnamon")
			(system "cinnamon-screensaver-command -l &"))
		      (,(_ "_Logout from Cinnamon")
		       (system "cinnamon-session-quit --logout &"))
		      (,(_ "_Shutdown from Cinnamon")
		       (system "cinnamon-session-quit --power-off &")))))))

  ;; Returns nil if Cinnamon is not found.
  ;; If detected, returns t, and do also mate support init.
  (define (detect-mate)
    (when (or (equal (getenv "XDG_CURRENT_DESKTOP") "X-Cinnamon")
              (equal (getenv "XDG_SESSION_DESKTOP") "cinnamon")
              (equal (getenv "XDG_SESSION_DESKTOP") "sawfish-cinnamon"))
      (init)
      t)))
