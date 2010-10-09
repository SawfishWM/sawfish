;; kde-int.jl -- KDE integration

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

(define-structure sawfish.wm.integration.kde

    (export detect-kde)

    (open rep
	  rep.system
          sawfish.wm.menus
	  sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.commands.launcher
	  sawfish.wm.ext.apps-menu)

  (define-structure-alias kde-int sawfish.wm.integration.kde)

  (defvar kde-desktop-directories
    '("/usr/share/applications/kde4/")
    "KDE specific directories where *.desktop files are stored.")

  (define (init)
    (let (menu
	  kde-logout-cmd)
      (setq desktop-environment "kde")
      (setq want-poweroff-menu nil)
      (setq desktop-directory
	    (append desktop-directory kde-desktop-directories))

      ;; invoke the KDE terminal instead of xterm
      (unless (variable-customized-p 'xterm-program)
	(setq xterm-program "konsole"))

      ;; use the KDE Browser
      (unless (variable-customized-p 'browser-program)
	(setq browser-program "konqueror"))

      ;; add some KDE help menus
      (when (setq menu (assoc (_ "_Help") root-menu))
	(nconc menu `(()
		      (,(_ "_KDE Help") (system "khelpcenter &"))
		      (,(_ "KDE _Website") (browser "http://www.kde.org")))))

      ;; add kde-logout menu item
      (when (setq menu (assoc (_ "Sessi_on") root-menu))
	(setq kde-logout-cmd "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout")
	(nconc menu `(()
		      (,(_ "_Customize KDE") (system "systemsettings &"))
		      ()
		      (,(_ "_Logout from KDE")
		       (system ,(concat kde-logout-cmd " 1 0 -1 &")))
		      (,(_ "_Reboot from KDE")
		       (system ,(concat kde-logout-cmd " 1 1 -1 &")))
		      (,(_ "_Shutdown from KDE")
		       (system ,(concat kde-logout-cmd " 1 2 -1 &"))))))))
  
  ;; Returns nil if kde is not found.
  ;; If detected, returns t, and do also kde support init.
  (define (detect-kde)
    (when (getenv "KDE_FULL_SESSION")
      (init)
      t)))
