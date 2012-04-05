;; kde-int.jl -- KDE integration

;; Copyright (C) 2010 Christopher Roy Bratusek <nano@tuxfamily.org>

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

    (export detect-kde
	    kde-late-init)

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

      ;; invoke the KDE terminal instead of xterm
      (unless (variable-customized-p 'xterm-program)
	(setq xterm-program "konsole"))

      ;; use the KDE Browser
      (unless (variable-customized-p 'browser-program)
	(setq browser-program "konqueror"))

      ;; use the KDE filemanager
      (unless (variable-customized-p 'filemanager-program)
	(setq filemanager-program "dolphin"))

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
		      (,(_ "_Edit KDE menu") (system "kmenuedit &"))
		      ()
		      (,(_ "L_ock screen from KDE")
			(system "qdbus org.kde.krunner /ScreenSaver Lock &"))
		      (,(_ "S_witch User from KDE")
			(system "qdbus org.kde.krunner /App switchUser &"))
		      ()
		      (,(_ "_Logout from KDE")
		       (system ,(concat kde-logout-cmd " 1 0 -1 &")))
		      (,(_ "_Reboot from KDE")
		       (system ,(concat kde-logout-cmd " 1 1 -1 &")))
		      (,(_ "_Shutdown from KDE")
		       (system ,(concat kde-logout-cmd " 1 2 -1 &")))
		      (,(_ "S_uspend from KDE")
			(system "qdbus org.kde.kded /org/freedesktop/PowerManagement Suspend &"))
		      (,(_ "_Hibernate from KDE")
			(system "qdbus org.kde.kded /org/freedesktop/PowerManagement Hibernate &")))))))

  ;; Returns nil if kde is not found.
  ;; If detected, returns t, and do also kde support init.
  (define (detect-kde)
    (when (or (equal (getenv "XDG_CURRENT_DESKTOP") "KDE")
              (getenv "KDE_FULL_SESSION"))
      (init)
      t))

  ;; Should be called after user customization is read.
  (define (kde-late-init)
    (setq desktop-directory
	  (append desktop-directory kde-desktop-directories))))
