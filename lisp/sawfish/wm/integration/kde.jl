;; kde-int.jl -- more KDE integration

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

    (export )

    (open rep
          sawfish.wm.menus
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.commands.launcher)

  (define-structure-alias kde-int sawfish.wm.integration.kde)

  (define-special-variable want-poweroff-menu nil)

  ;; invoke the KDE terminal instead of xterm
  (unless (variable-customized-p 'xterm-program)
    (setq xterm-program "konsole"))

  ;; use the KDE Browser
  (unless (variable-customized-p 'browser-program)
    (setq browser-program "konqueror"))

  ;; add some KDE help menus
  (let ((menu (assoc (_ "_Help") root-menu)))
    (when menu
      (nconc menu `(()
		    (,(_ "_KDE Help") (system "khelpcenter &"))
		    (,(_ "KDE Website") (browser "http://www.kde.org"))))))

  ;; add kde-logout menu item
  (let ((menu (assoc (_ "Sessi_on") root-menu))
        (kde-logout-cmd "qdbus org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout"))
    (when menu
      (nconc menu `(()
                    (,(_ "_Logout from KDE")
                     (system ,(concat kde-logout-cmd " 1 0 -1 &")))
                    (,(_ "_Reboot from KDE")
                     (system ,(concat kde-logout-cmd " 1 1 -1 &")))
                    (,(_ "_Shutdown from KDE")
                     (system ,(concat kde-logout-cmd " 1 2 -1 &"))))))))
