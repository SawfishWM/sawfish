;; xfce.jl -- more XFCE integration

;; Copyright (C) 2010 Christopher Bratusek <zanghar@freenet.de>

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

(define-structure sawfish.wm.integration.xfce

    (export )

    (open rep
          sawfish.wm.menus
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.commands.launcher)

  (define-structure-alias gnome-int sawfish.wm.integration.xfce)

  (defvar-setq want-poweroff-menu nil)
  (defvar-setq desktop-environment "xfce")

  ;; invoke the GNOME terminal instead of xterm
  (unless (variable-customized-p 'xterm-program)
    (setq xterm-program "xfce4-terminal"))

  ;; use the GNOME help browser and url launcher
  (unless (variable-customized-p 'browser-program)
    (setq browser-program "midori"))

  ;; add some GNOME help menus
  (let ((menu (assoc (_ "_Help") root-menu)))
    (when menu
      (nconc menu `(()
		    (,(_ "_XFCE Help") (system "xfhelp4 &"))
		    (,(_ "XFCE _Website") (browser "http://www.xfce.org"))
		    (,(_ "_About XFCE") (system "xfce4-about &"))))))

  ;; add gnome-logout menu item
  (let ((menu (assoc (_ "Sessi_on") root-menu)))
    (when menu
      (nconc menu `(()
                    (,(_ "_Customize XFCE") (system "xfce4-settings-manager &"))
                    ()
                    (,(_ "_Logout from XFCE")
                     (system "xfce4-session-logout --logout &"))
                    (,(_ "_Reboot from XFCE")
                     (system "xfce4-session-logout --reboot &"))
                    (,(_ "_Shutdown from XFCE")
                     (system "xfce4-session-logout --halt &")))))))
