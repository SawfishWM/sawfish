;; razor.jl -- Razor-Qt integration

;; Copyright (C) 2012 Christopher Roy Bratusek <nano@tuxfamily.org>

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

(define-structure sawfish.wm.integration.razor

    (export detect-razor)

    (open rep
	  rep.system
          sawfish.wm.menus
	  sawfish.wm.misc
          sawfish.wm.custom
	  sawfish.wm.windows
          sawfish.wm.commands
          sawfish.wm.commands.launcher)

  (define-structure-alias razor-int sawfish.wm.integration.mate)

  (define (init)
    (let (menu)
      (setq desktop-environment "razor")
      (setq want-poweroff-menu nil)

      ;; invoke the Razor-Qt terminal instead of xterm
      ;;
      ;; XXX Razor-Qt does not come with a default terminal-emulator
      ;; XXX candidate for int: konsole
      ;;
      ;; (unless (variable-customized-p 'xterm-program)
      ;; 	(setq xterm-program "razor-terminal"))

      ;; use the Razor-Qt help browser and url launcher
      ;;
      ;; XXX Razor-Qt does not come with a default browser
      ;; XXX candidate for int: QupZilla
      ;;
      ;; (unless (variable-customized-p 'browser-program)
      ;; 	(setq browser-program "razor-www-browser"))

      ;; add some Razor-Qt menu-entries
      (when (setq menu (assoc (_ "_Help") root-menu))
	(nconc menu `(()
		      (,(_ "Razor-Qt _Website") (browser "http://www.razor-qt.org")))))

      ;; add razor-logout and customize menu-entries
      (when (setq menu (assoc (_ "Sessi_on") root-menu))
	(nconc menu `(()
		      (,(_ "_Customize Razor-Qt") (system "razor-config &"))
		      ;; XXX candidate: kmenuedit
		      ;; (,(_ "_Edit Razor-Qt menu") (system " &"))
		      ()
		      (,(_ "_Logout from Razor-Qt")
		       (system "razor-power logout &"))
		      (,(_ "_Reboot from Razor-Qt")
		       (system "razor-power reboot &"))
		      (,(_ "_Shutdown from Razor-Qt")
		       (system "razor-power shutdown &"))
		      (,(_ "S_uspend from Razor-Qt")
		       (system "razor-power suspend &"))
		      (,(_ "_Hibernate from Razor-Qt")
		       (system "razor-power hibernate &")))))))

  ;; Returns nil if razor is not found.
  ;; If detected, returns t, and do also razor support init.
  (define (detect-razor)
    (when (or (equal (getenv "XDG_CURRENT_DESKTKOP") "Razor")
	      (equal (getenv "DESKTOP_SESSION") "razor")
	      (get-window-by-class "Razor-desktop" #:regex t))
      (init)
      t)))
