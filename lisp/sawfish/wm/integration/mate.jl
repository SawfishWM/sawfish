;; mate.jl -- MATE integration

;; Copyright (C) 2011 Christopher Roy Bratusek <nano@tuxfamily.org>

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

(define-structure sawfish.wm.integration.mate

    (export detect-mate)

    (open rep
	  rep.system
          sawfish.wm.menus
	  sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands
          sawfish.wm.commands.launcher)

  (define-structure-alias mate-int sawfish.wm.integration.mate)

  (define (init)
    (let (menu)
      (setq desktop-environment "mate")
      (setq want-poweroff-menu nil)

      ;; invoke the MATE terminal instead of xterm
      (unless (variable-customized-p 'xterm-program)
	(setq xterm-program "mate-terminal"))

      ;; use the MATE help browser and url launcher
      (unless (variable-customized-p 'browser-program)
	(setq browser-program "mate-www-browser"))

      ;; add some MATE menu-entries
      (when (setq menu (assoc (_ "_Help") root-menu))
	(nconc menu `(()
		      (,(_ "_MATE Help") (system "yelp &"))
		      (,(_ "MATE _Website") (browser "http://www.matsusoft.com.ar/projects/mate/"))
		      (,(_ "_About MATE") (system "mate-about &")))))

      ;; add mate-logout and customize menu-entries
      (when (setq menu (assoc (_ "Sessi_on") root-menu))
	(nconc menu `(()
		      (,(_ "_Customize MATE") (system "mate-control-center &"))
		      (,(_ "_Edit MATE menu") (system "alacarte &"))
		      ()
		      (,(_ "L_ock screen from MATE")
			(system "mate-screensaver-command -l &"))
		      (,(_ "_Logout from MATE")
		       (system "mate-session-save --logout-dialog &"))
		      (,(_ "_Shutdown from MATE")
		       (system "mate-session-save --shutdown-dialog &")))))))

  ;; Returns nil if mate is not found.
  ;; If detected, returns t, and do also mate support init.
  (define (detect-mate)
    (when (or (equal (getenv "XDG_CURRENT_DESKTOP") "MATE")
              (getenv "MATE_DESKTOP_SESSION_ID"))
      (init)
      t)))
