;; lxde.jl -- LXDE integration

;; Copyright (C) 2012 Christopher Roy Bratusek <nano@jpberlin.de>

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

(define-structure sawfish.wm.integration.lxde

    (export detect-lxde)

    (open rep
	  rep.system
          sawfish.wm.menus
	  sawfish.wm.misc
          sawfish.wm.custom
	  sawfish.wm.windows
          sawfish.wm.commands
          sawfish.wm.commands.launcher)

  (define-structure-alias lxde-int sawfish.wm.integration.lxde)

  (define (init)
    (let (menu)
      (setq desktop-environment "lxde")
      (setq want-poweroff-menu nil)

      ;; invoke the LXDE terminal instead of xterm
        (unless (variable-customized-p 'xterm-program)
		(setq xterm-program "lxterm"))

      ;; use the LXDE help browser and url launcher
      ;;
      ;; XXX LXDE does not come with a default browser
      ;; XXX candidate for int: ??
      ;;
      ;; (unless (variable-customized-p 'browser-program)
      ;; 	(setq browser-program "lxde-www-browser"))

      ;; use the LXDE filemanager
      (unless (variable-customized-p 'filemanager-program)
	(setq filemanager-program "pcmanfm"))

      ;; add some LXDE menu-entries
      (when (setq menu (assoc (_ "_Help") root-menu))
	(nconc menu `(()
		      (,(_ "LXDE _Website") (browser "http://www.lxde.org")))))

      ;; add lxde-logout and customize menu-entries
      (when (setq menu (assoc (_ "Sessi_on") root-menu))
	(nconc menu `(()
		      (,(_ "_LXSession Logout") (system "lxsession-logout &")))))))

  ;; Returns nil if lxde is not found.
  ;; If detected, returns t, and do also lxde support init.
  (define (detect-lxde)
    (when (or (equal (getenv "XDG_CURRENT_DESKTKOP") "LXDE")
	      (equal (getenv "DESKTOP_SESSION") "LXDE"))
      (init)
      t)))
