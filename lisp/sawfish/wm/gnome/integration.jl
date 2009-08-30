;; gnome-int.jl -- more GNOME integration
;; $Id: integration.jl,v 1.20 2003/08/14 06:55:36 jsh Exp $

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

(define-structure sawfish.wm.gnome.integration ()

    (open rep
	  sawfish.wm.state.gnome
	  sawfish.wm.menus
	  sawfish.wm.custom
	  sawfish.wm.commands.help
	  sawfish.wm.commands.user)

  (define-structure-alias gnome-int sawfish.wm.gnome.integration)

  ;; invoke the GNOME terminal instead of xterm
  (unless (variable-customized-p 'xterm-program)
    (setq xterm-program "x-terminal-emulator"))

  ;; use the GNOME help browser and url launcher
  (setq help-display-info-function help-call-info-gnome)
  (setq display-url-command "gnome-www-browser %s &")

  ;; add some GNOME help menus
  (let ((menu (assoc (_ "_Help") root-menu)))
    (when menu
      (nconc menu `(()
		    (,(_ "_GNOME Help...") gnome-help-browser)
		    (,(_ "GNOME WWW...") gnome-www-page)
		    (,(_ "About GNOME...") gnome-about))))))
