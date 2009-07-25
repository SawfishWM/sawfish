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
;; FIXME: add support for GNOME 2 applications
	  sawfish.wm.gnome.menus
	  sawfish.wm.menus
	  sawfish.wm.custom
	  sawfish.wm.commands.help
	  sawfish.wm.commands.xterm)

  (define-structure-alias gnome-int sawfish.wm.gnome.integration)

  ;; delete the `Restart' and `Quit' items from the root menu
  (let ((restart (rassoc '(restart) root-menu))
	(quit (rassoc '(quit) root-menu)))
;;	(programs (rassoc 'apps-menu root-menu)))
    (when restart
      (setq root-menu (delq restart root-menu)))
    (when quit
      (setq root-menu (delq quit root-menu)))
    ;; FIXME: until GNOME 2 applications are supported
;;    (when programs
;;      (setq root-menu (delq programs root-menu)))
    (when (null (last root-menu))
      (setq root-menu (delq (last root-menu) root-menu))))

  ;; this option was removed for gnome2
  (put 'gnome-use-capplet 'custom-obsolete t)

  ;; invoke the GNOME terminal instead of xterm
  (unless (variable-customized-p 'xterm-program)
    (setq xterm-program "x-terminal-emulator"))

  ;; use the GNOME help browser and url launcher
  (setq help-display-info-function help-call-info-gnome)
  (setq display-url-command "gnome-moz-remote --newwin '%s'")

  ;; add some GNOME help menus
  (let ((menu (assoc (_ "_Help") root-menu)))
    (when menu
      (nconc menu `(()
		    (,(_ "_GNOME Help...") gnome-help-browser)
		    (,(_ "GNOME WWW...") gnome-www-page)
		    (,(_ "About GNOME...") gnome-about))))))
