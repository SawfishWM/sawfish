;; gnome-int.jl -- more GNOME integration
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; load the GNOME-wm interaction code
(require 'gnome)

;; set the apps menu to the GNOME version
(require 'gnome-menu)

;; delete the `Restart' and `Quit' items from the root menu
(require 'menus)
(let
    ((restart (rassoc '(restart) root-menu))
     (quit (rassoc '(quit) root-menu)))
  (when restart
    (setq root-menu (delq restart root-menu)))
  (when quit
    (setq root-menu (delq quit root-menu)))
  (when (null (last root-menu))
    (setq root-menu (delq (last root-menu) root-menu))))

;; invoke the GNOME control-center to configure sawfish
(eval-when-compile (require 'customize))

(defcustom gnome-use-capplet t
  "Invoke the GNOME control center through the `Customize' menu."
  :type boolean
  :group misc
  :require gnome-int
  :after-set (lambda () (gnome-use-capplet-changed)))

(defun gnome-use-capplet-changed ()
  (if gnome-use-capplet
      (progn
	(setq customize-program "sawfish-capplet")
	(setq customize-group-opt "--sawfish-group")
	(setq custom-menu-includes-all-settings nil))
    (setq customize-program "sawfish-ui")
    (setq customize-group-opt "--group")
    (setq custom-menu-includes-all-settings t)))

(gnome-use-capplet-changed)

;; invoke the GNOME terminal instead of xterm
(setq xterm-program "gnome-terminal")

;; use the GNOME help browser and url launcher
(require 'help)
(setq help-display-info-function help-call-info-gnome)
(setq display-url-command "gnome-moz-remote --newwin '%s'")

;; add some GNOME help menus
(let ((menu (assoc (_ "_Help") root-menu)))
  (when menu
    (nconc menu `(()
		  (,(_ "_GNOME Help") gnome-help-browser)
		  (,(_ "GNOME WWW") gnome-www-page)))))

(provide 'gnome-int)
