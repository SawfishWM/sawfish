;; autoload.jl -- Initialise auto-load functions
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

;;; ::autoload-start::
(autoload 'server-open-p "server")
(autoload 'server-open "server" t)
(autoload 'server-reply "server")
(autoload 'move-window-interactively "move-resize" t)
(autoload 'resize-window-interactively "move-resize" t)
(autoload 'move-selected-window "move-resize" t)
(autoload 'resize-selected-window "move-resize" t)
(autoload 'select-window "select-window")
(autoload 'popup-menu "menus")
(autoload 'popup-window-menu "menus" t)
(autoload 'popup-root-menu "menus" t)
;;; ::autoload-end::
