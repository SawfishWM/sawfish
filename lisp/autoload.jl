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
(setq custom-required (cons 'move-resize custom-required))
(autoload 'move-window-interactively "move-resize" t)
(autoload 'resize-window-interactively "move-resize" t)
(autoload 'move-selected-window "move-resize" t)
(autoload 'resize-selected-window "move-resize" t)
(autoload 'select-window "select-window")
(setq custom-required (cons 'menus custom-required))
(autoload 'popup-menu "menus")
(autoload 'popup-window-menu "menus" t)
(autoload 'popup-root-menu "menus" t)
(autoload 'customize "customize" t)
(setq custom-required (cons 'auto-raise custom-required))
(autoload 'maximize-window "maximize" t)
(autoload 'unmaximize-window "maximize" t)
(autoload 'maximize-window-vertically "maximize" t)
(autoload 'maximize-window-horizontally "maximize" t)
(autoload 'maximize-window-toggle "maximize" t)
(autoload 'maximize-window-vertically-toggle "maximize" t)
(autoload 'maximize-window-horizontally-toggle "maximize" t)
(autoload 'load-session "sm-load")
(autoload 'save-session "sm-save")
(autoload 'map-keymap "keymap")
(autoload 'substitute-keymap-command "keymap")
(autoload 'substitute-keymap-event "keymap")
(autoload 'lazy-bind-keys "keymap")
(autoload 'where-is "keymap")
(autoload 'next-workspace-window "cycle" t)
(autoload 'previous-workspace-window "cycle" t)
(autoload 'next-window "cycle" t)
(autoload 'previous-window "cycle" t)
(autoload 'cycle-windows "x-cycle" t)
;;; ::autoload-end::
