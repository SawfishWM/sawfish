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
(autoload 'popup-menu "menus")
(autoload 'popup-window-menu "menus" t)
(autoload 'popup-root-menu "menus" t)
(autoload 'popup-apps-menu "menus" t)
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
(autoload 'read-event "keymap")
(autoload 'next-workspace-window "cycle" t)
(autoload 'previous-workspace-window "cycle" t)
(autoload 'next-window "cycle" t)
(autoload 'previous-window "cycle" t)
(autoload 'cycle-windows "x-cycle" t)
(autoload 'cycle-group "x-cycle" t)
(autoload 'place-window-first-fit "smart-placement")
(autoload 'place-window-best-fit "smart-placement")
(autoload 'place-window-first-fit-or-interactive "smart-placement")
(setq custom-required (cons 'edge-flip custom-required))
(autoload 'iconify-group "groups" t)
(autoload 'uniconify-group "groups" t)
(autoload 'make-group-sticky "groups" t)
(autoload 'make-group-unsticky "groups" t)
(autoload 'toggle-group-sticky "groups" t)
(autoload 'send-group-to-workspace "groups")
(autoload 'send-group-to-current-workspace "groups" t)
(autoload 'send-group-to-next-workspace "groups" t)
(autoload 'send-group-to-previous-workspace "groups" t)
(autoload 'move-group-to-current-viewport "groups" t)
(autoload 'move-group-viewport "groups")
(autoload 'move-group-left "groups" t)
(autoload 'move-group-right "groups" t)
(autoload 'move-group-up "groups" t)
(autoload 'move-group-down "groups" t)
(autoload 'raise-group "groups" t)
(autoload 'lower-group "groups" t)
(autoload 'raise-group-depth "groups" t)
(autoload 'lower-group-depth "groups" t)
(autoload 'set-group-frame-style "groups")
(autoload 'slide-window-left "slide-window" t)
(autoload 'slide-window-right "slide-window" t)
(autoload 'slide-window-up "slide-window" t)
(autoload 'slide-window-down "slide-window" t)
(autoload 'slide-group-left "slide-window" t)
(autoload 'slide-group-right "slide-window" t)
(autoload 'slide-group-up "slide-window" t)
(autoload 'slide-group-down "slide-window" t)
(autoload 'make-window-ignored "ignore-window" t)
(autoload 'make-window-not-ignored "ignore-window" t)
(autoload 'toggle-window-ignored "ignore-window" t)
(autoload 'gnome-toggle-skip-winlist "gnome-commands" t)
(autoload 'gnome-set-skip-winlist "gnome-commands" t)
(autoload 'gnome-clear-skip-winlist "gnome-commands" t)
(autoload 'gnome-toggle-skip-tasklist "gnome-commands" t)
(autoload 'gnome-set-skip-tasklist "gnome-commands" t)
(autoload 'gnome-clear-skip-tasklist "gnome-commands" t)
(autoload 'add-window-matcher "match-window")
(autoload 'remove-window-matcher "match-window")
(setq custom-required (cons 'match-window custom-required))
(autoload 'gnome-logout "gnome-commands" t)
;;; ::autoload-end::
