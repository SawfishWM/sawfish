;; keymaps.jl -- the default keymaps
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

(provide 'keymaps)

(defvar global-keymap (make-sparse-keymap)
  "Keymap containing bindings active anywhere.")

(defvar root-window-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the pointer is in the root window.")

(defvar title-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the pointer is in the title of
a window.")

(defvar iconify-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the pointer is in the iconify
button of a window.")

(defvar maximize-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the pointer is in the  maximize
button of a window.")

(defvar close-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the pointer is in the close button
of a window.")

(defvar menu-button-keymap (make-sparse-keymap)
  "Keymap containing bindings active when the pointer is in the menu button
of a window.")

(defvar window-keymap (make-sparse-keymap)
  "Keymap containing bindings active when a client window is focused.")


;; Arrange for window-keymap to be set in each window

(defun keymap-add-window (w)
  (unless (window-get w 'keymap)
    (window-put w 'keymap window-keymap)))

(add-hook 'add-window-hook 'keymap-add-window)


;; some bindings

(bind-keys title-keymap
  "Button3-Off" 'raise-lower-window
  "Button1-Move" 'move-window-interactively
  "Button2-Move" 'resize-window-interactively)

(bind-keys window-keymap
  "C-M-Up" 'raise-window
  "C-M-Down" 'lower-window
  "C-M-Left" 'send-to-previous-workspace
  "C-M-Right" 'send-to-next-workspace
  "M-Button1-Click1" 'move-window-interactively
  "M-Button2-Click1" 'nop		;ensure we get the ButtonRelease
  "M-Button2-Off" 'popup-window-menu
  "M-Button3-Click1" 'raise-lower-window)

(bind-keys global-keymap
  "C-Left" 'previous-workspace
  "C-Right" 'next-workspace
  "C-M-ESC" 'quit)

(bind-keys root-window-keymap
  "Button2-Click1" 'popup-root-menu
  "Button2-Off" 'nop)			;so it doesn't get proxyed

(bind-keys close-button-keymap
  "Button1-Off" 'delete-window
  "Button3-Off" 'popup-window-menu)

(bind-keys iconify-button-keymap
  "Button1-Off" 'iconify-window
  "Button3-Off" 'delete-window)

(bind-keys menu-button-keymap
  "Button1-Off" 'popup-window-menu
  "Button3-Off" 'delete-window)
