;; sawmill-default.jl -- User startup script if ~/.sawmillrc doesn't exist
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

;; open the server, use the sawmill-client program to connect to the
;; running window manager
(server-open)

;; use sloppy-focus
(setq sloppy-focus t)

;; the font to use for otherwise unspecified text
(setq default-font (get-font "-*-lucida-medium-r-*-*-10-*-*-*-*-*-*-*"))

;; load a theme, take your pick from the few choices..
(load "absolute-e")
;(load "brushed-metal")
;(load "simple")

;; ignore windows matching these regexps
(setq ignored-window-names
      (cons "^(xload|xmeter|Dali Clock|xapm|xbuffy|console)$"
	    ignored-window-names))

;; these windows are sticky
(setq sticky-window-names
      (cons "^(xload|xmeter|Dali Clock|xapm|xbuffy|console)$"
	    sticky-window-names))

;; if you use GNOME, uncomment this
;(require 'gnome)

;; alternatively, this will probably work if you use GDM, but only
;; sometimes use GNOME
;(when (equal (getenv "GDMSESSION") "Gnome")
;  (require 'gnome))
