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

;; Most of these settings can be overridden through customize

;; the font to use for otherwise unspecified text
(setq default-font (get-font "-*-lucida-medium-r-*-*-10-*-*-*-*-*-*-*"))

;; if you don't use GNOME, remove this (though it does no harm)
(require 'gnome)

;; alternatively, this will probably work if you use GDM, but only
;; sometimes use GNOME
;(when (equal (getenv "GDMSESSION") "Gnome")
;  (require 'gnome))

;; turn on tooltips for first-time users
(require 'tooltips)
(setq tooltips-enabled t)
