;; sawmill-default.jl -- default user startup
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

;; Commentary:

;; This file provides defaults for users without .sawmillrc files

;; ensure that the things people usually like doing show up in the
;; customization interface
(mapc custom-add-required '(edge-flip match-window move-resize
			    tooltips auto-raise))

;; if it looks like GNOME is the desktop environment, then load the
;; extra GNOME integration module
(unless batch-mode
  (catch 'out
    (mapc (lambda (prop)
	    (when (string-match "^GNOME_" (symbol-name prop))
	      (require 'gnome-int)
	      (throw 'out t)))
	  (list-x-properties 'root))))

;; turn on tooltips for first-time users
(require 'tooltips)
(setq tooltips-enabled t)
