;; defaults.jl -- do some user supports before loading rc file

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

;; Commentary:

;; This used to be read only if .sawfishrc lacks, thus named
;; "defaults", but starting from Sawfish-1.6, it is always read.

(declare (in-module user))

;; magic comment to get an alias installed
;; (define-structure-alias sawfish-defaults sawfish.wm.defaults)

(unless batch-mode

;; if it looks like GNOME is the desktop environment, then load the
;; extra GNOME integration module
  (if (getenv "GNOME_DESKTOP_SESSION_ID")
      (require 'sawfish.wm.integration.gnome)

;; if it looks like KDE is the desktop environment, then load the
;; extra KDE integration module
  (if (getenv "KDE_FULL_SESSION")
      (require 'sawfish.wm.integration.kde)
      
;; if it looks like XFCE is the desktop environment, then load the
;; extra XFCE integration module
  (if (get-x-property 'root '_DT_SAVE_MODE)
      (require 'sawfish.wm.integration.xfce)))))

;; save errors to aid debugging
(require 'sawfish.wm.ext.error-handler)
