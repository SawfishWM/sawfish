;; sawmill-default.jl -- default user startup
;; $Id: defaults.jl,v 1.19 2000/11/15 21:51:02 jsh Exp $

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

(declare (in-module user))

;; magic comment to get an alias installed
;; (define-structure-alias sawmill-defaults sawfish.wm.defaults)

;; this is probably good for novice users?
(require 'sawfish.wm.ext.window-history)

;; save errors to aid debugging
(require 'sawfish.wm.ext.error-handler)
