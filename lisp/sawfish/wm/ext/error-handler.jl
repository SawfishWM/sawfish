;; error-handler.jl -- replace the standard rep error handler
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

(define-structure sawfish.wm.ext.error-handler ()

    (open rep
	  rep.system
	  rep.io.streams
	  sawfish.wm.misc)

  (define-structure-alias error-handler sawfish.wm.ext.error-handler)

  (defvar error-handler-beep t)

  (define (handler err data)
    (when error-handler-beep
      (beep))
    (display-message (format nil "%s: %S" err data)))

  (setq error-handler-function handler))
