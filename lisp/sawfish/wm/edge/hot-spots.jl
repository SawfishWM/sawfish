;; hot-spots.jl 3.0.0 -- perform actions when hitting the screen-edge

;; Copyright (C) 2010 Christopher Roy Bratusek <zanghar@freenet.de>

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

(define-structure sawfish.wm.edge.hot-spots

    (export hot-spot-activate)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.custom
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.workspace)

  (define-structure-alias hot-spots sawfish.wm.edge.hot-spots)

  (defvar left-edge-program (lambda () t)
    "The program launched when hitting the left-edge.")

  (defvar top-left-corner-program (lambda () t)
    "The program launched when hitting the top-left-corner.")

  (defvar top-edge-program (lambda () t)
    "The program launched when hitting the top-edge.")

  (defvar top-right-corner-program (lambda () t)
    "The program launched when hitting the top-right-corner.")

  (defvar right-edge-program (lambda () t)
    "The program launched when hitting the right-edge.")

  (defvar bottom-right-corner-program (lambda () t)
    "The program launched when hitting the bottom-right-corner.")

  (defvar bottom-edge-program (lambda () t)
    "The program launched when hitting the bottom-edge.")

  (defvar bottom-left-corner-program (lambda () t)
    "The program launched when hitting the bottom-left-corner.")

  (define (hot-spot-activate spot)
    (case spot
      ((top-left)
       (funcall top-left-corner-program))
      ((top-right)
       (funcall top-right-corner-program))
      ((bottom-right)
       (funcall bottom-right-corner-program))
      ((bottom-left)
       (funcall bottom-left-corner-program))
      ((left)
       (funcall left-edge-program))
      ((top)
       (funcall top-edge-program))
      ((right)
       (funcall right-edge-program))
      ((bottom)
       (funcall bottom-edge-program)))))
