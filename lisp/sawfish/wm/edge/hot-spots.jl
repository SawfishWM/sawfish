;; hot-spots.jl 3.0.0 -- Invoke user functions when hitting the screen-edge

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

    (export hot-spot-invoke)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.custom
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.workspace)

  (define-structure-alias hot-spots sawfish.wm.edge.hot-spots)

  (defcustom hot-spot-delay 250
    "Delay (in milliseconds) of hot-spot."
    :group edge-actions
    :type number
    :range (0 . nil))

  (defvar left-edge-function nil
    "The function launched when hitting the left-edge.")

  (defvar top-left-corner-function nil
    "The function launched when hitting the top-left-corner.")

  (defvar top-edge-function nil
    "The function launched when hitting the top-edge.")

  (defvar top-right-corner-function nil
    "The function launched when hitting the top-right-corner.")

  (defvar right-edge-function nil
    "The function launched when hitting the right-edge.")

  (defvar bottom-right-corner-function nil
    "The function launched when hitting the bottom-right-corner.")

  (defvar bottom-edge-function nil
    "The function launched when hitting the bottom-edge.")

  (defvar bottom-left-corner-function nil
    "The function launched when hitting the bottom-left-corner.")

  (define (hot-spot-invoke spot)
    (let ((func (case spot
		  ((top-left)
		   top-left-corner-function)
		  ((top-right)
		   top-right-corner-function)
		  ((bottom-right)
		   bottom-right-corner-function)
		  ((bottom-left)
		   bottom-left-corner-function)
		  ((left)
		   left-edge-function)
		  ((top)
		   top-edge-function)
		  ((right)
		   right-edge-function)
		  ((bottom)
		   bottom-edge-function))))
      (if (functionp func)
	  (make-timer (lambda ()
			(funcall func))
		      (quotient hot-spot-delay 1000)
		      (mod hot-spot-delay 1000))
	(when func
	  ;; non-nil, but not a function?
	  (error "In hot-spot, you configuration of `%s' is wrong; it should be a function." spot))
	))))
