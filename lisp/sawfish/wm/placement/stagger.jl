;; stagger-placement.jl -- for jwz

;; $Id: stagger.jl,v 1.3 2002/05/27 05:18:56 jsh Exp $

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.placement.stagger ()

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.placement
	  sawfish.wm.custom
	  sawfish.wm.util.workarea)

  (defcustom stagger-placement-step 32
    (_"Distance between successive placements in stagger mode.")
    :type number
    :range (1)
    :group placement)

  (define place-window-stagger
    (let ((last-x 0)
	  (last-y 0))
      (lambda (w)
	(let ((dims (window-frame-dimensions w))
	      (workarea (calculate-workarea #:window w)))
	  (setq last-x (+ last-x stagger-placement-step))
	  (setq last-y (+ last-y stagger-placement-step))
	  (when (>= (+ last-x (car dims)) (nth 2 workarea))
	    (setq last-x (nth 0 workarea)))
	  (when (>= (+ last-y (cdr dims)) (nth 3 workarea))
	    (setq last-y (nth 1 workarea)))
	  (move-window-to w last-x last-y)))))

  ;;###autoload
  (define-placement-mode 'stagger place-window-stagger #:for-normal t))
