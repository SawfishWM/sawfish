;; edge-action.jl -- Edges taken to another dimension

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

(define-structure sawfish.wm.edge.actions

    (export edges-activate)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.custom
	  sawfish.wm.edge.util
	  sawfish.wm.edge.flip
	  sawfish.wm.edge.hot-spots
	  sawfish.wm.edge.viewport-drag)

  (define-structure-alias edge-actions sawfish.wm.edge.actions)

  (define func nil)
  (define no-enter nil)

  (defcustom edge-actions-delay 250
    "Delay (in miliseconds) before the edges are activated.
hot-spots are activated in half that time, viewport-drag is
activated immediately, aswell as actions for while-moving a window."
    :group edge-actions
    :type number
    :range (50 . nil))

  (defcustom left-right-edge-func 'none
    "Action for the left and right screen-edge."
    :group edge-actions
    :type (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom left-right-edge-move-func 'none
    "Action for the left and right screen-edge while moving a window."
    :group edge-actions
    :type  (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom top-bottom-edge-func 'none
    "Action for the top and bottom screen-edge."
    :group edge-actions
    :type (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom top-bottom-edge-move-func 'none
    "Action for the top and bottom screen-edge while moving."
    :group edge-actions
    :type  (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (define (edge-action-call func edge)
    (case func
      ((hot-spot)
       ;; halve the edge delay for hot-spots
       (make-timer (lambda ()
           (hot-spot-activate edge))
	 (quotient edge-actions-delay 2000)
	 (mod edge-actions-delay 2000)))
      ((viewport-drag)
       ;; no delay for viewport-drag
       (viewport-drag-activate edge))
      ;; full delay for flipping
      ((flip-workspace)
       (make-timer (lambda ()
	   (edge-flip-activate edge 'workspace))
	 (quotient edge-actions-delay 1000)
	 (mod edge-actions-delay 1000)))
      ((flip-viewport)
       (make-timer (lambda ()
           (edge-flip-activate edge 'viewport))
	 (quotient edge-actions-delay 1000)
	 (mod edge-actions-delay 1000)))))

  (define (edge-action-init)
    (unless no-enter
      (let ((corner (get-active-corner))
	    (edge (get-active-edge)))
	  (if corner
	      ;; halve the delay for hot-spots
	      (make-timer (lambda ()
		  (hot-spot-activate corner))
		(quotient edge-actions-delay 2000)
		(mod edge-actions-delay 2000))
	    (setq func nil)
	    (cond ((or (eq edge 'left)
		       (eq edge 'right))
	           (edge-action-call left-right-edge-func edge))
		  ((or (eq edge 'top)
		       (eq edge 'bottom))
		   (edge-action-call top-bottom-edge-func edge)))))))

  (define (edge-action-move-init)
    (setq func nil)
    (setq no-enter t)
    (let ((edge (get-active-edge)))
      (cond ((or (eq edge 'left)
		 (eq edge 'right))
	     (edge-action-call left-right-edge-move-func edge))
	    ((or (eq edge 'top)
		 (eq edge 'bottom))
	     (edge-action-call top-bottom-edge-move-func edge))))
    (setq no-enter nil))

  (define (edges-activate init)
    (if init
	(progn
	  (flippers-activate t)
	  (unless (in-hook-p 'enter-flipper-hook edge-action-init)
	      (add-hook 'enter-flipper-hook edge-action-init))
	  (unless (in-hook-p 'while-moving-hook edge-action-move-init)
	      (add-hook 'while-moving-hook edge-action-move-init)))
      (flippers-activate nil)
      (if (in-hook-p 'enter-flipper-hook edge-action-init)
	  (remove-hook 'enter-flipper-hook edge-action-init))
      (if (in-hook-p 'while-moving-hook edge-action-move-init)
	  (remove-hook 'while-moving-hook edge-action-move-init)))))
