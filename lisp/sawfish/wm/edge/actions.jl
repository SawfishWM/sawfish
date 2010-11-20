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

  (defcustom edge-actions-enabled nil
    "Activate the screen-edges."
    :group edge-actions
    :type boolean
    :after-set (lambda () (edges-activate)))

  (defcustom edge-actions-delay 250
    "Delay (in miliseconds) before the edges are activated."
    :group edge-actions
    :type number
    :range (50 . nil))

  (defcustom left-edge-func 'none
    "Action for the left screen-edge."
    :group edge-actions
    :type (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom left-edge-move-func 'none
    "Action for the left screen-edge while moving a window."
    :group edge-actions
    :type  (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom top-edge-func 'none
    "Action for the top screen-edge."
    :group edge-actions
    :type (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom top-edge-move-func 'none
    "Action for the top screen-edge while moving."
    :group edge-actions
    :type  (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom right-edge-func 'none
    "Action for the right screen-edge."
    :group edge-actions
    :type (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom right-edge-move-func 'none
    "Action for the right screen-edge."
    :group edge-actions
    :type  (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom bottom-edge-func 'none
    "Action for the bottom screen-edge."
    :group edge-actions
    :type (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (defcustom bottom-edge-move-func 'none
    "Action for the bottom screen-edge while moving."
    :group edge-actions
    :type  (choice hot-spot viewport-drag flip-workspace flip-viewport none))

  (define (edge-action-call func edge)
    (case func
      ((hot-spot)
       (hot-spot-activate edge))
      ((viewport-drag)
       (viewport-drag-activate edge))
      ((flip-workspace)
       (edge-flip-activate edge 'workspace))
      ((flip-viewport)
       (edge-flip-activate edge 'viewport))))

  (define (edge-action-init)
    (unless no-enter
      (let ((corner (get-active-corner))
	    (edge (get-active-edge)))
	  (if corner
	      (hot-spot-activate corner)
	    (setq func nil)
	    (cond ((eq edge 'left)
	           (edge-action-call left-edge-func edge))
	          ((eq edge 'right)
		   (edge-action-call right-edge-func edge))
		  ((eq edge 'top)
		   (edge-action-call top-edge-func edge))
		  ((eq edge 'bottom)
		   (edge-action-call bottom-edge-func edge)))))))

  (define (edge-action-move-init)
    (setq func nil)
    (setq no-enter t)
    (case (get-active-edge)
      ((left)
       (edge-action-call left-edge-move-func 'left))
      ((right)
       (edge-action-call right-edge-move-func 'right))
      ((top)
       (edge-action-call top-edge-move-func 'top))
      ((bottom)
       (edge-action-call bottom-edge-move-func 'bottom)))
    (setq no-enter nil))

  (define (edges-activate)
    (if edge-actions-enabled
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
