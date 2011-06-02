;; edge/actions.jl -- Sets up central user interface of edge-actions

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

    (export activate-edges
	    activate-edges-after-set)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.custom
	  sawfish.wm.edge.conf
	  sawfish.wm.edge.util
	  sawfish.wm.edge.flip
	  sawfish.wm.edge.hot-spots
	  sawfish.wm.edge.tile
	  sawfish.wm.edge.viewport-drag)

  (define-structure-alias edge-actions sawfish.wm.edge.actions)

  (defvar while-hot-move nil)
  (defvar while-mousetrap nil)
  (defvar before-edge-action-hook nil)
  (defvar after-edge-action-hook nil)

  (define (edge-action-call func edge #!key while-moving)
    (call-hook 'before-edge-action-hook (list func edge while-moving))
    (case func
      ((viewport-drag)
       (viewport-drag-invoke edge))
      ((flip-workspace)
       (edge-flip-invoke edge 'workspace))
      ((flip-viewport)
       (edge-flip-invoke edge 'viewport #:while-moving while-moving))
      ((tile-windows)
       (tile-windows #:while-moving while-moving))
      ((none/hot-spot)
       (hot-spot-invoke edge))
      ((none/hot-move)
       (hot-move-invoke edge))
      (t nil))
    (call-hook 'after-edge-action-hook (list func edge while-moving)))

  ;; Entry point without dragging 
  (define (edge-action-hook-func)
    (unless (and while-hot-move
		 while-mousetrap)
      (let ((corner (get-active-corner))
   	    (edge (get-active-edge)))
        (if corner
	    (hot-spot-invoke corner)
	  (cond ((or (eq edge 'left)
	  	     (eq edge 'right))
	         (edge-action-call left-right-edge-action edge))
	        ((or (eq edge 'top)
		     (eq edge 'bottom))
	         (edge-action-call top-bottom-edge-action edge)))))))

  ;; Entry point for window dragging
  (define (edge-action-move-hook-func)
    (unless (and while-hot-move
		 while-mousetrap)
      (setq while-hot-move t)
      (let ((edge (get-active-edge)))
        (cond ((or (eq edge 'left)
		   (eq edge 'right))
	       (edge-action-call left-right-edge-move-action edge #:while-moving t))
	     ((or (eq edge 'top)
		   (eq edge 'bottom))
	       (edge-action-call top-bottom-edge-move-action edge #:while-moving t))))
      ;; for one second after HotMove prevent HotSpot
      (make-timer (lambda () (setq while-hot-move nil)) 1)))

  (define (activate-edges init)
    (if init
	(progn
	  (activate-flippers t)
	  (unless (in-hook-p 'enter-flipper-hook edge-action-hook-func)
	    (add-hook 'enter-flipper-hook edge-action-hook-func))
	  ;; While the pointer is grabbed, window enter/leave events
	  ;; are not generated.
	  (unless (in-hook-p 'while-moving-hook edge-action-move-hook-func)
	    (add-hook 'while-moving-hook edge-action-move-hook-func)))
      (activate-flippers nil)
      (remove-hook 'enter-flipper-hook edge-action-hook-func)
      (remove-hook 'while-moving-hook edge-action-move-hook-func)))

  (define (activate-edges-after-set)
    (activate-edges edge-actions-enabled)))
