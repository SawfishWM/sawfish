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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

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
	  sawfish.wm.edge.expand
	  sawfish.wm.edge.flip
	  sawfish.wm.edge.hot-spots
	  sawfish.wm.edge.misc
	  sawfish.wm.edge.viewport-drag
	  sawfish.wm.workspace)

  (define-structure-alias edge-actions sawfish.wm.edge.actions)

  (defvar while-hot-move nil)
  (defvar while-mousetrap nil)

  (define (edge-action-call func edge while-moving)
    (case func
      ((viewport-drag)
       (viewport-drag-invoke edge while-moving))
      ((flip-workspace)
       (edge-flip-invoke edge func while-moving))
      ((flip-viewport)
       (edge-flip-invoke edge func while-moving))
      ((expand-window)
       (expand-action edge))
      ((maximize-window)
       (maximize-action))
      ((show-desktop)
       (toggle-desktop))
      ((none/hot-spot)
       (hot-spot-invoke edge))
      ((none/hot-move)
       (hot-move-invoke edge))
      (t nil)))

  ;; Entry point without dragging a windw
  (define (edge-action-hook-func)
    (unless (or while-hot-move
	        while-mousetrap)
      (let ((corner (get-active-corner))
   	    (edge (get-active-edge)))
        (if corner
	    (hot-spot-invoke corner)
	  (cond ((or (eq edge 'left)
	  	     (eq edge 'right))
	         (edge-action-call left-right-edge-action edge nil))
	        ((or (eq edge 'top)
		     (eq edge 'bottom))
	         (edge-action-call top-bottom-edge-action edge nil)))))))

  ;; Entry point while dragging a window
  (define (edge-action-move-hook-func)
    (unless while-mousetrap
      (setq while-hot-move t)
      (let ((edge (get-active-edge)))
        (cond ((or (eq edge 'left)
		   (eq edge 'right))
	       (edge-action-call left-right-edge-move-action edge t))
	     ((or (eq edge 'top)
		   (eq edge 'bottom))
	       (edge-action-call top-bottom-edge-move-action edge t))))
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
