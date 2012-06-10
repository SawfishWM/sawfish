;; flip.jl -- move viewport / workspace like flipping pages

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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

(define-structure sawfish.wm.edge.flip

    (export edge-flip-invoke)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.viewport
	  sawfish.wm.workspace
	  sawfish.wm.commands.move-resize
	  sawfish.wm.ext.workspace-grid)

  (define-structure-alias edge-flip sawfish.wm.edge.flip)

  (define edge-flip-timer nil)
  (define steps nil)

  (define (edge-flip-invoke edge type while-moving)
    (unless edge-flip-timer

      (call-hook 'before-edge-action-hook (list type edge while-moving))

      (setq edge-flip-timer
            (make-timer (lambda ()
                          (setq edge-flip-timer nil)
                          (if (eq type 'flip-viewport)
                              (flip-viewport edge while-moving)
                            (flip-workspace edge while-moving)))
                        (quotient edge-flip-delay 1000)
                        (mod edge-flip-delay 1000)))))

  (define (flip-viewport edge while-moving)
    (let ((ptr (query-pointer t)))

      (if while-moving
	  (setq steps 1)
	(setq steps scroll-viewport-steps))

      (let ((scroll-viewport-steps steps))
	(cond ((eq edge 'left)
	      (when (move-viewport -1 0)
		(rplaca ptr (- (screen-width) 2))))
	      ((eq edge 'right)
	      (when (move-viewport 1 0)
		(rplaca ptr 1)))
	      ((eq edge 'top)
	      (when (move-viewport 0 -1)
		(rplacd ptr (- (screen-height) 2))))
	      ((eq edge 'bottom)
	      (when (move-viewport 0 1)
		(rplacd ptr 1))))

	(warp-cursor (car ptr) (cdr ptr))

        (call-hook 'after-edge-action-hook (list 'flip-viewport edge while-moving)))))

  (define (flip-workspace edge while-moving)
    (let ((ptr (query-pointer t))
          (original-workspace current-workspace)
          (mr-win))

      (call-hook 'before-edge-action-hook (list 'flip-workspace edge while-moving))

      (when while-moving
        (setq mr-win move-resize-window))

      (cond ((eq edge 'left)
	      (workspace-left)
	      (rplaca ptr (- (screen-width) 2)))
	    ((eq edge 'right)
	      (workspace-right)
	      (rplaca ptr 1))
	    ((eq edge 'top)
	      (workspace-up)
	      (rplacd ptr (- (screen-height) 2)))
	    ((eq edge 'bottom)
	      (workspace-down)
	      (rplacd ptr 1)))

      (unless (= current-workspace original-workspace)
	(warp-cursor (car ptr) (cdr ptr))
        (when (and mr-win
	      (not (window-get mr-win 'sticky)))
	  (move-window-to-workspace mr-win original-workspace current-workspace t)))

      (call-hook 'after-edge-action-hook (list 'flip-workspace edge while-moving)))))
