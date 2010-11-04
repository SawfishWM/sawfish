;; edge-flip.jl -- move viewports by pushing pointer against screen edges

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

    (export edge-flip-activate)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.custom
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.viewport
	  sawfish.wm.workspace
	  sawfish.wm.commands.move-resize
	  sawfish.wm.ext.workspace-grid)

  (define-structure-alias edge-flip sawfish.wm.edge.flip)

  (define ef-current-edge nil)

  (define (edge-flip-activate edge type)
    (let ((ptr (query-pointer t)))
      (before-flip)
      (if (eq type 'viewport)
	  (progn
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
	    ;; always warp the pointer to keep it logically static
	    (warp-cursor (car ptr) (cdr ptr)))
	(let ((orig current-workspace))
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
	  (unless (= current-workspace orig)
	    (warp-cursor (car ptr) (cdr ptr)))))
      (after-flip type)))

;;; ugly hacks to make flipping work while dragging windows

;;; XXX xrefresh() to fix rubberband-traces? maybe a user-option
;;; XXX whether to do so? We'll see...

  ;; current-workspace before flipping
  (define original-space)

  (define (before-flip)
    (when move-resize-window
      (setq original-space current-workspace)))

  (define (after-flip type)
    (let ((w move-resize-window))
      (when w
	(when (and (eq type 'workspace)
		   (/= original-space current-workspace)
		   (not (window-get w 'sticky)))
	  (move-window-to-workspace w original-space current-workspace t))))))
