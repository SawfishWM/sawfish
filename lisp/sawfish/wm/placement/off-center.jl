#| off-center.jl -- place windows around the centre of the screen

   Copyright (C) 2000 Eazel, Inc.

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   $Id: off-center.jl,v 1.3 2002/04/24 06:57:02 jsh Exp $

   Authors: John Harper <jsh@eazel.com>
|#

;; Commentary:

;; This placement mode tries to put windows in the center of the
;; screen, but in such a way that the newly placed window doesn't fully
;; obscure an existing window.

;; This is to handle the case where two windows of the same size are
;; created one after the other, so that the user is sure to see (at
;; least part of) both windows

(define-structure sawfish.wm.placement.off-center ()

    (open rep
	  sawfish.wm.misc
	  sawfish.wm.placement
	  sawfish.wm.windows
	  sawfish.wm.viewport
	  sawfish.wm.workspace
	  sawfish.wm.util.rects
	  sawfish.wm.util.workarea
	  sawfish.wm.state.iconify)

  (define multipliers '[(0 . 0)		;centered

			(-1/8 . -1/8)	;diagonals
			(-1/8 . -1/8)
			(1/8 . -1/8)
			(1/8 . 1/8)
			(-1/8 . 1/8)
			(0 . -1/6)	;perpendicular
			(1/6 . 0)
			(0 . 1/6)
			(-1/6 . 0)

			(-1/4 . -1/4)	;diagonals
			(-1/4 . -1/4)
			(1/4 . -1/4)
			(1/4 . 1/4)
			(-1/4 . 1/4)
			(0 . -1/3)	;perpendicular
			(1/3 . 0)
			(0 . 1/3)
			(-1/3 . 0)

			(-1/2 . -1/2)	;diagonals
			(-1/2 . -1/2)
			(1/2 . -1/2)
			(1/2 . 1/2)
			(-1/2 . 1/2)
			(0 . -1/2)	;perpendicular
			(1/2 . 0)
			(0 . 1/2)
			(-1/2 . 0)])

  (define (make-point center dims index)
    (if (>= index (length multipliers))
	nil
      (let ((mult (aref multipliers index)))
	;; need to coerce to exact representation in case rep
	;; was compiled --without-gmp (and thus division may lose
	;; exactness)
	(cons (inexact->exact (round (- (+ (car center)
					   (* (car dims) (car mult)))
					(/ (car dims) 2))))
	      (inexact->exact (round (- (+ (cdr center)
					   (* (cdr dims) (cdr mult)))
					(/ (cdr dims) 2))))))))

  (define (obscured-windows w point dims)
    (filter-windows
     (lambda (x)
       (and (not (eq x w))
	    (window-mapped-p x)
	    (not (window-iconified-p x))
	    (window-in-workspace-p x current-workspace)
	    (let ((x-rect (car (rectangles-from-windows (list x)))))
	      (>= (rect-2d-overlap dims point x-rect)
		  (rectangle-area x-rect)))))))

  (define (within-rectangle point dims rect)
    (= (rect-2d-overlap dims point rect) (* (car dims) (cdr dims))))

  (define (place-window-off-center w)
    (let* ((workarea (calculate-workarea #:window w))
	   (fdims (window-frame-dimensions w))
	   (center (rectangle-center workarea)))

      (let loop ((index 0))
	(let ((point (make-point center fdims index)))
	  (if (null point)
	      ;; fall back to random placement
	      ((placement-mode 'randomly) w)

	    (if (and (within-rectangle point fdims workarea)
		     (null (obscured-windows w point fdims)))
		(move-window-to w (car point) (cdr point))

	      (loop (1+ index))))))))

  ;;###autoload
  (define-placement-mode 'off-center place-window-off-center #:for-normal t))
