#| top-left.jl -- place windows on the diagonal from the top-left corner

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

   $Id$

   Authors: John Harper <jsh@eazel.com>
|#

;; Commentary:

;; The idea here is that there's a fixed number of possible placement
;; positions on a diagonal from the top-left corner, each successive
;; spot is slightly below and to the right of the previous spot.

;; The placement algorithm searches these spots for the first one that
;; doesn't have the top-left corner of a window in its immediate
;; vicinity. If placing the window on the first found spot would push
;; the window off the right or bottom of the screen, then resize the
;; window to avoid this happening.

;; If no spot can be found to accomodate the window's minimum allowed
;; size, then fallback to placing the window randomly.

;; (note, for `screen' above, I actually mean the work-area, i.e. the
;; area not reserved for special windows, e.g. the panel)

(define-structure sawfish.wm.placement.top-left ()

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.placement
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.state.maximize
	  sawfish.wm.state.iconify)

  (define top-left '(8 . 8))
  (define fuzz '(8 . 16))
  (define step '(16 . 32))

  (define resize-windows nil)

  (define (round-up x mult) (* (quotient (+ x (1- mult)) mult) mult))

  (define (next-position point)
    (cons (+ (car point) (car step))
	  (+ (cdr point) (cdr step))))

  (define (windows-around point)
    (filter-windows
     (lambda (w)
       (and (window-get w 'placed)
	    (window-in-workspace-p w current-workspace)
	    (not (window-outside-viewport-p w))
	    (window-mapped-p w)
	    (not (window-iconified-p w))
	    (let ((w-point (window-position w)))
	      (and (< (abs (- (car w-point) (car point))) (car fuzz))
		   (< (abs (- (cdr w-point) (cdr point))) (cdr fuzz))))))))

  (define (next-free-position point boundary)
    (let loop ((point point))
      (cond ((or (>= (car point) (car boundary))
		 (>= (cdr point) (cdr boundary)))
	     nil)
	    ((null (windows-around point))
	     point)
	    (t (loop (next-position point))))))

  (define (place-window-top-left w)
    (let* ((workarea (maximize-find-workarea w #:head-fallback t))
	   (dims (window-dimensions w))
	   (f-dims (window-frame-dimensions w))
	   (hints (window-size-hints w))

	   (min-dims (if resize-windows
			 (cons (or (cdr (or (assq 'min-width hints)
					    (assq 'base-width hints))) 1)
			       (or (cdr (or (assq 'min-height hints)
					    (assq 'base-height hints))) 1))
		       dims))

	   (first (cons (max (round-up (nth 0 workarea) (car step))
			     (car top-left))
			(max (round-up (nth 1 workarea) (cdr step))
			     (cdr top-left))))

	   (boundary (cons (- (nth 2 workarea) (car min-dims)
			      (- (car f-dims) (car dims)))
			   (- (nth 3 workarea) (cdr min-dims)
			      (- (cdr f-dims) (cdr dims))))))

      (let ((point (next-free-position first boundary)))
	(if point
	    (progn
	      (move-window-to w (car point) (cdr point))
	      (let ((changed nil))
		(when (>= (+ (car point) (car f-dims)) (nth 2 workarea))
		  (rplaca dims (- (nth 2 workarea) (car point)
				  (- (car f-dims) (car dims))))
		  (setq changed t))
		(when (>= (+ (cdr point) (cdr f-dims)) (nth 3 workarea))
		  (rplacd dims (- (nth 3 workarea) (cdr point)
				  (- (cdr f-dims) (cdr dims))))
		  (setq changed t))
		(when changed
		  (resize-window-with-hints* w (car dims) (cdr dims)))))
	  ;; fall back to random placement
	  ((placement-mode 'randomly) w)))))

  ;;###autoload
  (define-placement-mode 'top-left place-window-top-left #:for-normal t))
