;; smart-placement.jl -- ``intelligent'' window placement
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; This implements two different algorithms: first-fit and best-fit. 

;; Both use the same inner core -- they scan for the position to place
;; the window where it overlaps the smallest area with other windows

;; The two algorithms differ where more than one position has this
;; smallest overlap area. The first-fit algorithm takes the first such
;; position found, while the best-fit algorithm tries to choose the
;; best such position.

;; The cost function used for the best-fit currently tries to combine
;; two factors: the future ``usefulness'' of the chosen space, and the
;; number (and size) of the window edges the window will abut when
;; placed in the position

(define-structure sawfish.wm.placement.smart

    (export sp-do-placement
	    sp-first-fit
	    sp-best-fit
	    sp-fit-or-nil
	    sp-cost:grid-lines
	    sp-cost:aligned-edges
	    sp-cost:pointer-locality
	    sp-cost:focus-locality
	    sp-cost:center-locality
	    sp-cost:overlap)

    (open rep
	  sawfish.wm.util.rects
	  sawfish.wm.util.groups
	  sawfish.wm.util.workarea
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.placement
	  sawfish.wm.custom
	  sawfish.wm.workspace)


;;; options/variables

  (defvar sp-avoided-windows-weight 100)
  (defvar sp-normal-windows-weight 1)

  (defvar sp-padding 0
    "Try to leave at least this many pixels between window edges in first/best-fit.")

  ;; the maximum number of points to keep in each grid dimension
  (defvar sp-max-points 10)

  ;; only when there's fewer X events than this pending do we try
  ;; to do smart placement
  (defvar sp-max-queued-events 256)


;;; utility functions

  ;; RECTS is a list of (LEFT TOP RIGHT BOTTOM); returns sorted, uniquified
  ;; (X-EDGES . Y-EDGES)
  (define (sp-make-grid rects #!optional with-root)
    (let ((grid (grid-from-rectangles rects)))
      ;; XXX fix for multiple heads
      (when with-root
	(unless (memql 0 (car grid))
	  (rplaca grid (cons 0 (car grid))))
	(unless (memql (screen-width) (car grid))
	  (rplaca grid (cons (screen-width) (car grid))))
	(unless (memql 0 (cdr grid))
	  (rplacd grid (cons 0 (cdr grid))))
	(unless (memql (screen-height) (cdr grid))
	  (rplacd grid (cons (screen-height) (cdr grid)))))
      (rplaca grid (sort (sp-prune-points (car grid) sp-max-points
					  (cons 0 (screen-width)))))
      (rplacd grid (sort (sp-prune-points (cdr grid) sp-max-points
					  (cons 0 (screen-height)))))
      grid))

  (define (sp-prune-points points maximum range)
    (setq points (delete-if-not (lambda (p)
				  (and (>= p (car range)) (<= p (cdr range))))
				points))
    (let ((total (length points)))
      (cond ((> total maximum)
	     ;; remove points
	     (let ((cutoff (* (max 0 (- total maximum)) 100)))
	       (setq total (* total 100))
	       (delete-if (lambda (unused)
			    (declare (unused unused))
			    (< (random total) cutoff)) points)))
	    ((< total maximum)
	     ;; add points
	     (while (< total maximum)
	       (setq points (cons (+ (car range)
				     (random (- (cdr range) (car range))))
				  points))
	       (setq total (1+ total)))))
      points))
	   
  ;; returns the list of windows to compare with when overlapping, by
  ;; default windows with their `ignored' property set are dropped
  (define (sp-get-windows w #!optional pred)
    (filter-windows
     (lambda (x)
       (not (or (eq x w)
		(not (window-mapped-p x))
		(and (or (and pred (not (pred x))) (window-get x 'ignored))
		     (not (window-avoided-p x)))
		(window-get x 'iconified)
		(not (windows-share-workspace-p x w)))))))


;;; calculating overlaps

  ;; returns (POINT . OVERLAP), find the alignment of the rectangle
  ;; of dimensions DIMS at POINT with the least overlap compared to
  ;; RECTS
  (define (sp-least-overlap dims point rects)
    (let (min-overlap min-point)
      (mapc (lambda (delta)
	      (let ((point-foo (cons (+ (car point)
					(* (car delta) (car dims)))
				     (+ (cdr point)
					(* (cdr delta) (cdr dims)))))
		    tem)
		(when (and (>= (car point-foo) 0)
			   (>= (cdr point-foo) 0)
			   (<= (+ (car point-foo) (car dims)) (screen-width))
			   (<= (+ (cdr point-foo) (cdr dims)) (screen-height))
			   (= (rectangle-heads
			       (rectangle-from-coords point-foo dims)) 1))
		  (setq tem (rect-total-overlap dims point-foo rects))
		  (when (or (not min-point) (< tem min-overlap))
		    (setq min-overlap tem)
		    (setq min-point point-foo)))))
	    ;; try aligning all four corners to this point
	    '((0 . 0) (0 . -1) (-1 . 0) (-1 . -1)))
      (and min-point (cons min-point min-overlap))))


;;; first-fit search

  (define (sp-first-fit dims grid rects)
    (let ((point (cons 0 0))
	  min-point min-overlap tem)
      (catch 'done
	(mapc (lambda (y)
		(rplacd point y)
		(mapc (lambda (x)
			(rplaca point x)
			(setq tem (sp-least-overlap dims point rects))
			(when tem
			  (when (zerop (cdr tem))
			    (throw 'done (car tem)))
			  (when (or (not min-overlap)
				    (< (cdr tem) min-overlap))
			    (setq min-overlap (cdr tem))
			    (setq min-point (car tem)))))
		      (car grid)))
	      (cdr grid))
	;; no zero overlap point, use the point with least overlap
	min-point)))

  (define (sp-fit-or-nil dims grid rects)
    (let ((point (cons 0 0))
	  tem)
      (catch 'done
	(mapc (lambda (y)
		(rplacd point y)
		(mapc (lambda (x)
			(rplaca point x)
			(setq tem (sp-least-overlap dims point rects))
			(when (and tem (zerop (cdr tem)))
			  (throw 'done (car tem))))
		      (car grid)))
	      (cdr grid))
	nil)))


;;; best-fit search

  (defmacro sp-edges-adjacent (align-1 start-1 end-1 align-2 start-2 end-2)
    `(if (= ,align-1 ,align-2)
	 (rect-1d-overlap ,start-1 ,end-1 ,start-2 ,end-2)
       0))

  (define (sp-cost:grid-lines point dims grid rects)
    "Smart placement cost function. Cost is proportional to the number of grid
lines that the proposed placement crosses."
    (declare (unused rects))
    (let ((win-left (car point))
	  (win-top (cdr point))
	  (win-right (+ (car point) (car dims)))
	  (win-bottom (+ (cdr point) (cdr dims)))
	  (x-cross 0)
	  (y-cross 0)
	  (x-total 0)
	  (y-total 0))
      ;; count the number of grid lines this position crosses
      ;; the idea is to maximize this, since it's likely that it
      ;; will use up the annoying small parts of the screen
      (mapc (lambda (x)
	      (when (and (>= x win-left) (<= x win-right))
		(setq x-cross (1+ x-cross)))
	      (setq x-total (1+ x-total)))
	    (car grid))
      (mapc (lambda (y)
	      (when (and (>= y win-top) (<= y win-bottom))
		(setq y-cross (1+ y-cross)))
	      (setq y-total (1+ y-total)))
	    (cdr grid))
      (/ (* x-cross y-cross) (* x-total y-total))))

  (define (sp-cost:aligned-edges point dims grid rects)
    "Smart placement cost function. Cost is proportional to the length of the
edges that the proposed placement abuts."
    (declare (unused grid))
    (let ((win-left (car point))
	  (win-top (cdr point))
	  (win-right (+ (car point) (car dims)))
	  (win-bottom (+ (cdr point) (cdr dims)))
	  (edges (make-vector 4 0)))
      ;; how many window edges does this position abut?
      ;; it can save space to cluster windows as much as possible
      (mapc (lambda (r)
	      (aset edges 0 (max (sp-edges-adjacent
				  win-right win-top win-bottom
				  (car r) (nth 1 r) (nth 3 r))
				 (aref edges 0)))
	      (aset edges 1 (max (sp-edges-adjacent
				  win-left win-top win-bottom
				  (nth 2 r) (nth 1 r) (nth 3 r))
				 (aref edges 1)))
	      (aset edges 2 (max (sp-edges-adjacent
				  win-bottom win-left win-right
				  (nth 1 r) (car r) (nth 2 r))
				 (aref edges 2)))
	      (aset edges 3 (max (sp-edges-adjacent
				  win-top win-left win-right
				  (nth 3 r) (car r) (nth 2 r))
				 (aref edges 3))))
	    rects)
      (/ (+ (aref edges 0) (aref edges 1)
	    (aref edges 2) (aref edges 3))
	 (+ (* 2 (car dims)) (* 2 (cdr dims))))))

  ;; calculate the cost associated with the distance between POINT-1 and
  ;; POINT-2. As they get nearer the cost tends to zero, as they get
  ;; further apart the cost tends to one (hopefully)
  (define (sp-cost-from-distance point-1 point-2)
    (/ (sqrt (+ (expt (- (screen-width)
			 (abs (- (car point-2) (car point-1)))) 2)
		(expt (- (screen-height)
			 (abs (- (cdr point-2) (cdr point-1)))) 2)))
       (sqrt (+ (expt (screen-width) 2) (expt (screen-height) 2)))))

  (define (sp-cost:pointer-locality point dims grid rects)
    "Smart placement cost function. Cost is proportional to the distance
from the proposed placement to the current pointer position."
    (declare (unused grid rects))
    (sp-cost-from-distance (rectangle-center* point dims) (query-pointer)))

  (define (sp-cost:focus-locality point dims grid rects)
    "Smart placement cost function. Cost is proportional to the distance from
the proposed placement to the position of the currently focused window."
    (declare (unused grid rects))
    (let ((focus (input-focus)))
      (if focus
	  (sp-cost-from-distance (rectangle-center* point dims)
				 (rectangle-center* (window-position focus)
						    (window-dimensions focus)))
	1)))

  (define (sp-cost:center-locality point dims grid rects)
    "Smart placement cost function. Cost is proportional to the distance from
the proposed placement to the center of the screen."
    (declare (unused grid rects))
    (sp-cost-from-distance (rectangle-center* point dims)
			   (cons (/ (screen-width) 2) (/ (screen-height) 2))))

  (define (sp-cost:overlap point dims grid rects overlap)
    (declare (unused point dims grid rects))
    (exp (- (/ overlap (* (screen-width) (screen-height))))))

  (defvar sp-cost-components (list (cons sp-cost:overlap 7/8)
				   (cons sp-cost:center-locality 1/8))
    "Alist defining smart placement cost functions and their associated weights
(multipliers).")

  ;; This is the crux of the problem -- this function must assign a value
  ;; to placing a window of DIMS at POINT. GRID defines the grid from which
  ;; POINT was chosen, RECTS defines all other windows on the screen.
  ;; The returned value must be non-negative, with higher values reflecting
  ;; better placements
  (define (sp-cost point dims grid rects overlap)
    (let ((total 0))
      (mapc (lambda (cell)
	      (setq total
		    (+ total (* ((car cell) point dims grid rects overlap)
				(cdr cell)))))
	    sp-cost-components)
      total))

  (define (sp-best-fit dims grid rects)
    (let ((point (cons 0 0))
	  points tem)

      ;; 1. find the list of possible placements (POINT . OVERLAP)
      (mapc (lambda (y)
	      (rplacd point y)
	      (mapc (lambda (x)
		      (rplaca point x)
		      (setq tem (sp-least-overlap dims point rects))
		      (when tem
			(setq points (cons tem points))))
		    (car grid)))
	    (cdr grid))

      ;; 2. choose the best of these points
      (cond ((null points)
	     ;; no choice
	     nil)
	    ((null (cdr points))
	     ;; one choice
	     (caar points))
	    (t
	     ;; n choices
	     (let ((max-cost 0)
		   (max-point nil))
	       (mapc (lambda (p)
		       (let ((cost (sp-cost (car p) dims grid rects (cdr p))))
			 (when (> cost max-cost)
			   (setq max-cost cost)
			   (setq max-point (car p)))))
		     points)
	       max-point)))))


;;; entry-points

  (define (sp-do-placement w fit-fun #!optional fall-back-fun #!key window-filter)
    (if (and sp-max-queued-events (> (x-events-queued) sp-max-queued-events))
	;; fitted placement can cause event tailbacks when there's
	;; lots of windows being opened with lots of windows already
	;; open
	((placement-mode 'randomly) w)
      (let* ((windows (sp-get-windows w window-filter))
	     (rects (rectangles-from-windows
		     windows
		     (lambda (x)
		       (cond ((window-avoided-p x)
			      sp-avoided-windows-weight)
			     ((window-get x 'placement-weight))
			     (t sp-normal-windows-weight)))))
	     (grid (sp-make-grid rects t))
	     (dims (window-frame-dimensions w))
	     (workarea (calculate-workarea #:window w))
	     (workarea-width (- (nth 2 workarea) (nth 0 workarea)))
	     (workarea-height (- (nth 3 workarea) (nth 1 workarea)))
	     point)

	;; first try with padding
	(when (and (> sp-padding 0)
		   (<= (+ (car dims) (* sp-padding 2)) workarea-width)
		   (<= (+ (cdr dims) (* sp-padding 2)) workarea-height))
	  (let ((padded-dims (cons (+ (car dims) (* sp-padding 2))
				   (+ (cdr dims) (* sp-padding 2)))))
	    (setq point (fit-fun padded-dims grid rects)))
	  (when point
	    (rplaca point (+ (car point) sp-padding))
	    (rplacd point (+ (cdr point) sp-padding))))

	;; then try without padding
	(when (null point)
	  (let ((smallest-dims (cons (min (car dims) workarea-width)
				     (min (cdr dims) workarea-height))))
	    (setq point (fit-fun smallest-dims grid rects))))

	(if point
	    (move-window-to w (car point) (cdr point))
	  ((or fall-back-fun (placement-mode 'randomly)) w)))))

  (define (place-window-first-fit w)
    (sp-do-placement w sp-first-fit))

  (define (place-window-best-fit w)
    (sp-do-placement w sp-best-fit))

  (define (place-window-best-fit-group w)
    (let ((group (windows-in-group w)))
      (sp-do-placement w sp-best-fit nil
		       #:window-filter (lambda (x) (memq x group)))))

  (define (place-window-first-fit-or-interactive w)
    (sp-do-placement w sp-fit-or-nil (placement-mode 'interactively)))
						     


;;; init

  ;;###autoload
  (define-placement-mode 'first-fit place-window-first-fit #:for-normal t)
  (define-placement-mode 'best-fit place-window-best-fit)
  (define-placement-mode 'best-fit-group place-window-best-fit-group)
  (define-placement-mode 'first-fit-or-interactive place-window-first-fit-or-interactive))
