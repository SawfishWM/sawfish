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

(require 'rects)

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


;; options/variables

(defvar sp-avoided-windows-weight 10)

(defcustom sp-padding 4
  "Try to leave at least this many pixels between window edges in first/best-fit."
  :group placement
  :type number
  :range (0 . 64))

(defvar sp-area-weight 512
  "Weighting between future usefulness and edge alignment in best-fit mode.
A value between 0 and 1023 inclusive.")

(defconst sp-cost-max 1024)

;; the maximum number of points to keep in each grid dimension
(defvar sp-max-points 10)

;; only when there's fewer X events than this pending do we try
;; to do smart placement
(defvar sp-max-queued-events 256)


;; utility functions

;; RECTS is a list of (LEFT TOP RIGHT BOTTOM); returns sorted, uniquified
;; (X-EDGES . Y-EDGES)
(defun sp-make-grid (rects &optional with-root)
  (let*
      ((grid (grid-from-rectangles rects with-root)))
    (rplaca grid (sort (sp-prune-points (car grid) sp-max-points)))
    (rplacd grid (sort (sp-prune-points (cdr grid) sp-max-points)))
    grid))

(defun sp-prune-points (points maximum)
  (let*
      ((total (length points))
       (cutoff (- total maximum))
       tem)
    (setq tem points)
    (while (cdr (cdr tem))
      (if (< (random total) cutoff)
          (rplacd tem (cdr (cdr tem)))
        (setq tem (cdr tem))))
    points))

;; returns the list of windows to compare with when overlapping, by
;; default windows with their `ignored' property set are dropped
(defun sp-get-windows (w)
  (delete-if (lambda (x)
	       (or (eq x w)
		   (and (window-get x 'ignored)
			(not (window-avoided-p x)))
		   (window-get x 'iconified)
		   (not (windows-share-workspace-p x w))))
	     (managed-windows)))


;; calculating overlaps

;; returns (POINT . OVERLAP), find the alignment of the rectangle
;; of dimensions DIMS at POINT with the least overlap compared to
;; RECTS
(defun sp-least-overlap (dims point rects)
  (let
      ((screen-rect (list 0 0 (screen-width) (screen-height)))
       min-overlap min-point)
    (mapc (lambda (delta)
	    (let
		((point-foo (cons (+ (car point)
				     (* (car delta) (car dims)))
				  (+ (cdr point)
				     (* (cdr delta) (cdr dims)))))
		 tem)
	      (when (and (>= (car point-foo) 0)
			 (>= (cdr point-foo) 0)
			 (<= (+ (car point-foo) (car dims)) (screen-width))
			 (<= (+ (cdr point-foo) (cdr dims)) (screen-height)))
		(setq tem (rect-total-overlap dims point-foo rects))
		(when (or (not min-point) (< tem min-overlap))
		  (setq min-overlap tem)
		  (setq min-point point-foo)))))
	  ;; try aligning all four corners to this point
	  '((0 . 0) (0 . -1) (-1 . 0) (-1 . -1)))
    (and min-point (cons min-point min-overlap))))


;; first-fit search

(defun sp-first-fit (dims grid rects)
  (let
      ((point (cons 0 0))
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

(defun sp-fit-or-nil (dims grid rects)
  (let
      ((point (cons 0 0))
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


;; best-fit search

(defmacro sp-edges-adjacent (align-1 start-1 end-1 align-2 start-2 end-2)
  `(if (= ,align-1 ,align-2)
       (rect-1d-overlap ,start-1 ,end-1 ,start-2 ,end-2)
     0))

;; This is the crux of the problem -- this function must assign a value
;; to placing a window of DIMS at POINT. GRID defines the grid from which
;; POINT was chosen, RECTS defines all other windows on the screen.
;; The returned value must be between zero and sp-cost-max, with higher
;; values better placements
(defun sp-cost (point dims grid rects)
  (let
      ((win-left (car point))
       (win-top (cdr point))
       (win-right (+ (car point) (car dims)))
       (win-bottom (+ (cdr point) (cdr dims)))
       (edges (make-vector 4 0))
       (x-cross 0)
       (y-cross 0)
       (x-total 0)
       (y-total 0)
       tem)

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
    (setq edges (+ (aref edges 0) (aref edges 1)
		   (aref edges 2) (aref edges 3)))

    ;; factor in the two quantities scaled upto sp-cost-max by sp-area-weight
    (+ (/ (* sp-area-weight (* x-cross y-cross)) (* x-total y-total))
       (/ (* (- sp-cost-max sp-area-weight) edges)
	  (+ (* 2 (car dims)) (* 2 (cdr dims)))))))

(defun sp-best-fit (dims grid rects)
  (let
      ((point (cons 0 0))
       points min-overlap tem)

    ;; 1. find the list of points with the smallest overlap
    (mapc (lambda (y)
	    (rplacd point y)
	    (mapc (lambda (x)
		    (rplaca point x)
		    (setq tem (sp-least-overlap dims point rects))
		    (when tem
		      (cond ((or (not min-overlap)
				 (< (cdr tem) min-overlap))
			     (setq min-overlap (cdr tem))
			     (setq points (list (car tem))))
			    ((= (cdr tem) min-overlap)
			     (setq points (cons (car tem) points))))))
		  (car grid)))
	  (cdr grid))

    ;; 2. choose the best of these points
    (cond ((null points)
	   ;; no choice
	   nil)
	  ((null (cdr points))
	   ;; one choice
	   (car points))
	  (t
	   ;; n choices
	   (let
	       ((max-cost 0)
		(max-point nil))
	     (mapc (lambda (p)
		     (let
			 ((cost (sp-cost p dims grid rects)))
		       (when (> cost max-cost)
			 (setq max-cost cost)
			 (setq max-point p))))
		   points)
	     max-point)))))


;; entry-points

(defun sp-do-placement (w fit-fun &optional fall-back-fun)
  (if (and sp-max-queued-events (> (x-events-queued) sp-max-queued-events))
      ;; fitted placement can cause event tailbacks when there's
      ;; lots of windows being opened with lots of windows already
      ;; open
      (place-window-randomly w)
    (let*
	((windows (sp-get-windows w))
	 (rects (rectangles-from-windows
		 windows
		 (lambda (x)
		   (if (window-avoided-p x)
		       sp-avoided-windows-weight
		     (window-get x 'placement-weight)))))
	 (grid (sp-make-grid rects t))
	 (dims (window-frame-dimensions w))
	 point)

      ;; first try with padding
      (when (and (> sp-padding 0)
		 (<= (+ (car dims) sp-padding) (screen-width))
		 (<= (+ (cdr dims) sp-padding) (screen-height)))
	(rplaca dims (+ (car dims) (* sp-padding 2)))
	(rplacd dims (+ (cdr dims) (* sp-padding 2)))
	(setq point (fit-fun dims grid rects))
	(when point
	  (rplaca point (+ (car point) sp-padding))
	  (rplacd point (+ (cdr point) sp-padding))))

      ;; then try without padding
      (when (null point)
	(setq dims (window-frame-dimensions w))
	(rplaca dims (min (car dims) (screen-width)))
	(rplacd dims (min (cdr dims) (screen-height)))
	(setq point (fit-fun dims grid rects)))

      (if point
	  (move-window-to w (car point) (cdr point))
	((or fall-back-fun place-window-randomly) w)))))

;;;###autoload
(defun place-window-first-fit (w)
  (sp-do-placement w sp-first-fit))

;;;###autoload
(defun place-window-best-fit (w)
  (sp-do-placement w sp-best-fit))

;;;###autoload
(defun place-window-first-fit-or-interactive (w)
  (sp-do-placement w sp-fit-or-nil place-window-interactively))


;; reinitialise autoload defs

(put 'first-fit 'placement-mode place-window-first-fit)
(put 'best-fit 'placement-mode place-window-best-fit)
(put 'first-fit-or-interactive 'placement-mode
     place-window-first-fit-or-interactive)
