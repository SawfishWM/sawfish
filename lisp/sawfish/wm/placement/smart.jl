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


;; options/variables

(defcustom sp-important-windows nil
  "Regular expression matching windows not to overlap in first/best-fit modes."
  :group placement
  :type string
  :allow-nil t)

(defvar sp-important-windows-weight 10)

(defvar sp-auto-weight-alist nil
  "List of `(REGEXP . WEIGHT)' mapping window names to the weight to give
their area when placing windows in best-fit and first-fit mode. Higher
weights mean that the window is harder to cover.")

(defcustom sp-padding 4
  "Try to leave at least this many pixels between window edges in first/best-fit."
  :group placement
  :type number
  :range (0 . 64))

(defcustom sp-area-weight 512
  "Weighting between future usefulness and edge alignment in best-fit mode."
  :group placement
  :type number
  :range (0 . 1024))

(defconst sp-cost-max 1024)

;; the maximum number of points to keep in each grid dimension
(defvar sp-max-points 5)


;; utility functions

;; RECTS is a list of (LEFT TOP RIGHT BOTTOM); returns sorted, uniquified
;; (X-EDGES . Y-EDGES)
(defun sp-make-grid (rects &optional with-root)
  (let
      ((x-edges (nconc (mapcar 'car rects)
		       (mapcar #'(lambda (x) (nth 2 x)) rects)))
       (y-edges (nconc (mapcar #'(lambda (x) (nth 1 x)) rects)
		       (mapcar #'(lambda (x) (nth 3 x)) rects))))
    (when with-root
      (setq x-edges (cons 0 (nconc x-edges (list (screen-width)))))
      (setq y-edges (cons 0 (nconc y-edges (list (screen-height))))))
    (setq x-edges (sort (sp-prune-points x-edges sp-max-points)))
    (setq y-edges (sort (sp-prune-points y-edges sp-max-points)))
    (cons x-edges y-edges)))

(defun sp-prune-points (points max)
  (let*
      ((total (length points))
       (cutoff (- total max))
       tem)
    (setq tem points)
    (while (cdr (cdr tem))
      (if (< (random total) cutoff)
          (rplacd tem (cdr (cdr tem)))
        (setq tem (cdr tem))))
    points))

;; returns a list of (LEFT TOP RIGHT BOTTOM [OVERLAP-WEIGHT])
(defun sp-make-rects (windows)
  (mapcar #'(lambda (w)
	      (let
		  ((dims (window-frame-dimensions w))
		   (pos (window-position w))
		   tem)
		(list* (car pos) (cdr pos)
		       (+ (car pos) (car dims))
		       (+ (cdr pos) (cdr dims))
		       (if (and sp-important-windows
				(string-match
				 sp-important-windows (window-name w)))
			    (list sp-important-windows-weight)
			 (and (setq tem (assoc-regexp (window-name w)
						      sp-auto-weight-alist))
			      (list (cdr tem)))))))
	  windows))

;; returns the list of windows to compare with when overlapping, by
;; default windows with their `ignored' property set are dropped
(defun sp-get-windows (w)
  (delete-if #'(lambda (x)
		 (or (eq x w)
		     (and (window-get x 'ignored)
			  (or (not (stringp sp-important-windows))
			      (not (string-match sp-important-windows
						 (window-name x)))))
		     (window-get x 'iconified)
		     (/= (or (window-get x 'workspace)
			     current-workspace)
			 (or (window-get w 'workspace)
			     current-workspace))))
	     (managed-windows)))


;; calculating overlaps

;; returns the overlap between two 1d line segments, A-1 to A-2, and
;; B-1 to B-2
(defmacro sp-1d-overlap (a-1 a-2 b-1 b-2)
  `(max 0 (- (min ,a-2 ,b-2) (max ,a-1 ,b-1))))

;; returns the overlap between two rectangles, one defined as having
;; dimensions DIMS and origin POINT, while the other is defined
;; by RECT
(defun sp-2d-overlap (dims point rect)
  (* (sp-1d-overlap (car point) (+ (car point) (car dims))
		    (car rect) (nth 2 rect))
     (sp-1d-overlap (cdr point) (+ (cdr point) (cdr dims))
		    (nth 1 rect) (nth 3 rect))
     ;; include the weight of the rectangle if it has one
     (or (nth 4 rect) 1)))

;; returns the total area of the overlap between a rectangle defined by
;; DIMS and POINT, and the list of rectangles RECTS
;; when placed at POINT
(defun sp-total-overlap (dims point rects)
  (let
      ((total 0))
    (mapc #'(lambda (r)
	      (setq total (+ total (sp-2d-overlap dims point r))))
	  rects)
    total))

;; returns (POINT . OVERLAP), find the alignment of the rectangle
;; of dimensions DIMS at POINT with the least overlap compared to
;; RECTS
(defun sp-least-overlap (dims point rects)
  (let
      ((screen-rect (list 0 0 (screen-width) (screen-height)))
       min-overlap min-point)
    (mapc #'(lambda (delta)
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
		  (setq tem (sp-total-overlap dims point-foo rects))
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
      (mapc #'(lambda (y)
		(rplacd point y)
		(mapc #'(lambda (x)
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


;; best-fit search

(defmacro sp-edges-adjacent (align-1 start-1 end-1 align-2 start-2 end-2)
  `(if (= ,align-1 ,align-2)
       (sp-1d-overlap ,start-1 ,end-1 ,start-2 ,end-2)
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
    (mapc #'(lambda (x)
	      (when (and (>= x win-left) (<= x win-right))
		(setq x-cross (1+ x-cross)))
	      (setq x-total (1+ x-total)))
	  (car grid))
    (mapc #'(lambda (y)
	      (when (and (>= y win-top) (<= y win-bottom))
		(setq y-cross (1+ y-cross)))
	      (setq y-total (1+ y-total)))
	  (cdr grid))

    ;; how many window edges does this position abut?
    ;; it can save space to cluster windows as much as possible
    (mapc #'(lambda (r)
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
    (mapc #'(lambda (y)
	      (rplacd point y)
	      (mapc #'(lambda (x)
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
	     (mapc #'(lambda (p)
		       (let
			   ((cost (sp-cost p dims grid rects)))
			 (when (> cost max-cost)
			   (setq max-cost cost)
			   (setq max-point p))))
		   points)
	     max-point)))))


;; entry-points

(defun sp-do-placement (w mode)
  (let*
      ((windows (sp-get-windows w))
       (rects (sp-make-rects windows))
       (grid (sp-make-grid rects t))
       (dims (window-frame-dimensions w))
       point)

    ;; first try with padding
    (when (and (> sp-padding 0)
	       (<= (+ (car dims) sp-padding) (screen-width))
	       (<= (+ (cdr dims) sp-padding) (screen-height)))
      (rplaca dims (+ (car dims) (* sp-padding 2)))
      (rplacd dims (+ (cdr dims) (* sp-padding 2)))
      (setq point (funcall (if (eq mode 'first-fit) 'sp-first-fit 'sp-best-fit)
			   dims grid rects))
      (when point
	(rplaca point (+ (car point) sp-padding))
	(rplacd point (+ (cdr point) sp-padding))))

    ;; then try without padding
    (when (null point)
      (setq dims (window-frame-dimensions w))
      (rplaca dims (min (car dims) (screen-width)))
      (rplacd dims (min (cdr dims) (screen-height)))
      (setq point (funcall (if (eq mode 'first-fit) 'sp-first-fit 'sp-best-fit)
			   dims grid rects)))

    (if point
	(move-window-to w (car point) (cdr point))
      (place-window-randomly w))))

;;;###autoload
(defun place-window-first-fit (w)
  (sp-do-placement w 'first-fit))

;;;###autoload
(defun place-window-best-fit (w)
  (sp-do-placement w 'best-fit))
