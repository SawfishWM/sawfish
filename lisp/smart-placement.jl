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

;; This only does first-fit placement currently. I need to work out an
;; optimization function to do best-fit placement.

(defcustom sp-non-ignored-windows nil
  "Regular expression matching windows not to overlap in first/best-fit modes."
  :group placement
  :type string
  :allow-nil t)

(defcustom sp-area-weight 512
  "Weighting between used area and edge alignment in best-fit mode."
  :group placement
  :type number
  :range (0 . 1024))

(defconst sp-cost-max 1024)


;; utility functions

;; RECTS is a list of (LEFT TOP RIGHT BOTTOM); returns sorted, uniquified
;; (X-EDGES . Y-EDGES)
(defun sp-make-grid (rects &optional with-root)
  (let
      ((x-edges (nconc (mapcar 'car rects)
		       (mapcar #'(lambda (x) (nth 2 x)) rects)))
       (y-edges (nconc (mapcar #'(lambda (x) (nth 1 x)) rects)
		       (mapcar #'(lambda (x) (nth 3 x)) rects)))
       tem)
    (when with-root
      (setq x-edges (cons 0 (nconc x-edges (list (screen-width)))))
      (setq y-edges (cons 0 (nconc y-edges (list (screen-height))))))
    (setq x-edges (sort x-edges))
    (setq y-edges (sort y-edges))
    (setq tem x-edges)
    (while (cdr tem)
      (if (= (car tem) (car (cdr tem)))
	  (rplacd tem (cdr (cdr tem)))
	(setq tem (cdr tem))))
    (setq tem y-edges)
    (while (cdr tem)
      (if (= (car tem) (car (cdr tem)))
	  (rplacd tem (cdr (cdr tem)))
	(setq tem (cdr tem))))
    (cons x-edges y-edges)))

;; returns a list of (LEFT TOP RIGHT BOTTOM)
(defun sp-make-rects (windows)
  (mapcar #'(lambda (w)
	      (let
		  ((dims (window-frame-dimensions w))
		   (pos (window-position w)))
		(list (car pos) (cdr pos)
		      (+ (car pos) (car dims))
		      (+ (cdr pos) (cdr dims)))))
	  windows))

(defun sp-screen-rects ()
  (list (list (- (screen-width)) 0 0 (screen-height))
	(list (screen-width) 0 (* 2 (screen-width)) (screen-height))
	(list 0 (- (screen-height)) (screen-width) 0)
	(list 0 (screen-height) (screen-width) (* 2 (screen-height)))))

(defmacro sp-1d-overlap (a-1 a-2 b-1 b-2)
  `(max 0 (- (min ,a-2 ,b-2) (max ,a-1 ,b-1))))

(defun sp-2d-overlap (dims point rect)
  (* (sp-1d-overlap (car point) (+ (car point) (car dims))
		    (car rect) (nth 2 rect))
     (sp-1d-overlap (cdr point) (+ (cdr point) (cdr dims))
		    (nth 1 rect) (nth 3 rect))))

;; returns zero if a rectangle of size DIMS doesn't overlap any of RECTS
;; when placed at POINT
(defun sp-total-overlap (dims point rects)
  (let
      ((total 0))
    (mapc #'(lambda (r)
	      (setq total (+ total (sp-2d-overlap dims point r))))
	  rects)
    total))

;; returns (POINT . OVERLAP)
(defun sp-least-overlap (dims point rects)
  (let
      ((screen-rect (list 0 0 (screen-width) (screen-height)))
       min-overlap min-point)
    (mapc #'(lambda (delta)
	      (let
		  ((point-foo (cons (+ (car point) (car delta))
				    (+ (cdr point) (cdr delta))))
		   tem)
		(when (and (>= (car point-foo) 0)
			   (>= (cdr point-foo) 0)
			   (<= (+ (car point-foo) (car dims)) (screen-width))
			   (<= (+ (cdr point-foo) (cdr dims)) (screen-height)))
		  (setq tem (sp-total-overlap dims point-foo rects))
		  (when (or (not min-point) (< tem min-overlap))
		    (setq min-overlap tem)
		    (setq min-point point-foo)))))
	  (list '(0 . 0) (cons 0 (- (cdr dims))) (cons (- (car dims)) 0)
		(cons (- (car dims)) (- (cdr dims)))))
    (and min-point (cons min-point min-overlap))))

(defun sp-get-windows (w)
  (delete-if #'(lambda (x)
		 (or (eq x w)
		     (and (window-get x 'ignored)
			  (or (not (stringp sp-non-ignored-windows))
			      (not (string-match sp-non-ignored-windows
						 (window-name x)))))
		     (window-get x 'iconified)
		     (/= (or (window-get x 'workspace)
			     current-workspace)
			 (or (window-get w 'workspace)
			     current-workspace))))
	     (managed-windows)))


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

(defmacro sp-edges-adjacent-p (align-1 start-1 end-1 align-2 start-2 end-2)
  `(and (= ,align-1 ,align-2)
	(or (> ,start-1 ,end-2) (< ,end-1 ,start-2))))

;; This is the crux of the problem -- this function must assign a value
;; to placing a window of DIMS at POINT. GRID defines the grid from which
;; POINT was chosen, RECTS defines all other windows on the screen.
;; The returned value must be between zero and sp-cost-max, with higher
;; values better placements
(defun sp-cost (point dims grid rects)
  (let
      ((rect-left (car point))
       (rect-top (cdr point))
       (rect-right (+ (car point) (car dims)))
       (rect-bottom (+ (cdr point) (cdr dims)))
       (edges (make-vector 4 0))
       tem)

    ;; try to find the width and height of the containing rectangle.
    ;; this is wrong since the rectangles are pretty meaningless in
    ;; this context..

    (when (setq tem (cdr (memq rect-left (car grid))))
      (while (and tem (< (car tem) (+ rect-left (car dims))))
	(setq tem (cdr tem)))
      (setq rect-right (car tem)))
    (unless rect-right
      (setq rect-right (screen-width)))

    (when (setq tem (cdr (memq rect-top (cdr grid))))
      (while (and tem (< (car tem) (+ rect-top (cdr dims))))
	(setq tem (cdr tem)))
      (setq rect-bottom (car tem)))
    (unless rect-bottom
      (setq rect-bottom (screen-height)))

    ;; how many window edges does this grid square abut?
    (mapc #'(lambda (r)
	      (when (sp-edges-adjacent-p rect-right rect-top rect-bottom
					 (car r) (nth 1 r) (nth 3 r))
		(aset edges 0 1))
	      (when (sp-edges-adjacent-p rect-left rect-top rect-bottom
					 (nth 2 r) (nth 1 r) (nth 3 r))
		(aset edges 1 1))
	      (when (sp-edges-adjacent-p rect-bottom rect-left rect-right
					 (nth 1 r) (car r) (nth 2 r))
		(aset edges 2 1))
	      (when (sp-edges-adjacent-p rect-top rect-left rect-right
					 (nth 3 r) (car r) (nth 2 r))
		(aset edges 3 1)))
	  rects)
    (setq edges (+ (aref edges 0) (aref edges 1)
		   (aref edges 2) (aref edges 3)))

    ;; this function is trying to account for the unused area in
    ;; the assigned grid rectangle, and the number of abutted edges
    (+ (/ (* sp-area-weight
	     (* (- rect-right rect-left) (- rect-bottom rect-top)))
	  (* (car dims) (cdr dims)))
       (/ (* (- sp-cost-max sp-area-weight) edges) 4))))

(defun sp-best-fit (dims grid rects)
  (let
      ((points nil)
       point)

    ;; 1. find all possible positions
    (setq point (cons nil nil))
    (mapc #'(lambda (y)
	      (rplacd point y)
	      (mapc #'(lambda (x)
			(rplaca point x)
			(when (zerop (sp-total-overlap dims point rects))
			  (setq points (cons (cons x y) points))))
		    (car grid)))
	  (cdr grid))

    (cond ((null points)
	   nil)
	  ((null (cdr points))
	   (car points))
	  (t
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

;;;###autoload
(defun place-window-first-fit (w)
  (let*
      ((windows (sp-get-windows w))
       (rects (sp-make-rects windows))
       (grid (sp-make-grid rects t))
       (point (sp-first-fit (window-frame-dimensions w) grid rects)))
    (if point
	(move-window-to w (car point) (cdr point))
      (place-window-randomly w))))

;;;###autoload
(defun place-window-best-fit (w)
  (place-window-first-fit w))

;  (let*
;      ((windows (sp-get-windows w))
;       (rects (sp-make-rects windows))
;       (grid (sp-make-grid rects t))
;    (if point
;	(move-window-to w (car point) (cdr point))
;      (place-window-randomly w))))
