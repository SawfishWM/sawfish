;; rects.jl -- rectangle manipulation
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

(provide 'rects)

;; Commentary:

;; A rectangle is (LEFT TOP RIGHT BOTTOM [WEIGHT])


;; rectangles

(defun rectangles-from-grid (x-points y-points &optional pred)
  "The two lists of integers X-POINTS and Y-POINTS define a rectangular
grid. Return the complete list of rectangles formed by the
intersections of the grid.

If PRED is defined, it is called for each rectangle, and only those for
which it returns t are added to the returned list.

Each rectangle is represented by a list `(LEFT TOP RIGHT BOTTOM)'.

Assumes that X-POINTS and Y-POINTS are both sorted smallest->largest."
  (let
      (rects left-x right-x top-y bottom-y tem-y-points tem-x tem-y rect)
    (while x-points
      (setq left-x (car x-points))
      (setq x-points (cdr x-points))
      (setq tem-y-points y-points)
      (while tem-y-points
	(setq top-y (car tem-y-points))
	(setq tem-y-points (cdr tem-y-points))
	(setq tem-x x-points)
	(while tem-x
	  (setq right-x (car tem-x))
	  (setq tem-x (cdr tem-x))
	  (setq tem-y tem-y-points)
	  (while tem-y
	    (setq bottom-y (car tem-y))
	    (setq tem-y (cdr tem-y))
	    (setq rect (list left-x top-y right-x bottom-y))
	    (when (or (not pred) (pred rect))
	      (setq rects (cons rect rects)))))))
    (nreverse rects)))

(defun rectangles-from-windows (windows &optional weight-fun)
  "Returns a list of (LEFT TOP RIGHT BOTTOM [OVERLAP-WEIGHT])
representing the rectangles represented by the list of window objects
WINDOWS.

If WEIGHT-FUN is defined, it is a function to call on each window to
return the weight for that window. If not defined, the window's
`weight' property is used instead."
  (mapcar (lambda (w)
	    (let
		((dims (window-frame-dimensions w))
		 (pos (window-position w))
		 tem)
	      (list* (car pos) (cdr pos)
		     (+ (car pos) (car dims))
		     (+ (cdr pos) (cdr dims))
		     (and (setq tem (if weight-fun
					(weight-fun w)
				      (window-get w 'weight)))
			  (list tem)))))
	  windows))

(defun grid-from-rectangles (rects &optional with-root)
  "Given a list of rectangles RECTS (each rectangle represented by a
list of at least four elemements, `(LEFT TOP RIGHT BOTTOM)'), return
a cons cell `(X-POINTS . Y-POINTS)' defining the grid they represent.

If WITH-ROOT is non-nil, then add the root window boundaries to the
grid."
  (let
      ((x-edges (nconc (mapcar car rects)
		       (mapcar (lambda (x) (nth 2 x)) rects)))
       (y-edges (nconc (mapcar (lambda (x) (nth 1 x)) rects)
		       (mapcar (lambda (x) (nth 3 x)) rects))))
    (when with-root
      (setq x-edges (cons 0 (nconc x-edges (list (screen-width)))))
      (setq y-edges (cons 0 (nconc y-edges (list (screen-height))))))
    (cons (uniquify-list x-edges) (uniquify-list y-edges))))

(defun rectangle-area (rect)
  "Given a rectangle RECT, return its area."
  (* (- (nth 2 rect) (nth 0 rect))
     (- (nth 3 rect) (nth 1 rect))))

(defun rectangle-from-coords (point dims)
  "Return a list representing the rectangle with origin POINT and dimensions
DIMS."
  (list (car point) (cdr point)
	(+ (car point) (car dims)) (+ (cdr point) (cdr dims))))

(defun rectangle-corners (rect)
  "Return a list of four cons cells representing the coordinates of the corners
of the rectangle RECT."
  (list (cons (nth 0 rect) (nth 1 rect))
	(cons (nth 0 rect) (nth 3 rect))
	(cons (nth 2 rect) (nth 1 rect))
	(cons (nth 2 rect) (nth 3 rect))))


;; overlap

(defun rect-1d-overlap (a-1 a-2 b-1 b-2)
  "Returns the overlap between two 1D line segments, A-1 to A-2, and
B-1 to B-2."
  (max 0 (- (min a-2 b-2) (max a-1 b-1))))

(defun rect-2d-overlap (dims point rect)
  "Returns the overlap between two rectangles, one defined as having
dimensions DIMS and origin POINT, while the other is defined by RECT."
  (* (rect-1d-overlap (car point) (+ (car point) (car dims))
		      (car rect) (nth 2 rect))
     (rect-1d-overlap (cdr point) (+ (cdr point) (cdr dims))
		      (nth 1 rect) (nth 3 rect))
     ;; include the weight of the rectangle if it has one
     (or (nth 4 rect) 1)))

(defun rect-2d-overlap* (rect1 rect2)
  "Returns the overlap between two rectangles, RECT1 and RECT2."
  (* (rect-1d-overlap (car rect1) (nth 2 rect1) (car rect2) (nth 2 rect2))
     (rect-1d-overlap (nth 1 rect1) (nth 3 rect1) (nth 1 rect2) (nth 3 rect2))
     ;; include the weight of the rectangle if it has one
     (or (nth 4 rect1) 1)
     (or (nth 4 rect2) 1)))

(defun rect-total-overlap (dims point rects)
  "Returns the total area of the overlap between a rectangle defined by
DIMS and POINT, and the list of rectangles RECTS when placed at POINT."
  (let
      ((total 0))
    (mapc (lambda (r)
	    (setq total (+ total (rect-2d-overlap dims point r))))
	  rects)
    total))

(defun rect-wholly-visible-p (rect)
  "Return t if RECT is completely inside the screen boundaries."
  (and (>= (car rect) 0) (>= (nth 1 rect) 0)
       (<= (nth 2 rect) (screen-width)) (<= (nth 3 rect) (screen-height))))

(defun rectangle-heads (rect)
  "Return the number of screen heads that rectangle RECT appears on."
  (let loop ((head 0)
	     (count 0))
    (if (= head (head-count))
	count
      (loop (1+ head)
	    (if (> (rect-2d-overlap (head-dimensions head)
				    (head-offset head) rect) 0)
		(1+ count)
	      count)))))
