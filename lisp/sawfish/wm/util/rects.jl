;; rects.jl -- rectangle manipulation

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.util.rects

    (export rect-left rect-top rect-right rect-bottom
            rect-obscured rect-minus
            rectangles-from-grid
	    rectangles-from-windows
	    grid-from-rectangles
	    rectangle-area
	    rectangle-from-coords
	    rectangle-corners
	    rectangle-center
	    rectangle-center*
	    rectangle-union
	    rectangle-intersection
	    rect-1d-overlap
	    rect-2d-overlap
	    rect-2d-overlap*
	    rect-total-overlap
            rect-wholly-within-rect
	    rect-wholly-visible-p
	    rectangle-heads)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc)

  (define-structure-alias rects sawfish.wm.util.rects)

  ;; Commentary:

  ;; A rectangle is (LEFT TOP RIGHT BOTTOM [WEIGHT])
  ;; The left and top edges are considered part of the rectangle,
  ;; the right and bottom edges are not.


;;; rectangles

  (define rect-left car)
  (define rect-top cadr)
  (define rect-right caddr)
  (define rect-bottom cadddr)

  (define (rect-obscured r by)
    "Check whether rectangle R is wholly or partially contained in
rectangle BY.  Return `unobscured', `partially-obscured' or `fully-obscured'."
    (cond ((or (<= (rect-right by) (rect-left r))
               (<= (rect-right r) (rect-left by))
               (<= (rect-bottom by) (rect-top r))
               (<= (rect-bottom r) (rect-top by)))
           'unobscured)
          ((and (<= (rect-left by) (rect-left r))
                (<= (rect-right r) (rect-right by))
                (<= (rect-top by) (rect-top r))
                (<= (rect-bottom r) (rect-bottom by)))
           'fully-obscured)
          (t 'partially-obscured)))

  (define (rect-minus r s #!optional tail)
    "Return a list of disjoint rectangles whose union is the part of
rectangle R not contained in rectangle S.  If TAIL is given, the
result is prepended to it."
    (let rminus ((r r) (result tail))
         (cond
          ;; Check for complete disjointness.
          ((or (<= (rect-right s) (rect-left r))
               (<= (rect-right r) (rect-left s))
               (<= (rect-bottom s) (rect-top r))
               (<= (rect-bottom r) (rect-top s)))
           (cons r result))
          ;; Extract a free slice from the bottom of r.
          ((< (rect-bottom s) (rect-bottom r))
           (rminus (list (rect-left r) (rect-top r)
                         (rect-right r) (rect-bottom s))
                   (cons (list (rect-left r) (rect-bottom s)
                               (rect-right r) (rect-bottom r))
                         result)))
          ;; Extract a free slice from the right side of r.
          ((< (rect-right s) (rect-right r))
           (rminus (list (rect-left r) (rect-top r)
                         (rect-right s) (rect-bottom r))
                   (cons (list (rect-right s) (rect-top r)
                               (rect-right r) (rect-bottom r))
                         result)))
          ;; Extract a free slice from the top of r.
          ((> (rect-top s) (rect-top r))
           (rminus (list (rect-left r) (rect-top s)
                         (rect-right r) (rect-bottom r))
                   (cons (list (rect-left r) (rect-top r)
                               (rect-right r) (rect-top s))
                         result)))
          ;; Extract a free slice from the left side of r.
          ((> (rect-left s) (rect-left r))
           (rminus (list (rect-left s) (rect-top r)
                         (rect-right r) (rect-bottom r))
                   (cons (list (rect-left r) (rect-top r)
                               (rect-left s) (rect-bottom r))
                         result)))
          ;; Completely covered.
          (t result))))

  (define (rectangles-from-grid x-points y-points #!optional pred)
    "The two lists of integers X-POINTS and Y-POINTS define a rectangular
grid. Return the complete list of rectangles formed by the
intersections of the grid.

If PRED is defined, it is called for each rectangle, and only those for
which it returns t are added to the returned list.

Each rectangle is represented by a list `(LEFT TOP RIGHT BOTTOM)'.

Assumes that X-POINTS and Y-POINTS are both sorted smallest->largest."

    (let (rects left-x right-x top-y bottom-y tem-y-points tem-x tem-y rect)
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

  (define (rectangles-from-windows windows #!optional weight-fun)
    "Returns a list of (LEFT TOP RIGHT BOTTOM [OVERLAP-WEIGHT])
representing the rectangles represented by the list of window objects
WINDOWS.

If WEIGHT-FUN is defined, it is a function to call on each window to
return the weight for that window. If not defined, the window's
`weight' property is used instead."

    (mapcar (lambda (w)
	      (let ((dims (window-frame-dimensions w))
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

  (define (grid-from-rectangles rects #!optional with-root)
    "Given a list of rectangles RECTS (each rectangle represented by a
list of at least four elemements, `(LEFT TOP RIGHT BOTTOM)'), return
a cons cell `(X-POINTS . Y-POINTS)' defining the grid they represent.

If WITH-ROOT is non-nil, then add the root window boundaries to the
grid."

    (let ((x-edges (nconc (mapcar car rects)
                          (mapcar (lambda (x) (nth 2 x)) rects)))
          (y-edges (nconc (mapcar (lambda (x) (nth 1 x)) rects)
                          (mapcar (lambda (x) (nth 3 x)) rects))))
      (when with-root
        (setq x-edges (cons 0 (nconc x-edges (list (screen-width)))))
        (setq y-edges (cons 0 (nconc y-edges (list (screen-height))))))
      (cons (uniquify-list x-edges) (uniquify-list y-edges))))

  (define (rectangle-area rect)
    "Given a rectangle RECT, return its area."
    (* (- (nth 2 rect) (nth 0 rect))
       (- (nth 3 rect) (nth 1 rect))))

  (define (rectangle-from-coords point dims)
    "Return a list representing the rectangle with origin POINT and dimensions
DIMS."
    (list (car point) (cdr point)
	  (+ (car point) (car dims)) (+ (cdr point) (cdr dims))))

  (define (rectangle-corners rect)
    "Return a list of four cons cells representing the coordinates of the
corners of the rectangle RECT."
    (list (cons (nth 0 rect) (nth 1 rect))
	  (cons (nth 0 rect) (nth 3 rect))
	  (cons (nth 2 rect) (nth 1 rect))
	  (cons (nth 2 rect) (nth 3 rect))))

  (define (rectangle-center rect)
    (cons (+ (nth 0 rect) (/ (- (nth 2 rect) (nth 0 rect)) 2))
	  (+ (nth 1 rect) (/ (- (nth 3 rect) (nth 1 rect)) 2))))

  (define (rectangle-center* point dims)
    (cons (+ (car point) (/ (car dims) 2))
	  (+ (cdr point) (/ (cdr dims) 2))))

  (define (rectangle-union rect1 rect2)
    (list (min (nth 0 rect1) (nth 0 rect2))
	  (min (nth 1 rect1) (nth 1 rect2))
	  (max (nth 2 rect1) (nth 2 rect2))
	  (max (nth 3 rect1) (nth 3 rect2))
	  (nth 4 rect1)))

  (define (rectangle-intersection rect1 rect2)
    (let ((rect (list (max (nth 0 rect1) (nth 0 rect2))
		      (max (nth 1 rect1) (nth 1 rect2))
		      (min (nth 2 rect1) (nth 2 rect2))
		      (min (nth 3 rect1) (nth 3 rect2))
		      (nth 4 rect1))))
      (if (or (>= (nth 0 rect) (nth 2 rect))
	      (>= (nth 1 rect) (nth 3 rect)))
	  ;; empty
	  nil
	rect)))

;;; overlap

  (define (rect-1d-overlap a-1 a-2 b-1 b-2)
    "Returns the overlap between two 1D line segments, A-1 to A-2, and
B-1 to B-2."
    (max 0 (- (min a-2 b-2) (max a-1 b-1))))

  (define (rect-2d-overlap dims point rect)
    "Returns the overlap between two rectangles, one defined as having
dimensions DIMS and origin POINT, while the other is defined by RECT."

    (* (rect-1d-overlap (car point) (+ (car point) (car dims))
			(car rect) (nth 2 rect))
       (rect-1d-overlap (cdr point) (+ (cdr point) (cdr dims))
			(nth 1 rect) (nth 3 rect))
       ;; include the weight of the rectangle if it has one
       (or (nth 4 rect) 1)))

  (define (rect-2d-overlap* rect1 rect2)
    "Returns the overlap between two rectangles, RECT1 and RECT2."
    (* (rect-1d-overlap (car rect1) (nth 2 rect1)
			(car rect2) (nth 2 rect2))
       (rect-1d-overlap (nth 1 rect1) (nth 3 rect1)
			(nth 1 rect2) (nth 3 rect2))
       ;; include the weight of the rectangle if it has one
       (or (nth 4 rect1) 1)
       (or (nth 4 rect2) 1)))

  (define (rect-total-overlap dims point rects)
    "Returns the total area of the overlap between a rectangle defined by
DIMS and POINT, and the list of rectangles RECTS when placed at POINT."
    (let ((total 0))
      (mapc (lambda (r)
	      (setq total (+ total (rect-2d-overlap dims point r))))
	    rects)
      total))

  (define (rect-wholly-within-rect outer inner)
    "Return t if INNER is completely inside the boundaries of OUTER."
    (eq (rect-obscured inner outer) 'fully-obscured))

  (define (rect-wholly-visible-p rect)
    "Return t if RECT is completely inside the screen boundaries."
    (rect-wholly-within-rect (list 0 0 (screen-width) (screen-height))
                             rect))

  (define (rectangle-heads rect)
    "Return the number of screen heads that rectangle RECT appears on."
    (let loop ((head 0)
	       (count 0))
      (if (= head (head-count))
	  count
	(loop (1+ head)
	      (if (> (rect-2d-overlap (head-dimensions head)
				      (head-offset head) rect) 0)
		  (1+ count)
		count))))))
