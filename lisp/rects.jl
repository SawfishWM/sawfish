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

;; assumes that X-POINTS and Y-POINTS are both sorted smallest->largest
(defun rectangles-from-grid (x-points orig-y-points &optional pred)
  (let
      (rects left-x right-x top-y bottom-y y-points tem-x tem-y rect)
    (while x-points
      (setq left-x (car x-points))
      (setq x-points (cdr x-points))
      (setq y-points orig-y-points)
      (while y-points
	(setq top-y (car y-points))
	(setq y-points (cdr y-points))
	(setq tem-x x-points)
	(while tem-x
	  (setq right-x (car tem-x))
	  (setq tem-x (cdr tem-x))
	  (setq tem-y y-points)
	  (while tem-y
	    (setq bottom-y (car tem-y))
	    (setq tem-y (cdr tem-y))
	    (setq rect (list left-x top-y right-x bottom-y))
	    (when (or (not pred) (pred rect))
	      (setq rects (cons rect rects)))))))
    (nreverse rects)))

;; returns a list of (LEFT TOP RIGHT BOTTOM [OVERLAP-WEIGHT])
(defun rectangles-from-windows (windows &optional weight-fun)
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

;; returns (X-POINTS . Y-POINTS)
(defun grid-from-rectangles (rects &optional with-root)
  (let
      ((x-edges (nconc (mapcar car rects)
		       (mapcar (lambda (x) (nth 2 x)) rects)))
       (y-edges (nconc (mapcar (lambda (x) (nth 1 x)) rects)
		       (mapcar (lambda (x) (nth 3 x)) rects))))
    (when with-root
      (setq x-edges (cons 0 (nconc x-edges (list (screen-width)))))
      (setq y-edges (cons 0 (nconc y-edges (list (screen-height))))))
    (cons (uniquify-list x-edges) (uniquify-list y-edges))))

(defmacro rectangle-area (rect)
  `(* (- (nth 2 ,rect) (nth 0 ,rect))
      (- (nth 3 ,rect) (nth 1 ,rect))))


;; overlap

;; returns the overlap between two 1d line segments, A-1 to A-2, and
;; B-1 to B-2
(defmacro rect-1d-overlap (a-1 a-2 b-1 b-2)
  `(max 0 (- (min ,a-2 ,b-2) (max ,a-1 ,b-1))))

;; returns the overlap between two rectangles, one defined as having
;; dimensions DIMS and origin POINT, while the other is defined
;; by RECT
(defun rect-2d-overlap (dims point rect)
  (* (rect-1d-overlap (car point) (+ (car point) (car dims))
		      (car rect) (nth 2 rect))
     (rect-1d-overlap (cdr point) (+ (cdr point) (cdr dims))
		      (nth 1 rect) (nth 3 rect))
     ;; include the weight of the rectangle if it has one
     (or (nth 4 rect) 1)))

;; similar but for two rectangles
(defun rect-2d-overlap* (rect1 rect2)
  (* (rect-1d-overlap (car rect1) (nth 2 rect1) (car rect2) (nth 2 rect2))
     (rect-1d-overlap (nth 1 rect1) (nth 3 rect1) (nth 1 rect2) (nth 3 rect2))
     ;; include the weight of the rectangle if it has one
     (or (nth 4 rect1) 1)
     (or (nth 4 rect2) 1)))

;; returns the total area of the overlap between a rectangle defined by
;; DIMS and POINT, and the list of rectangles RECTS
;; when placed at POINT
(defun rect-total-overlap (dims point rects)
  (let
      ((total 0))
    (mapc (lambda (r)
	    (setq total (+ total (rect-2d-overlap dims point r))))
	  rects)
    total))

;; return t if RECT is a subset of the screen area
(defun rect-wholly-visible-p (rect)
  (and (>= (car rect) 0) (>= (nth 1 rect) 0)
       (<= (nth 2 rect) (screen-width)) (<= (nth 3 rect) (screen-height))))
