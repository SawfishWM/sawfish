;; edges.jl -- Identify all window edges
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

(provide 'edges)

;; returns (X-EDGES . Y-EDGES), X-EDGES is a list of (X Y1 Y2 OPEN-P), and
;; Y-EDGES is a list of (Y X1 X2 OPEN-P). OPEN-P is t if the edge is
;; left or top edge of a window. For the root window, the meaning of
;; OPEN-P is reversed

;; the returned lists may contain duplicates, and are unsorted

;; keywords:
;;	:with-ignored-windows t
;;	:windows-to-ignore LIST
;;	:windows LIST
;;	:include-root t

(defun get-visible-window-edges (&rest args)
  (let
      ((with-ignored-windows (car (cdr (memq ':with-ignored-windows args))))
       (windows-to-ignore (car (cdr (memq ':windows-to-ignore args))))
       (windows (cdr (memq ':windows args)))
       x-edges y-edges)
    (mapc (lambda (w)
	    (when (and (window-mapped-p w)
		       (window-visible-p w)
		       (not (window-outside-viewport-p w))
		       (or with-ignored-windows
			   (not (window-get w 'ignored)))
		       (not (memq w windows-to-ignore))
		       (or (null windows) (memq w (car windows))))
	      (let
		  ((dims (window-frame-dimensions w))
		   (coords (window-position w)))
		(setq x-edges (cons (list (car coords) (cdr coords)
					  (+ (cdr coords) (cdr dims)) t)
				    (cons (list (+ (car coords) (car dims))
						(cdr coords)
						(+ (cdr coords) (cdr dims))
						nil)
					  x-edges)))
		(setq y-edges (cons (list (cdr coords) (car coords)
					  (+ (car coords) (car dims)) t)
				    (cons (list (+ (cdr coords) (cdr dims))
						(car coords)
						(+ (car coords) (car dims))
						nil)
					  y-edges))))))
	  (managed-windows))
    (when (car (cdr (memq ':include-root args)))
      (setq x-edges (cons (list 0 0 (screen-height) nil)
			  (cons (list (screen-width) 0 (screen-height) t)
				x-edges)))
      (setq y-edges (cons (list 0 0 (screen-width) nil)
			  (cons (list (screen-height) 0 (screen-width) t)
				y-edges))))
    (cons x-edges y-edges)))

(defun grid-from-edges (x-edges y-edges)
  (cons (uniquify-list (mapcar car x-edges))
	(uniquify-list (mapcar car y-edges))))

;; returns (EDGE-1 EDGE-2) where they're within EPSILON of each other. MODE
;; is one of the symbols `magnetism', `resistance', or `attraction'. DELTA
;; is the (signed) number of pixels moved since the last call
(defun edges-within-epsilon (list-1 list-2 epsilon delta
			     old-edge &optional mode)
  (let
      ((compare (cond ((or (null mode) (eq mode 'magnetism))
		       (lambda (e1 e2)
			 (and (< (nth 1 e1) (nth 2 e2))
			      (> (nth 2 e1) (nth 1 e2))
			      (<= (- (max (car e1) (car e2))
				     (min (car e1) (car e2)))
				  epsilon))))
		      ((eq mode 'attraction)
		       (cond ((zerop delta)
			      (lambda (e1 e2) nil))
			     ((> delta 0)
			      (lambda (e1 e2)
				(and (not (nth 3 e1)) (nth 3 e2)
				     (< (nth 1 e1) (nth 2 e2))
				     (> (nth 2 e1) (nth 1 e2))
				     (<= 0 (- (car e2) (car e1)) epsilon))))
			     ((< delta 0)
			      (lambda (e1 e2)
				(and (nth 3 e1) (not (nth 3 e2))
				     (< (nth 1 e1) (nth 2 e2))
				     (> (nth 2 e1) (nth 1 e2))
				     (<= (- epsilon) (- (car e2)
							(car e1)) 0))))))
		      ((eq mode 'resistance)
		       (cond ((zerop delta)
			      (lambda (e1 e2) nil))
			     ((> delta 0)
			      (lambda (e1 e2)
				(and (or (and (not (nth 3 e1)) (nth 3 e2)
					      (>= (car e1) (car e2)))
					 (and (equal old-edge e2)
					      (<= (car e1) (car e2))))
				     (< (nth 1 e1) (nth 2 e2))
				     (> (nth 2 e1) (nth 1 e2))
				     (<= (- epsilon)
					 (- (car e1) (car e2))
					 epsilon))))
			     ((< delta 0)
			      (lambda (e1 e2)
				(and (or (and (nth 3 e1) (not (nth 3 e2))
					      (<= (car e1) (car e2)))
					 (and (equal old-edge e2)
					      (>= (car e1) (car e2))))
				     (< (nth 1 e1) (nth 2 e2))
				     (> (nth 2 e1) (nth 1 e2))
				     (<= (- epsilon)
					 (- (car e1) (car e2))
					 epsilon)))))))))
    (catch 'out
      (mapc (lambda (edge-1)
	      (mapc (lambda (edge-2)
		      (when (compare edge-1 edge-2)
			(throw 'out (cons edge-1 edge-2))))
		    list-2))
	    list-1)
      nil)))

;; returns the new (X . Y) of WINDOW. STATE is a cons cell that will be
;; used to record state across successive calls
(defun snap-window-position-to-edges (window coords deltas state
				      &optional epsilon edges mode)
  (let*
      ((dims (window-frame-dimensions window))
       (w-x-edges (list (list (car coords) (cdr coords)
			      (+ (cdr coords) (cdr dims)) t)
			(list (+ (car coords) (car dims))
			      (cdr coords) (+ (cdr coords) (cdr dims)) nil)))
       (w-y-edges (list (list (cdr coords) (car coords)
			      (+ (car coords) (car dims)) t)
			(list (+ (cdr coords) (cdr dims))
			      (car coords) (+ (car coords) (car dims)) nil)))
       (hysteresis (or (car state) (rplaca state (cons 0 0))))
       (old-snap-edges (or (cdr state) (rplacd state (cons))))
       tem)
    (setq coords (cons (car coords) (cdr coords)))
    (unless edges
      (setq edges (get-visible-window-edges)))
    (unless epsilon
      (setq epsilon 8))
    (unless mode
      (setq mode 'magnetism))

    (unless (zerop (car deltas))
      (rplaca hysteresis (car deltas)))
    (unless (zerop (cdr deltas))
      (rplacd hysteresis (cdr deltas)))

    ;; snap in X direction
    (if (setq tem (edges-within-epsilon
		   w-x-edges (car edges) epsilon
		   (car hysteresis) (car old-snap-edges) mode))
	(progn
	  (rplaca old-snap-edges (cdr tem))
	  (setq tem (+ (car coords) (- (car (cdr tem)) (car (car tem)))))
	  (when (and (> tem (- (car dims))) (< tem (screen-width)))
	    (rplaca coords tem)))
      (rplaca old-snap-edges nil))

    ;; snap in Y direction
    (if (setq tem (edges-within-epsilon
		   w-y-edges (cdr edges) epsilon
		   (cdr hysteresis) (cdr old-snap-edges) mode))
	(progn
	  (rplacd old-snap-edges (cdr tem))
	  (setq tem (+ (cdr coords) (- (car (cdr tem)) (car (car tem)))))
	  (when (and (> tem (- (cdr dims))) (< tem (screen-height)))
	    (rplacd coords tem)))
      (rplacd old-snap-edges nil))
    coords))
