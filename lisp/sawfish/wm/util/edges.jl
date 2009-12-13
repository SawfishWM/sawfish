;; edges.jl -- Identify all window edges

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

(define-structure sawfish.wm.util.edges

    (export get-visible-window-edges
	    grid-from-edges
	    edges-within-epsilon
	    snap-window-position-to-edges)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.viewport)

  (define-structure-alias edges sawfish.wm.util.edges)

  (define (get-visible-window-edges #!key with-ignored-windows
				    windows-to-ignore (windows t)
				    include-screen include-heads
                                    viewport)
    "Returns (X-EDGES . Y-EDGES), X-EDGES is a list of (X Y1 Y2 OPEN-P),
and Y-EDGES is a list of (Y X1 X2 OPEN-P). OPEN-P is t if the edge is
the left or top edge of a window. For the root window, the meaning of
OPEN-P is reversed.

#:include-heads may be a list of head ids, or a true non-list, in which
case all heads are included.

#:viewport may be a cons cell specifying (col . row); if specified
edges are given for that viewport rather than the current one.

The returned lists may contain duplicates, and are unsorted."

    (let* ((width (screen-width))
           (height (screen-height))
           (vp-offset (viewport-offset-coord viewport))
           x-edges y-edges)
      (map-windows
       (lambda (w)
	 (when (and (window-mapped-p w)
		    (window-visible-p w)
		    (not (window-outside-viewport-p w viewport))
		    (or with-ignored-windows
			(not (window-get w 'ignored)))
		    (not (memq w windows-to-ignore))
		    (or (not (listp windows)) (memq w windows)))
           (let ((dims (window-frame-dimensions w))
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
                                       y-edges)))))))

      (when include-screen
        (let* ((x-min (car vp-offset))
               (y-min (cdr vp-offset))
               (x-max (+ x-min width))
               (y-max (+ y-min height)))
          (setq x-edges (list* (list x-min y-min y-max nil)
                               (list x-max y-min y-max t)
                               x-edges))
          (setq y-edges (list* (list y-min x-min x-max nil)
                               (list y-max x-min x-max t)
                               y-edges))))

      (when (and include-heads (not (listp include-heads)))
	(setq include-heads (do ((i 0 (1+ i))
				 (lst '() (cons i lst)))
				((= i (head-count)) lst))))
      (when include-heads
	(mapc (lambda (h)
		(let* ((dims (head-dimensions h))
                       (offset (head-offset h))
                       (x-min (+ (car offset) (car vp-offset)))
                       (y-min (+ (cdr offset) (cdr vp-offset)))
                       (x-max (+ x-min (car dims)))
                       (y-max (+ y-min (cdr dims))))
		  (setq x-edges (list* (list x-min y-min y-max nil)
				       (list x-max y-min y-max t)
				       x-edges))
		  (setq y-edges (list* (list y-min x-min x-max nil)
				       (list y-max x-min x-max t)
				       y-edges))))
	      include-heads))

      (cons x-edges y-edges)))

  (define (grid-from-edges x-edges y-edges)
    "Given lists of edges X-EDGES and Y-EDGES, return a cons cell
`(X-POINTS . Y-POINTS)' describing the grid they represent."
    (cons (uniquify-list (mapcar car x-edges))
	  (uniquify-list (mapcar car y-edges))))

  ;; returns (EDGE-1 EDGE-2) where they're within EPSILON of each other. MODE
  ;; is one of the symbols `magnetism', `resistance', or `attraction'. DELTA
  ;; is the (signed) number of pixels moved since the last call
  (define (edges-within-epsilon list-1 list-2 epsilon delta
				old-edge #!optional mode)
    (let ((compare
	   (cond ((or (null mode) (eq mode 'magnetism))
		  (lambda (e1 e2)
		    (and (< (nth 1 e1) (nth 2 e2))
			 (> (nth 2 e1) (nth 1 e2))
			 (<= (- (max (car e1) (car e2))
				(min (car e1) (car e2)))
			     epsilon))))

		 ((eq mode 'attraction)
		  (lambda (e1 e2)
		    (and (< (nth 1 e1) (nth 2 e2))
			 (> (nth 2 e1) (nth 1 e2))
			 (or
			  (and (nth 3 e1) (not (nth 3 e2)) (<= delta 0)
			       (<= (- epsilon) (- (car e2) (car e1)) 0)
			       (or (< delta 0) (equal old-edge e2)))
			  (and (not (nth 3 e1)) (nth 3 e2) (>= delta 0)
			       (<= 0 (- (car e2) (car e1)) epsilon)
			       (or (> delta 0) (equal old-edge e2)))))))

		 ((eq mode 'resistance)
		  (lambda (e1 e2)
		    (and (< (nth 1 e1) (nth 2 e2))
			 (> (nth 2 e1) (nth 1 e2))
			 (or
			  (and (nth 3 e1) (not (nth 3 e2))
			       (<= (car e1) (car e2) (- (car e1) delta)))
			  (and (nth 3 e1) (not (nth 3 e2))
			       (equal old-edge e2)
			       (<= (- epsilon) (- (car e1) (car e2)) 0))
			  (and (not (nth 3 e1)) (nth 3 e2)
			       (>= (car e1) (car e2) (- (car e1) delta)))
			  (and (not (nth 3 e1)) (nth 3 e2)
			       (equal old-edge e2)
			       (<= 0 (- (car e1) (car e2)) epsilon)))))))))
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
  (define (snap-window-position-to-edges window coords deltas state
					 #!optional epsilon edges mode)
    (let* ((dims (window-frame-dimensions window))
	   (w-x-edges (list (list (car coords) (cdr coords)
				  (+ (cdr coords) (cdr dims)) t)
			    (list (+ (car coords) (car dims))
				  (cdr coords)
				  (+ (cdr coords) (cdr dims)) nil)))
	   (w-y-edges (list (list (cdr coords) (car coords)
				  (+ (car coords) (car dims)) t)
			    (list (+ (cdr coords) (cdr dims))
				  (car coords)
				  (+ (car coords) (car dims)) nil)))
	   tem)
      (setq coords (cons (car coords) (cdr coords)))
      (unless edges
	(setq edges (get-visible-window-edges)))
      (unless epsilon
	(setq epsilon 8))
      (unless mode
	(setq mode 'magnetism))

      ;; snap in X direction
      (if (setq tem (edges-within-epsilon
		     w-x-edges (car edges) epsilon
		     (car deltas) (car state) mode))
	  (progn
	    (rplaca state (cdr tem))
	    (setq tem (+ (car coords) (- (car (cdr tem)) (car (car tem)))))
	    (when (and (> tem (- (car dims))) (< tem (screen-width)))
	      (rplaca coords tem)))
	(rplaca state nil))

      ;; snap in Y direction
      (if (setq tem (edges-within-epsilon
		     w-y-edges (cdr edges) epsilon
		     (cdr deltas) (cdr state) mode))
	  (progn
	    (rplacd state (cdr tem))
	    (setq tem (+ (cdr coords) (- (car (cdr tem)) (car (car tem)))))
	    (when (and (> tem (- (cdr dims))) (< tem (screen-height)))
	      (rplacd coords tem)))
	(rplacd state nil))
      coords)))
