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

;; returns (X-EDGES . Y-EDGES), X-EDGES is a list of (X Y1 Y2), and
;; Y-EDGES is a list of (Y X1 X2)

;; keywords:
;;	:with-ignored-windows t
;;	:windows-to-ignore LIST
;;	:include-root t

(defun get-visible-window-edges (&rest args)
  (let
      ((with-ignored-windows (car (cdr (memq ':with-ignored-windows args))))
       (windows-to-ignore (car (cdr (memq ':windows-to-ignore args))))
       x-edges y-edges)
    (mapc #'(lambda (w)
	      (when (and (window-visible-p w)
			 (or with-ignored-windows
			     (not (window-get w 'ignored)))
			 (not (memq w windows-to-ignore)))
		(let
		    ((dims (window-frame-dimensions w))
		     (coords (window-position w)))
		  (setq x-edges (cons (list (car coords) (cdr coords)
					    (+ (cdr coords) (cdr dims)))
				      (cons (list (+ (car coords) (car dims))
						  (cdr coords)
						  (+ (cdr coords) (cdr dims)))
					    x-edges)))
		  (setq y-edges (cons (list (cdr coords) (car coords)
					    (+ (car coords) (car dims)))
				      (cons (list (+ (cdr coords) (cdr dims))
						  (car coords)
						  (+ (car coords) (car dims)))
					    y-edges))))))
	  (managed-windows))
    (when (car (cdr (memq ':include-root args)))
      (setq x-edges (cons (list 0 0 (screen-height))
			  (cons (list (screen-width) 0 (screen-height))
				x-edges)))
      (setq y-edges (cons (list 0 0 (screen-width))
			  (cons (list (screen-height) 0 (screen-width))
				y-edges))))
    (cons x-edges y-edges)))

(defmacro edges-abs (x)
  `(max x (- x)))

;; returns (EDGE-1 EDGE-2) where they're within EPSILON of each other
(defun edges-within-epsilon (list-1 list-2 epsilon)
  (catch 'out
    (mapc #'(lambda (edge-1)
	      (mapc #'(lambda (edge-2)
			(when (and (< (nth 1 edge-1) (nth 2 edge-2))
				   (> (nth 2 edge-1) (nth 1 edge-2))
				   (<= (- (max (car edge-1) (car edge-2))
					  (min (car edge-1) (car edge-2)))
				       epsilon))
			  (throw 'out (cons edge-1 edge-2))))
		    list-2))
	  list-1)
    nil))

;; returns the new (X . Y) of WINDOW
(defun snap-window-position-to-edges (window coords &optional epsilon edges)
  (let*
      ((dims (window-frame-dimensions window))
       (w-x-edges (list (list (car coords) (cdr coords)
			      (+ (cdr coords) (cdr dims)))
			(list (+ (car coords) (car dims))
			      (cdr coords) (+ (cdr coords) (cdr dims)))))
       (w-y-edges (list (list (cdr coords) (car coords)
			      (+ (car coords) (car dims)))
			(list (+ (cdr coords) (cdr dims))
			      (car coords) (+ (car coords) (car dims)))))
       tem)
    (setq coords (cons (car coords) (cdr coords)))
    (unless edges
      (setq edges (get-visible-window-edges)))
    (unless epsilon
      (setq epsilon 8))
    (when (setq tem (edges-within-epsilon w-x-edges (car edges) epsilon))
      (rplaca coords (+ (car coords) (- (car (cdr tem)) (car (car tem))))))
    (when (setq tem (edges-within-epsilon w-y-edges (cdr edges) epsilon))
      (rplacd coords (+ (cdr coords) (- (car (cdr tem)) (car (car tem))))))
    coords))
