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
  "Regular expression matching windows not to overlap in first-fit mode."
  :group placement
  :type string
  :allow-nil t)


;; utility functions

;; RECTS is a list of (LEFT RIGHT TOP BOTTOM); returns sorted, uniquified
;; (X-EDGES . Y-EDGES)
(defun sp-make-grid (rects &optional with-root)
  (let
      ((x-edges (nconc (mapcar 'car rects)
		       (mapcar #'(lambda (x) (nth 2 x)) rects)))
       (y-edges (nconc (mapcar #'(lambda (x) (nth 1 x)) rects)
		       (mapcar #'(lambda (x) (nth 3 x)) rects)))
       tem)
    (when with-root
      (setq x-edges (cons 0 x-edges))
      (setq y-edges (cons 0 y-edges)))
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

;; returns t if a rectangle of size DIMS doesn't overlap any of RECTS
;; when placed at POINT
(defun sp-rect-fits-p (dims point rects)
  (let*
      ((left-edge (car point))
       (top-edge (cdr point))
       (right-edge (+ (car point) (car dims)))
       (bottom-edge (+ (cdr point) (cdr dims))))
    (catch 'out
      (mapc #'(lambda (r)
		(when (and (or (and (<= (car r) left-edge)
				    (< left-edge (nth 2 r)))
			       (and (< (car r) right-edge)
				    (<= right-edge (nth 2 r)))
			       (and (<= left-edge (car r))
				    (<= (nth 2 r) right-edge)))
			   (or (and (<= (nth 1 r) top-edge)
				    (< top-edge (nth 3 r)))
			       (and (< (nth 1 r) bottom-edge)
				    (<= bottom-edge (nth 3 r)))
			       (and (<= top-edge (nth 1 r))
				    (<= (nth 3 r) bottom-edge))))
		  (throw 'out nil)))
	    rects)
      t)))

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
      ((point (cons 0 0)))
    (catch 'done
      (mapc #'(lambda (y)
		(rplacd point y)
		(mapc #'(lambda (x)
			  (rplaca point x)
			  (when (sp-rect-fits-p dims point rects)
			    (throw 'done point)))
		      (car grid)))
	    (cdr grid))
      nil)))


;; entry-points

;;;###autoload
(defun place-window-first-fit (w)
  (let*
      ((windows (sp-get-windows w))
       (rects (sp-make-rects windows))
       (grid (sp-make-grid rects t))
       (point (sp-first-fit (window-frame-dimensions w)
			    grid (nconc (sp-screen-rects) rects))))
    (if point
	(move-window-to w (car point) (cdr point))
      (place-window-randomly w))))
