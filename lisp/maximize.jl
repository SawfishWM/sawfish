;; maximize.jl -- window maximization
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

(require 'edges)
(require 'rects)
(provide 'maximize)

;; Commentary:

;; This sets the window property `unmaximized-geometry' of each
;; currently maximize window to `(X Y W H)', the saved geometry.

(defcustom maximize-always-expands nil
  "Maxmizing a window dimension always increases the size of that dimension."
  :group maximize
  :type boolean)

(defcustom maximize-raises t
  "Raise windows when they're maximized."
  :group maximize
  :type boolean)

(defcustom maximize-ignore-when-filling t
  "Let ignored windows be overlapped when filling windows."
  :group maximize
  :type boolean)

;; called when a window is maximized, args (W &optional DIRECTION)
(defvar window-maximized-hook nil)

;; called when a window is un-maximized, args (W &optional DIRECTION)
(defvar window-unmaximized-hook nil)


;; handling maximized state

(defun window-maximized-p (w)
  (window-get w 'unmaximized-geometry))

(defun window-maximized-horizontally-p (w)
  (window-get w 'maximized-horizontally))

(defun window-maximized-vertically-p (w)
  (window-get w 'maximized-vertically))

(defun maximize-discard (w &optional horizontally vertically)
  (when horizontally
    (window-put w 'maximized-horizontally nil))
  (when vertically
    (window-put w 'maximized-vertically nil))
  (let
      ((dims (window-dimensions w))
       (coords (window-position w))
       (saved (window-get w 'unmaximized-geometry)))
    (when saved
      (unless (window-maximized-horizontally-p w)
	(rplaca saved (car coords))
	(rplaca (nthcdr 2 saved) (car dims)))
      (unless (window-maximized-vertically-p w)
	(rplaca (cdr saved) (cdr coords))
	(rplaca (nthcdr 3 saved) (cdr dims))))
    (when (and (not (window-maximized-vertically-p w))
	       (not (window-maximized-horizontally-p w)))
      (window-put w 'unmaximized-geometry nil))))

(defun maximize-discard-move (w directions)
  (maximize-discard w (memq 'horizontal directions)
		    (memq 'vertical directions)))

(defun maximize-discard-resize (w edges)
  (maximize-discard w (or (memq 'left edges) (memq 'right edges))
		    (or (memq 'top edges) (memq 'bottom edges))))


;; 1D packing

(defmacro maximize-edges-touching (start end edge)
  `(> (- (min ,end (nth 2 ,edge)) (max ,start (nth 1 ,edge))) 0))

(defun maximize-expand-edges (start end min max perp-1 perp-2 edges)
  (if maximize-always-expands
      (progn
	(mapc #'(lambda (edge)
		  ;; EDGE is (PERP START END OPEN-P)
		  (if (nth 3 edge)
		      (when (and (< (car edge) max)
				 (>= (car edge) perp-2)
				 (> (car edge) min)
				 (maximize-edges-touching start end edge))
			(setq max (car edge)))
		    (when (and (> (car edge) min)
			       (<= (car edge) perp-1)
			       (< (car edge) max)
			       (maximize-edges-touching start end edge))
		      (setq min (car edge)))))
	      edges)
	(cons min max))
    (let
	((opening (sort (mapcar 'car (filter #'(lambda (e)
						 (and (nth 3 e)
						      (maximize-edges-touching
						       start end e)))
					     edges))))
	 (closing (sort (mapcar 'car (filter #'(lambda (e)
						 (and (not (nth 3 e))
						      (maximize-edges-touching
						       start end e)))
					     edges))))
	 max-perp
	 (max-size 0))
      ;; find the maximum gap between a closing and an opening edge which
      ;; doesn't include any other opening edges
      (while (and opening closing)
	(while (and opening (<= (car opening) (car closing)))
	  (setq opening (cdr opening)))
	(when (and (> (- (or (car opening) max) (car closing)) max-size)
		   (or (not (nth 1 closing))
		       (>= (nth 1 closing) (or (car opening) max))))
	  (setq max-perp (car closing))
	  (setq max-size (- (car opening) (car closing))))
	(setq closing (cdr closing)))
      (if max-perp
	  (cons max-perp (+ max-perp max-size))
	(cons perp-1 perp-2)))))

(defun maximize-do-horizontal (w edges coords dims fdims)
  (let
      ((x-span (maximize-expand-edges (cdr coords) (+ (cdr coords)
						      (cdr fdims))
				      0 (screen-width)
				      (car coords) (+ (car coords)
						      (car fdims))
				      (car edges))))
    (rplaca coords (car x-span))
    (rplaca dims (- (- (cdr x-span) (car x-span))
		    (- (car fdims) (car dims))))
    w))

(defun maximize-do-vertical (w edges coords dims fdims)
  (let
      ((y-span (maximize-expand-edges (car coords) (+ (car coords)
						      (car fdims))
				      0 (screen-height)
				      (cdr coords) (+ (cdr coords)
							  (cdr fdims))
				      (cdr edges))))
    (rplacd coords (car y-span))
    (rplacd dims (- (- (cdr y-span) (car y-span))
		    (- (cdr fdims) (cdr dims))))
    w))


;; 2D packing

(defun maximize-do-both (window avoided edges coords dims fdims)
  (let*
      ((grid (grid-from-edges (car edges) (cdr edges)))
       rects)
    (setq rects (rectangles-from-grid
		 (sort (car grid)) (sort (cdr grid))
		 #'(lambda (rect)
		     ;; the rectangle mustn't overlap any avoided windows
		     (catch 'foo
		       (mapc #'(lambda (w)
				 (when (> (rect-2d-overlap
					   (window-frame-dimensions w)
					   (window-position w)
					   rect) 0)
				   (throw 'foo nil)))
			     avoided)
		       t))))
	  
    ;; find the largest rectangle
    (let
	((max-area 0)
	 (max-rect nil))
      (mapc #'(lambda (rect)
		(when (and (rect-wholly-visible-p rect)
			   (> (rectangle-area rect) max-area))
		  (setq max-area (rectangle-area rect))
		  (setq max-rect rect))) rects)
      (when max-rect
	(rplaca coords (nth 0 max-rect))
	(rplacd coords (nth 1 max-rect))
	(rplaca dims (- (- (nth 2 max-rect) (nth 0 max-rect))
			(- (car fdims) (car dims))))
	(rplacd dims (- (- (nth 3 max-rect) (nth 1 max-rect))
			(- (cdr fdims) (cdr dims))))
	window))))


;; commands

;;;###autoload
(defun maximize-window (w &optional direction only-1d)
  "Maximize the dimensions of the window."
  (interactive "%W")
  (let*
      ((coords (window-position w))
       (dims (window-dimensions w))
       (fdims (window-frame-dimensions w))
       (avoided (avoided-windows w))
       (edges (get-visible-window-edges ':with-ignored-windows t
					':windows avoided
					':include-root t)))
    (unless (window-get w 'unmaximized-geometry)
      (window-put w 'unmaximized-geometry (list (car coords) (cdr coords)
						(car dims) (cdr dims))))
    (cond ((null direction)
	   (if (not only-1d)
	       (maximize-do-both w avoided edges coords dims fdims)
	     (maximize-do-horizontal w edges coords dims fdims)
	     (maximize-do-vertical w edges coords dims fdims))
	   (window-put w 'maximized-horizontally t)
	   (window-put w 'maximized-vertically t))
	  ((eq direction 'horizontal)
	   (maximize-do-horizontal w edges coords dims fdims)
	   (window-put w 'maximized-horizontally t))
	  ((eq direction 'vertical)
	   (maximize-do-vertical w edges coords dims fdims)
	   (window-put w 'maximized-vertically t)))
    (move-window-to w (car coords) (cdr coords))
    (resize-window-to w (car dims) (cdr dims))
    (when maximize-raises
      (raise-window w))
    (call-window-hook 'window-maximized-hook w (list direction))
    (call-window-hook 'window-state-change-hook w)))

;;;###autoload
(defun unmaximize-window (w &optional direction)
  "Restore the dimensions of the window to its original, unmaximized, state."
  (interactive "%W")
  (let
      ((geom (window-get w 'unmaximized-geometry))
       (coords (window-position w))
       (dims (window-dimensions w)))
    (when geom
      (when (or (null direction) (eq direction 'horizontal))
	(rplaca coords (nth 0 geom))
	(rplaca dims (nth 2 geom))
	(window-put w 'maximized-horizontally nil))
      (when (or (null direction) (eq direction 'vertical))
	(rplacd coords (nth 1 geom))
	(rplacd dims (nth 3 geom))
	(window-put w 'maximized-vertically nil))
      (resize-window-to w (car dims) (cdr dims))
      (move-window-to w (car coords) (cdr coords))
      (when (and (not (window-maximized-vertically-p w))
		 (not (window-maximized-horizontally-p w)))
	(window-put w 'unmaximized-geometry nil))
      (call-window-hook 'window-unmaximized-hook w (list direction))
      (call-window-hook 'window-state-change-hook w))))

;;;###autoload
(defun maximize-window-vertically (w)
  "Maximize the vertical dimension of the window."
  (interactive "%W")
  (maximize-window w 'vertical))

;;;###autoload
(defun maximize-window-horizontally (w)
  "Maximize the horizontal dimension of the window."
  (interactive "%W")
  (maximize-window w 'horizontal))

;;;###autoload
(defun maximize-window-toggle (w &optional direction)
  "Toggle the state of the window between maximized and unmaximized."
  (interactive "%W")
  (if (window-maximized-p w)
      (unmaximize-window w direction)
    (maximize-window w direction)))

;;;###autoload
(defun maximize-window-vertically-toggle (w)
  "Toggle the state of the window between vertically maximized and
unmaximized."
  (interactive "%W")
  (maximize-window-toggle w 'vertical))

;;;###autoload
(defun maximize-window-horizontally-toggle (w)
  "Toggle the state of the window between horizontally maximized and
unmaximized."
  (interactive "%W")
  (maximize-window-toggle w 'horizontal))


;; fill commands

;;;###autoload
(defun maximize-fill-window (w &optional direction)
  "Maximize the window without obscuring any other windows."
  (interactive "%W")
  (let
      ((avoid-by-default t)
       (maximize-always-expands t)
       (avoid-ignored maximize-ignore-when-filling))
    (maximize-window w direction t)))

;;;###autoload
(defun maximize-fill-window-vertically (w)
  "Maximize the window vertically without obscuring any other windows."
  (interactive "%W")
  (maximize-fill-window w 'vertical))

;;;###autoload
(defun maximize-fill-window-horizontally (w)
  (interactive "%W")
  "Maximize the window horizontally without obscuring any other windows."
  (maximize-fill-window w 'horizontal))

;;;###autoload
(defun maximize-fill-window-toggle (w &optional direction)
  "Toggle the state of the window between maximized-filled and unmaximized."
  (interactive "%W")
  (if (window-maximized-p w)
      (unmaximize-window w direction)
    (maximize-fill-window w direction)))

;;;###autoload
(defun maximize-fill-window-vertically-toggle (w)
  "Toggle the state of the window between vertically maximized-filled and
unmaximized."
  (interactive "%W")
  (maximize-fill-window-toggle w 'vertical))

;;;###autoload
(defun maximize-fill-window-horizontally-toggle (w)
  "Toggle the state of the window between horizontally maximized-filled and
unmaximized."
  (interactive "%W")
  (maximize-fill-window-toggle w 'horizontal))


;; initialisation

(sm-add-saved-properties
 'unmaximized-geometry 'maximized-vertically 'maximized-horizontally)

(add-hook 'after-move-hook 'maximize-discard-move)
(add-hook 'after-resize-hook 'maximize-discard-resize)
