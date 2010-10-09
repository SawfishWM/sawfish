;; workarea.jl -- calculating the usable region of a head
;;
;; Author: John Harper <jsh@unfactored.org>
;;
;; Copyright (C) 2002 John Harper
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.util.workarea

    (export define-window-strut
	    largest-rectangle-from-edges
	    calculate-workarea
	    calculate-workarea-from-struts)

    (open rep
	  rep.system
	  sawfish.wm.util.edges
	  sawfish.wm.util.rects
	  sawfish.wm.windows
	  sawfish.wm.workspace
          sawfish.wm.viewport
	  sawfish.wm.misc)

  (define (define-window-strut w left top right bottom)
    (let ((new (list left top right bottom))
	  (old (window-get w 'workarea-strut)))
      (unless (equal old new)
	(window-put w 'workarea-strut new)
	(call-hook 'workarea-changed-hook))))

  (define (combined-struts #!key (space current-workspace))
    (let ((struts (mapcar (lambda (x)
			    (window-get x 'workarea-strut))
			  (filter-windows
			   (lambda (x)
			     (window-appears-in-workspace-p x space))))))
      (list (apply max (cons 0 (delq nil (mapcar car struts))))
	    (apply max (cons 0 (delq nil (mapcar cadr struts))))
	    (apply max (cons 0 (delq nil (mapcar caddr struts))))
	    (apply max (cons 0 (delq nil (mapcar cadddr struts)))))))

  (define (apply-struts-to-rect struts head-rect)
    (list (+ (nth 0 head-rect) (nth 0 struts))
	  (+ (nth 1 head-rect) (nth 1 struts))
	  (- (nth 2 head-rect) (nth 2 struts))
	  (- (nth 3 head-rect) (nth 3 struts))))

  (define (largest-rectangle-from-edges edges #!key avoided head)

    (define (pred rect)
      ;; the rectangle mustn't overlap any avoided windows
      ;; or span multiple heads, or be on a different head
      ;; to that requested
      (let* ((viewport (viewport-at (nth 0 rect)
                                    (nth 1 rect)))
             (cur-vp (screen-viewport))
             (x-offset (and viewport (* (screen-width)
                                        (- (car viewport)
                                           (car cur-vp)))))
             (y-offset (and viewport (* (screen-height)
                                        (- (cdr viewport)
                                           (cdr cur-vp))))))
        (let loop ((rest avoided))
	  (cond ((null rest) (rect-within-head-p rect head))
		((> (rect-2d-overlap
		     (window-frame-dimensions (car rest))
		     (let ((pos (window-position (car rest))))
		       (if (window-get (car rest) 'sticky-viewport)
			   (cons (+ (car pos) x-offset)
				 (+ (cdr pos) y-offset))
			 pos))
		     rect)
		    0)
		 nil)
		(t (loop (cdr rest)))))))

    (let* ((grid (grid-from-edges (car edges) (cdr edges)))
	   ;; find all possible rectangles
           (rects (rectangles-from-grid (sort (car grid))
                                        (sort (cdr grid))
                                        pred)))

      ;; return the largest
      (let ((max-area 0)
	    (max-rect nil))
	(mapc (lambda (rect)
		(when (and (rect-within-viewport-p rect)
			   (> (rectangle-area rect) max-area))
		  (setq max-area (rectangle-area rect))
		  (setq max-rect rect)))
              rects)
	max-rect)))

  (define (calculate-workarea #!key window head)
    "Return the rectangle representing the largest rectangle on the
screen that doesn't overlap any avoided windows, or nil.  If HEAD is
true, then an empty workarea will be replaced with the area of HEAD,
not the current head (of WINDOW)."
    (unless head
      (setq head (current-head window)))
    (let* ((avoided (avoided-windows window))
	   ;; Find the edges of all "do not cover" windows
	   (edges (get-visible-window-edges
		   #:with-ignored-windows t
		   #:windows avoided
		   #:include-heads (list head)))
	   ;; Find the rectangle covering the current head
	   (head-rect (rectangle-from-coords (head-offset head)
					     (head-dimensions head)))
	   ;; Find the largest rectangle
	   (rect (or (largest-rectangle-from-edges
		      edges #:avoided avoided #:head head) head-rect)))
      ;; Shrink that to the union of all struts
      (rectangle-intersection
       rect (apply-struts-to-rect (combined-struts) head-rect))))

  (define (calculate-workarea-from-struts #!key (workspace current-workspace))
    (apply-struts-to-rect (combined-struts workspace)
			  (rectangle-from-coords
			   (cons 0 0) (screen-dimensions))))

  (define (on-unmap w)
    (when (window-get w 'workarea-strut)
      (call-hook 'workarea-changed-hook)))

  (add-hook 'unmap-notify-hook on-unmap))
