;; configure.jl -- default configure-request handler
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.state.configure

    (export windows-intersect-p
	    window-occludes-p
	    configure-request-handler
	    configure-choose-gravity)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.workspace
	  sawfish.wm.stacking
	  sawfish.wm.viewport)

  (defcustom configure-auto-gravity t
    "Automatically select window gravity from position on screen."
    :type boolean
    :user-level expert
    :group move)

  ;; Returns true if window window1 and window2 intersect, false otherwise.
  (defun windows-intersect-p (window1 window2)
    (let ((w1pos (window-position window1))
	  (w2pos (window-position window2))
	  (w1dim (window-dimensions window1))
	  (w2dim (window-dimensions window2)))
      (not (or (not (windows-share-workspace-p window1 window2))
	       (> (car w1pos) (+ (car w2pos) (car w2dim)))
	       (> (car w2pos) (+ (car w1pos) (car w1dim)))
	       (> (cdr w1pos) (+ (cdr w2pos) (cdr w2dim)))
	       (> (cdr w2pos) (+ (cdr w1pos) (cdr w1dim)))))))

  ;; Returns true if window w occludes any window in the window
  ;; list wlist, false otherwise.  Windows do not occlude
  ;; themself.  If inverse is set and true, this function
  ;; returns whether w is occluded _by_ any window in wlist.
  (defun window-occludes-p (w wlist #!optional inverse)
    (letrec ((xid< (lambda (a b) (< (window-id a) (window-id b))))
	     (order (if inverse (reverse (stacking-order)) (stacking-order)))
	     (below-w (sort (cdr (member w order)) xid<))
	     (sorted-wlist (sort (copy-sequence wlist) xid<))
	     (iter (lambda (below wins)
		     (cond ((or (not below) (not wins)) nil)
			   ((xid< (car below) (car wins))
			    (iter (cdr below) wins))
			   ((xid< (car wins) (car below))
			    (iter below (cdr wins)))
			   ((windows-intersect-p w (car wins)) t)
			   (t (iter (cdr below) (cdr wins)))))))
      (iter below-w sorted-wlist)))
  
  (defun configure-request-handler (w alist)
    (let ((coords (window-position w))
	  (dims (window-dimensions w))
	  (hints (window-size-hints w))
	  tem)

      (when (setq tem (cdr (assq 'stack alist)))
	(let ((relation (car tem))
	      (sibling (car (cdr tem))))
	  (case relation
	    ((above)
	     (if sibling (stack-window-above w sibling) (raise-window w)))
	    ((below)
	     (if sibling (stack-window-below w sibling) (lower-window w)))
	    ((top-if)
	     (if (window-occludes-p w (if sibling (list sibling)
					(stacking-order)) t)
		 (raise-window w)))
	    ((bottom-if)
	     (if (window-occludes-p w (if sibling (list sibling)
					(stacking-order)))
		 (lower-window w)))
	    ((opposite)
	     (cond ((window-occludes-p w (if sibling (list sibling)
					   (stacking-order)) t)
		    (raise-window w))
		   ((window-occludes-p w (if sibling (list sibling)
					   (stacking-order)))
		    (lower-window w))))
	    (t (error "Bad stacking relation: %s" relation)))))

      (when (setq tem (cdr (assq 'dimensions alist)))
	(when (not (assq 'position alist))
	  (let ((gravity (or (window-get w 'gravity)
			     (and (window-get w 'auto-gravity)
				  (configure-choose-gravity w))
			     (cdr (assq 'window-gravity hints))
			     (and configure-auto-gravity
				  (not (window-get w 'client-set-position))
				  (configure-choose-gravity w)))))

	    ;; anchor the window to the point specified by the gravity
	    (when (memq gravity '(east south-east north-east))
	      ;; [x] placed relative to the right of the frame
	      (rplaca coords (- (car coords) (- (car tem) (car dims)))))
	    (when (memq gravity '(north center south))
	      ;; [x] placed relative to the center
	      (rplaca coords (- (car coords) (quotient (- (car tem)
							  (car dims)) 2))))
	    (when (memq gravity '(south south-east south-west))
	      ;; [y] placed relative to the bottom of the frame
	      (rplacd coords (- (cdr coords) (- (cdr tem) (cdr dims)))))
	    (when (memq gravity '(east center west))
	      ;; [y] placed relative to the center
	      (rplacd coords (- (cdr coords) (quotient (- (cdr tem)
							  (cdr dims)) 2))))))
	(setq dims tem))

      (when (setq tem (cdr (assq 'position alist)))
	;; if the program is setting its position, best not to interfere..
	(window-put w 'client-set-position t)
	(let ((grav (or (window-get w 'gravity)
			(cdr (assq 'window-gravity hints))
			;; default gravity is NorthWest (from ICCCM)
			'north-west)))
	  (setq coords (adjust-position-for-gravity w grav tem))))

      (move-resize-window-to w (car coords) (cdr coords) (car dims) (cdr dims))

      ;; force the window to be somewhere in the virtual workspace..
      (when (and (not (window-get w 'client-set-position))
		 (window-outside-workspace-p w))
	(move-window-to-current-viewport w))))

  ;; decide which gravity to use to resize window W
  (defun configure-choose-gravity (w)
    (let* ((delta (lambda (x-1 y-1 x-2 y-2)
		    (+ (* (- x-2 x-1) (- x-2 x-1))
		       (* (- y-2 y-1) (- y-2 y-1)))))
	   (width (screen-width))
	   (height (screen-height))
	   (dims (window-frame-dimensions w))
	   (coords (window-absolute-position w))
	   (min-delta 1000000)
	   (min-i 1)
	   (min-j 1))

      (do ((i 0 (1+ i)))
	  ((= i 3))
	(do ((j 0 (1+ j)))
	    ((= j 3))
	  ;; divide the window into quarters; for each vertex find the
	  ;; distance to the same vertex of the screen. The minimum
	  ;; distance found gives the vertex to anchor the window to
	  (let
	      ((d (delta (+ (* i (/ (car dims) 2)) (car coords))
			 (+ (* j (/ (cdr dims) 2)) (cdr coords))
			 (* i (/ width 2))
			 (* j (/ height 2)))))
	    (when (< d min-delta)
	      (setq min-delta d)
	      (setq min-i i)
	      (setq min-j j)))))
      (aref (aref [[north-west north north-east]
		   [west center east]
		   [south-west south south-east]] min-j) min-i)))

  (add-hook 'configure-request-hook configure-request-handler))
