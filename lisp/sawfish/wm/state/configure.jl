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

(provide 'configure)

(defcustom configure-auto-gravity t
  "Automatically select window gravity from position on screen."
  :type boolean
  :group (move advanced))

(defun configure-request-handler (w alist)
  (let
      ((coords (window-position w))
       (dims (window-dimensions w))
       (fdims (window-frame-dimensions w))
       tem)

    (when (setq tem (cdr (assq 'stack alist)))
      (cond ((eq tem 'below)
	     (lower-window w))
	    ((eq tem 'above)
	     ;; for the old GNOME pager's benefit..
	     (uniconify-window w)
	     (raise-window w))))

    (when (setq tem (cdr (assq 'dimensions alist)))
      (when (not (assq 'position alist))
	(let
	    ((gravity (or (window-get w 'gravity)
			  (and (window-get w 'auto-gravity)
			       (configure-choose-gravity w))
			  (cdr (assq 'window-gravity (window-size-hints w)))
			  (and configure-auto-gravity
			       (not (window-get w 'fixed-position))
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
      (setq coords tem)
      ;; if the program is setting its position, best not to interfere..
      (window-put w 'fixed-position t))
    (move-resize-window-to w (car coords) (cdr coords) (car dims) (cdr dims))

    ;; force the window to be somewhere in the virtual workspace..
    (when (and (not (window-get w 'fixed-position))
	       (window-outside-workspace-p w))
      (move-window-to-current-viewport w))))

;; decide which gravity to use to resize window W
(defun configure-choose-gravity (w)
  (let*
      ((abs (lambda (x)
	      (max x (- x))))
       (delta (lambda (x-1 y-1 x-2 y-2)
		;; no sqrt function...
		(+ (* (- x-2 x-1) (- x-2 x-1)) (* (- y-2 y-1) (- y-2 y-1)))))
       (width (screen-width))
       (height (screen-height))
       (dims (window-frame-dimensions w))
       (coords (window-absolute-position w))
       (min-delta 1000000)
       (min-i 1)
       (min-j 1)
       i j)
    ;; divide the window into quarters; for each vertex find the
    ;; distance to the same vertex of the screen. The minimum
    ;; distance found gives the vertex to anchor the window to
    (setq i 0)
    (while (< i 3)
      (setq j 0)
      (while (< j 3)
	(let
	    ((d (delta (+ (* i (/ (car dims) 2)) (car coords))
		       (+ (* j (/ (cdr dims) 2)) (cdr coords))
		       (* i (/ width 2))
		       (* j (/ height 2)))))
	  (when (< d min-delta)
	    (setq min-delta d)
	    (setq min-i i)
	    (setq min-j j)))
	(setq j (1+ j)))
      (setq i (1+ i)))
    (aref (aref [[north-west north north-east]
		 [west center east]
		 [south-west south south-east]] min-j) min-i)))

(add-hook 'configure-request-hook configure-request-handler)
