;; gtk/theme.jl
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

(require 'gtkrc)

(let*
    ((minimize (list (make-image "as_min.png") nil
		     nil (make-image "as_min-b.png")))
     ;; 15x15
     (close (list (make-image "as_close.png") nil
		  nil (make-image "as_close-b.png")))

     ;; image tiles constructed when no background pixmaps, only colours
     (background-images nil)

     ;; frame definitions, constructed dynamically by construct-frame-defs
     (frame nil)
     (shaped-frame nil)
     (transient-frame nil)
     (shaped-transient-frame nil)

     ;; for pixmap frames; this is going to use horrendous amounts of memory,
     ;; but what other options are there..?
     (render-bg (lambda (img state)
		  (let
		      ((bg (cond ((eq state nil)
				  (nth 0 gtkrc-background-pixmaps))
				 ((eq state 'focused)
				  (nth 1 gtkrc-background-pixmaps))
				 ((eq state 'highlighted)
				  (nth 2 gtkrc-background-pixmaps))
				 (t
				  (nth 3 gtkrc-background-pixmaps)))))
		    (when bg
		      (tile-image img bg))
		    (bevel-image img 1 (not (eq state 'clicked))))))

     ;; foreground and background for frame defs
     (foreground (lambda () gtkrc-foreground))
     (background (lambda ()
		   (if background-images
		       (cons 'background background-images)
		     (cons 'renderer render-bg))))

     (construct-frame-defs
      (lambda ()
	(setq frame `((,(background)
		       (foreground . ,foreground)
		       (text . ,window-name)
		       (x-justify . 30)
		       (y-justify . center)
		       (left-edge . 0)
		       (right-edge . 0)
		       (top-edge . -21)
		       (height . 21)
		       (class . title))
		      ((background . "black")
		       (left-edge . 0)
		       (right-edge . 0)
		       (top-edge . -22)
		       (height . 1))
		      ((background . "black")
		       (left-edge . -1)
		       (width . 1)
		       (top-edge . -22)
		       (bottom-edge . -5))
		      ((background . "black")
		       (right-edge . -1)
		       (width . 1)
		       (top-edge . -22)
		       (bottom-edge . -5))
		      (,(background)
		       (left-edge . 0)
		       (right-edge . 0)
		       (bottom-edge . -4)
		       (height . 4)
		       (class . bottom-border))
		      ((background . "black")
		       (left-edge . 0)
		       (right-edge . 0)
		       (bottom-edge . -5)
		       (height . 1))
		      ((background . ,minimize)
		       (left-edge . 4)
		       (top-edge . -18)
		       (class . iconify-button)
		       (removable . t))
		      ((background . ,close)
		       (right-edge . 4)
		       (top-edge . -18)
		       (class . close-button)
		       (removable . t))))

	(setq shaped-frame `((,(background)
			      (foreground . ,foreground)
			      (text . ,window-name)
			      (x-justify . 30)
			      (y-justify . center)
			      (left-edge . 0)
			      (right-edge . 0)
			      (top-edge . -22)
			      (height . 21)
			      (class . title))
			     ((background . "black")
			      (left-edge . 0)
			      (right-edge . 0)
			      (top-edge . -23)
			      (height . 1))
			     ((background . "black")
			      (left-edge . -1)
			      (width . 1)
			      (top-edge . -23)
			      (height . 23))
			     ((background . "black")
			      (right-edge . -1)
			      (width . 1)
			      (top-edge . -23)
			      (height . 23))
			     ((background . "black")
			      (left-edge . 0)
			      (right-edge . 0)
			      (top-edge . -1)
			      (height . 1))
			     ((background . ,minimize)
			      (left-edge . 4)
			      (top-edge . -19)
			      (class . iconify-button)
			      (removable . t))
			     ((background . ,close)
			      (right-edge . 4)
			      (top-edge . -19)
			      (class . close-button)
			      (removable . t))))

	(setq transient-frame `((,(background)
				 (left-edge . 0)
				 (right-edge . 0)
				 (top-edge . -4)
				 (height . 4)
				 (class . title))
				((background . "black")
				 (left-edge . 0)
				 (right-edge . 0)
				 (top-edge . -5)
				 (height . 1))
				((background . "black")
				 (left-edge . -1)
				 (width . 1)
				 (top-edge . -5)
				 (bottom-edge . -5))
				((background . "black")
				 (right-edge . -1)
				 (width . 1)
				 (top-edge . -5)
				 (bottom-edge . -5))
				(,(background)
				 (render-scale . 2)
				 (left-edge . 0)
				 (right-edge . 0)
				 (bottom-edge . -4)
				 (height . 4)
				 (class . bottom-border))
				((background . "black")
				 (left-edge . 0)
				 (right-edge . 0)
				 (bottom-edge . -5)
				 (height . 1))))

	(setq shaped-transient-frame `((,(background)
				        (left-edge . 0)
				        (right-edge . 0)
				        (top-edge . -5)
				        (height . 4)
				        (class . title))
				       ((background . "black")
					(left-edge . 0)
					(right-edge . 0)
					(top-edge . -6)
					(height . 1))
				       ((background . "black")
					(left-edge . -1)
					(width . 1)
					(top-edge . -6)
					(height . 6))
				       ((background . "black")
					(right-edge . -1)
					(width . 1)
					(top-edge . -6)
					(height . 6))
				       ((background . "black")
					(left-edge . 0)
					(right-edge . 0)
					(top-edge . -1)
					(height . 1))))))

     (rebuild-frames
      (lambda ()
	(if gtkrc-background-pixmaps
	    (gtkrc-load-pixmaps)
	  ;; build image tiles for each colour in background-images
	  (let
	      ((i -1))
	    (setq background-images
		  (mapcar (lambda (x)
			    (setq i (1+ i))
			    (when (colorp x)
			      (setq x (make-sized-image 16 16 x))
			      (bevel-image x 1 (/= i 3))
			      (set-image-border x 1 1 1 1)
			      x))
			  gtkrc-background))))
	(construct-frame-defs)
	(reframe-windows-with-style 'gtk))))

  (unless batch-mode
    (rebuild-frames)
    (gtkrc-call-after-changed rebuild-frames)
    (add-frame-style 'gtk
		     (lambda (w type)
		       (cond ((eq type 'shaped)
			      shaped-frame)
			     ((eq type 'transient)
			      transient-frame)
			     ((eq type 'shaped-transient)
			      shaped-transient-frame)
			     ((eq type 'unframed)
			      nil-frame)
			     (t
			      frame))))))
