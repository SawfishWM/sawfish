;; absolute-e/theme.jl
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

;; The images and design of this theme are from Hallvar Helleseth's
;; Absolute E theme for Enlightenment

;; from the ABOUT file:

;;    Thanks for using Absolute E, the theme that thinks diffrently.
;;    This theme is created by Hallvar Helleseth using nothing but The
;;    Gimp.  Everything is originaly made by me - Propably the only
;;    Etheme that can say that ;) Though alot of people have helped me
;;    (too many to list here/can't remeber all of them either...) But I
;;    give them a BIG thank you!

;;    There is a GTK and WMaker version of this theme available at
;;    themes.org

;;    This theme is going to be following the cvs version of E and not
;;    the stable version (DR0.15.5)

;;    Reach me at hallvar@ii.uib.no (www.ii.uib.no/~hallvar)

;;    Date: July 19 1999.

(flet ((title-width (w)
	 (let
	     ((w-width (car (window-dimensions w))))
	   (- (min (max (/ w-width 2) 100) w-width) 16))))

  (let*
      ((bar-images (mapcar #'(lambda (i)
			       (set-image-border i 4 4 4 4))
			   (list (make-image "bar_normal.png")
				 (make-image "bar_normal_active.png")
				 (make-image "bar_hilited_active.png")
				 (make-image "bar_clicked_active.png"))))
       ;; 100x16

       (frame (make-closure
	       `(((background . ,bar-images)
		  (left-edge . 0)
		  (width . 16)
		  (top-edge . -16)
		  (height . 16)
		  (class . menu-button))
		 
		 ((background . ,bar-images)
		  (foreground . "white")
		  (text . window-name)
		  (x-justify . center)
		  (y-justify . center)
		  (left-edge . 16)
		  (width . title-width)
		  (top-edge . -16)
		  (class . title))
		 
		 ((background . ,bar-images)
		  (left-edge . ,#'(lambda (w) (+ (title-width w) 16)))
		  (right-edge . 0)
		  (top-edge . -4)
		  (height . 4)
		  (class . top-border))
		 
		 ((background . ,bar-images)
		  (right-edge . -4)
		  (width . 4)
		  (top-edge . 0)
		  (bottom-edge . 0)
		  (class . right-border))
		 
		 ((background . ,bar-images)
		  (left-edge . -4)
		  (width . 4)
		  (top-edge . 0)
		  (bottom-edge . 0)
		  (class . left-border))
		 
		 ((background . ,bar-images)
		  (left-edge . 0)
		  (right-edge . 0)
		  (bottom-edge . -4)
		  (height . 4)
		  (class . bottom-border))
		 
		 ((background . ,bar-images)
		  (left-edge . -4)
		  (width . 4)
		  (top-edge . -4)
		  (height . 4)
		  (class . top-left-corner))
		 
		 ((background . ,bar-images)
		  (right-edge . -4)
		  (width . 4)
		  (top-edge . -4)
		  (height . 4)
		  (class . top-right-corner))
		 
		 ((background . ,bar-images)
		  (left-edge . -4)
		  (width . 4)
		  (bottom-edge . -4)
		  (height . 4)
		  (class . bottom-left-corner))
		 
		 ((background . ,bar-images)
		  (right-edge . -4)
		  (width . 4)
		  (bottom-edge . -4)
		  (height . 4)
		  (class . bottom-right-corner)))))

       (shaped-frame (make-closure
		      `(((background . ,bar-images)
			 (left-edge . 0)
			 (width . 16)
			 (top-edge . -16)
			 (height . 16)
			 (class . menu-button))
			
			((background . ,bar-images)
			 (foreground . "white")
			 (text . window-name)
			 (x-justify . center)
			 (y-justify . center)
			 (left-edge . 16)
			 (width . title-width)
			 (top-edge . -16)
			 (class . title))
			
			((background . ,bar-images)
			 (left-edge . ,#'(lambda (w) (+ (title-width w) 16)))
			 (right-edge . 0)
			 (top-edge . -4)
			 (height . 4)
			 (class . title)))))

       (transient-frame (make-closure
			 `(((background . ,bar-images)
			    (left-edge . 0)
			    (right-edge . 0)
			    (top-edge . -4)
			    (height . 4)
			    (class . title))
			   
			   ((background . ,bar-images)
			    (left-edge . 0)
			    (right-edge . 0)
			    (bottom-edge . -4)
			    (height . 4)
			    (class . bottom-border))
			   
			   ((background . ,bar-images)
			    (left-edge . -4)
			    (width . 4)
			    (top-edge . 0)
			    (bottom-edge . 0)
			    (class . left-border))
			   
			   ((background . ,bar-images)
			    (right-edge . -4)
			    (width . 4)
			    (top-edge . 0)
			    (bottom-edge . 0)
			    (class . right-border))
			   
			   ((background . ,bar-images)
			    (right-edge . -4)
			    (width . 4)
			    (top-edge . -4)
			    (height . 4)
			    (class . top-right-corner))
			   
			   ((background . ,bar-images)
			    (left-edge . -4)
			    (width . 4)
			    (top-edge . -4)
			    (height . 4)
			    (class . top-left-corner))
			   
			   ((background . ,bar-images)
			    (left-edge . -4)
			    (width . 4)
			    (bottom-edge . -4)
			    (height . 4)
			    (class . bottom-left-corner))
			   
			   ((background . ,bar-images)
			    (right-edge . -4)
			    (width . 4)
			    (bottom-edge . -4)
			    (height . 4)
			    (class . bottom-right-corner)))))

       (shaped-transient-frame (make-closure
				`(((background . ,bar-images)
				   (left-edge . 0)
				   (right-edge . 0)
				   (top-edge . -4)
				   (height . 4)
				   (class . title))))))

    (add-frame-style 'absolute-e
		     #'(lambda (w type)
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
