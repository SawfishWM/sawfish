;; brushed-metal/theme.jl
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

;; The images in this theme are by tigert, it matches the GTK theme of
;; the same name; they were originally taken from Enlightenment 0.15

(let*
    ((bottom-images (list (make-image "b6-.png") (make-image "b6.png")))
     ;; 217x4

     ;; 4x217
     (top-images (list (make-image "b2-.png") (make-image "b2.png")))

     (right-images (list (flip-image-diagonally
			  (copy-image (nth 0 bottom-images)))
			 (flip-image-diagonally
			  (copy-image (nth 1 bottom-images)))))

     ;; 4x219
     (left-images (list (flip-image-diagonally
			 (copy-image (nth 0 top-images)))
			(flip-image-diagonally
			 (copy-image (nth 1 top-images)))))

     ;; 4x4
     (top-left-images (list (make-image "b1-.png") (make-image "b1.png")))
     (bottom-left-images (list (make-image "b7-.png") (make-image "b7.png")))
     (top-right-images (list (make-image "b3-.png") (make-image "b3.png")))
     (bottom-right-images (list (make-image "b5-.png") (make-image "b5.png")))

     ;; 21x15
     (menu-images (list (make-image "t1-.png") (make-image "t1.png")
			nil (make-image "t1b.png")))

     ;; 150x15
     (title-images (list (set-image-border (make-image "t2-.png") 10 10 0 0)
			 (set-image-border (make-image "t2.png") 10 10 0 0)))

     ;; 17x15
     (iconify-images (list (make-image "t3-.png") (make-image "t3.png")
			   nil (make-image "t3b.png")))

     ;; 14x15
     (maximize-images (list (make-image "t4-.png") (make-image "t4.png")
			    nil (make-image "t4b.png")))

     ;; 14x15
     (close-images (list (make-image "t5-.png") (make-image "t5.png")
			 nil (make-image "t5b.png")))

     (frame `(((background . ,menu-images)
	       (top-edge . -15)
	       (left-edge . 0)
	       (class . menu-button))
	      ((background . ,title-images)
	       (foreground . "black")
	       (text . ,window-name)
	       (x-justify . 10)
	       (y-justify . center)
	       (left-edge . 21)
	       (right-edge . 45)
	       (top-edge . -15)
	       (class . title))
	      ((background . ,iconify-images)
	       (right-edge . 28)
	       (top-edge . -15)
	       (class . iconify-button))
	      ((background . ,maximize-images)
	       (right-edge . 14)
	       (top-edge . -15)
	       (class . maximize-button))
	      ((background . ,close-images)
	       (right-edge . 0)
	       (top-edge . -15)
	       (class . close-button))
	      ((background . ,left-images)
	       (left-edge . -4)
	       (top-edge . -15)
	       (bottom-edge . 0)
	       (class . left-border))
	      ((background . ,right-images)
	       (right-edge . -4)
	       (top-edge . -15)
	       (bottom-edge . 0)
	       (class . right-border))
	      ((background . ,top-images)
	       (left-edge . 0)
	       (right-edge . 0)
	       (top-edge . -19)
	       (class . top-border))
	      ((background . ,bottom-images)
	       (left-edge . 0)
	       (right-edge . 0)
	       (bottom-edge . -4)
	       (class . bottom-border))
	      ((background . ,top-left-images)
	       (left-edge . -4)
	       (top-edge . -19)
	       (class . top-left-corner))
	      ((background . ,top-right-images)
	       (right-edge . -4)
	       (top-edge . -19)
	       (class . top-right-corner))
	      ((background . ,bottom-left-images)
	       (left-edge . -4)
	       (bottom-edge . -4)
	       (class . bottom-left-corner))
	      ((background . ,bottom-right-images)
	       (right-edge . -4)
	       (bottom-edge . -4)
	       (class . bottom-right-corner))))

     (shaped-frame `(((background . ,menu-images)
		      (top-edge . -19)
		      (left-edge . 0)
		      (class . menu-button))
		     ((background . ,title-images)
		      (foreground . "black")
		      (text . ,window-name)
		      (x-justify . 10)
		      (y-justify . center)
		      (left-edge . 21)
		      (right-edge . 45)
		      (top-edge . -19)
		      (class . title))
		     ((background . ,iconify-images)
		      (right-edge . 28)
		      (top-edge . -19)
		      (class . iconify-button))
		     ((background . ,maximize-images)
		      (right-edge . 14)
		      (top-edge . -19)
		      (class . maximize-button))
		     ((background . ,close-images)
		      (right-edge . 0)
		      (top-edge . -19)
		      (class . close-button))
		     ((background . ,top-images)
		      (left-edge . 0)
		      (right-edge . 0)
		      (top-edge . -23)
		      (class . top-border))
		     ((background . ,bottom-images)
		      (left-edge . 0)
		      (right-edge . 0)
		      (top-edge . -4)
		      (class . bottom-border))
		     ((background . ,left-images)
		      (left-edge . -4)
		      (top-edge . -19)
		      (height . 15)
		      (class . left-border))
		     ((background . ,right-images)
		      (right-edge . -4)
		      (top-edge . -19)
		      (height . 15)
		      (class . right-border))
		     ((background . ,top-left-images)
		      (left-edge . -4)
		      (top-edge . -23)
		      (class . top-left-corner))
		     ((background . ,top-right-images)
		      (right-edge . -4)
		      (top-edge . -23)
		      (class . top-right-corner))
		     ((background . ,bottom-left-images)
		      (left-edge . -4)
		      (top-edge . -4)
		      (class . bottom-left-corner))
		     ((background . ,bottom-right-images)
		      (right-edge . -4)
		      (top-edge . -4)
		      (class . bottom-right-corner))))

     (transient-frame `(((background . ,top-images)
			 (left-edge . 0)
			 (right-edge . 0)
			 (top-edge . -4)
			 (class . title))
			((background . ,bottom-images)
			 (left-edge . 0)
			 (right-edge . 0)
			 (bottom-edge . -4)
			 (class . bottom-border))
			((background . ,left-images)
			 (left-edge . -4)
			 (top-edge . -4)
			 (bottom-edge . 0)
			 (class . left-border))
			((background . ,right-images)
			 (right-edge . -4)
			 (top-edge . -4)
			 (bottom-edge . 0)
			 (class . right-border))
			((background . ,top-left-images)
			 (left-edge . -4)
			 (top-edge . -4)
			 (class . top-left-corner))
			((background . ,top-right-images)
			 (right-edge . -4)
			 (top-edge . -4)
			 (class . top-right-corner))
			((background . ,bottom-left-images)
			 (left-edge . -4)
			 (bottom-edge . -4)
			 (class . bottom-left-corner))
			((background . ,bottom-right-images)
			 (right-edge . -4)
			 (bottom-edge . -4)
			 (class . bottom-right-corner))))

     (shaped-transient-frame `(((background . ,top-images)
				(left-edge . 0)
				(right-edge . 0)
				(top-edge . -4)
				(class . title))
			       ((background . ,top-left-images)
				(left-edge . -4)
				(top-edge . -4)
				(class . top-left-corner))
			       ((background . ,top-right-images)
				(right-edge . -4)
				(top-edge . -4)
				(class . top-right-corner)))))

  (add-frame-style 'brushed-metal
		   (lambda (w type)
		     (cond ((eq type 'shaped)
			    shaped-frame)
			   ((eq type 'shaped-transient)
			    shaped-transient-frame)
			   ((eq type 'transient)
			    transient-frame)
			   ((eq type 'unframed)
			    nil-frame)
			   (t
			    frame)))))
