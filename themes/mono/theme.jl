;; mono/theme.jl
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

(defgroup mono-frame "Mono frame")

(defcustom mono:gtk-background-color nil
  "Use the GTK+ background color for inactive frames."
  :type boolean
  :group mono-frame)

(defcustom mono:normal-color "#8d7ee1"
  "Color of inactive frames. (When the above option is unset.)"
  :type color
  :group mono-frame
  :after-set after-setting-frame-option)

(defcustom mono:active-color "yellowgreen"
  "Color of active frames."
  :type color
  :group mono-frame
  :after-set after-setting-frame-option)

(let*
    ((minimize (list (make-image "min.png") nil
		     nil (make-image "min-c.png")))
     ;; 16x16
     (close (list (make-image "close.png") nil
		  nil (make-image "close-c.png")))
     (maximize (list (make-image "max.png") nil
		     nil (make-image "max-c.png")))
     (menu (list (make-image "menu.png") nil
		 nil (make-image "menu-c.png")))

     (initialised-gtk nil)

     (rebuild (lambda ()
		(when (and mono:gtk-background-color (not initialised-gtk))
		  (setq initialised-gtk t)
		  (require 'gtkrc)
		  (gtkrc-call-after-changed
		   (lambda () (rebuild-frames-with-style 'mono))))
		(rebuild-frames-with-style 'mono)))

     (frame-colors (lambda (w)
		     (list (or (window-get w 'frame-inactive-color)
			       (and mono:gtk-background-color
				    (car gtkrc-background))
			       mono:normal-color)
			   (or (window-get w 'frame-active-color)
			       mono:active-color))))

     (frame `(((background . ,frame-colors)
	       (foreground . "black")
	       (text . ,window-name)
	       (x-justify . 24)
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
	       
	      ((background . ,frame-colors)
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
	       
	      ((background . ,frame-colors)
	       (foreground . ,menu)
	       (left-edge . 4)
	       (top-edge . -18)
	       (width . 16)
	       (height . 16)
	       (class . menu-button)
	       (removable . t))
	       
	      ((background . ,frame-colors)
	       (foreground . ,minimize)
	       (right-edge . 35)
	       (top-edge . -18)
	       (width . 16)
	       (height . 16)
	       (class . iconify-button)
	       (removable . t))
	       
	      ((background . ,frame-colors)
	       (foreground . ,maximize)
	       (right-edge . 19)
	       (top-edge . -18)
	       (width . 16)
	       (height . 16)
	       (class . maximize-button)
	       (removable . t))
	       
	      ((background . ,frame-colors)
	       (foreground . ,close)
	       (right-edge . 0)
	       (top-edge . -18)
	       (width . 19)
	       (height . 16)
	       (class . close-button)
	       (removable . t))))

     (shaped-frame `(((background . ,frame-colors)
		      (foreground . "black")
		      (text . ,window-name)
		      (x-justify . 24)
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
	       
		     ((background . ,frame-colors)
		      (foreground . ,menu)
		      (left-edge . 4)
		      (top-edge . -19)
		      (width . 16)
		      (height . 16)
		      (class . menu-button)
		      (removable . t))
		     
		     ((background . ,frame-colors)
		      (foreground . ,minimize)
		      (right-edge . 35)
		      (top-edge . -19)
		      (width . 16)
		      (height . 16)
		      (class . iconify-button)
		      (removable . t))
		     
		     ((background . ,frame-colors)
		      (foreground . ,maximize)
		      (right-edge . 19)
		      (top-edge . -19)
		      (width . 16)
		      (height . 16)
		      (class . maximize-button)
		      (removable . t))
		     
		     ((background . ,frame-colors)
		      (foreground . ,close)
		      (right-edge . 0)
		      (top-edge . -19)
		      (width . 19)
		      (height . 16)
		      (class . close-button)
		      (removable . t))))

     (transient-frame `(((background . ,frame-colors)
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

			((background . ,frame-colors)
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

     (shaped-transient-frame `(((background . ,frame-colors)
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
				(height . 1)))))

  (add-frame-style 'mono
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
			    frame))))
  (rebuild)
  (custom-set-property 'mono:gtk-background-color ':after-set rebuild))
