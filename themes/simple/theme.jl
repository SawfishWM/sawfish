;; simple/theme.jl

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(defgroup simple "Simple Theme"
  :group appearance)

(defcustom simple:normal-color "lightsteelblue4"
  "Color of inactive frames in `simple' frame style."
  :type color
  :group (appearance simple)
  :after-set after-setting-frame-option)

(defcustom simple:active-color "goldenrod"
  "Color of active frames in `simple' frame style."
  :type color
  :group (appearance simple)
  :after-set after-setting-frame-option)

(let*
    ((minimize (list (make-image "as_min.png") nil
		     nil (make-image "as_min-b.png")))
     ;; 15x15
     (close (list (make-image "as_close.png") nil
		  nil (make-image "as_close-b.png")))

     (frame-colors (lambda (w)
		     (list (or (window-get w 'frame-inactive-color)
			       simple:normal-color)
			   (or (window-get w 'frame-active-color)
			       simple:active-color))))

     (frame `(((background . ,frame-colors)
	       (foreground . "black")
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
	       (bottom-edge . -5)
	       (class . right-border))

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

     (shaped-frame `(((background . ,frame-colors)
		      (foreground . "black")
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
		      (height . 23))

		     ((background . "black")
		      (right-edge . -1)
		      (width . 1)
		      (top-edge . -22)
		      (height . 23)
		      (class . right-border))

		     ((background . "black")
		      (left-edge . 0)
		      (right-edge . 0)
		      (top-edge . 0)
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
			 (bottom-edge . -5)
			 (class . right-border))

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
				(height . 6)
				(class . right-border))

			       ((background . "black")
				(left-edge . 0)
				(right-edge . 0)
				(top-edge . -1)
				(height . 1)))))

  (add-frame-style 'simple
		   (lambda (w type)
		     (case type
		       ((default) frame)
		       ((transient) transient-frame)
		       ((shaped) shaped-frame)
		       ((shaped-transient) shaped-transient-frame)))))
