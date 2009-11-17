;; gradient/theme.jl

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

(require 'gradient)

;; Commentary:

;; This is mainly just a test of the on-the-fly image rendering code.
;; It should be possible to implement gradients using a couple of
;; static images letting Imlib resize them. Maybe you get smoother
;; gradients this way, who knows..?

(defgroup gradient "Gradient Theme"
  :group appearance)

(defcustom gradient:gradient-type 'horizontal
  "Direction of gradient in `gradient' frame style."
  :type symbol
  :options (horizontal vertical diagonal)
  :group (appearance gradient)
  :after-set after-setting-frame-option)

(defcustom gradient:normal-from-color (get-color "#b6b6b6")
  "`From' color of inactive frames in `gradient' frame style."
  :type color
  :group (appearance gradient)
  :after-set after-setting-frame-option)

(defcustom gradient:normal-to-color (get-color "#323232")
  "`To' color of inactive frames in `gradient' frame style."
  :type color
  :group (appearance gradient)
  :after-set after-setting-frame-option)

(defcustom gradient:active-from-color (get-color "#64b4df")
  "`From' color of active frames in `gradient' frame style."
  :type color
  :group (appearance gradient)
  :after-set after-setting-frame-option)

(defcustom gradient:active-to-color (get-color "#000030")
  "`To' color of active frames in `gradient' frame style."
  :type color
  :group (appearance gradient)
  :after-set after-setting-frame-option)

(defcustom gradient:save-memory t
  "Use less memory when creating gradients, possibly affecting quality."
  :type boolean
  :group (appearance gradient)
  :after-set after-setting-frame-option)

(let*
    ((render-bg (lambda (img state)
		  (apply (cond ((eq gradient:gradient-type 'diagonal)
				draw-diagonal-gradient)
			       ((eq gradient:gradient-type 'horizontal)
				draw-horizontal-gradient)
			       ((eq gradient:gradient-type 'vertical)
				draw-vertical-gradient))
			 img (if state
				 (list gradient:active-from-color
				       gradient:active-to-color)
			       (list gradient:normal-from-color
				     gradient:normal-to-color)))
		  (when (> (cdr (image-dimensions img)) 4)
		    (bevel-image img 1 (not (eq state 'clicked))))
		  (set-image-border img 1 1 1 1)))

     (scale (lambda () (if gradient:save-memory 2 1)))

     ;; 15x15
     (minimize (list (make-image "as_min.png") nil
		     nil (make-image "as_min-b.png")))
     (close (list (make-image "as_close.png") nil
		  nil (make-image "as_close-b.png")))

     (frame `(((renderer . ,render-bg)
	       (render-scale . ,scale)
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
	       (bottom-edge . -5))
	      ((renderer . ,render-bg)
	       (render-scale . ,scale)
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

     (shaped-frame `(((renderer . ,render-bg)
		      (render-scale . ,scale)
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
		      (height . 23))
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

     (transient-frame `(((renderer . ,render-bg)
			 (render-scale . ,scale)
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
			((renderer . ,render-bg)
			 (render-scale . ,scale)
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

     (shaped-transient-frame `(((renderer . ,render-bg)
				(render-scale . ,scale)
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

  (add-frame-style 'gradient
		   (lambda (w type)
		     (case type
		       ((default) frame)
		       ((transient) transient-frame)
		       ((shaped) shaped-frame)
		       ((shaped-transient) shaped-transient-frame)))))
