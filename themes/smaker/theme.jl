;; smaker/theme.jl -- somewhat windowmaker like theme, heavily customizable

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

(defgroup smaker "SMaker Theme"
  :group appearance)

(defcustom smaker:inverted-buttons nil
  "Use black-on-white button images."
  :group (appearance smaker)
  :type boolean)

(defcustom smaker:fg-color "white"
  "Color to use when drawing text."
  :group (appearance smaker)
  :type color)

(defcustom smaker:bar-normal "bar_normal.png"
  "Bar image for unfocused windows."
  :group (appearance smaker)
  :type file-name)

(defcustom smaker:bar-normal-active "bar_normal_active.png"
  "Bar image for focused windows."
  :group (appearance smaker)
  :type file-name)

(defcustom smaker:bar-hilited-active "bar_hilited_active.png"
  "Bar image for highlighted parts."
  :group (appearance smaker)
  :type file-name)

(defcustom smaker:bar-clicked-active "bar_clicked_active.png"
  "Bar image for clicked parts."
  :group (appearance smaker)
  :type file-name)

(defcustom smaker:bar-dimension 16
  "Height of title bar."
  :group (appearance smaker)
  :type number
  :range (14 . nil))

(defcustom smaker:border-dimension 4
  "Width of window border."
  :group (appearance smaker)
  :type number
  :range (1 . nil))

(defcustom smaker:image-border 4
  "Border width of bar images."
  :group (appearance smaker)
  :type number)

;; image loading

(let*
    ((install-dir (if (string-match "^(.*/)[^/]+$" load-filename)
		      (expand-last-match "\\1") ""))
     (actual-bar-images nil)

     ;; 12x12
     (close-image (make-image "close.png"))
     (min-image (make-image "min.png"))
     (close-inv-image (make-image "close_inv.png"))
     (min-inv-image (make-image "min_inv.png"))

     (make-image
      (lambda (i)
	(or (condition-case nil
		(make-image i)
	      (error))
	    (make-image (concat install-dir i)))))

     (rebuild (lambda ()
		(rebuild-frames-with-style 'smaker)))

     (close-button
      (lambda ()
	(if smaker:inverted-buttons close-inv-image close-image)))

     (minimize-button
      (lambda ()
	(if smaker:inverted-buttons min-inv-image min-image)))

     (bar-images (lambda () actual-bar-images))
     (foreground (lambda () smaker:fg-color))
     (bar-dimension (lambda () smaker:bar-dimension))
     (-bar-dimension (lambda () (- smaker:bar-dimension)))
     (title-offset (lambda ()
		     (- smaker:bar-dimension smaker:border-dimension)))
     (border-dimension (lambda () smaker:border-dimension))
     (-border-dimension (lambda () (- smaker:border-dimension)))

     (load-images
      (lambda ()
	(setq actual-bar-images
	      (mapcar (lambda (i)
			(set-image-border
			 i smaker:image-border smaker:image-border
			 smaker:image-border smaker:image-border))
		      (list (make-image smaker:bar-normal)
			    (make-image smaker:bar-normal-active)
			    (make-image smaker:bar-hilited-active)
			    (make-image smaker:bar-clicked-active))))
	(rebuild)))

     ;; frame definitions
     (frame `(((background . ,bar-images)
	       (foreground . ,minimize-button)
	       (x-justify . center)
	       (y-justify . center)
	       (left-edge . ,-border-dimension)
	       (width . ,bar-dimension)
	       (top-edge . ,-bar-dimension)
	       (height . ,bar-dimension)
	       (class . iconify-button))
	      ((background . ,bar-images)
	       (foreground . ,foreground)
	       (text . ,window-name)
	       (x-justify . center)
	       (y-justify . center)
	       (left-edge . ,title-offset)
	       (right-edge . ,title-offset)
	       (top-edge . ,-bar-dimension)
	       (height . ,bar-dimension)
	       (class . title))
	      ((background . ,bar-images)
	       (foreground . ,close-button)
	       (x-justify . center)
	       (y-justify . center)
	       (right-edge . ,-border-dimension)
	       (width . ,bar-dimension)
	       (top-edge . ,-bar-dimension)
	       (height . ,bar-dimension)
	       (class . close-button))
	      ((background . ,bar-images)
	       (right-edge . ,-border-dimension)
	       (width . ,border-dimension)
	       (top-edge . 0)
	       (bottom-edge . 0)
	       (class . right-border))
	      ((background . ,bar-images)
	       (left-edge . ,-border-dimension)
	       (width . ,border-dimension)
	       (top-edge . 0)
	       (bottom-edge . 0)
	       (class . left-border))
	      ((background . ,bar-images)
	       (left-edge . 0)
	       (right-edge . 0)
	       (bottom-edge . ,-border-dimension)
	       (height . ,border-dimension)
	       (class . bottom-border))
	      ((background . ,bar-images)
	       (left-edge . ,-border-dimension)
	       (width . ,border-dimension)
	       (bottom-edge . ,-border-dimension)
	       (height . ,border-dimension)
	       (class . bottom-left-corner))
	      ((background . ,bar-images)
	       (right-edge . ,-border-dimension)
	       (width . ,border-dimension)
	       (bottom-edge . ,-border-dimension)
	       (height . ,border-dimension)
	       (class . bottom-right-corner))))

     (shaped-frame `(((background . ,bar-images)
		      (foreground . ,minimize-button)
		      (x-justify . center)
		      (y-justify . center)
		      (left-edge . ,-border-dimension)
		      (width . ,bar-dimension)
		      (top-edge . ,-bar-dimension)
		      (height . ,bar-dimension)
		      (class . iconify-button))
		     ((background . ,bar-images)
		      (foreground . ,foreground)
		      (text . ,window-name)
		      (x-justify . center)
		      (y-justify . center)
		      (left-edge . ,title-offset)
		      (right-edge . ,title-offset)
		      (top-edge . ,-bar-dimension)
		      (height . ,bar-dimension)
		      (class . title))
		     ((background . ,bar-images)
		      (foreground . ,close-button)
		      (x-justify . center)
		      (y-justify . center)
		      (right-edge . ,-border-dimension)
		      (width . ,bar-dimension)
		      (top-edge . ,-bar-dimension)
		      (height . ,bar-dimension)
		      (class . close-button))))

     (transient-frame `(((background . ,bar-images)
			 (left-edge . 0)
			 (right-edge . 0)
			 (top-edge . ,-border-dimension)
			 (height . ,border-dimension)
			 (class . title))
			((background . ,bar-images)
			 (left-edge . 0)
			 (right-edge . 0)
			 (bottom-edge . ,-border-dimension)
			 (height . ,border-dimension)
			 (class . bottom-border))
			((background . ,bar-images)
			 (left-edge . ,-border-dimension)
			 (width . ,border-dimension)
			 (top-edge . 0)
			 (bottom-edge . 0)
			 (class . left-border))
			((background . ,bar-images)
			 (right-edge . ,-border-dimension)
			 (width . ,border-dimension)
			 (top-edge . 0)
			 (bottom-edge . 0)
			 (class . right-border))
			((background . ,bar-images)
			 (right-edge . ,-border-dimension)
			 (width . ,border-dimension)
			 (top-edge . ,-border-dimension)
			 (height . ,border-dimension)
			 (class . top-right-corner))
			((background . ,bar-images)
			 (left-edge . ,-border-dimension)
			 (width . ,border-dimension)
			 (top-edge . ,-border-dimension)
			 (height . ,border-dimension)
			 (class . top-left-corner))
			((background . ,bar-images)
			 (left-edge . ,-border-dimension)
			 (width . ,border-dimension)
			 (bottom-edge . ,-border-dimension)
			 (height . ,border-dimension)
			 (class . bottom-left-corner))
			((background . ,bar-images)
			 (right-edge . ,-border-dimension)
			 (width . ,border-dimension)
			 (bottom-edge . ,-border-dimension)
			 (height . ,border-dimension)
			 (class . bottom-right-corner))))

     (shaped-transient-frame `(((background . ,bar-images)
				(left-edge . 0)
				(right-edge . 0)
				(top-edge . ,-border-dimension)
				(height . ,border-dimension)
				(class . title)))))

  (add-frame-style 'smaker
		   (lambda (w type)
		     (case type
		       ((default) frame)
		       ((transient) transient-frame)
		       ((shaped) shaped-frame)
		       ((shaped-transient) shaped-transient-frame))))
  (unless batch-mode
    (load-images))

  (mapc (lambda (sym)
	  (custom-set-property sym ':after-set rebuild))
	'(smaker:inverted-buttons smaker:fg-color smaker:bar-dimension
	  smaker:border-dimension smaker:image-border))
  (mapc (lambda (sym)
	  (custom-set-property sym ':after-set load-images))
	'(smaker:bar-normal smaker:bar-normal-active
	  smaker:bar-hilited-active smaker:bar-clicked-active)))
