;; smaker/theme.jl -- somewhat windowmaker like theme, heavily customizable
;; $Id: theme.jl,v 1.3 1999/09/25 12:58:37 john Exp $

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

(defgroup smaker-frame "SMaker frame")

(defcustom smaker:inverted-buttons nil
  "Use black-on-white button images."
  :group smaker-frame
  :type boolean
  :after-set smaker:rebuild)

(defcustom smaker:fg-color "white"
  "Color to use when drawing text."
  :group smaker-frame
  :type color
  :after-set smaker:rebuild)

(defcustom smaker:bar-normal "bar_normal.png"
  "Bar image for unfocused windows."
  :group smaker-frame
  :type file-name
  :after-set smaker:load-images)

(defcustom smaker:bar-normal-active "bar_normal_active.png"
  "Bar image for focused windows."
  :group smaker-frame
  :type file-name
  :after-set smaker:load-images)

(defcustom smaker:bar-hilited-active "bar_hilited_active.png"
  "Bar image for highlighted parts."
  :group smaker-frame
  :type file-name
  :after-set smaker:load-images)

(defcustom smaker:bar-clicked-active "bar_clicked_active.png"
  "Bar image for clicked parts."
  :group smaker-frame
  :type file-name
  :after-set smaker:load-images)

(defcustom smaker:bar-dimension 16
  "Height of title bar."
  :group smaker-frame
  :type number
  :range (14 . nil)
  :after-set smaker:rebuild)

(defcustom smaker:border-dimension 4
  "Width of window border."
  :group smaker-frame
  :type number
  :range (1 . nil)
  :after-set smaker:rebuild)

(defcustom smaker:image-border 4
  "Border width of bar images."
  :group smaker-frame
  :type number
  :after-set smaker:rebuild)


;; image loading

(defvar smaker:install-dir (file-name-as-directory (car image-load-path)))

;; 100x16
(defvar smaker:bar-images nil)

;; 12x12
(defvar smaker:close-image (make-image "close.png"))
(defvar smaker:min-image (make-image "min.png"))
(defvar smaker:close-inv-image (make-image "close_inv.png"))
(defvar smaker:min-inv-image (make-image "min_inv.png"))

(defun smaker:load-images ()
  (let
      ((image-load-path (cons smaker:install-dir image-load-path)))
    (setq smaker:bar-images
	  (mapcar #'(lambda (i)
		      (set-image-border
		       i smaker:image-border smaker:image-border
		       smaker:image-border smaker:image-border))
		  (list (make-image smaker:bar-normal)
			(make-image smaker:bar-normal-active)
			(make-image smaker:bar-hilited-active)
			(make-image smaker:bar-clicked-active)))))
  (smaker:rebuild))

(defun smaker:rebuild ()
  (mapc 'rebuild-frame (managed-windows)))


;; functions used to dynamically define frame part attributes

(defun smaker:close-button ()
  (if smaker:inverted-buttons smaker:close-inv-image smaker:close-image))

(defun smaker:minimize-button ()
  (if smaker:inverted-buttons smaker:min-inv-image smaker:min-image))

(defun smaker:bar-images ()
  smaker:bar-images)

(defun smaker:foreground ()
  smaker:fg-color)

(defun smaker:bar-dimension ()
  smaker:bar-dimension)

(defun smaker:-bar-dimension ()
  (- smaker:bar-dimension))

(defun smaker:title-offset ()
  (- smaker:bar-dimension smaker:border-dimension))

(defun smaker:border-dimension ()
  smaker:border-dimension)

(defun smaker:-border-dimension ()
  (- smaker:border-dimension))


;; frame definitions

(defvar smaker:frame
  '(;; left button
    ((background . smaker:bar-images)
     (foreground . smaker:minimize-button)
     (x-justify . center)
     (y-justify . center)
     (left-edge . smaker:-border-dimension)
     (width . smaker:bar-dimension)
     (top-edge . smaker:-bar-dimension)
     (height . smaker:bar-dimension)
     (class . iconify-button))
    ;; title bar
    ((background . smaker:bar-images)
     (foreground . smaker:foreground)
     (text . window-name)
     (x-justify . center)
     (y-justify . center)
     (left-edge . smaker:title-offset)
     (right-edge . smaker:title-offset)
     (top-edge . smaker:-bar-dimension)
     (height . smaker:bar-dimension)
     (class . title))
    ;; right button
    ((background . smaker:bar-images)
     (foreground . smaker:close-button)
     (x-justify . center)
     (y-justify . center)
     (right-edge . smaker:-border-dimension)
     (width . smaker:bar-dimension)
     (top-edge . smaker:-bar-dimension)
     (height . smaker:bar-dimension)
     (class . close-button))
    ;; left frame
    ((background . smaker:bar-images)
     (right-edge . smaker:-border-dimension)
     (width . smaker:border-dimension)
     (top-edge . 0)
     (bottom-edge . 0)
     (class . right-border))
    ;; right frame
    ((background . smaker:bar-images)
     (left-edge . smaker:-border-dimension)
     (width . smaker:border-dimension)
     (top-edge . 0)
     (bottom-edge . 0)
     (class . left-border))
    ;; bottom frame
    ((background . smaker:bar-images)
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . smaker:-border-dimension)
     (height . smaker:border-dimension)
     (class . bottom-border))
    ;; bottom-left corner
    ((background . smaker:bar-images)
     (left-edge . smaker:-border-dimension)
     (width . smaker:border-dimension)
     (bottom-edge . smaker:-border-dimension)
     (height . smaker:border-dimension)
     (class . bottom-left-corner))
    ;; bottom-right corner
    ((background . smaker:bar-images)
     (right-edge . smaker:-border-dimension)
     (width . smaker:border-dimension)
     (bottom-edge . smaker:-border-dimension)
     (height . smaker:border-dimension)
     (class . bottom-right-corner))))

(defvar smaker:shaped-frame
  '(;; left button
    ((background . smaker:bar-images)
     (foreground . smaker:minimize-button)
     (x-justify . center)
     (y-justify . center)
     (left-edge . smaker:-border-dimension)
     (width . smaker:bar-dimension)
     (top-edge . smaker:-bar-dimension)
     (height . smaker:bar-dimension)
     (class . iconify-button))
    ;; title bar
    ((background . smaker:bar-images)
     (foreground . smaker:foreground)
     (text . window-name)
     (x-justify . center)
     (y-justify . center)
     (left-edge . smaker:title-offset)
     (right-edge . smaker:title-offset)
     (top-edge . smaker:-bar-dimension)
     (height . smaker:bar-dimension)
     (class . title))
    ;; right button
    ((background . smaker:bar-images)
     (foreground . smaker:close-button)
     (x-justify . center)
     (y-justify . center)
     (right-edge . smaker:-border-dimension)
     (width . smaker:bar-dimension)
     (top-edge . smaker:-bar-dimension)
     (height . smaker:bar-dimension)
     (class . close-button))))

(defvar smaker:transient-frame
 '(((background . smaker:bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . smaker:-border-dimension)
    (height . smaker:border-dimension)
    (class . title))
   ((background . smaker:bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . smaker:-border-dimension)
    (height . smaker:border-dimension)
    (class . bottom-border))
   ((background . smaker:bar-images)
    (left-edge . smaker:-border-dimension)
    (width . smaker:border-dimension)
    (top-edge . 0)
    (bottom-edge . 0)
    (class . left-border))
   ((background . smaker:bar-images)
    (right-edge . smaker:-border-dimension)
    (width . smaker:border-dimension)
    (top-edge . 0)
    (bottom-edge . 0)
    (class . right-border))
   ;; top-right corner
   ((background . smaker:bar-images)
    (right-edge . smaker:-border-dimension)
    (width . smaker:border-dimension)
    (top-edge . smaker:-border-dimension)
    (height . smaker:border-dimension)
    (class . top-right-corner))
   ;; top-left corner
   ((background . smaker:bar-images)
    (left-edge . smaker:-border-dimension)
    (width . smaker:border-dimension)
    (top-edge . smaker:-border-dimension)
    (height . smaker:border-dimension)
    (class . top-left-corner))
   ;; bottom-left corner
   ((background . smaker:bar-images)
    (left-edge . smaker:-border-dimension)
    (width . smaker:border-dimension)
    (bottom-edge . smaker:-border-dimension)
    (height . smaker:border-dimension)
    (class . bottom-left-corner))
   ;; bottom-right corner
   ((background . smaker:bar-images)
    (right-edge . smaker:-border-dimension)
    (width . smaker:border-dimension)
    (bottom-edge . smaker:-border-dimension)
    (height . smaker:border-dimension)
    (class . bottom-right-corner))))

(defvar smaker:shaped-transient-frame
 '(((background . smaker:bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . smaker:-border-dimension)
    (height . smaker:border-dimension)
    (class . title))))



;; called to produce a frame definition for window W (of type TYPE)
(defun smaker:frame-style (w type)
  (cond ((eq type 'shaped)
	 'smaker:shaped-frame)
	((eq type 'transient)
	 'smaker:transient-frame)
	((eq type 'shaped-transient)
	 'smaker:shaped-transient-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'smaker:frame)))


;;  initialisation

(add-frame-style 'smaker 'smaker:frame-style)

(unless batch-mode
  (smaker:load-images))
