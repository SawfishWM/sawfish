;; smaker/theme.jl
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

;; 100x16
(defvar smaker:bar-images
  (mapcar #'(lambda (i)
	      (set-image-border i 4 4 4 4))
	  (list (make-image "bar_normal.png")
		(make-image "bar_normal_active.png")
		(make-image "bar_hilited_active.png")
		(make-image "bar_clicked_active.png"))))

;; 12x12
(defvar smaker:close-image (make-image "close.png"))
(defvar smaker:min-image (make-image "min.png"))

(defvar smaker:frame
  `(;; left button
    ((background . ,smaker:bar-images)
     (foreground . ,smaker:min-image)
     (x-justify . center)
     (y-justify . center)
     (left-edge . -4)
     (width . 16)
     (top-edge . -16)
     (height . 16)
     (class . iconify-button))
    ;; title bar
    ((background . ,smaker:bar-images)
     (foreground . "white")
     (text . window-name)
     (x-justify . center)
     (y-justify . center)
     (left-edge . 12)
     (right-edge . 12)
     (top-edge . -16)
     (height . 16)
     (class . title))
    ;; right button
    ((background . ,smaker:bar-images)
     (foreground . ,smaker:close-image)
     (x-justify . center)
     (y-justify . center)
     (right-edge . -4)
     (width . 16)
     (top-edge . -16)
     (height . 16)
     (class . close-button))
    ;; left frame
    ((background . ,smaker:bar-images)
     (right-edge . -4)
     (width . 4)
     (top-edge . 0)
     (bottom-edge . 0)
     (class . right-border))
    ;; right frame
    ((background . ,smaker:bar-images)
     (left-edge . -4)
     (width . 4)
     (top-edge . 0)
     (bottom-edge . 0)
     (class . left-border))
    ;; bottom frame
    ((background . ,smaker:bar-images)
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . -4)
     (height . 4)
     (class . bottom-border))
    ;; bottom-left corner
    ((background . ,smaker:bar-images)
     (left-edge . -4)
     (width . 4)
     (bottom-edge . -4)
     (height . 4)
     (class . bottom-left-corner))
    ;; bottom-right corner
    ((background . ,smaker:bar-images)
     (right-edge . -4)
     (width . 4)
     (bottom-edge . -4)
     (height . 4)
     (class . bottom-right-corner))))

(defvar smaker:shaped-frame
  `(;; left button
    ((background . ,smaker:bar-images)
     (foreground . ,smaker:min-image)
     (x-justify . center)
     (y-justify . center)
     (left-edge . -4)
     (width . 16)
     (top-edge . -16)
     (height . 16)
     (class . iconify-button))
    ;; title bar
    ((background . ,smaker:bar-images)
     (foreground . "white")
     (text . window-name)
     (x-justify . center)
     (y-justify . center)
     (left-edge . 12)
     (right-edge . 12)
     (top-edge . -16)
     (height . 16)
     (class . title))
    ;; right button
    ((background . ,smaker:bar-images)
     (foreground . ,smaker:close-image)
     (x-justify . center)
     (y-justify . center)
     (right-edge . -4)
     (width . 16)
     (top-edge . -16)
     (height . 16)
     (class . close-button))))

(defvar smaker:transient-frame
 `(((background . ,smaker:bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -4)
    (height . 4)
    (class . title))
   ((background . ,smaker:bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (height . 4)
    (class . bottom-border))
   ((background . ,smaker:bar-images)
    (left-edge . -4)
    (width . 4)
    (top-edge . 0)
    (bottom-edge . 0)
    (class . left-border))
   ((background . ,smaker:bar-images)
    (right-edge . -4)
    (width . 4)
    (top-edge . 0)
    (bottom-edge . 0)
    (class . right-border))
   ;; top-right corner
   ((background . ,smaker:bar-images)
    (right-edge . -4)
    (width . 4)
    (top-edge . -4)
    (height . 4)
    (class . top-right-corner))
   ;; top-left corner
   ((background . ,smaker:bar-images)
    (left-edge . -4)
    (width . 4)
    (top-edge . -4)
    (height . 4)
    (class . top-left-corner))
   ;; bottom-left corner
   ((background . ,smaker:bar-images)
    (left-edge . -4)
    (width . 4)
    (bottom-edge . -4)
    (height . 4)
    (class . bottom-left-corner))
   ;; bottom-right corner
   ((background . ,smaker:bar-images)
    (right-edge . -4)
    (width . 4)
    (bottom-edge . -4)
    (height . 4)
    (class . bottom-right-corner))))

(defvar smaker:shaped-transient-frame
 `(((background . ,smaker:bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -4)
    (height . 4)
    (class . title))))

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

(add-frame-style 'smaker 'smaker:frame-style)
