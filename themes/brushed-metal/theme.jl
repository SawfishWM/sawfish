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


;; images

;; 217x4
(defvar brushed-metal:bottom-images
  (list (make-image "b6-.png") (make-image "b6.png")))

(defvar brushed-metal:top-images
  (list (make-image "b2-.png") (make-image "b2.png")))

;; 4x217
(defvar brushed-metal:right-images
  (list (flip-image-diagonally
	 (copy-image (nth 0 brushed-metal:bottom-images)))
	(flip-image-diagonally
	 (copy-image (nth 1 brushed-metal:bottom-images)))))

;; 4x219
(defvar brushed-metal:left-images
  (list (flip-image-diagonally (copy-image (nth 0 brushed-metal:top-images)))
	(flip-image-diagonally (copy-image (nth 1 brushed-metal:top-images)))))

;; 4x4
(defvar brushed-metal:top-left-images
  (list (make-image "b1-.png") (make-image "b1.png")))
(defvar brushed-metal:bottom-left-images
  (list (make-image "b7-.png") (make-image "b7.png")))
(defvar brushed-metal:top-right-images
  (list (make-image "b3-.png") (make-image "b3.png")))
(defvar brushed-metal:bottom-right-images
  (list (make-image "b5-.png") (make-image "b5.png")))

;; 21x15
(defvar brushed-metal:menu-images
  (list (make-image "t1-.png") (make-image "t1.png")
	nil (make-image "t1b.png")))

;; 150x15
(defvar brushed-metal:title-images
  (list (set-image-border (make-image "t2-.png") 10 10 0 0)
	(set-image-border (make-image "t2.png") 10 10 0 0)))

;; 17x15
(defvar brushed-metal:iconify-images
  (list (make-image "t3-.png") (make-image "t3.png")
	nil (make-image "t3b.png")))

;; 14x15
(defvar brushed-metal:maximize-images
  (list (make-image "t4-.png") (make-image "t4.png")
	nil (make-image "t4b.png")))

;; 14x15
(defvar brushed-metal:close-images
  (list (make-image "t5-.png") (make-image "t5.png")
	nil (make-image "t5b.png")))


;; frame layout

(defvar brushed-metal:frame
 `(;; menu button
   ((background . ,brushed-metal:menu-images)
    (top-edge . -15)
    (left-edge . 0)
    (class . menu-button))

   ;; title bar
   ((background . ,brushed-metal:title-images)
    (foreground . "black")
    (text . window-name)
    (x-justify . 10)
    (y-justify . center)
    (left-edge . 21)
    (right-edge . 45)
    (top-edge . -15)
    (class . title))

   ;; iconify button
   ((background . ,brushed-metal:iconify-images)
    (right-edge . 28)
    (top-edge . -15)
    (class . iconify-button))

   ;; maximize button
   ((background . ,brushed-metal:maximize-images)
    (right-edge . 14)
    (top-edge . -15)
    (class . maximize-button))

   ;; delete button
   ((background . ,brushed-metal:close-images)
    (right-edge . 0)
    (top-edge . -15)
    (class . close-button))

   ;; left border
   ((background . ,brushed-metal:left-images)
    (left-edge . -4)
    (top-edge . -15)
    (bottom-edge . 0)
    (class . left-border))

   ;; right border
   ((background . ,brushed-metal:right-images)
    (right-edge . -4)
    (top-edge . -15)
    (bottom-edge . 0)
    (class . right-border))

   ;; top border
   ((background . ,brushed-metal:top-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -19)
    (class . top-border))

   ;; bottom border
   ((background . ,brushed-metal:bottom-images)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (class . bottom-border))

   ;; top-left corner
   ((background . ,brushed-metal:top-left-images)
    (left-edge . -4)
    (top-edge . -19)
    (class . top-left-corner))

   ;; top-right corner
   ((background . ,brushed-metal:top-right-images)
    (right-edge . -4)
    (top-edge . -19)
    (class . top-right-corner))

   ;; bottom-left corner
   ((background . ,brushed-metal:bottom-left-images)
    (left-edge . -4)
    (bottom-edge . -4)
    (class . bottom-left-corner))

   ;; bottom-right corner
   ((background . ,brushed-metal:bottom-right-images)
    (right-edge . -4)
    (bottom-edge . -4)
    (class . bottom-right-corner))))

(defvar brushed-metal:shaped-frame
 `(;; menu button
   ((background . ,brushed-metal:menu-images)
    (top-edge . -19)
    (left-edge . 0)
    (class . menu-button))

   ;; title bar
   ((background . ,brushed-metal:title-images)
    (foreground . "black")
    (text . window-name)
    (x-justify . 10)
    (y-justify . center)
    (left-edge . 21)
    (right-edge . 45)
    (top-edge . -19)
    (class . title))

   ;; iconify button
   ((background . ,brushed-metal:iconify-images)
    (right-edge . 28)
    (top-edge . -19)
    (class . iconify-button))

   ;; maximize button
   ((background . ,brushed-metal:maximize-images)
    (right-edge . 14)
    (top-edge . -19)
    (class . maximize-button))

   ;; delete button
   ((background . ,brushed-metal:close-images)
    (right-edge . 0)
    (top-edge . -19)
    (class . close-button))

   ;; top border
   ((background . ,brushed-metal:top-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -23)
    (class . top-border))

   ;; bottom border
   ((background . ,brushed-metal:bottom-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -4)
    (class . bottom-border))

   ;; left border
   ((background . ,brushed-metal:left-images)
    (left-edge . -4)
    (top-edge . -19)
    (height . 15)
    (class . left-border))

   ;; right border
   ((background . ,brushed-metal:right-images)
    (right-edge . -4)
    (top-edge . -19)
    (height . 15)
    (class . right-border))

   ;; top-left corner
   ((background . ,brushed-metal:top-left-images)
    (left-edge . -4)
    (top-edge . -23)
    (class . top-left-corner))

   ;; top-right corner
   ((background . ,brushed-metal:top-right-images)
    (right-edge . -4)
    (top-edge . -23)
    (class . top-right-corner))

   ;; bottom-left corner
   ((background . ,brushed-metal:bottom-left-images)
    (left-edge . -4)
    (top-edge . -4)
    (class . bottom-left-corner))

   ;; bottom-right corner
   ((background . ,brushed-metal:bottom-right-images)
    (right-edge . -4)
    (top-edge . -4)
    (class . bottom-right-corner))))

(defvar brushed-metal:transient-frame
  `(;; top
    ((background . ,brushed-metal:top-images)
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -4)
     (class . title))

    ;; bottom
    ((background . ,brushed-metal:bottom-images)
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . -4)
     (class . bottom-border))

   ;; left border
   ((background . ,brushed-metal:left-images)
    (left-edge . -4)
    (top-edge . -4)
    (bottom-edge . 0)
    (class . left-border))

   ;; right border
   ((background . ,brushed-metal:right-images)
    (right-edge . -4)
    (top-edge . -4)
    (bottom-edge . 0)
    (class . right-border))

   ;; top-left corner
   ((background . ,brushed-metal:top-left-images)
    (left-edge . -4)
    (top-edge . -4)
    (class . top-left-corner))

   ;; top-right corner
   ((background . ,brushed-metal:top-right-images)
    (right-edge . -4)
    (top-edge . -4)
    (class . top-right-corner))

   ;; bottom-left corner
   ((background . ,brushed-metal:bottom-left-images)
    (left-edge . -4)
    (bottom-edge . -4)
    (class . bottom-left-corner))

   ;; bottom-right corner
   ((background . ,brushed-metal:bottom-right-images)
    (right-edge . -4)
    (bottom-edge . -4)
    (class . bottom-right-corner))))

(defun brushed-metal:frame-style (w type)
  (cond ((eq type 'shaped)
	 'brushed-metal:shaped-frame)
	((eq type 'shaped-transient)
	 'nil-frame)
	((eq type 'transient)
	 'brushed-metal:transient-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'brushed-metal:frame)))

(add-frame-style 'brushed-metal 'brushed-metal:frame-style)
