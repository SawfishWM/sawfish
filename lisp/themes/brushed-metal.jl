;; brushed-metal.jl
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

(let
    ((image-load-path (cons (expand-file-name "brushed-metal" image-directory) 
			    image-load-path)))
  ;; 217x4
  (defvar bm-bottom-images (list (make-image "b6-.png")
				 (make-image "b6.png")))
  (defvar bm-top-images (list (make-image "b2-.png")
			      (make-image "b2.png")))

  ;; 4x217
  (defvar bm-right-images (list (flip-image-diagonally
				 (copy-image (nth 0 bm-bottom-images)))
				(flip-image-diagonally
				 (copy-image (nth 1 bm-bottom-images)))))

  ;; 4x219
  (defvar bm-left-images (list (flip-image-diagonally
				(copy-image (nth 0 bm-top-images)))
			       (flip-image-diagonally
				(copy-image (nth 1 bm-top-images)))))

  ;; 4x4
  (defvar bm-top-left-images (list (make-image "b1-.png")
				   (make-image "b1.png")))
  (defvar bm-bottom-left-images (list (make-image "b7-.png")
				      (make-image "b7.png")))
  (defvar bm-top-right-images (list (make-image "b3-.png")
				    (make-image "b3.png")))
  (defvar bm-bottom-right-images (list (make-image "b5-.png")
				       (make-image "b5.png")))

  ;; 21x15
  (defvar bm-menu-images (list (make-image "t1-.png")
			       (make-image "t1.png")
			       (make-image "t1-b.png")
			       (make-image "t1b.png")))

  ;; 150x15
  (defvar bm-title-images (list (set-image-border
				 (make-image "t2-.png") 10 10 0 0)
				(set-image-border
				 (make-image "t2.png") 10 10 0 0)))

  ;; 17x15
  (defvar bm-iconify-images (list (make-image "t3-.png")
				  (make-image "t3.png")
				  (make-image "t3-b.png")
				  (make-image "t3b.png")))

  ;; 14x15
  (defvar bm-maximize-images (list (make-image "t4-.png")
				   (make-image "t4.png")
				   (make-image "t4-b.png")
				   (make-image "t4b.png")))

  ;; 14x15
  (defvar bm-close-images (list (make-image "t5-.png")
				(make-image "t5.png")
				(make-image "t5-b.png")
				(make-image "t5b.png"))))


;; frame layout

(put 'brushed-metal-frame 'unshaped t)
(defvar brushed-metal-frame
 `(;; menu button
   ((background . ,bm-menu-images)
    (top-edge . -15)
    (left-edge . 0)
    (keymap . menu-button-keymap))

   ;; title bar
   ((background . ,bm-title-images)
    (foreground . "black")
    (text . window-name)
    (x-justify . 10)
    (y-justify . center)
    (left-edge . 21)
    (right-edge . 45)
    (top-edge . -15)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; iconify button
   ((background . ,bm-iconify-images)
    (right-edge . 28)
    (top-edge . -15)
    (keymap . iconify-button-keymap))

   ;; maximize button
   ((background . ,bm-maximize-images)
    (right-edge . 14)
    (top-edge . -15)
    (keymap . maximize-button-keymap))

   ;; delete button
   ((background . ,bm-close-images)
    (right-edge . 0)
    (top-edge . -15)
    (keymap . close-button-keymap))

   ;; left border
   ((background . ,bm-left-images)
    (left-edge . -4)
    (top-edge . -15)
    (bottom-edge . 0)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; right border
   ((background . ,bm-right-images)
    (right-edge . -4)
    (top-edge . -15)
    (bottom-edge . 0)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; top border
   ((background . ,bm-top-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -19)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; bottom border
   ((background . ,bm-bottom-images)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; top-left corner
   ((background . ,bm-top-left-images)
    (left-edge . -4)
    (top-edge . -19))

   ;; top-right corner
   ((background . ,bm-top-right-images)
    (right-edge . -4)
    (top-edge . -19))

   ;; bottom-left corner
   ((background . ,bm-bottom-left-images)
    (left-edge . -4)
    (bottom-edge . -4))

   ;; bottom-right corner
   ((background . ,bm-bottom-right-images)
    (right-edge . -4)
    (bottom-edge . -4))))

(put 'brushed-metal-shaped-frame 'unshaped t)
(defvar brushed-metal-shaped-frame
 `(;; menu button
   ((background . ,bm-menu-images)
    (top-edge . -19)
    (left-edge . 0)
    (keymap . menu-button-keymap))

   ;; title bar
   ((background . ,bm-title-images)
    (foreground . "black")
    (text . window-name)
    (x-justify . 10)
    (y-justify . center)
    (left-edge . 21)
    (right-edge . 45)
    (top-edge . -19)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; iconify button
   ((background . ,bm-iconify-images)
    (right-edge . 28)
    (top-edge . -19)
    (keymap . iconify-button-keymap))

   ;; maximize button
   ((background . ,bm-maximize-images)
    (right-edge . 14)
    (top-edge . -19)
    (keymap . maximize-button-keymap))

   ;; delete button
   ((background . ,bm-close-images)
    (right-edge . 0)
    (top-edge . -19)
    (keymap . close-button-keymap))

   ;; top border
   ((background . ,bm-top-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -23)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; bottom border
   ((background . ,bm-bottom-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -4)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; left border
   ((background . ,bm-left-images)
    (left-edge . -4)
    (top-edge . -19)
    (height . 15)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; right border
   ((background . ,bm-right-images)
    (right-edge . -4)
    (top-edge . -19)
    (height . 15)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; top-left corner
   ((background . ,bm-top-left-images)
    (left-edge . -4)
    (top-edge . -23))

   ;; top-right corner
   ((background . ,bm-top-right-images)
    (right-edge . -4)
    (top-edge . -23))

   ;; bottom-left corner
   ((background . ,bm-bottom-left-images)
    (left-edge . -4)
    (top-edge . -4))

   ;; bottom-right corner
   ((background . ,bm-bottom-right-images)
    (right-edge . -4)
    (top-edge . -4))))

(put 'brushed-metal-transient-frame 'unshaped t)
(defvar brushed-metal-transient-frame
  `(;; top
    ((background . ,bm-top-images)
     (left-edge . 0)
     (right-edge . 0)
     (top-edge . -4)
     (cursor . hand2)
     (keymap . title-keymap))

    ;; bottom
    ((background . ,bm-bottom-images)
     (left-edge . 0)
     (right-edge . 0)
     (bottom-edge . -4)
     (cursor . hand2)
     (keymap . title-keymap))

   ;; left border
   ((background . ,bm-left-images)
    (left-edge . -4)
    (top-edge . -4)
    (bottom-edge . 0)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; right border
   ((background . ,bm-right-images)
    (right-edge . -4)
    (top-edge . -4)
    (bottom-edge . 0)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; top-left corner
   ((background . ,bm-top-left-images)
    (left-edge . -4)
    (top-edge . -4))

   ;; top-right corner
   ((background . ,bm-top-right-images)
    (right-edge . -4)
    (top-edge . -4))

   ;; bottom-left corner
   ((background . ,bm-bottom-left-images)
    (left-edge . -4)
    (bottom-edge . -4))

   ;; bottom-right corner
   ((background . ,bm-bottom-right-images)
    (right-edge . -4)
    (bottom-edge . -4))))

(defun brushed-metal-frame-style (w type)
  (cond ((eq type 'shaped)
	 'brushed-metal-shaped-frame)
	((eq type 'shaped-transient)
	 'nil-frame)
	((eq type 'transient)
	 'brushed-metal-transient-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'brushed-metal-frame)))

(add-frame-style 'brushed-metal 'brushed-metal-frame-style)
