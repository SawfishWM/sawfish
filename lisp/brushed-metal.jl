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
  (defvar bm-bottom-image (make-image "b6-.png"))
  (defvar bm-bottom-hl-image (make-image "b6.png"))
  (defvar bm-top-image (make-image "b2-.png"))
  (defvar bm-top-hl-image (make-image "b2.png"))

  ;; 4x217
  (defvar bm-right-image (copy-image bm-bottom-image))
  (flip-image-diagonally bm-right-image)
  (defvar bm-right-hl-image (copy-image bm-bottom-hl-image))
  (flip-image-diagonally bm-right-hl-image)

  ;; 4x219
  (defvar bm-left-image (copy-image bm-top-image))
  (flip-image-diagonally bm-left-image)
  (defvar bm-left-hl-image (copy-image bm-top-hl-image))
  (flip-image-diagonally bm-left-hl-image)

  ;; 4x4
  (defvar bm-top-left-image (make-image "b1-.png"))
  (defvar bm-top-left-hl-image (make-image "b1.png"))
  (defvar bm-bottom-left-image (make-image "b7-.png"))
  (defvar bm-bottom-left-hl-image (make-image "b7.png"))
  (defvar bm-top-right-image (make-image "b3-.png"))
  (defvar bm-top-right-hl-image (make-image "b3.png"))
  (defvar bm-bottom-right-image (make-image "b5-.png"))
  (defvar bm-bottom-right-hl-image (make-image "b5.png"))

  ;; 21x15
  (defvar bm-menu-image (make-image "t1-.png"))
  (defvar bm-menu-hl-image (make-image "t1.png"))
  (defvar bm-menu-clicked-image (make-image "t1-b.png"))
  (defvar bm-menu-hl-clicked-image (make-image "t1b.png"))

  ;; 150x15
  (defvar bm-title-image (make-image "t2-.png"))
  (set-image-border bm-title-image 10 10 0 0)
  (defvar bm-title-hl-image (make-image "t2.png"))
  (set-image-border bm-title-hl-image 10 10 0 0)

  ;; 17x15
  (defvar bm-iconify-image (make-image "t3-.png"))
  (defvar bm-iconify-hl-image (make-image "t3.png"))
  (defvar bm-iconify-clicked-image (make-image "t3-b.png"))
  (defvar bm-iconify-hl-clicked-image (make-image "t3b.png"))

  ;; 14x15
  (defvar bm-maximize-image (make-image "t4-.png"))
  (defvar bm-maximize-hl-image (make-image "t4.png"))
  (defvar bm-maximize-clicked-image (make-image "t4-b.png"))
  (defvar bm-maximize-hl-clicked-image (make-image "t4b.png"))

  ;; 14x15
  (defvar bm-close-image (make-image "t5-.png"))
  (defvar bm-close-hl-image (make-image "t5.png"))
  (defvar bm-close-clicked-image (make-image "t5-b.png"))
  (defvar bm-close-hl-clicked-image (make-image "t5b.png")))


;; frame layout

(defvar brushed-metal-frame (make-frame "brushed-metal"))
(frame-put brushed-metal-frame 'unshaped t)

(set-frame-generator brushed-metal-frame
 `(;; menu button
   ((background . (,bm-menu-image ,bm-menu-hl-image
		   ,bm-menu-clicked-image ,bm-menu-hl-clicked-image))
    (top-edge . -15)
    (left-edge . 0)
    (keymap . menu-button-keymap))

   ;; title bar
   ((background . (,bm-title-image ,bm-title-hl-image))
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
   ((background . (,bm-iconify-image ,bm-iconify-hl-image
		   ,bm-iconify-clicked-image ,bm-iconify-hl-clicked-image))
    (right-edge . 28)
    (top-edge . -15)
    (keymap . iconify-button-keymap))

   ;; maximize button
   ((background . (,bm-maximize-image ,bm-maximize-hl-image
		   ,bm-maximize-clicked-image ,bm-maximize-hl-clicked-image))
    (right-edge . 14)
    (top-edge . -15)
    (keymap . maximize-button-keymap))

   ;; delete button
   ((background . (,bm-close-image ,bm-close-hl-image
		   ,bm-close-clicked-image ,bm-close-hl-clicked-image))
    (right-edge . 0)
    (top-edge . -15)
    (keymap . close-button-keymap))

   ;; left border
   ((background . (,bm-left-image ,bm-left-hl-image))
    (left-edge . -4)
    (top-edge . -15)
    (bottom-edge . 0)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; right border
   ((background . (,bm-right-image ,bm-right-hl-image))
    (right-edge . -4)
    (top-edge . -15)
    (bottom-edge . 0)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; top border
   ((background . (,bm-top-image ,bm-top-hl-image))
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -19)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; bottom border
   ((background . (,bm-bottom-image ,bm-bottom-hl-image))
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (cursor . hand2)
    (keymap . title-keymap))

   ;; top-left corner
   ((background . (,bm-top-left-image ,bm-top-left-hl-image))
    (left-edge . -4)
    (top-edge . -19))

   ;; top-right corner
   ((background . (,bm-top-right-image ,bm-top-right-hl-image))
    (right-edge . -4)
    (top-edge . -19))

   ;; bottom-left corner
   ((background . (,bm-bottom-left-image ,bm-bottom-left-hl-image))
    (left-edge . -4)
    (bottom-edge . -4))

   ;; bottom-right corner
   ((background . (,bm-bottom-right-image ,bm-bottom-right-hl-image))
    (right-edge . -4)
    (bottom-edge . -4))))

(setq default-frame brushed-metal-frame)
