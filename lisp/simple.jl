;; simple.jl
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

(defvar simple-frame-colors '("lightsteelblue4" "lightsteelblue2"))

(let
    ((image-load-path (cons (expand-file-name "misc" image-directory)
			    image-load-path)))
  ;; 15x15
  (defvar simple-minimize (make-image "as_min.png"))
  (defvar simple-minimize-clicked (make-image "as_min-b.png"))
  (defvar simple-close (make-image "as_close.png"))
  (defvar simple-close-clicked (make-image "as_close-b.png")))

(defvar simple-frame
 `(;; title bar
   ((background . ,simple-frame-colors)
    (foreground . "black")
    (text . window-name)
    (x-justify . 30)
    (y-justify . center)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -21)
    (height . 21)
    (keymap . title-keymap))
   ;; title frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -22)
    (height . 1))
   ;; left frame
   ((background . "black")
    (left-edge . -1)
    (width . 1)
    (top-edge . -22)
    (bottom-edge . -5))
   ;; right frame
   ((background . "black")
    (right-edge . -1)
    (width . 1)
    (top-edge . -22)
    (bottom-edge . -5))
   ;; bottom bar
   ((background . ,simple-frame-colors)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (height . 4)
    (keymap . title-keymap))
   ;; bottom frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -5)
    (height . 1))
   ;; minimize button
   ((background . ,(list simple-minimize simple-minimize
			 simple-minimize-clicked simple-minimize-clicked))
    (left-edge . 5)
    (top-edge . -18)
    (keymap . iconify-button-keymap))
   ;; close button
   ((background . ,(list simple-close simple-close
			 simple-close-clicked simple-close-clicked))
    (right-edge . 5)
    (top-edge . -18)
    (keymap . close-button-keymap))))
(put 'simple-frame 'unshaped t)

;; XXX get rid of this
(setq default-frame 'simple-frame)
