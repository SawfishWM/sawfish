;; simple/theme.jl
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

(defgroup simple "Simple frame")

(defcustom simple-normal-color "lightsteelblue4"
  "Color of inactive frames in `simple' frame style."
  :type color
  :group simple
  :after-set after-setting-frame-option)

(defcustom simple-active-color "goldenrod"
  "Color of active frames in `simple' frame style."
  :type color
  :group simple
  :after-set after-setting-frame-option)

;; 15x15
(defvar simple-minimize (list (make-image "as_min.png")
			      nil nil (make-image "as_min-b.png")))
(defvar simple-close (list (make-image "as_close.png")
			   nil nil (make-image "as_close-b.png")))

(defun simple-frame-colors ()
  (list simple-normal-color simple-active-color))

(defvar simple-frame
 `(;; title bar
   ((background . simple-frame-colors)
    (foreground . "black")
    (text . window-name)
    (x-justify . 30)
    (y-justify . center)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -21)
    (height . 21)
    ,@(fp-class 'title))
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
   ((background . simple-frame-colors)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (height . 4)
    ,@(fp-class 'bottom-border))
   ;; bottom frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -5)
    (height . 1))
   ;; minimize button
   ((background . ,simple-minimize)
    (left-edge . 4)
    (top-edge . -18)
    ,@(fp-class 'iconify))
   ;; close button
   ((background . ,simple-close)
    (right-edge . 4)
    (top-edge . -18)
    ,@(fp-class 'close))))
(put 'simple-frame 'unshaped t)

(defvar simple-shaped-frame
 `(;; title bar
   ((background . simple-frame-colors)
    (foreground . "black")
    (text . window-name)
    (x-justify . 30)
    (y-justify . center)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -22)
    (height . 21)
    ,@(fp-class 'title))
   ;; title frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -23)
    (height . 1))
   ;; left frame
   ((background . "black")
    (left-edge . -1)
    (width . 1)
    (top-edge . -23)
    (height . 23))
   ;; right frame
   ((background . "black")
    (right-edge . -1)
    (width . 1)
    (top-edge . -23)
    (height . 23))
   ;; bottom frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -1)
    (height . 1))
   ;; minimize button
   ((background . ,simple-minimize)
    (left-edge . 5)
    (top-edge . -19)
    ,@(fp-class 'iconify))
   ;; close button
   ((background . ,simple-close)
    (right-edge . 5)
    (top-edge . -19)
    ,@(fp-class 'close))))
(put 'simple-shaped-frame 'unshaped t)

(defvar simple-transient-frame
 `(;; title bar
   ((background . simple-frame-colors)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -4)
    (height . 4)
    ,@(fp-class 'title))
   ;; title frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -5)
    (height . 1))
   ;; left frame
   ((background . "black")
    (left-edge . -1)
    (width . 1)
    (top-edge . -5)
    (bottom-edge . -5))
   ;; right frame
   ((background . "black")
    (right-edge . -1)
    (width . 1)
    (top-edge . -5)
    (bottom-edge . -5))
   ;; bottom bar
   ((background . simple-frame-colors)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (height . 4)
    ,@(fp-class 'bottom-border))
   ;; bottom frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -5)
    (height . 1))))
(put 'simple-transient-frame 'unshaped t)

(defvar simple-shaped-transient-frame
 `(;; title bar
   ((background . simple-frame-colors)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -5)
    (height . 4)
    ,@(fp-class 'title))
   ;; title frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -6)
    (height . 1))
   ;; left frame
   ((background . "black")
    (left-edge . -1)
    (width . 1)
    (top-edge . -6)
    (height . 6))
   ;; right frame
   ((background . "black")
    (right-edge . -1)
    (width . 1)
    (top-edge . -6)
    (height . 6))
   ;; bottom frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -1)
    (height . 1))))
(put 'simple-transient-shaped-frame 'unshaped t)

(defun simple-frame-style (w type)
  (cond ((eq type 'shaped)
	 'simple-shaped-frame)
	((eq type 'transient)
	 'simple-transient-frame)
	((eq type 'shaped-transient)
	 'simple-shaped-transient-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'simple-frame)))

(add-frame-style 'simple 'simple-frame-style)
