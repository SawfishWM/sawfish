;; gradient/theme.jl
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

(require 'gradient)

;; Commentary:

;; This is mainly just a test of the on-the-fly image rendering code.
;; It should be possible to implement gradients using a couple of
;; static images letting Imlib resize them. Maybe you get smoother
;; gradients this way, who knows..?

(defgroup gradient-frame "Gradient frame")

(defcustom gradient-gradient-type 'diagonal
  "Direction of gradient in `gradient' frame style."
  :type (set horizontal vertical diagonal)
  :group gradient-frame
  :after-set after-setting-frame-option)

(defcustom gradient-normal-from-color (get-color "#b6b6b6")
  "`From' color of inactive frames in `gradient' frame style."
  :type color
  :group gradient-frame
  :after-set after-setting-frame-option)

(defcustom gradient-normal-to-color (get-color "#323232")
  "`To' color of inactive frames in `gradient' frame style."
  :type color
  :group gradient-frame
  :after-set after-setting-frame-option)

(defcustom gradient-active-from-color (get-color "#64b4df")
  "`From' color of active frames in `gradient' frame style."
  :type color
  :group gradient-frame
  :after-set after-setting-frame-option)

(defcustom gradient-active-to-color (get-color "#000030")
  "`To' color of active frames in `gradient' frame style."
  :type color
  :group gradient-frame
  :after-set after-setting-frame-option)

(defcustom gradient-save-memory t
  "Use less memory when creating gradients, possibly affecting quality."
  :type boolean
  :group gradient-frame
  :after-set after-setting-frame-option)

;; 15x15
(defvar gradient-minimize (list (make-image "as_min.png")
				nil nil (make-image "as_min-b.png")))
(defvar gradient-close (list (make-image "as_close.png")
			     nil nil (make-image "as_close-b.png")))

(defun gradient-render-bg (img state)
  (apply (cond ((eq gradient-gradient-type 'diagonal)
		'draw-diagonal-gradient)
	       ((eq gradient-gradient-type 'horizontal)
		'draw-horizontal-gradient)
	       ((eq gradient-gradient-type 'vertical)
		'draw-vertical-gradient))
	 img (if state
		 (list gradient-active-from-color gradient-active-to-color)
	       (list gradient-normal-from-color gradient-normal-to-color)))
  (bevel-image img 1 (not (eq state 'clicked)))
  (set-image-border img 1 1 1 1))

(defun gradient-scale ()
  (if gradient-save-memory 2 1))

(defvar gradient-frame
 `(;; title bar
   ((renderer . gradient-render-bg)
    (render-scale . gradient-scale)
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
   ((renderer . gradient-render-bg)
    (render-scale . gradient-scale)
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
   ((background . ,gradient-minimize)
    (left-edge . 4)
    (top-edge . -18)
    ,@(fp-class 'iconify))
   ;; close button
   ((background . ,gradient-close)
    (right-edge . 4)
    (top-edge . -18)
    ,@(fp-class 'close))))
(put 'gradient-frame 'unshaped t)

(defvar gradient-shaped-frame
 `(;; title bar
   ((renderer . gradient-render-bg)
    (render-scale . gradient-scale)
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
   ((background . ,gradient-minimize)
    (left-edge . 5)
    (top-edge . -19)
    ,@(fp-class 'iconify))
   ;; close button
   ((background . ,gradient-close)
    (right-edge . 5)
    (top-edge . -19)
    ,@(fp-class 'close))))
(put 'gradient-shaped-frame 'unshaped t)

(defvar gradient-transient-frame
 `(;; title bar
   ((renderer . gradient-render-bg)
    (render-scale . gradient-scale)
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
   ((renderer . gradient-render-bg)
    (render-scale . gradient-scale)
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
(put 'gradient-transient-frame 'unshaped t)

(defvar gradient-shaped-transient-frame
 `(;; title bar
   ((renderer . gradient-render-bg)
    (render-scale . gradient-scale)
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
(put 'gradient-transient-shaped-frame 'unshaped t)

(defun gradient-frame-style (w type)
  (cond ((eq type 'shaped)
	 'gradient-shaped-frame)
	((eq type 'transient)
	 'gradient-transient-frame)
	((eq type 'shaped-transient)
	 'gradient-shaped-transient-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'gradient-frame)))

(add-frame-style 'gradient 'gradient-frame-style)
