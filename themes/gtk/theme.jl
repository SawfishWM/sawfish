;; gtk/theme.jl
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

(require 'gtkrc)

;; 15x15
(defvar gtk:minimize (list (make-image "as_min.png")
			   nil nil (make-image "as_min-b.png")))
(defvar gtk:close (list (make-image "as_close.png")
			nil nil (make-image "as_close-b.png")))

;; image tiles constructed when no background pixmaps, only colours
(defvar gtk:background-images nil)


;; frame defs

(defvar gtk:frame nil)
(defvar gtk:shaped-frame nil)
(defvar gtk:transient-frame nil)
(defvar gtk:shaped-transient-frame nil)

(defun gtk:rebuild-frames ()
  (if gtkrc-background-pixmaps
      (gtkrc-load-pixmaps)
    ;; build image tiles for each colour in gtk:background-images
    (let
	((i -1))
      (setq gtk:background-images
	    (mapcar #'(lambda (x)
			(setq i (1+ i))
			(when (colorp x)
			  (setq x (make-sized-image 16 16 x))
			  (bevel-image x 1 (/= i 3))
			  (set-image-border x 1 1 1 1)
			  x))
		    gtkrc-background))))
  (gtk:construct-frame-defs)
  (mapc #'(lambda (w)
	    (when (eq (window-get w 'current-frame-style) 'gtk)
	      (set-window-frame-style w 'gtk)))
	(managed-windows)))

(defun gtk:construct-frame-defs ()
  (setq gtk:frame
	`(;; title bar
	 (,(gtk:background)
	  (foreground . gtk:foreground)
	  (text . window-name)
	  (x-justify . 30)
	  (y-justify . center)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -21)
	  (height . 21)
	  (class . title))
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
	 (,(gtk:background)
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -4)
	  (height . 4)
	  (class . bottom-border))
	 ;; bottom frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -5)
	  (height . 1))
	 ;; minimize button
	 ((background . ,gtk:minimize)
	  (left-edge . 4)
	  (top-edge . -18)
	  (class . iconify-button)
	  (removable . t))
	 ;; close button
	 ((background . ,gtk:close)
	  (right-edge . 4)
	  (top-edge . -18)
	  (class . close-button)
	  (removable . t))))

  (setq gtk:shaped-frame
	`(;; title bar
	 (,(gtk:background)
	  (foreground . gtk:foreground)
	  (text . window-name)
	  (x-justify . 30)
	  (y-justify . center)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -22)
	  (height . 21)
	  (class . title))
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
	 ((background . ,gtk:minimize)
	  (left-edge . 4)
	  (top-edge . -19)
	  (class . iconify-button)
	  (removable . t))
	 ;; close button
	 ((background . ,gtk:close)
	  (right-edge . 4)
	  (top-edge . -19)
	  (class . close-button)
	  (removable . t))))

  (setq gtk:transient-frame
	`(;; title bar
	 (,(gtk:background)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -4)
	  (height . 4)
	  (class . title))
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
	 (,(gtk:background)
	  (render-scale . 2)
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -4)
	  (height . 4)
	  (class . bottom-border))
	 ;; bottom frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -5)
	  (height . 1))))

  (setq gtk:shaped-transient-frame
	`(;; title bar
	 (,(gtk:background)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -5)
	  (height . 4)
	  (class . title))
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
	  (height . 1)))))

(defun gtk:foreground ()
  gtkrc-foreground)

(defun gtk:background ()
  (if gtk:background-images
      (cons 'background gtk:background-images)
    (cons 'renderer 'gtk:render-bg)))

;; for pixmap frames; this is going to use horrendous amounts of memory,
;; but what other options are there..?
(defun gtk:render-bg (img state)
  (let
      ((bg (cond ((eq state nil)
		  (nth 0 gtkrc-background-pixmaps))
		 ((eq state 'focused)
		  (nth 1 gtkrc-background-pixmaps))
		 ((eq state 'highlighted)
		  (nth 2 gtkrc-background-pixmaps))
		 (t
		  (nth 3 gtkrc-background-pixmaps)))))
    (tile-image img bg)
    (bevel-image img 1 (not (eq state 'clicked)))))

(defun gtk:frame-style (w type)
  (cond ((eq type 'shaped)
	 'gtk:shaped-frame)
	((eq type 'transient)
	 'gtk:transient-frame)
	((eq type 'shaped-transient)
	 'gtk:shaped-transient-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'gtk:frame)))

(unless batch-mode
  (gtk:construct-frame-defs)
  (add-frame-style 'gtk 'gtk:frame-style)
  (add-hook 'gtkrc-changed-hook 'gtk:rebuild-frames))
