;; viewport.jl -- virtual desktops
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

(provide 'viewport)

;; Commentary:

;; Virtual workspaces are implemented by moving windows in and out of
;; the screen dimensions. E.g. moving to the left moves all windows one
;; screen-width to the right. 

(defcustom viewport-dimensions '(1 . 1)
  "Number of columns and rows in each virtual workspace: \\w"
  :group workspace
  :type (pair (number 1) (number 1))
  :user-level novice
  :after-set (lambda () (viewport-size-changed)))

(defcustom uniconify-to-current-viewport t
  "Windows uniconify to the current viewport."
  :type boolean
  :group (min-max iconify))


;; raw viewport handling

(defvar viewport-x-offset 0)
(defvar viewport-y-offset 0)

(defun set-viewport (x y)
  (unless (and (= viewport-x-offset x) (= viewport-y-offset y))
    (mapc (lambda (w)
	    (unless (window-get w 'sticky-viewport)
	      (let
		  ((pos (window-position w)))
		(move-window-to w (- (+ (car pos) viewport-x-offset) x)
				(- (+ (cdr pos) viewport-y-offset) y)))))
	  (managed-windows))
    (setq viewport-x-offset x)
    (setq viewport-y-offset y)
    (call-hook 'viewport-moved-hook)))

(defun viewport-before-exiting ()
  (set-screen-viewport 0 0))

(add-hook 'before-exit-hook viewport-before-exiting t)


;; screen sized viewport handling

(defun screen-viewport ()
  (cons (quotient viewport-x-offset (screen-width))
	(quotient viewport-y-offset (screen-height))))

;; returns t if it actually moved the viewport
(defun set-screen-viewport (col row)
  (when (and (>= col 0) (< col (car viewport-dimensions))
	     (>= row 0) (< row (cdr viewport-dimensions)))
    (set-viewport (* col (screen-width))
		  (* row (screen-height)))
    t))
  
;; returns t if it actually moved the viewport
(defun move-viewport (right down)
  (let
      ((port (screen-viewport)))
    (set-screen-viewport (+ (car port) right)
			 (+ (cdr port) down))))

(defun move-viewport-to-window (window)
  (when (window-outside-viewport-p window)
    (let
	((pos (window-position window)))
      (rplaca pos (+ (car pos) viewport-x-offset))
      (rplacd pos (+ (cdr pos) viewport-y-offset))
      (set-screen-viewport (quotient (car pos) (screen-width))
			   (quotient (cdr pos) (screen-height))))))

(defun window-outside-workspace-p (window)
  (let
      ((pos (window-position window))
       (dims (window-frame-dimensions window))
       (left (- viewport-x-offset))
       (right (- (* (car viewport-dimensions) (screen-width))
		 viewport-x-offset))
       (top (- viewport-y-offset))
       (bottom (- (* (cdr viewport-dimensions) (screen-height))
		  viewport-y-offset)))
    (or (>= (car pos) right) (>= (cdr pos) bottom)
	(<= (+ (car pos) (car dims)) left) (<= (+ (cdr pos) (cdr dims)) top))))

(defun window-outside-viewport-p (window)
  (let
      ((pos (window-position window))
       (dims (window-frame-dimensions window)))
    (or (<= (+ (car pos) (car dims)) 0)
	(<= (+ (cdr pos) (cdr dims)) 0)
	(>= (car pos) (screen-width))
	(>= (cdr pos) (screen-height)))))

(defun move-window-to-current-viewport (window)
  (when (window-outside-viewport-p window)
    (let
	((pos (window-position window)))
      (move-window-to window (mod (car pos) (screen-width))
		      (mod (cdr pos) (screen-height))))))

(defun set-window-viewport (window col row)
  (let
      ((pos (window-position window)))
    (setq col (max 0 (min (1- (car viewport-dimensions)) col)))
    (setq row (max 0 (min (1- (cdr viewport-dimensions)) row)))
    (setq col (+ (* col (screen-width)) (mod (car pos) (screen-width))))
    (setq row (+ (* row (screen-height)) (mod (cdr pos) (screen-height))))
    (move-window-to
     window (- col viewport-x-offset) (- row viewport-y-offset))))

(defun move-window-viewport (window col row)
  (let
      ((pos (window-position window)))
    (set-window-viewport window
			 (+ (quotient (+ (car pos) viewport-x-offset)
				      (screen-width)) col)
			 (+ (quotient (+ (cdr pos) viewport-y-offset)
				      (screen-height)) row))))

(defun window-viewport (w)
  (let
      ((position (window-position w)))
    (cons (quotient (+ (car position) viewport-x-offset) (screen-width))
	  (quotient (+ (cdr position) viewport-y-offset) (screen-height)))))

(defun window-absolute-position (w)
  (let
      ((position (window-position w)))
    (if (window-outside-viewport-p w)
	(cons (mod (+ (car position) viewport-x-offset) (screen-width))
	      (mod (+ (cdr position) viewport-y-offset) (screen-height)))
      position)))

(defun viewport-size-changed ()
  (let
      ((port (screen-viewport)))
    (set-screen-viewport (min (car port) (1- (car viewport-dimensions)))
			 (min (cdr port) (1- (cdr viewport-dimensions))))
    (mapc (lambda (w)
	    (when (window-outside-workspace-p w)
	      (move-window-to-current-viewport w)))
	  (managed-windows))
    (call-hook 'viewport-resized-hook)))


;; misc

(defun viewport-window-uniconified (w)
  (when uniconify-to-current-viewport
    (move-window-to-current-viewport w)))


;; commands

(defun move-viewport-right ()
  "Move the viewport one screen to the right."
  (interactive)
  (move-viewport 1 0))

(defun move-viewport-left ()
  "Move the viewport one screen to the left."
  (interactive)
  (move-viewport -1 0))

(defun move-viewport-down ()
  "Move the viewport one screen down."
  (interactive)
  (move-viewport 0 1))

(defun move-viewport-up ()
  "Move the viewport one screen up."
  (interactive)
  (move-viewport 0 -1))

(defun move-window-left (w)
  "Move the window to the viewport on the left."
  (interactive "%W")
  (move-window-viewport w -1 0)
  (move-viewport-left))

(defun move-window-right (w)
  "Move the window to the viewport on the right."
  (interactive "%W")
  (move-window-viewport w 1 0)
  (move-viewport-right))

(defun move-window-down (w)
  "Move the window to the viewport below."
  (interactive "%W")
  (move-window-viewport w 0 1)
  (move-viewport-down))

(defun move-window-up (w)
  "Move the window to the viewport above."
  (interactive "%W")
  (move-window-viewport w 0 -1)
  (move-viewport-up))


;; session management

(defun viewport-saved-state (w)
  (let
      ((position (window-position w)))
    (when (window-get w 'sticky-viewport)
      (rplaca position (mod (car position) (screen-width)))
      (rplacd position (mod (cdr position) (screen-height))))
    `((position . ,(window-absolute-position w))
      (viewport . ,(window-viewport w)))))

(defun viewport-load-state (w alist)
  (let
      ((position (cdr (assq 'position alist)))
       (viewport (cdr (assq 'viewport alist))))
    (when position
      (if (or (not viewport) (window-get w 'sticky-viewport))
	  (move-window-to w (car position) (cdr position))
	(move-window-to w (+ (* (car viewport) (screen-width)) (car position))
			(+ (* (cdr viewport) (screen-height)) (cdr position)))
	(when (window-outside-workspace-p w)
	  (move-window-to-current-viewport w)))
      (window-put w 'placed t))))
			     
(sm-add-saved-properties 'sticky-viewport)
(add-hook 'sm-window-save-functions viewport-saved-state)
(add-hook 'sm-restore-window-hook viewport-load-state)


;; initialisation

(add-hook 'viewport-moved-hook window-order-focus-most-recent)
(add-hook 'uniconify-window-hook viewport-window-uniconified)
