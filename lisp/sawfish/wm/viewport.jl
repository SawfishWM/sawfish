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

(defcustom viewport-columns 1
  "Number of columns in each virtual workspace."
  :group workspace
  :type number
  :range (1 . nil)
  :after-set viewport-size-changed)

(defcustom viewport-rows 1
  "Number of rows in each virtual workspace."
  :group workspace
  :type number
  :range (1 . nil)
  :after-set viewport-size-changed)

(defun viewport-size-changed ()
  (call-hook 'screen-viewport-resized-hook))

(defun screen-viewport ()
  (let
      ((origin (get-viewport)))
    (rplaca origin (/ (car origin) (screen-width)))
    (rplacd origin (/ (cdr origin) (screen-height)))
    origin))

(defun set-screen-viewport (col row)
  (when (and (>= col 0) (< col viewport-columns)
	     (>= row 0) (< row viewport-rows))
    (set-viewport (* col (screen-width))
		  (* row (screen-height)))
    (call-hook 'screen-viewport-moved-hook (list col row))))
  
(defun move-viewport (right down)
  (let
      ((port (screen-viewport)))
    (set-screen-viewport (+ (car port) right)
			 (+ (cdr port) down))))

(defun move-viewport-right ()
  (interactive)
  (move-viewport 1 0))

(defun move-viewport-left ()
  (interactive)
  (move-viewport -1 0))

(defun move-viewport-down ()
  (interactive)
  (move-viewport 0 1))

(defun move-viewport-up ()
  (interactive)
  (move-viewport 0 -1))


;; session management

(defun viewport-saved-state (w)
  (let
      ((position (window-position w))
       (origin (get-viewport)))
    (when (window-get w 'fixed-position)
      (rplaca position (mod (car position) (screen-width)))
      (rplacd position (mod (cdr position) (screen-height))))
    `((position . ,(cons (mod (car position) (screen-width))
			 (mod (cdr position) (screen-height))))
      (viewport . ,(cons (/ (+ (car position) (car origin)) (screen-width))
			 (/ (+ (cdr position) (cdr origin)) (screen-height)))))))

(defun viewport-load-state (w alist)
  (let
      ((position (cdr (assq 'position alist)))
       (viewport (cdr (assq 'viewport alist))))
    (when position
      (if (or (not viewport) (window-get w 'fixed-position))
	  (move-window-to w (car position) (cdr position))
	(move-window-to w (+ (* (car viewport) (screen-width)) (car position))
			(+ (* (cdr viewport) (screen-height)) (cdr position))))
      (window-put w 'placed t))))
			     
(sm-add-saved-properties 'fixed-position)
(add-hook 'sm-window-save-functions 'viewport-saved-state)
(add-hook 'sm-restore-window-hook 'viewport-load-state)
