;; move-resize.jl -- interactive moving and resizing of windows
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

(provide 'move-resize)

;; todo:
;;  * obey the aspect ratio size hints
;;  * resize has truly bizarre behaviour

;;;###autoload (setq custom-required (cons 'move-resize custom-required))

(defcustom move-outline-mode 'opaque
  "The method of drawing windows being moved interactively."
  :type (set opaque box)
  :group move)

(defcustom resize-outline-mode 'opaque
  "The method of drawing windows being resized interactively."
  :type (set opaque box)
  :group move)

(defcustom move-resize-raise-window nil
  "Raise windows being moved or resized interactively."
  :group move
  :type boolean)

(defvar move-resize-map (bind-keys (make-sparse-keymap)
			  "Any-PointerUp" 'move-resize-finished
			  "Any-PointerMove" 'move-resize-motion))

;; specials
(defvar move-resize-window nil)
(defvar move-resize-function nil)
(defvar move-resize-x nil)
(defvar move-resize-y nil)
(defvar move-resize-width nil)
(defvar move-resize-height nil)
(defvar move-resize-old-x nil)
(defvar move-resize-old-y nil)
(defvar move-resize-old-width nil)
(defvar move-resize-old-height nil)
(defvar move-resize-old-ptr-x nil)
(defvar move-resize-old-ptr-y nil)
(defvar move-resize-mode nil)
(defvar move-resize-hints nil)
(defvar move-resize-frame nil)

;; called to initiate a move or resize on window W. FUNCTION is either
;; `move' or `resize'
(defun move-resize-window (w function)
  (when move-resize-raise-window
    (raise-window w))
  (let*
      ((from-motion-event (string-match "-Move$" (event-name (current-event))))
       (override-keymap move-resize-map)
       (move-resize-window w)
       (move-resize-function function)
       (move-resize-old-x (car (window-position w)))
       (move-resize-old-y (cdr (window-position w)))
       (move-resize-old-width (car (window-dimensions w)))
       (move-resize-old-height (cdr (window-dimensions w)))
       (move-resize-x move-resize-old-x)
       (move-resize-y move-resize-old-y)
       (move-resize-width move-resize-old-width)
       (move-resize-height move-resize-old-height)
       (move-resize-old-ptr-x (car (if from-motion-event
				       (query-last-pointer)
				     (query-pointer))))
       (move-resize-old-ptr-y (cdr (if from-motion-event
				       (query-last-pointer)
				     (query-pointer))))
       (move-resize-hints (window-size-hints w))
       (move-resize-frame (cons (- (car (window-frame-dimensions w))
				   move-resize-old-width)
				(- (cdr (window-frame-dimensions w))
				   move-resize-old-height)))
       (move-resize-mode (if (eq function 'move)
			     move-outline-mode
			   resize-outline-mode)))
    (unless (eq move-resize-mode 'opaque)
      ;; prevent any other programs drawing on the display
      (grab-server))
    (unwind-protect
	(progn
	  ;; ensure that we catch _all_ mouse events
	  (grab-pointer w)
	  (unwind-protect
	      (progn
		(unless (eq move-resize-mode 'opaque)
		  (draw-window-outline move-resize-mode
				       move-resize-x move-resize-y
				       (+ move-resize-width
					  (car move-resize-frame))
				       (+ move-resize-height
					  (cdr move-resize-frame))))
		(catch 'move-resize-done
		  (when from-motion-event
		    (move-resize-motion))
		  (recursive-edit))))
	  (ungrab-pointer)))
    (unless (eq move-resize-mode 'opaque)
      (ungrab-server))))

;; round up a window dimension X in increments of INC, with minimum
;; value BASE
(defsubst move-resize-roundup (x inc base)
  (+ base (max 0 (* (1+ (/ (1- (- x base)) inc)) inc))))

;; called each pointer motion event during move/resize
(defun move-resize-motion ()
  (interactive)
  (let
      ((ptr-x (car (query-pointer)))
       (ptr-y (cdr (query-pointer))))
    (unless (eq move-resize-mode 'opaque)
      (erase-window-outline (if (eq move-resize-function 'move)
				move-outline-mode
			      resize-outline-mode)
			    move-resize-x move-resize-y
			    (+ move-resize-width (car move-resize-frame))
			    (+ move-resize-height (cdr move-resize-frame))))
    (cond ((eq move-resize-function 'move)
	   (setq move-resize-x (+ move-resize-old-x
				  (- ptr-x move-resize-old-ptr-x)))
	   (setq move-resize-y (+ move-resize-old-y
				  (- ptr-y move-resize-old-ptr-y))))
	  ((eq move-resize-function 'resize)
	   (setq move-resize-width
		 (move-resize-roundup
		  (+ move-resize-old-width (- ptr-x move-resize-old-ptr-x))
		  (or (cdr (assq 'width-inc move-resize-hints)) 1)
		  (or (cdr (or (assq 'base-width move-resize-hints)
			       (assq 'min-width move-resize-hints))) 1)))
	   (setq move-resize-height
		 (move-resize-roundup
		  (+ move-resize-old-height (- ptr-y move-resize-old-ptr-y))
		  (or (cdr (assq 'height-inc move-resize-hints)) 1)
		  (or (cdr (or (assq 'base-height move-resize-hints)
			       (assq 'min-height move-resize-hints))) 1)))))
    (if (eq move-resize-mode 'opaque)
	(move-resize-apply)
      (draw-window-outline (if (eq move-resize-function 'move)
			       move-outline-mode
			     resize-outline-mode)
			   move-resize-x move-resize-y
			   (+ move-resize-width (car move-resize-frame))
			   (+ move-resize-height (cdr move-resize-frame))))))

;; called when the move/resize finished (i.e. button-release event)
(defun move-resize-finished ()
  (interactive)
  (unless (eq move-resize-mode 'opaque)
    (erase-window-outline (if (eq move-resize-function 'move)
			      move-outline-mode
			    resize-outline-mode)
			  move-resize-x move-resize-y
			  (+ move-resize-width (car move-resize-frame))
			  (+ move-resize-height (cdr move-resize-frame))))
  (move-resize-apply)
  (throw 'move-resize-done t))

;; commit the current state of the move or resize
(defun move-resize-apply ()
  (cond ((eq move-resize-function 'move)
	 (move-window-to move-resize-window move-resize-x move-resize-y))
	((eq move-resize-function 'resize)
	 (resize-window-to move-resize-window
			   move-resize-width move-resize-height))))


;; Entry points

;;;###autoload
(defun move-window-interactively (w)
  "Move window W interactively."
  (interactive "f")
  (move-resize-window w 'move))

;;;###autoload
(defun resize-window-interactively (w)
  "Resize window W interactively."
  (interactive "f")
  (move-resize-window w 'resize))

;;;###autoload
(defun move-selected-window ()
  "Wait for the user to select a window, then interactively move that window."
  (interactive)
  (let
      ((w (select-window)))
    (if w
	(move-window-interactively w)
      (beep))))

;;;###autoload
(defun resize-selected-window ()
  "Wait for the user to select a window, then interactively resize that window."
  (interactive)
  (let
      ((w (select-window)))
    (if w
	(resize-window-interactively w)
      (beep))))
