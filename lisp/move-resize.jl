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

(defcustom move-show-position t
  "Show the current position while moving windows interactively."
  :group move
  :type boolean)

(defcustom resize-show-dimensions t
  "Show the current dimensions while resizing windows interactively."
  :group move
  :type boolean)

(defcustom move-snap-edges nil
  "Snap window position to edges of other windows when interactively moving."
  :group move
  :type boolean)

(defcustom move-snap-epsilon 8
  "Proximity in pixels before snapping to a window edge."
  :group move
  :type number
  :range (0 . 64))

(defcustom move-snap-ignored-windows nil
  "Snap to otherwise-ignored windows."
  :group move
  :type boolean)

(defvar move-resize-map (bind-keys (make-keymap)
			  "Any-Off" 'move-resize-finished
			  "Any-Move" 'move-resize-motion
			  "Any-ESC" 'move-resize-cancel))

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
(defvar move-resize-edges nil)
(defvar move-resize-last-outline nil)
(defvar move-resize-moving-edges nil)

;; called to initiate a move or resize on window W. FUNCTION is either
;; `move' or `resize'
(defun move-resize-window (w function)
  (when move-resize-raise-window
    (raise-window w))
  (let*
      ((from-motion-event (and (current-event)
			       (string-match "-Move$" (event-name
						       (current-event)))))
       (override-keymap move-resize-map)
       ;; don't want any complications..
       (unbound-key-hook nil)
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
				     (query-pointer t))))
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
			   resize-outline-mode))
       (move-resize-edges (and move-snap-edges
			       (progn
				 (require 'edges)
				 (get-visible-window-edges
				  ':with-ignored-windows
				  move-snap-ignored-windows
				  ':windows-to-ignore (list w)
				  ':include-root t))))
       (move-resize-last-outline nil)
       (move-resize-moving-edges nil)
       (frame-draw-mutex (not (eq move-resize-mode 'opaque)))
       (frame-state-mutex 'clicked)
       server-grabbed)
    (unless (eq move-resize-mode 'opaque)
      ;; prevent any other programs drawing on the display
      (grab-server)
      (setq server-grabbed t))
    (unwind-protect
	(progn
	  (allow-events 'async-pointer)
	  ;; ensure that we catch _all_ mouse events
	  (when (grab-pointer w (get-cursor 'hand2))
	    (unwind-protect
		(when (grab-keyboard w)
		  (unwind-protect
		      (progn
			(unless (eq move-resize-mode 'opaque)
			  (setq move-resize-last-outline
				(list move-resize-mode
				      move-resize-x move-resize-y
				      (+ move-resize-width
					 (car move-resize-frame))
				      (+ move-resize-height
					 (cdr move-resize-frame))))
			  (apply 'draw-window-outline
				 move-resize-last-outline))
			(when (eq move-resize-function 'resize)
			  (move-resize-infer-anchor))
			(catch 'move-resize-done
			  (when from-motion-event
			    (move-resize-motion))
			  (recursive-edit)))
		    (ungrab-keyboard)))
	      (ungrab-pointer))))
      (when server-grabbed
	(ungrab-server))
      (show-message nil))))

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
      (apply 'erase-window-outline move-resize-last-outline))
    (cond ((eq move-resize-function 'move)
	   (setq move-resize-x (+ move-resize-old-x
				  (- ptr-x move-resize-old-ptr-x)))
	   (setq move-resize-y (+ move-resize-old-y
				  (- ptr-y move-resize-old-ptr-y)))
	   (when move-show-position
	     (show-message (format nil "%+d%+d" move-resize-x move-resize-y))))
	  ((eq move-resize-function 'resize)
	   (let
	       ((x-base (or (cdr (or (assq 'base-width move-resize-hints)
				     (assq 'min-width move-resize-hints))) 1))
		(x-inc (or (cdr (assq 'width-inc move-resize-hints)) 1))
		(y-base (or (cdr (or (assq 'base-height move-resize-hints)
				     (assq 'min-height move-resize-hints))) 1))
		(y-inc (or (cdr (assq 'height-inc move-resize-hints)) 1)))
	     (cond
	      ((memq 'right move-resize-moving-edges)
	       (setq move-resize-width
		     (move-resize-roundup
		      (+ move-resize-old-width
			 (- ptr-x move-resize-old-ptr-x)) x-inc x-base)))
	      ((memq 'left move-resize-moving-edges)
	       (setq move-resize-width
		     (move-resize-roundup
		      (+ move-resize-old-width
			 (- move-resize-old-ptr-x ptr-x)) x-inc x-base))
	       (setq move-resize-x (- move-resize-old-x
				      (- move-resize-width
					 move-resize-old-width)))))
	     (cond
	      ((memq 'bottom move-resize-moving-edges)
	       (setq move-resize-height
		     (move-resize-roundup
		      (+ move-resize-old-height
			 (- ptr-y move-resize-old-ptr-y)) y-inc y-base)))
	    ((memq 'top move-resize-moving-edges)
	     (setq move-resize-height
		   (move-resize-roundup
		    (+ move-resize-old-height
		       (- move-resize-old-ptr-y ptr-y)) y-inc y-base))
	     (setq move-resize-y (- move-resize-old-y
				    (- move-resize-height
				       move-resize-old-height)))))
	     (when resize-show-dimensions
	       (show-message (format nil "%dx%d"
				     (/ (- move-resize-width
					   x-base) x-inc)
				     (/ (- move-resize-height
					   y-base) y-inc)))))))
    (if (eq move-resize-mode 'opaque)
	(move-resize-apply)
      (if (and (eq move-resize-function 'move) move-snap-edges)
	  (let
	      ((coords (snap-window-position-to-edges
			move-resize-window
			(cons move-resize-x move-resize-y)
			move-snap-epsilon move-resize-edges)))
	    (setq move-resize-last-outline
		  (list move-resize-mode (car coords) (cdr coords)
			(+ move-resize-width (car move-resize-frame))
			(+ move-resize-height (cdr move-resize-frame)))))
	(setq move-resize-last-outline
	      (list move-resize-mode move-resize-x move-resize-y
		    (+ move-resize-width (car move-resize-frame))
		    (+ move-resize-height (cdr move-resize-frame)))))
      (apply 'draw-window-outline move-resize-last-outline))))

;; called when the move/resize finished (i.e. button-release event)
(defun move-resize-finished ()
  (interactive)
  (unless (eq move-resize-mode 'opaque)
    (apply 'erase-window-outline move-resize-last-outline))
  (move-resize-apply)
  (throw 'move-resize-done t))

(defun move-resize-cancel ()
  (interactive)
  (if (eq move-resize-mode 'opaque)
      (progn
	(move-window-to move-resize-window move-resize-old-x move-resize-old-y)
	(resize-window-to
	 move-resize-window move-resize-old-width move-resize-old-height))
    (apply 'erase-window-outline move-resize-last-outline))
  (throw 'move-resize-done nil))

;; commit the current state of the move or resize
(defun move-resize-apply ()
  (if (or (eq move-resize-function 'resize) (not move-snap-edges))
      (unless (and (= move-resize-old-x move-resize-x)
		   (= move-resize-old-y move-resize-y))
	(move-window-to move-resize-window move-resize-x move-resize-y))
    (let
	((coords (snap-window-position-to-edges
		  move-resize-window
		  (cons move-resize-x move-resize-y)
		  move-snap-epsilon move-resize-edges)))
      (move-window-to move-resize-window (car coords) (cdr coords))))
  (unless (and (= move-resize-old-width move-resize-width)
	       (= move-resize-old-height move-resize-height))
    (resize-window-to move-resize-window
		      move-resize-width move-resize-height)))

;; called when moving, tries to decide which edges to move, which to stick
(defun move-resize-infer-anchor ()
  (cond ((<= (- move-resize-old-ptr-x move-resize-old-x)
	    (/ move-resize-old-width 3))
	 (setq move-resize-moving-edges
	       (cons 'left move-resize-moving-edges)))
	((>= (- move-resize-old-ptr-x move-resize-old-x)
	     (* (/ move-resize-old-width 3) 2))
	 (setq move-resize-moving-edges
	       (cons 'right move-resize-moving-edges))))
  (cond ((<= (- move-resize-old-ptr-y move-resize-old-y)
	    (/ move-resize-old-height 3))
	 (setq move-resize-moving-edges
	       (cons 'top move-resize-moving-edges)))
	((>= (- move-resize-old-ptr-y move-resize-old-y)
	     (* (/ move-resize-old-height 3) 2))
	 (setq move-resize-moving-edges
	       (cons 'bottom move-resize-moving-edges))))
  (when (null move-resize-moving-edges)
    (setq move-resize-moving-edges '(bottom right))))


;; hook functions

(defun move-resize-lost-window (w)
  (when (eq move-resize-window w)
    (move-resize-finished)))

(add-hook 'unmap-notify-hook 'move-resize-lost-window t)
(add-hook 'destroy-notify-hook 'move-resize-lost-window t)


;; Entry points

;;;###autoload
(defun move-window-interactively (w)
  "Move the window interactively using the mouse."
  (interactive "%W")
  (move-resize-window w 'move))

;;;###autoload
(defun resize-window-interactively (w)
  "Resize the window interactively using the mouse."
  (interactive "%W")
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
