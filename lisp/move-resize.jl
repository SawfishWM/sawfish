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

(require 'maximize)
(eval-when-compile (require 'auto-raise))	;we bind to disable-auto-raise
(provide 'move-resize)

;; todo:
;;  * obey the aspect ratio size hints

;;;###autoload (custom-add-required 'move-resize)

(defcustom resize-by-frame-class t
  "Choose resized window edges by border-part, not by mouse position."
  :type boolean
  :group (move advanced))

(defcustom resize-add-edges t
  "Allow other window edges to be grabbed while resizing interactively."
  :type boolean
  :group (move advanced))

(defcustom move-outline-mode 'opaque
  "The method of drawing windows being moved interactively."
  :type symbol
  :options (opaque box)
  :group move)

(defcustom resize-outline-mode 'opaque
  "The method of drawing windows being resized interactively."
  :type symbol
  :options (opaque box)
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
  :group (move advanced)
  :type boolean)

(defcustom move-snap-epsilon 8
  "Proximity in pixels before snapping to a window edge."
  :group (move advanced)
  :type number
  :range (0 . 64))

(defcustom move-snap-ignored-windows nil
  "Snap to otherwise-ignored windows."
  :group (move advanced)
  :type boolean)

(defcustom move-lock-when-maximized t
  "Lock window geometry while the window is maximized."
  :type boolean
  :group maximize)

(defcustom move-resize-inhibit-configure nil
  "Only redraw window contents after the move/resize has finished."
  :type boolean
  :group (move advanced))

(defvar move-resize-map (bind-keys (make-keymap)
			  "Any-Off" 'move-resize-finished
			  "Any-Move" 'move-resize-motion
			  "Any-ESC" 'move-resize-cancel
			  "Any-RET" 'move-resize-finished
			  "Up" 'move-cursor-up
			  "Down" 'move-cursor-down
			  "Left" 'move-cursor-left
			  "Right" 'move-cursor-right
			  "S-Up" 'move-cursor-up-fine
			  "S-Down" 'move-cursor-down-fine
			  "S-Left" 'move-cursor-left-fine
			  "S-Right" 'move-cursor-right-fine))

(defvar move-resize-fp-edges-alist '((top-border top)
				     (left-border left)
				     (right-border right)
				     (bottom-border bottom)
				     (top-left-corner top left)
				     (top-right-corner top right)
				     (bottom-left-corner bottom left)
				     (bottom-right-corner bottom right)))

(defvar move-cursor-shape 'hand2)
(defvar resize-cursor-shape 'hand2)

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
(defvar move-resize-directions nil)

;; called to initiate a move or resize on window W. FUNCTION is either
;; `move' or `resize'
(defun move-resize-window (w function)
  (if (eq function 'move)
      (call-window-hook 'before-move-hook w)
    (call-window-hook 'before-resize-hook w))
  (let*
      ((from-motion-event (and (current-event)
			       (string-match "-Move$" (event-name
						       (current-event)))))
       (override-keymap move-resize-map)
       ;; don't want any complications..
       (unbound-key-hook nil)
       (disable-auto-raise t)
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
				       (query-button-press-pointer)
				     (query-pointer t))))
       (move-resize-old-ptr-y (cdr (if from-motion-event
				       (query-button-press-pointer)
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
       (move-resize-moving-edges move-resize-moving-edges)
       (move-resize-directions move-resize-directions)
       (frame-draw-mutex (not (eq move-resize-mode 'opaque)))
       (frame-state-mutex 'clicked)
       (synthetic-configure-mutex move-resize-inhibit-configure)
       server-grabbed)
    (when (and move-resize-raise-window (eq move-resize-mode 'opaque))
      ;; only raise window initially if the display will get updated
      (raise-window w))
    (unless (eq move-resize-mode 'opaque)
      ;; prevent any other programs drawing on the display
      (grab-server)
      (setq server-grabbed t))
    (unwind-protect
	(progn
	  (allow-events 'async-pointer)
	  ;; ensure that we catch _all_ mouse events
	  (when (grab-pointer
		 nil (if (eq move-resize-function 'move)
			 move-cursor-shape resize-cursor-shape))
	    (unwind-protect
		(progn
		  (grab-keyboard w)	;this may fail
		  (unless (eq move-resize-mode 'opaque)
		    (setq move-resize-last-outline
			  (list move-resize-mode
				move-resize-x move-resize-y
				(+ move-resize-width
				   (car move-resize-frame))
				(+ move-resize-height
				   (cdr move-resize-frame))))
		    (apply draw-window-outline move-resize-last-outline))
		  (if (eq move-resize-function 'resize)
			    (move-resize-infer-anchor)
		    (move-resize-infer-directions))
		  (catch 'move-resize-done
		    (when from-motion-event
		      (move-resize-motion))
		    (recursive-edit)))
	      (ungrab-keyboard)
	      (ungrab-pointer))))
      (when server-grabbed
	(ungrab-server))
      (display-message nil))
    (when (and move-resize-raise-window (not (eq move-resize-mode 'opaque)))
      (raise-window w))
    (if (eq function 'move)
	(call-window-hook 'after-move-hook w (list move-resize-directions))
      (call-window-hook
       'after-resize-hook w (list move-resize-moving-edges)))))

;; round up a window dimension X in increments of INC, with minimum
;; value BASE
(defsubst move-resize-roundup (x inc base &optional maximum)
  (min (+ base (max 0 (* (1+ (/ (1- (- x base)) inc)) inc)))
       (or maximum 65535)))

;; compute move-resize position
(defun move-resize-pos ()
  (if (and (eq move-resize-function 'move) move-snap-edges)
      (snap-window-position-to-edges move-resize-window
				     (cons move-resize-x move-resize-y)
				     move-snap-epsilon move-resize-edges)
    (cons move-resize-x move-resize-y)))

;; called each pointer motion event during move/resize
(defun move-resize-motion ()
  (interactive)
  (let
      ((ptr-x (car (query-pointer)))
       (ptr-y (cdr (query-pointer)))
       m-pos)
    (unless (eq move-resize-mode 'opaque)
      (apply erase-window-outline move-resize-last-outline))
    (cond ((eq move-resize-function 'move)
	   (when (memq 'horizontal move-resize-directions)
	     (setq move-resize-x (+ move-resize-old-x
				    (- ptr-x move-resize-old-ptr-x))))
	   (when (memq 'vertical move-resize-directions)
	     (setq move-resize-y (+ move-resize-old-y
				    (- ptr-y move-resize-old-ptr-y)))))
	  ((eq move-resize-function 'resize)
	   (let
	       ((x-base (or (cdr (or (assq 'base-width move-resize-hints)
				     (assq 'min-width move-resize-hints))) 1))
		(x-inc (or (cdr (assq 'width-inc move-resize-hints)) 1))
		(y-base (or (cdr (or (assq 'base-height move-resize-hints)
				     (assq 'min-height move-resize-hints))) 1))
		(y-inc (or (cdr (assq 'height-inc move-resize-hints)) 1))
		(x-max (cdr (assq 'max-width move-resize-hints)))
		(y-max (cdr (assq 'max-height move-resize-hints))))
	     (when resize-add-edges
	       (move-resize-add-edges ptr-x ptr-y))
	     (cond
	      ((memq 'right move-resize-moving-edges)
	       (setq move-resize-width
		     (move-resize-roundup
		      (+ move-resize-old-width
			 (- ptr-x move-resize-old-ptr-x)) x-inc x-base x-max)))
	      ((memq 'left move-resize-moving-edges)
	       (setq move-resize-width
		     (move-resize-roundup
		      (+ move-resize-old-width
			 (- move-resize-old-ptr-x ptr-x)) x-inc x-base x-max))
	       (setq move-resize-x (- move-resize-old-x
				      (- move-resize-width
					 move-resize-old-width)))))
	     (cond
	      ((memq 'bottom move-resize-moving-edges)
	       (setq move-resize-height
		     (move-resize-roundup
		      (+ move-resize-old-height
			 (- ptr-y move-resize-old-ptr-y)) y-inc y-base y-max)))
	    ((memq 'top move-resize-moving-edges)
	     (setq move-resize-height
		   (move-resize-roundup
		    (+ move-resize-old-height
		       (- move-resize-old-ptr-y ptr-y)) y-inc y-base y-max))
	     (setq move-resize-y (- move-resize-old-y
				    (- move-resize-height
				       move-resize-old-height)))))
	     (when resize-show-dimensions
	       (display-message (format nil "%dx%d"
					(/ (- move-resize-width
					      x-base) x-inc)
					(/ (- move-resize-height
					      y-base) y-inc)))))))
    (setq m-pos (move-resize-pos))
    (when (and (eq move-resize-function 'move) move-show-position)
      (display-message (format nil "%+d%+d" (car m-pos) (cdr m-pos))))
    (call-window-hook (if (eq move-resize-function 'move)
			  'while-moving-hook
			'while-resizing-hook) move-resize-window)
    (if (eq move-resize-mode 'opaque)
	(move-resize-apply m-pos)
      (let
	  ((m-dim-x (+ move-resize-width (car move-resize-frame)))
	   (m-dim-y (+ move-resize-height (cdr move-resize-frame))))
	(setq move-resize-last-outline (list move-resize-mode
					     (car m-pos) (cdr m-pos)
					     m-dim-x m-dim-y))
	(apply draw-window-outline move-resize-last-outline)))))

;; called when the move/resize finished (i.e. button-release event)
(defun move-resize-finished ()
  (interactive)
  (unless (eq move-resize-mode 'opaque)
    (apply erase-window-outline move-resize-last-outline))
  (move-resize-apply)
  (throw 'move-resize-done t))

(defun move-resize-cancel ()
  (interactive)
  (if (eq move-resize-mode 'opaque)
      (move-resize-window-to move-resize-window
			     move-resize-old-x move-resize-old-y
			     move-resize-old-width move-resize-old-height)
    (apply erase-window-outline move-resize-last-outline))
  (throw 'move-resize-done nil))

;; commit the current state of the move or resize
(defun move-resize-apply (&optional m-pos)
  (unless m-pos
    (setq m-pos (move-resize-pos)))
  (cond
   ((>= (car m-pos) (screen-width))
    (rplaca m-pos (1- (screen-width))))
   ((<= (car m-pos) (- (+ move-resize-width (car move-resize-frame))))
    (rplaca m-pos (1+ (- (+ move-resize-width (car move-resize-frame)))))))
  (cond
   ((>= (cdr m-pos) (screen-height))
    (rplacd m-pos (1- (screen-height))))
   ((<= (cdr m-pos) (- (+ move-resize-height (cdr move-resize-frame))))
    (rplacd m-pos (1+ (- (+ move-resize-height (cdr move-resize-frame)))))))
  (move-resize-window-to move-resize-window
			 (car m-pos) (cdr m-pos)
			 move-resize-width move-resize-height))

;; called when moving, tries to decide which edges to move, which to stick
(defun move-resize-infer-anchor ()
  (unless move-resize-moving-edges
    (let
	(tem)
      (if (and resize-by-frame-class
	       (clicked-frame-part)
	       (setq tem (frame-part-get (clicked-frame-part) 'class))
	       (setq tem (cdr (assq tem move-resize-fp-edges-alist))))
	  (setq move-resize-moving-edges (copy-sequence tem))
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
		     (cons 'bottom move-resize-moving-edges)))))))
  (when (null move-resize-moving-edges)
    (setq move-resize-moving-edges '(bottom right)))
  (when move-lock-when-maximized
    (when (window-maximized-vertically-p move-resize-window)
      (setq move-resize-moving-edges
	    (delq 'top (delq 'bottom move-resize-moving-edges))))
    (when (window-maximized-horizontally-p move-resize-window)
      (setq move-resize-moving-edges
	    (delq 'left (delq 'right move-resize-moving-edges))))))

(defun move-resize-add-edges (ptr-x ptr-y)
  (unless (or (and move-lock-when-maximized
		   (window-maximized-horizontally-p move-resize-window))
	      (memq 'left move-resize-moving-edges)
	      (memq 'right move-resize-moving-edges))
    (cond ((< ptr-x move-resize-x)
	   (setq move-resize-moving-edges
		 (cons 'left move-resize-moving-edges))
	   (setq move-resize-old-ptr-x move-resize-x))
	  ((> ptr-x (+ move-resize-x move-resize-width
		       (car move-resize-frame)))
	   (setq move-resize-moving-edges
		 (cons 'right move-resize-moving-edges))
	   (setq move-resize-old-ptr-x (+ move-resize-x move-resize-width
					  (car move-resize-frame))))))
  (unless (or (and move-lock-when-maximized
		   (window-maximized-vertically-p move-resize-window))
	      (memq 'top move-resize-moving-edges)
	      (memq 'bottom move-resize-moving-edges))
    (cond ((< ptr-y move-resize-y)
	   (setq move-resize-moving-edges
		 (cons 'top move-resize-moving-edges))
	   (setq move-resize-old-ptr-y move-resize-y))
	  ((> ptr-y (+ move-resize-y move-resize-height
		       (cdr move-resize-frame)))
	   (setq move-resize-moving-edges
		 (cons 'bottom move-resize-moving-edges))
	   (setq move-resize-old-ptr-y (+ move-resize-y move-resize-height
					  (cdr move-resize-frame)))))))
(defun move-resize-infer-directions ()
  (unless move-resize-directions
    (setq move-resize-directions (list 'vertical 'horizontal)))
  (when move-lock-when-maximized
    (when (window-maximized-horizontally-p move-resize-window)
      (setq move-resize-directions (delq 'horizontal move-resize-directions)))
    (when (window-maximized-vertically-p move-resize-window)
      (setq move-resize-directions (delq 'vertical move-resize-directions)))))


;; hook functions

(defun move-resize-lost-window (w)
  (when (eq move-resize-window w)
    (move-resize-finished)))

(add-hook 'unmap-notify-hook move-resize-lost-window t)
(add-hook 'destroy-notify-hook move-resize-lost-window t)


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
    (when w
      (move-window-interactively w))))

;;;###autoload
(defun resize-selected-window ()
  "Wait for the user to select a window, then interactively resize that window."
  (interactive)
  (let
      ((w (select-window)))
    (when w
      (resize-window-interactively w))))
