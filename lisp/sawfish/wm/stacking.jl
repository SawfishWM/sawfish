;; stacking.jl -- window stacking
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

(provide 'stacking)

;; Commentary:

;; Each window will have a `depth' property--an integer, zero
;; represents the level of normal windows, negative for windows below
;; this level, and positive for windows above the normal level

(defcustom transients-above 'parents
  "Keep transient windows stacked above: \\w"
  :group misc
  :type (choice all parents none)
  :user-level expert)


;; constraint mechanics (predicates actually..)

(define (stacking-constraint:layer w)
  (let ((depth (window-get w 'depth)))
    (lambda (above below)
      ;; constraints are met locally in ABOVE and BELOW, so just
      ;; test against windows adjacent to W
      (and (or (null above)
	       (<= depth (window-get (car above) 'depth)))
	   (or (null below)
	       (>= depth (window-get (car below) 'depth)))))))

(define (stacking-constraint:transients-above-parent w)
  (let ((parent (and (window-transient-p w)
		     (get-window-by-id (window-transient-p w))))
	(children (delete-if-not (lambda (x)
				   (eql (window-transient-p x) (window-id w)))
				 (managed-windows))))
    (lambda (above below)
      (and (or (null parent) (not (memq parent above)))
	   (or (null children)
	       (let loop ((rest children))
		 (cond ((null rest) t)
		       ((memq (car rest) below) nil)
		       (t (loop (cdr rest))))))))))

(define (stacking-constraint:transients-above-all w)
  (if (window-transient-p w)
      ;; ensure there are no normal windows above W
      (lambda (above below)
	(let loop ((rest above))
	  (cond ((null rest) t)
		((not (window-transient-p (car rest))) nil)
		(t (loop (cdr rest))))))
    ;; ensure no transients below W
    (lambda (above below)
      (let loop ((rest below))
	(cond ((null rest) t)
	      ((window-transient-p (car rest)) nil)
	      (t (loop (cdr rest))))))))

(define (combine-constraints constraints)
  "Combine the list of secondary constraint functions into a single
function (using logical `and' combinator)."
  (lambda (above below)
    (let loop ((rest constraints))
      (cond ((null rest) t)
	    ((not ((car rest) above below)) nil)
	    (t (loop (cdr rest)))))))

(defvar basic-stacking-constraints (list stacking-constraint:layer)
  "List of stacking constraint functions to adhere to (excluding transient
stacking modes controlled by `transients-above' option).")

(define (make-constraint w)
  "Return the constraint function to use when stacking window W, a function of
two arguments, the list of windows above W (from bottom to top) and the
list of windows below W (from top to bottom). If the constraint holds
for a particular stacking configuration, it will return non-`nil'."
  (combine-constraints
   (mapcar (lambda (c) (c w))
	   (case transients-above
	     ((parents)
	      (cons stacking-constraint:transients-above-parent
		    basic-stacking-constraints))
	     ((all)
	      (cons stacking-constraint:transients-above-all
		    basic-stacking-constraints))
	     (t basic-stacking-constraints)))))


;; utilities

;; returns (BEFORE . AFTER), modifies LST
(define (break-window-list lst pivot)
  "Given a list of windows LST and a window PIVOT, return a cons cell
containing two lists, the windows that occur in LST before PIVOT does (in
reverse order), and the list of windows occurring in LST after PIVOT.

LST is destructively modified by this procedure."
  (let loop ((rest lst)
	     (before '()))
    (cond ((null rest)
	   (cons before rest))
	  ((eq (car rest) pivot)
	   (cons before (cdr rest)))
	  (t
	   (let ((next (cdr rest)))
	     (rplacd rest before)
	     (loop next rest))))))

(define (stack-rotate-upwards cell)
  "Given a cons cell containing two lists of windows `(ABOVE . BELOW)',
rotate the lists so that the first element of BELOW becomes the first
element of ABOVE. The lists are modified. Returns `nil' if BELOW is
the empty list."
  (if (null (car cell))
      nil
    (let ((next (car cell)))
      (rplaca cell (cdar cell))
      (rplacd next (cdr cell))
      (rplacd cell next)
      cell)))

(define (stack-rotate-downwards cell)
  "Given a cons cell containing two lists of windows `(ABOVE . BELOW)',
rotate the lists so that the first element of ABOVE becomes the first
element of BELOW. The lists are modified. Returns `nil' if ABOVE is
the empty list."
  (if (null (cdr cell))
      nil
    (let ((next (cdr cell)))
      (rplacd cell (cddr cell))
      (rplacd next (car cell))
      (rplaca cell next)
      cell)))

(define (mapped-stacking-order)
  (delete-if-not window-mapped-p (stacking-order)))


;; stacking functions

(defun raise-window (w)
  "Raise the window to its highest allowed position in the stacking order."
  (interactive "%W")
  ;; work downwards from top
  (let ((constraint (make-constraint w))
	(stack (cons '() (delq w (mapped-stacking-order)))))
    (let loop ()
      (cond ((constraint (car stack) (cdr stack))
	     (if (car stack)
		 (x-lower-window w (car (car stack)))
	       (x-raise-window w (car (cdr stack)))))
	    ((null (cdr stack))
	     ;; no position
	     nil)
	    (t
	     (stack-rotate-downwards stack)
	     (loop))))))

(defun lower-window (w)
  "Lower the window to its lowest allowed position in the stacking order."
  (interactive "%W")
  (let ((constraint (make-constraint w))
	(stack (cons (nreverse (delq w (mapped-stacking-order))) '())))
    ;; work upwards from bottom
    (let loop ()
      (cond ((constraint (car stack) (cdr stack))
	     ;; found the lowest position
	     (if (cdr stack)
		 (x-raise-window w (car (cdr stack)))
	       (x-lower-window w (car (car stack)))))
	    ((null (car stack))
	     ;; no possible position..
	     nil)
	    (t
	     (stack-rotate-upwards stack)
	     (loop))))))

(defun stack-window-above (above below)
  "Change the stacking of window ABOVE so that it is as closely above window
BELOW as possible."
  (let ((constraint (make-constraint above))
	(stack (break-window-list (delq above (mapped-stacking-order)) below)))
    (rplacd stack (cons below (cdr stack)))
    (let loop ()
      (cond ((constraint (car stack) (cdr stack))
	     ;; found a suitable position
	     (if (car stack)
		 (x-lower-window above (car (car stack)))
	       (x-raise-window above (car (cdr stack)))))
	    ((null (car stack))
	     ;; reached the top
	     nil)
	    (t
	     (stack-rotate-upwards stack)
	     (loop))))))

(define (stack-window-below below above)
  "Change the stacking of window BELOW so that it is as closely below window
ABOVE as possible."
  (let ((constraint (make-constraint below))
	(stack (break-window-list (delq below (mapped-stacking-order)) above)))
    (rplaca stack (cons above (car stack)))
    (let loop ()
      (cond ((constraint (car stack) (cdr stack))
	     (if (cdr stack)
		 (x-raise-window below (car (cdr stack)))
	       (x-lower-window below (car (car stack)))))
	    ((null (cdr stack))
	     ;; reached the bottom
	     nil)
	    (t
	     (stack-rotate-downwards stack)
	     (loop))))))

(define (restack-window w)
  "Assuming that the current stacking order is in a consistent state
except, possibly, for the position of window W, restore the consistent
state including window W. This is achieved by raising or lowering
window W as appropriate."
  (let ((constraint (make-constraint w))
	(stack (break-window-list (mapped-stacking-order) w)))
    (unless (constraint (car stack) (cdr stack))
      (if (< (length (car stack)) (length (cdr stack)))
	  (raise-window w)
	(lower-window w)))))

(define (stacking-order-by-depth depth)
  "Return a list of windows containing only those in depth DEPTH, in the order
they are stacked within the layer (top to bottom)."
  (let
      ((order (stacking-order)))
    (delete-if (lambda (x)
		 (/= (window-get x 'depth) depth)) order)))

(define (set-window-depth w depth)
  "Set the stacking depth of window W to DEPTH."
  (let
      ((old (window-get w 'depth)))
    (window-put w 'depth depth)
    (cond ((> old depth)
           ;; window's going downwards
           (raise-window w))
          ((< old depth)
           ;; window's going upwards
           (lower-window w)))
    (call-window-hook 'window-depth-change-hook w (list depth))
    (call-window-hook 'window-state-change-hook w (list '(stacking)))))

(define (window-on-top-p w)
  "Return t if window W is as high as it can legally go in the stacking order."
  (let* ((constraint (make-constraint w))
	 (order (delete-if-not
		 ;; XXX doesn't handle viewports..
		 (let ((space (nearest-workspace-with-window
			       w current-workspace)))
		   (lambda (x)
		     (window-appears-in-workspace-p x space)))
		 (mapped-stacking-order)))
	 (old-posn (- (length order) (length (memq w order))))
	 (stack (cons '() (delq w order))))
    (let loop ()
      (if (or (constraint (car stack) (cdr stack))
	      (null (cdr stack)))
	  ;; found highest position
	  (= (length (car stack)) old-posn)
	(stack-rotate-downwards stack)
	(loop)))))

(defun raise-lower-window (w)
  "If the window is at its highest possible position, then lower it to its
lowest possible position. Otherwise raise it as far as allowed."
  (interactive "%W")
  (if (or (eq (window-visibility w) 'unobscured)
	  (window-on-top-p w))
      (lower-window w)
    (raise-window w)))

(defun lower-window-depth (w)
  "Put the window in the stacking level beneath its current level."
  (interactive "%W")
  (set-window-depth w (1- (window-get w 'depth))))

(defun raise-window-depth (w)
  "Put the window in the stacking level above its current level."
  (interactive "%W")
  (set-window-depth w (1+ (window-get w 'depth))))


;; stacking groups of windows

(defun raise-windows (w order)
  (mapc raise-window order)
  (raise-window w))

(defun lower-windows (w order)
  (mapc lower-window (nreverse order))
  (lower-window w))

(defun raise-lower-windows (w order)
  (if (or (eq (window-visibility w) 'unobscured)
	  (and (window-on-top-p (car order))
	       ;; look for the group as a block.. this is a heuristic
	       (let loop ((rest (memq (car order) (stacking-order))))
		 (cond ((null rest) nil)
		       ((eq (car rest) w) t)
		       ((memq (car rest) order) (loop (cdr rest)))
		       (t nil)))))
      (lower-windows w order)
    (raise-windows w order)))


;; hooks

;; Called from the add-window-hook
(define (stacking-add-window w)
  (unless (window-get w 'depth)
    (window-put w 'depth 0)))

(add-hook 'add-window-hook stacking-add-window t)
(add-hook 'after-add-window-hook restack-window t)
(add-hook 'map-notify-hook restack-window t)

(sm-add-saved-properties 'depth)
(add-swapped-properties 'depth)
