;; shade-stack.jl -- maintain a stack of shaded windows as iconify substitute

;; Copyright (C) 1999 Luke Gorrie <luke@vegetable.org>
;; Modifications by: 	The Glyph <glyph@twistedmatrix.com>
;;			Christopher Roy Bratusek <nano@jpberlin.de>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.ext.shade-stack

  (export toggle-window-shade-stacked
	  shade-stack-window
	  shade-unstack-window
	  window-shade-stacked-p)

  (open rep
	rep.system
	sawfish.wm.misc
	sawfish.wm.custom
	sawfish.wm.windows
	sawfish.wm.commands.move-resize
	sawfish.wm.state.shading
	sawfish.wm.stacking
	sawfish.wm.workspace
	sawfish.wm.frames)

  (define-structure-alias shade-stack sawfish.wm.ext.shade-stack)

  (defvar shade-stacks '())
  (defcustom shade-stack-columns 3
    "Number of columns for the shade stack."
    :group misc
    :type (range 1 4))
  ;; TODO: automatically calculate
  (defvar shade-stack-item-height 22)

  ;;; API commands

  (define (toggle-window-shade-stacked window)
    "Toggle shade-stacking of a window"
    (if (window-shade-stacked-p window (get-shade-stack))
	(shade-unstack-window window)
      (shade-stack-window window)))

  (define (shade-stack-window window)
    "Shade a window and stack it at the top of the screen"
    (if (window-shade-stacked-p window (get-shade-stack))
	t
      (let ((shaded-position (shade-stack-insert window)))
	(lower-window window)
	(shade-window window)
	(move-window-to window (car shaded-position) (cdr shaded-position))
	(resize-window-to window (- (quotient (screen-width) shade-stack-columns) 10)
			  (cdr (window-dimensions window))))))

  (define (shade-unstack-window window)
    "Unshade a window from the stack"
    (let* ((original-configuration (shade-stack-extract window))
	  (original-position (car original-configuration))
	  (original-dimensions (car (cdr original-configuration))))
      (if (window-get window 'shade-hover-unshaded)
	  (progn
	    (window-put window 'shaded nil)
	    (window-put window 'hide-client nil)
	    (window-put window 'shade-hover-unshaded nil)
	    (reframe-window window)
	    (call-window-hook 'unshade-window-hook window)
	    (call-window-hook 'window-state-change-hook
			      window (list '(shaded))))
	(unshade-window window))
      (raise-window window)
      (move-window-to window (car original-position) (cdr original-position))
      (resize-window-to window
			(car original-dimensions)
			(cdr original-dimensions))))

  ;;; Main stacking code

  (define (shade-stack-insert window)
    "Put a window into the shaded stack registry. Return its slot number."
      (let ((insert-result
	    (do-shade-stack-insert window
				    (remove-leading-empty (get-shade-stack)))))
	(set-shade-stack (cdr insert-result))
	(let ((idx (car insert-result)))
	  (cons (* (quotient (screen-width) shade-stack-columns)
		  (mod idx shade-stack-columns))
		(* shade-stack-item-height (quotient idx shade-stack-columns))))))

  (define (do-shade-stack-insert window stack)
    (let ((empty-slot (last-empty-slot-idx stack 0 nil)))
      (if (null empty-slot)
	  (cons (length stack)
		(cons (win-info window) stack))
	(cons (- (length stack) 1 empty-slot)
	      (set-elem stack empty-slot (win-info window))))))

  (define (shade-stack-extract window)
    "Take a window off the shaded stack registry. Return its original properties"
    (let ((extract-result (do-shade-stack-extract window (get-shade-stack) '())))
      (set-shade-stack (cdr extract-result))
      (car extract-result)))

  (define (do-shade-stack-extract window stack accu)
    (if (null stack)
	(error "window not on shade stack")
      (let ((elem (car stack)))
	(if (eq (car elem) window)
	    (cons (cdr elem)
		  (append (reverse (cons 'empty accu)) (cdr stack)))
	  (do-shade-stack-extract window
				  (cdr stack)
				  (cons (car stack) accu))))))

  (define (window-shade-stacked-p window stack)
    "Is window already on stack?"
    (if (null stack)
	nil
      (if (and (consp (car stack))
	      (eq (car (car stack)) window))
	  t
	(window-shade-stacked-p window (cdr stack)))))

  (define (get-shade-stack)
    "Get the shade stack for the current viewport / workspace"
    (let ((stack (assoc (make-key) shade-stacks)))
      (if stack
	  (cdr stack)
	nil)))

  (define (set-shade-stack stack)
    "Set the shade stack for the current viewport / workspace"
    (let ((key (make-key)))
      (setq shade-stacks (cons (cons key stack)
			      (filter (lambda (x) (not (equal (car x) key)))
				      shade-stacks)))))

  ;;; Utility functions

  (define (make-key)
    "Make a key to uniquely identify this viewport / workspace"
    (list current-workspace viewport-x-offset viewport-y-offset))

  (define (win-info window)
    "Capture pre-stacking window properties"
    (cons window (list (window-position window) (window-dimensions window))))

  (define (last-empty-slot-idx stack idx prev-idx)
    "Return the index of an empty slot, or nil if none exists"
    (if (null stack)
	prev-idx
      (let ((curr-idx (if (eq (car stack) 'empty)
			  idx
			prev-idx)))
	(last-empty-slot-idx (cdr stack) (+ idx 1) curr-idx))))

  (define (set-elem lst idx val)
    "Set element idx of lst to val"
    (if (= idx 0)
	(cons val (cdr lst))
      (cons (car lst) (set-elem (cdr lst) (- idx 1) val))))

  (define (remove-leading-empty lst)
    "Strip leading empty slots"
    (if (eq (car lst) 'empty)
	(remove-leading-empty (cdr lst))
      lst))

  (define (cleanup-after-window w)
    "If a shade-stacked window is destroyed, free up its slot"
    (setq shade-stacks
	  (mapcar (lambda (x) (cleanup-environment w x)) shade-stacks)))

  (define (cleanup-environment w stack-alist)
    (let ((key (car stack-alist))
	  (stack (cdr stack-alist)))
      (cons key
	    (mapcar (lambda (x)
		      (if (equal (car x) w)
			  'empty
			x))
		    stack))))

  (add-hook 'destroy-notify-hook cleanup-after-window))
