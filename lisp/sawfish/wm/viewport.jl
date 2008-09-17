;; viewport.jl -- virtual desktops
;; $Id: viewport.jl,v 1.46 2002/04/23 03:44:18 jsh Exp $

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

(define-structure sawfish.wm.viewport

    (export set-viewport
	    screen-viewport
	    set-screen-viewport
	    select-workspace-and-viewport
	    move-viewport
	    move-viewport-to-window
	    window-outside-workspace-p
	    window-outside-viewport-p
	    move-window-to-current-viewport
	    set-window-viewport
	    move-window-viewport
	    window-viewport
	    window-absolute-position
	    set-number-of-viewports)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.commands
	  sawfish.wm.workspace
	  sawfish.wm.custom
	  sawfish.wm.session.init)

  ;; Virtual workspaces are implemented by moving windows in and out of
  ;; the screen dimensions. E.g. moving to the left moves all windows one
  ;; screen-width to the right. 

(defcustom viewport-dimensions '(1 . 1)
  "Number of columns and rows in each virtual workspace: \\w"
  :group workspace
  :type (pair (number 1) (number 1))
  :after-set (lambda () (viewport-size-changed)))

  (defcustom uniconify-to-current-viewport t
    "Windows uniconify to the current viewport."
    :type boolean
    :group min-max)                     ; iconify)
  
  (defcustom scroll-viewport-steps 5
    "Number of steps in which to scroll between viewports (less steps = faster scrolling)."
    :group workspace
    :type number
    :range (1 . 100))


;;; raw viewport handling

  (defvar viewport-x-offset 0)
  (defvar viewport-y-offset 0)

  (define (warp-viewport x y)
    "Change view by incrementing the coordinates of the x,y position."
    ;; move W to its new position
    (define (move-window w)
      (unless (window-get w 'sticky-viewport)
	(let ((pos (window-position w)))
	  (move-window-to w (- (+ (car pos) viewport-x-offset) x)
			  (- (+ (cdr pos) viewport-y-offset) y)))))

    (unless (and (= viewport-x-offset x) (= viewport-y-offset y))
      (let loop ((rest (stacking-order))
		 (inside '())
		 (outside '()))
	(cond ((null rest)
	       (with-server-grabbed
		;; First move all windows not on the old viewport, and
		;; move in top-to-bottom order..
		(mapc move-window (nreverse outside))
		;; ..then move away the windows on the old viewport,
		;; in bottom-to-top order
		(mapc move-window inside)))

	      ((window-outside-viewport-p (car rest))
	       (loop (cdr rest) inside (cons (car rest) outside)))

	      (t (loop (cdr rest) (cons (car rest) inside) outside))))

      (setq viewport-x-offset x)
      (setq viewport-y-offset y)
      (call-hook 'viewport-moved-hook)))

  (define (set-viewport x y)
    "Scroll viewport view by incrementing the coordinates of the x,y position.
The scrolling makes a number of increments equal to `scroll-viewport-steps'."
    (let ((step-count scroll-viewport-steps)
          (xstep (quotient (- x viewport-x-offset) scroll-viewport-steps))
          (ystep (quotient (- y viewport-y-offset) scroll-viewport-steps)))
      (when (= xstep ystep 0)
        (setq step-count 0))
      (while (> step-count 1)
        (warp-viewport (+ viewport-x-offset xstep) (+ viewport-y-offset ystep))
        (setq step-count (1- step-count)))
      (warp-viewport x y)))

  (define (viewport-before-exiting)
    (set-screen-viewport 0 0))

  (add-hook 'before-exit-hook viewport-before-exiting t)


;; screen sized viewport handling

  (define (screen-viewport)
    (cons (quotient viewport-x-offset (screen-width))
	  (quotient viewport-y-offset (screen-height))))

  ;; returns t if it actually moved the viewport
  (define (set-screen-viewport col row)
    (when (and (>= col 0) (< col (car viewport-dimensions))
	       (>= row 0) (< row (cdr viewport-dimensions)))
      (set-viewport (* col (screen-width))
		    (* row (screen-height)))
      t))

  (define (select-workspace-and-viewport space col row)
    (select-workspace space nil (lambda ()
				  (set-screen-viewport col row))))
  
  ;; returns t if it actually moved the viewport
  (define (move-viewport right down)
    (let ((port (screen-viewport)))
      (set-screen-viewport (+ (car port) right)
			   (+ (cdr port) down))))

  (define (move-viewport-to-window window)
    (when (window-outside-viewport-p window)
      (let ((pos (window-position window)))
	(rplaca pos (+ (car pos) viewport-x-offset))
	(rplacd pos (+ (cdr pos) viewport-y-offset))
	(set-screen-viewport (quotient (car pos) (screen-width))
			     (quotient (cdr pos) (screen-height))))))

  (define (window-outside-workspace-p window)
    (let ((pos (window-position window))
	  (dims (window-frame-dimensions window))
	  (left (- viewport-x-offset))
	  (right (- (* (car viewport-dimensions) (screen-width))
		    viewport-x-offset))
	  (top (- viewport-y-offset))
	  (bottom (- (* (cdr viewport-dimensions) (screen-height))
		     viewport-y-offset)))
      (or (>= (car pos) right)
	  (>= (cdr pos) bottom)
	  (<= (+ (car pos) (car dims)) left)
	  (<= (+ (cdr pos) (cdr dims)) top))))

  (define (window-outside-viewport-p window)
    (let ((pos (window-position window))
	  (dims (window-frame-dimensions window)))
      (or (<= (+ (car pos) (car dims)) 0)
	  (<= (+ (cdr pos) (cdr dims)) 0)
	  (>= (car pos) (screen-width))
	  (>= (cdr pos) (screen-height)))))

  (define (move-window-to-current-viewport window)
    (when (and (window-outside-viewport-p window)
	       (not (window-get window 'sticky-viewport)))
      (let ((pos (window-position window)))
	(move-window-to window (mod (car pos) (screen-width))
			(mod (cdr pos) (screen-height))))))

  (define (set-window-viewport window col row)
    (unless (window-get window 'sticky-viewport)
      (let ((pos (window-position window)))
	(setq col (max 0 (min (1- (car viewport-dimensions)) col)))
	(setq row (max 0 (min (1- (cdr viewport-dimensions)) row)))
	(setq col (+ (* col (screen-width)) (mod (car pos) (screen-width))))
	(setq row (+ (* row (screen-height)) (mod (cdr pos) (screen-height))))
	(move-window-to
	 window (- col viewport-x-offset) (- row viewport-y-offset)))))

  (define (move-window-viewport window col row)
    (let ((pos (window-position window)))
      (set-window-viewport window
			   (+ (quotient (+ (car pos) viewport-x-offset)
					(screen-width)) col)
			   (+ (quotient (+ (cdr pos) viewport-y-offset)
					(screen-height)) row))))

  (define (window-viewport w)
    (let ((position (window-position w)))
      (cons (quotient (+ (car position) viewport-x-offset) (screen-width))
	    (quotient (+ (cdr position) viewport-y-offset) (screen-height)))))

  (define (window-absolute-position w)
    (let ((position (window-position w)))
      (if (window-outside-viewport-p w)
	  (cons (mod (+ (car position) viewport-x-offset) (screen-width))
		(mod (+ (cdr position) viewport-y-offset) (screen-height)))
	position)))

  (define (viewport-size-changed)
    (let ((port (screen-viewport)))
      (set-screen-viewport (min (car port) (1- (car viewport-dimensions)))
			   (min (cdr port) (1- (cdr viewport-dimensions))))
      (map-windows (lambda (w)
		     (when (window-outside-workspace-p w)
		       (move-window-to-current-viewport w))))
      (call-hook 'viewport-resized-hook)))

  (define (set-number-of-viewports width height)
    (setq viewport-dimensions (cons width height))
    (viewport-size-changed))


;; commands

  (define (activate-viewport x y)
    "Select the specified viewport."
    (set-screen-viewport (1- x) (1- y)))

  (define-command 'activate-viewport activate-viewport
    #:spec "NX:\nNY:"
    #:type `(and (labelled ,(_ "Column:") (number 1))
		 (labelled ,(_ "Row:") (number 1)))
    #:class 'default)

  (define (activate-viewport-column x)
    "Select the specified viewport column."
    (set-screen-viewport (1- x) (cdr (screen-viewport))))

  (define-command 'activate-viewport-column activate-viewport-column
    #:spec "NX:"
    #:type `(and (labelled ,(_ "Column:") (number 1)))
    #:class 'default)

  (define (activate-viewport-row y)
    "Select the specified viewport row."
    (set-screen-viewport (car (screen-viewport)) (1- y)))

  (define-command 'activate-viewport-row activate-viewport-row
    #:spec "NY:"
    #:type `(and (labelled ,(_ "Row:") (number 1)))
    #:class 'default)

  (define (move-window-to-viewport x y)
    "Move the current window to the specified viewport."
    (move-window-viewport (current-event-window) (1- x) (1- y)))

  (define-command 'move-window-to-viewport move-window-to-viewport
    #:spec "NX:\nNY:"
    #:type '(and (labelled "X:" (number 1)) (labelled "Y:" (number 1)))
    #:class 'default)

  (define (move-viewport-right)
    "Move the viewport one screen to the right."
    (move-viewport 1 0))

  (define (move-viewport-left)
    "Move the viewport one screen to the left."
    (move-viewport -1 0))

  (define (move-viewport-down)
    "Move the viewport one screen down."
    (move-viewport 0 1))

  (define (move-viewport-up)
    "Move the viewport one screen up."
    (move-viewport 0 -1))

  ;; Moves the window by the specified offsets and then flips to the
  ;; viewport that is relative those offsets to the current viewport.
  (define (move-window-to-viewport-and-move-viewport window col row)
    (require 'sawfish.wm.util.stacking)
    (let ((sticky-viewport (window-get window 'sticky-viewport)))
      (window-put window 'sticky-viewport t)
      (with-server-grabbed
       (raise-window* window)
       (move-viewport col row)
       (set-input-focus window))
      (unless sticky-viewport
	(window-put window 'sticky-viewport nil))))

  (define (move-window-left w)
    "Move the window to the viewport on the left, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w -1 0))

  (define (move-window-right w)
    "Move the window to the viewport on the right, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w 1 0))

  (define (move-window-down w)
    "Move the window to the viewport below, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w 0 1))

  (define (move-window-up w)
    "Move the window to the viewport above, and switch to that viewport."
    (move-window-to-viewport-and-move-viewport w 0 -1))

  (define-command 'move-viewport-right move-viewport-right)
  (define-command 'move-viewport-left move-viewport-left)
  (define-command 'move-viewport-up move-viewport-up)
  (define-command 'move-viewport-down move-viewport-down)
  (define-command 'move-window-right move-window-right #:spec "%W")
  (define-command 'move-window-left move-window-left #:spec "%W")
  (define-command 'move-window-up move-window-up #:spec "%W")
  (define-command 'move-window-down move-window-down #:spec "%W")


;;; session management, config

  (define (viewport-saved-state w)
    (let ((position (window-position w)))
      (when (window-get w 'sticky-viewport)
	(rplaca position (mod (car position) (screen-width)))
	(rplacd position (mod (cdr position) (screen-height))))
      `((position . ,(window-absolute-position w))
	(viewport . ,(window-viewport w)))))

  (define (viewport-load-state w alist)
    (let ((position (cdr (assq 'position alist)))
	  (viewport (cdr (assq 'viewport alist))))
      (when position
	(if (or (not viewport) (window-get w 'sticky-viewport))
	    (move-window-to w (car position) (cdr position))
	  (move-window-to w (+ (* (car viewport) (screen-width))
			       (car position)
			       (- viewport-x-offset))
			  (+ (* (cdr viewport) (screen-height))
			     (cdr position)
			     (- viewport-y-offset)))
	  (when (window-outside-workspace-p w)
	    (move-window-to-current-viewport w)))
	(window-put w 'placed t))))
			     
  (sm-add-saved-properties 'sticky-viewport)
  (add-hook 'sm-window-save-functions viewport-saved-state)
  (add-hook 'sm-restore-window-hook viewport-load-state)

  (define (viewport-window-uniconified w)
    (when uniconify-to-current-viewport
      (move-window-to-current-viewport w)))

  (add-hook 'uniconify-window-hook viewport-window-uniconified))
