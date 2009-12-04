;; move-resize.jl -- interactive moving and resizing of windows

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.commands.move-resize

    (export move-window-interactively
	    resize-window-interactively
	    move-selected-window
	    resize-selected-window
	    double-window-size
	    halve-window-size
	    move-window-center)

    (open rep
	  rep.system
	  rep.regexp
	  sawfish.wm.windows
	  sawfish.wm.frames
	  sawfish.wm.misc
	  sawfish.wm.state.maximize
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.events
	  sawfish.wm.cursors
	  sawfish.wm.util.stacking
	  sawfish.wm.util.edges)

  (define-structure-alias move-resize sawfish.wm.commands.move-resize)

  ;; we bind to disable-auto-raise
  (eval-when-compile (require 'sawfish.wm.ext.auto-raise))

  ;; todo:
  ;;  * obey the aspect ratio size hints


  (defcustom move-outline-mode 'opaque
    "How windows being moved are animated"
    :type (choice opaque box cross elliptical draft)
    :group (appearance animation))

  (defcustom resize-outline-mode 'opaque
    "How windows being resized are animated"
    :type (choice opaque box cross elliptical draft)
    :group (appearance animation))

  (defcustom move-resize-raise-window nil
    "Raise windows when they are moved or resized."
    :type boolean
    :group move)

  (defcustom move-show-position nil
    "Show current position of windows while moving."
    :group move
    :type boolean)

  (defcustom resize-show-dimensions t
    "Show current dimensions of windows while resizing."
    :group move
    :type boolean)

  (defcustom resize-edge-mode 'border-grab
    "How to choose window edges when resizing."
    :type (choice region border grab border-grab)
    :group move)

  (defcustom move-snap-epsilon 12
    "Distance in pixels before window edges align with each other."
    :group move
    :type (number 0 64)
    :tooltip "When moving a window, this option lets you align one of \
its edges with an edge of another window.")

  (defvar move-snap-mode 'resistance
    "How to snap together window edges, one of `magnetism', `resistance', or
`attraction'.")

  (defvar move-snap-ignored-windows nil
    "Snap to otherwise-ignored windows.")

  (defvar move-resize-inhibit-configure nil
    "Only update window contents after it has stopped moving.")

  (defvar move-resize-map (bind-keys (make-keymap)
                                     "Any-Off1" (lambda () (finished))
                                     "Any-Off2" (lambda () (finished))
                                     "Any-Off3" (lambda () (finished))
                                     "Any-Move" (lambda () (motion))
                                     "Any-ESC" (lambda () (cancel))
                                     "Any-RET" (lambda () (finished))
                                     "Up" 'move-cursor-up
                                     "Down" 'move-cursor-down
                                     "Left" 'move-cursor-left
                                     "Right" 'move-cursor-right
                                     "S-Up" 'move-cursor-up-fine
                                     "S-Down" 'move-cursor-down-fine
                                     "S-Left" 'move-cursor-left-fine
                                     "S-Right" 'move-cursor-right-fine))

  (define fp-edges-alist '((top-border top)
			   (left-border left)
			   (right-border right)
			   (bottom-border bottom)
			   (top-left-corner top left)
			   (top-right-corner top right)
			   (bottom-left-corner bottom left)
			   (bottom-right-corner bottom right)))

  (defvar move-cursor-shape 'hand2)
  (defvar resize-cursor-shape 'crosshair)

  ;; specials, should make these fluids (external users?)
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
  (defvar move-resize-last-ptr nil)
  (defvar move-resize-snap-state nil)
  (defvar move-resize-mode nil)
  (defvar move-resize-hints nil)
  (defvar move-resize-frame nil)
  (defvar move-resize-edges nil)
  (defvar move-resize-last-outline nil)
  (defvar move-resize-moving-edges nil)
  (defvar move-resize-directions nil)

  (define (delete-unmovable-directions directions)
    (when move-lock-when-maximized
      (when (window-maximized-vertically-p move-resize-window)
	(setq directions (delq 'vertical directions)))
      (when (window-maximized-horizontally-p move-resize-window)
	(setq directions (delq 'horizontal directions))))
    directions)

  (define (delete-unresizable-edges edges)
    (when move-lock-when-maximized
      (when (window-maximized-vertically-p move-resize-window)
	(setq edges (delq 'top (delq 'bottom edges))))
      (when (window-maximized-horizontally-p move-resize-window)
	(setq edges (delq 'left (delq 'right edges)))))
    edges)

  ;; called to initiate a move or resize on window W. FUNCTION is either
  ;; `move' or `resize'
  (define (do-move-resize w function)
    (if (eq function 'move)
	(call-window-hook 'before-move-hook w)
      (call-window-hook 'before-resize-hook w))
    (let* ((from-motion-event (and (current-event)
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
	   (move-resize-last-ptr  (if from-motion-event
				      (query-button-press-pointer)
				    (query-pointer t)))
	   (move-resize-snap-state (cons))
	   (move-resize-old-ptr-x (car move-resize-last-ptr))
	   (move-resize-old-ptr-y (cdr move-resize-last-ptr))
	   (move-resize-hints (window-size-hints w))
	   (move-resize-frame (cons (- (car (window-frame-dimensions w))
				       move-resize-old-width)
				    (- (cdr (window-frame-dimensions w))
				       move-resize-old-height)))
	   (move-resize-mode (if (eq function 'move)
				 move-outline-mode
			       resize-outline-mode))
	   (move-resize-edges nil)
	   (move-resize-last-outline nil)
	   (move-resize-moving-edges move-resize-moving-edges)
	   (move-resize-directions move-resize-directions)
	   (old-frame-draw-mutex
	    (frame-draw-mutex (not (eq move-resize-mode 'opaque))))
	   (old-frame-state-mutex (frame-state-mutex 'clicked))
	   (old-synthetic-configure-mutex
	    (synthetic-configure-mutex move-resize-inhibit-configure))
	   (was-successful nil)
	   server-grabbed)

      (when (and move-resize-raise-window (eq move-resize-mode 'opaque))
	;; only raise window initially if the display will get updated
	(raise-window* w))

      (update-edges)
      (unless (eq move-resize-mode 'opaque)
	(require 'sawfish.wm.util.window-outline)
	;; prevent any other programs drawing on the display
	(grab-server)
	(setq server-grabbed t))

      (add-hook 'enter-workspace-hook update-edges)
      (add-hook 'viewport-moved-hook update-edges)
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
				     (cdr move-resize-frame)))))
		    (if (eq move-resize-function 'resize)
			(unless (eq resize-edge-mode 'grab)
			  (infer-anchor))
		      (infer-directions))
		    (when (viable-move-resize-p)
		      (unless (eq move-resize-mode 'opaque)
			(apply draw-window-outline move-resize-last-outline))
		      (setq was-successful
			    (catch 'move-resize-done
			      (when from-motion-event
				(motion))
			      (recursive-edit)))))
		(ungrab-keyboard)
		(ungrab-pointer))))

	(when server-grabbed
	  (ungrab-server))
	(display-message nil)
	(frame-draw-mutex old-frame-draw-mutex)
	(frame-state-mutex old-frame-state-mutex)
	(synthetic-configure-mutex old-synthetic-configure-mutex)
	(remove-hook 'enter-workspace-hook update-edges)
	(remove-hook 'viewport-moved-hook update-edges))

      (when (and move-resize-raise-window (not (eq move-resize-mode 'opaque)))
	(raise-window* w))
      (if (eq function 'move)
	  (call-window-hook 'after-move-hook w
			    (list move-resize-directions
				  #:successful was-successful))
	(call-window-hook 'after-resize-hook w
			  (list move-resize-moving-edges
				#:successful was-successful)))))

  (define (update-edges)
    (setq move-resize-edges
	  (and (> move-snap-epsilon 0)
	       (progn
		 (get-visible-window-edges
		  #:with-ignored-windows move-snap-ignored-windows
		  #:windows-to-ignore (list move-resize-window)
		  #:include-heads t)))))

  ;; called each pointer motion event during move/resize
  (define (motion)
    (let* ((this-ptr (query-pointer))
	   (ptr-x (car this-ptr))
	   (ptr-y (cdr this-ptr))
	   logical-width logical-height)
      (unless (eq move-resize-mode 'opaque)
	(apply erase-window-outline move-resize-last-outline))
      (cond ((eq move-resize-function 'move)
	     (when (memq 'horizontal move-resize-directions)
	       (setq move-resize-x (+ move-resize-old-x
				      (- ptr-x move-resize-old-ptr-x))))
	     (when (memq 'vertical move-resize-directions)
	       (setq move-resize-y (+ move-resize-old-y
				      (- ptr-y move-resize-old-ptr-y))))
	     (when (> move-snap-epsilon 0)
	       (let
		   ((coords (snap-window-position-to-edges
			     move-resize-window (cons move-resize-x
						      move-resize-y)
			     (cons (- ptr-x (car move-resize-last-ptr))
				   (- ptr-y (cdr move-resize-last-ptr)))
			     move-resize-snap-state move-snap-epsilon
			     move-resize-edges move-snap-mode)))
		 (setq move-resize-x (car coords))
		 (setq move-resize-y (cdr coords)))))
	    ((eq move-resize-function 'resize)
	     (let
		 ((x-base (or (cdr (assq 'base-width move-resize-hints)) 0))
		  (x-inc (or (cdr (assq 'width-inc move-resize-hints)) 1))
		  (y-base (or (cdr (assq 'base-height move-resize-hints)) 0))
		  (y-inc (or (cdr (assq 'height-inc move-resize-hints)) 1))
		  (min-aspect (assq 'min-aspect move-resize-hints))
		  (max-aspect (assq 'max-aspect move-resize-hints)))

	       (when (memq resize-edge-mode '(grab border-grab))
		 (add-edges ptr-x ptr-y))
	       (cond
		((memq 'right move-resize-moving-edges)
		 (setq move-resize-width
		       (constrain-dimension-to-hints
			(+ move-resize-old-width
			   (- ptr-x move-resize-old-ptr-x))
			'x move-resize-hints))
                 (when (or min-aspect max-aspect)
                   (setq move-resize-height
                         (constrain-aspect-to-hints
                          move-resize-width
                          move-resize-old-height 'x min-aspect max-aspect))))

		((memq 'left move-resize-moving-edges)
		 (setq move-resize-width
		       (constrain-dimension-to-hints
			(+ move-resize-old-width
			   (- move-resize-old-ptr-x ptr-x))
                        'x move-resize-hints))
                 (when (or min-aspect max-aspect)
                   (setq move-resize-height
                         (constrain-aspect-to-hints
                          move-resize-width
                          move-resize-old-height 'x min-aspect max-aspect)))
		 (setq move-resize-x (- move-resize-old-x
					(- move-resize-width
					   move-resize-old-width)))))
	       (cond
		((memq 'bottom move-resize-moving-edges)
		 (setq move-resize-height
		       (constrain-dimension-to-hints
			(+ move-resize-old-height
			   (- ptr-y move-resize-old-ptr-y))
			'y move-resize-hints))
                 (when (or min-aspect max-aspect)
                   (setq move-resize-width
                         (constrain-aspect-to-hints
                          move-resize-height
                          move-resize-old-width 'y min-aspect max-aspect)))
                 )
		((memq 'top move-resize-moving-edges)
		 (setq move-resize-height
		       (constrain-dimension-to-hints
			(+ move-resize-old-height
			   (- move-resize-old-ptr-y ptr-y))
			'y move-resize-hints))
                 (when (or min-aspect max-aspect)
                   (setq move-resize-width
                         (constrain-aspect-to-hints
                          move-resize-height
                          move-resize-old-width 'y min-aspect max-aspect)))
		 (setq move-resize-y (- move-resize-old-y
					(- move-resize-height
					   move-resize-old-height)))))
	       (setq logical-width (quotient (- move-resize-width
						x-base) x-inc))
	       (setq logical-height (quotient (- move-resize-height
						 y-base) y-inc)))))
      (call-window-hook (if (eq move-resize-function 'move)
			    'while-moving-hook
			  'while-resizing-hook) move-resize-window)
      (cond ((and (eq move-resize-function 'resize) resize-show-dimensions)
	     (display-message (format nil "%dx%d"
				      ;; XXX broken if while-resizing-hook
				      ;; XXX changes dimensions..
				      logical-width logical-height)))
	    ((and (eq move-resize-function 'move) move-show-position)
	     (display-message (format nil "%+d%+d"
				      move-resize-x move-resize-y))))
      (if (eq move-resize-mode 'opaque)
	  (apply-changes)
	(let ((m-dim-x (+ move-resize-width (car move-resize-frame)))
	      (m-dim-y (+ move-resize-height (cdr move-resize-frame))))
	  (setq move-resize-last-outline (list move-resize-mode
					       move-resize-x move-resize-y
					       m-dim-x m-dim-y))
	  (apply draw-window-outline move-resize-last-outline)))
      (setq move-resize-last-ptr this-ptr)))

  ;; called when the move/resize finished (i.e. button-release event)
  (define (finished)
    (unless (eq move-resize-mode 'opaque)
      (apply erase-window-outline move-resize-last-outline))
    (apply-changes)
    (throw 'move-resize-done t))

  (define (cancel)
    (if (eq move-resize-mode 'opaque)
	(move-resize-window-to move-resize-window
			       move-resize-old-x move-resize-old-y
			       move-resize-old-width move-resize-old-height)
      (apply erase-window-outline move-resize-last-outline))
    (throw 'move-resize-done nil))

  ;; commit the current state of the move or resize
  (define (apply-changes)
    (cond
     ((>= move-resize-x (screen-width))
      (setq move-resize-x (1- (screen-width))))
     ((<= move-resize-x (- (+ move-resize-width (car move-resize-frame))))
      (setq move-resize-x (1+ (- (+ move-resize-width
				    (car move-resize-frame)))))))
    (cond
     ((>= move-resize-y (screen-height))
      (setq move-resize-y (1- (screen-height))))
     ((<= move-resize-y (- (+ move-resize-height (cdr move-resize-frame))))
      (setq move-resize-y (1+ (- (+ move-resize-height
				    (cdr move-resize-frame)))))))
    (move-resize-window-to move-resize-window
			   move-resize-x move-resize-y
			   move-resize-width move-resize-height))

  ;; called when moving, tries to decide which edges to move, which to stick
  (define (infer-anchor)
    (unless move-resize-moving-edges
      (let (tem)
	(if (and (memq resize-edge-mode '(border border-grab))
		 (clicked-frame-part)
		 (setq tem (frame-part-get (clicked-frame-part) 'class))
		 (setq tem (cdr (assq tem fp-edges-alist))))
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
    (setq move-resize-moving-edges (delete-unresizable-edges
				    move-resize-moving-edges)))

  (define (add-edges ptr-x ptr-y)
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

  (define (infer-directions)
    (unless move-resize-directions
      (setq move-resize-directions
	    (if (window-get move-resize-window 'fixed-position)
		'()
	      (list 'vertical 'horizontal))))
    (setq move-resize-directions (delete-unmovable-directions
				  move-resize-directions)))

  (define (viable-move-resize-p)
    (if (eq move-resize-function 'move)
	(not (null (delete-unmovable-directions
		    (copy-sequence move-resize-directions))))
      (not (null (delete-unresizable-edges
		  (if (memq resize-edge-mode '(region border))
		      ;; can't grab edges
		      (copy-sequence move-resize-moving-edges)
		    (list 'top 'bottom 'left 'right)))))))

  (define (resize-by-factor win amount)
    "Multiply win's dimensions by amount"
    (let* (
           (orig-wid (car (window-dimensions win)))
           (orig-hgt (cdr (window-dimensions win)))
           (new-wid (inexact->exact (floor (* amount orig-wid))))
           (new-hgt (inexact->exact (floor (* amount orig-hgt)))))
      ;; this expects integers ("800 600") and fails on floats ("800. 600.")
      (resize-window-with-hints* win new-wid new-hgt)))

  (define (double-window-size w)
    (resize-by-factor w 2))

  (define (halve-window-size w)
    (resize-by-factor w 0.5))

;;; hook functions

  (define (lost-window w)
    (when (eq move-resize-window w)
      (finished)))

  (add-hook 'unmap-notify-hook lost-window t)
  (add-hook 'destroy-notify-hook lost-window t)

;;; Entry points

  (define (move-window-interactively w)
    "Move the window interactively, with mouse or keyboard."
    (do-move-resize w 'move))

  (define (resize-window-interactively w)
    "Resize the window interactively, with mouse or keyboard."
    (do-move-resize w 'resize))

  (define (move-selected-window)
    "Wait for the user to select a window, then interactively move
that window."
    (let ((w (select-window)))
      (when w
	(move-window-interactively w))))

  (define (resize-selected-window)
    "Wait for the user to select a window, then interactively resize
that window."
    (let ((w (select-window)))
      (when w
	(resize-window-interactively w))))

  ;; Move Window To Center

  (define (move-window-center w)
    (move-window-to w
                    (quotient (- (screen-width)
                                 (car (window-frame-dimensions w))) 2)
                    (quotient (- (screen-height)
                                 (cdr (window-frame-dimensions w))) 2)))

  ;;###autoload
  (define-command 'move-window-center
    move-window-center #:spec "%W")
  (define-command 'move-window-interactively
    move-window-interactively #:spec "%W")
  (define-command 'resize-window-interactively
    resize-window-interactively #:spec "%W")
  (define-command 'move-selected-window
    move-selected-window)
  (define-command 'resize-selected-window
    resize-selected-window)
  (define-command 'double-window-size
    double-window-size #:spec "%W")
  (define-command 'halve-window-size
    halve-window-size #:spec "%W"))
