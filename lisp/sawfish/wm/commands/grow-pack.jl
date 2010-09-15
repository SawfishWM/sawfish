;; grow-pack.jl -- window resize and movement

;; Copyright (C) 2000, 01 Kai Grossjohann <Kai.Grossjohann@CS.Uni-Dortmund.DE>

;; This file is free software; you can redistribute it and/or modify it
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

;;; Commentary:

;; This package provides functions to `grow' or `pack' a window in four
;; directions.  `Growing' means to grow the window in the indicated
;; direction until it touches another window or by a given amount.
;; `Packing' means to move the window in the indicated direction until
;; it touches another window or by a given amount.

(define-structure sawfish.wm.commands.grow-pack

    (export grow-window-left
	    grow-window-right
	    grow-window-up
	    grow-window-down
	    pack-window-left
	    pack-window-right
	    pack-window-up
	    pack-window-down)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.events
	  sawfish.wm.misc
	  sawfish.wm.state.maximize
	  sawfish.wm.state.iconify
	  sawfish.wm.state.ignored
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.focus
	  sawfish.wm.workspace
	  sawfish.wm.stacking
	  sawfish.wm.util.stacking)

  (define-structure-alias grow-pack sawfish.wm.commands.grow-pack)

;;; Customization options.

  (defgroup gpsy "Grow, Pack, Shrink & Yank" :group move)

  (defcustom grow-is-maximize t
    "Whether growing is considered to be maximization."
    :type boolean
    :group (move gpsy))

  (defcustom pack-warp-pointer 'maybe
    "Whether and how to move the pointer when packing windows.
`maybe' means that the pointer is moved along with the window, if the
pointer was within the window area before packing. `always' warps the
pointer to the center of the window if it isn't already in the
window, then does like `maybe'.  `never' means not to warp the
pointer."
    :type (choice always maybe never)
    :group (move gpsy))

  (defcustom grow-pack-bump-obscured nil
    "Whether to bump into fully obscured windows."
    :type boolean
    :group (move gpsy))

  (defcustom grow-pack-bump-other-depth 'always
    "Whether to bump into windows on a different depth. When 'maybe,
Only `avoided' windows are bumped."
    :type (choice always maybe never)
    :group (move gpsy))

  (defcustom grow-pack-bump-ignored t
    "Whether to bump into ignored windows."
    :type boolean
    :group (move gpsy))

  (defcustom shrink-window-minimum-size 10
    "The minimum height or width to which a window may be shrunk."
    :type number
    :group (move gpsy))

  (defcustom yank-window-minimum-visible 10
    "The minimum amount of window left visible, if yanked over the edge."
    :type number
    :group (move gpsy))

;;; Code:

  ;; Entry points.

  (define (grow-window-left w #!optional arg)
    "Grows window to the left until it touches another window.
See `grow-window-up' for full doc."
    (grow-window w 'left arg))

  (define (grow-window-right w #!optional arg)
    "Grows window to the right until it touches another window.
See `grow-window-up' for full doc."
    (grow-window w 'right arg))

  (define (grow-window-up w #!optional arg)
    "Grows window upwards until it touches another window.
If the top edge was beyond the screen edge, it is brought back in.
With a universal prefix arg, maximize upwards instead.
With a numeric prefix arg, grow upwards by that many increments specified by
window or pixels instead."
    (grow-window w 'up arg))

  (define (grow-window-down w #!optional arg)
    "Grows window downwards until it touches another window.
See `grow-window-up' for full doc."
    (grow-window w 'down arg))

  (define (pack-window-left w #!optional arg)
    "Moves window to the left until it touches another window.
See `pack-window-up' for full doc."
    (pack-window w 'left arg))

  (define (pack-window-right w #!optional arg)
    "Moves window to the right until it touches another window.
See `pack-window-up' for full doc."
    (pack-window w 'right arg))

  (define (pack-window-up w #!optional arg)
    "Moves window upwards until it touches another window.
If the top edge was beyond the screen edge, it is moved back in.
With a universal prefix arg, move upwards maximally instead.
With a numeric prefix arg, move upwards by that many pixels instead."
    (pack-window w 'up arg))

  (define (pack-window-down w #!optional arg)
    "Moves window downwards until it touches another window.
See `pack-window-up' for full doc."
    (pack-window w 'down arg))

  ;; Command defs

  ;;###autoload
  (define-command 'grow-window-left grow-window-left #:spec "%W\nP")
  (define-command 'grow-window-right grow-window-right #:spec "%W\nP")
  (define-command 'grow-window-up grow-window-up #:spec "%W\nP")
  (define-command 'grow-window-down grow-window-down #:spec "%W\nP")
  (define-command 'pack-window-left pack-window-left #:spec "%W\nP")
  (define-command 'pack-window-right pack-window-right #:spec "%W\nP")
  (define-command 'pack-window-up pack-window-up #:spec "%W\nP")
  (define-command 'pack-window-down pack-window-down #:spec "%W\nP")

  ;; Implementation part.

  (define (bump-get-head w direction)
    (let* ((frame-dims (window-frame-dimensions w))
           (frame-pos (window-position w))
           (head1 (current-head w))
           (head2 (find-head
                   (case direction
                     ((left) (- (car frame-pos) 1))
                     ((right) (+ (car frame-pos) (car frame-dims)))
                     (t (car frame-pos)))
                   (case direction
                     ((up) (- (cdr frame-pos) 1))
                     ((down) (+ (cdr frame-pos) (cdr frame-dims)))
                     (t (cdr frame-pos))))))
      (if head2 head2 head1)))

  (define (bump-distance w direction maximize min-dist)
    (let* ((a (window-position w))
	   (z (window-frame-dimensions w))
	   (depth (window-get w 'depth))
	   (head (bump-get-head w direction))
	   (head-dims (head-dimensions head))
	   (head-offs (head-offset head))
	   bump cmp
	   xa xz
	   min-next border xborders)
      (let ((x (if (memq direction '(up down)) cdr car))
	    (y (if (memq direction '(up down)) car cdr)))
	(if (memq direction '(left up))
	    (setq border (x a)
		  min-next (- border min-dist)
		  xborders (lambda () `(,(+ (x xa) (x xz))
					,(y xa) . ,(+ (y xa) (y xz))))
		  a (y a)
		  z (+ a (y z))
	          bump (if (eq direction 'up)
	                   (cdr head-offs)
	                   (car head-offs))
		  cmp <=)
	  (setq border (+ (x a) (x z))
		min-next (+ border min-dist)
		xborders (lambda () `(,(x xa)
				      ,(y xa) . ,(+ (y xa) (y xz))))
		a (y a)
		z (+ a (y z))
	        bump (if (eq direction 'down)
	                 (+ (cdr head-offs) (cdr head-dims))
	                 (+ (car head-offs) (car head-dims)))
		cmp >=)))
      (if (cmp bump min-next)
	  (mapc
	   (lambda (x)
	     (and (not (window-iconified-p x))
		  (window-appears-in-workspace-p x current-workspace)
		  (if maximize
		      (if maximize-avoid-avoided (window-avoided-p x))
		    (or (eq depth (window-get x 'depth))
			(if (eq grow-pack-bump-other-depth 'maybe)
			    (window-avoided-p x)
			  (eq grow-pack-bump-other-depth 'always))))
		  (or grow-pack-bump-obscured
                      (not (eq (stacking-visibility x) 'fully-obscured)))
		  (or grow-pack-bump-ignored
		      (not (window-ignored-p x)))
		  (setq xa (window-position x)
			xz (window-frame-dimensions x))
		  (cmp bump (car (setq x (xborders))) min-next)
		  (< a (cddr x))
		  (< (cadr x) z)
		  (setq bump (car x))))
	   (managed-windows)))
      (- bump border)))

  (define (grow-window w direction #!optional arg)
    (if (eq arg '-) (setq arg -1))
    (let* ((horizontal (memq direction '(left right)))
	   (pos (window-position w))
	   (x (car pos))
	   (y (cdr pos))
	   (dim (window-dimensions w))
	   (maximize (when (consp arg) (setq arg ()) t))
	   (hints (window-size-hints w))
	   (inc (or (cdr (assq (if horizontal 'width-inc 'height-inc) hints))
		    1))
	   (distance (if arg
			 (* arg inc)
		       (abs (bump-distance w direction maximize inc))))
	   (new-dim (if horizontal
			(cons (+ (car dim) distance) (cdr dim))
		      (cons (car dim) (+ (cdr dim) distance)))))
      (when (or grow-is-maximize maximize)
	(unless (window-get w 'unmaximized-geometry)
	  (window-put w 'unmaximized-geometry (list x y (car dim) (cdr dim))))
	(window-put w
		    (if horizontal 'maximized-horizontally
                      'maximized-vertically)
		    t))
      (setq dim (cons (car new-dim) (cdr new-dim)))
      (maximize-truncate-dims w new-dim (if horizontal 'horizontal 'vertical)
			      hints)
      (when (memq direction '(left up))
	(setq distance (- distance (if horizontal
				       (- (car dim) (car new-dim))
				     (- (cdr dim) (cdr new-dim)))))
	(if (eq direction 'left)
	    (setq x (- x distance))
	  (setq y (- y distance))))
      (move-resize-window-to w x y (car new-dim) (cdr new-dim))
      (when maximize-raises (raise-window* w))
      (when (or grow-is-maximize maximize)
	(call-window-hook 'window-maximized-hook w
			  (list (if horizontal 'horizontal 'vertical)))
	(call-window-hook 'window-state-change-hook w (list '(maximized))))))

  (define (pack-window w direction #!optional arg)
    (if (eq arg '-) (setq arg -1))
    (let* ((horizontal (memq direction '(left right)))
	   (pos (window-position w))
	   (x (car pos))
	   (y (cdr pos))
	   (maximize (when (consp arg) (setq arg ()) t))
	   (distance (if arg
			 (if (memq direction '(left up)) (- arg) arg)
		       (bump-distance w direction maximize 1)))
	   (pointerw (query-pointer-window))
	   (xoffset (- (car (query-pointer)) x))
	   (yoffset (- (cdr (query-pointer)) y)))
      (if horizontal
	  (setq x (+ x distance))
	(setq y (+ y distance)))
      (move-window-to w x y)
      (case pack-warp-pointer
	((always)
	 (warp-cursor-to-window w))
	((maybe)
	 (when (equal pointerw w)
	   (warp-cursor (+ x xoffset) (+ y yoffset)))))
      (call-window-hook 'after-move-hook w
			`((,(if horizontal 'horizontal 'vertical)))))))
