;;; shrink windows to fit or yank them free.
;; Copyright 2000, 2001, 2003, 2005 by Timo Korvola <tkorvola@iki.fi>

;;; Commentary:
;; This package provides functions to shrink or yank a window in the
;; four cardinal directions.  Shrinking resizes the window by moving one
;; of its edges and yanking moves the window to meet the following condition:
;; - if the window was partially (or in case of yanking even entirely) outside
;;   the screen it will be entirely on the screen,
;; - otherwise, if the window intersected with other windows it will intersect
;;   with one less window,
;; - otherwise the window will not be moved or resized.
;;
;; If the window reaches its minimum size before this condition can be
;; satisfied the window is resized to the minimum size instead.  There
;; is also a minimum size constraint `shrink-window-minimum', which
;; applies to all windows.  However, it is measured in pixels and
;; windows may actually become smaller than the specified value due to
;; size truncation.
;;
;; If the window would have to be yanked off the screen to satisfy the
;; condition `yank-window-minimum-visible' pixels will be left visible instead.

(define-structure sawfish.wm.commands.shrink-yank

    (export shrink-window-left
            shrink-window-right
            shrink-window-up
            shrink-window-down
            yank-window-left
            yank-window-right
            yank-window-up
            yank-window-down)

    (open   rep
            sawfish.wm.commands
            sawfish.wm.commands.grow-pack
            sawfish.wm.events
            sawfish.wm.misc
            sawfish.wm.state.maximize
            sawfish.wm.state.iconify
            sawfish.wm.util.rects
            sawfish.wm.windows
            sawfish.wm.workspace
            sawfish.wm.custom)

  (define-structure-alias shrink-yank sawfish.wm.commands.shrink-yank)

;;; Customization options are defined in grow-pack.jl

;;; Commands:

  (define (shrink-window-left window)
    "Shrinks WINDOW by moving the right edge to the left until it
overlaps with one less window than before."
    (shrink-window window 'left))

  (define (shrink-window-right window)
    "Shrinks WINDOW by moving the left edge to the right until it
overlaps with one less window than before."
    (shrink-window window 'right))

  (define (shrink-window-up window)
    "Shrinks WINDOW by moving the lower edge upwards until it
overlaps with one less window than before."
    (shrink-window window 'up))

  (define (shrink-window-down window)
    "Shrinks WINDOW by moving the upper edge downwards until it
overlaps with one less window than before."
    (shrink-window window 'down))

  (define (yank-window-left window)
    "Moves WINDOW to the left until it overlaps with one less window
than before."
    (yank-window window 'left))

  (define (yank-window-right window)
    "Moves WINDOW to the right until it overlaps with one less window
than before."
    (yank-window window 'right))

  (define (yank-window-up window)
    "Moves WINDOW upwards until it overlaps with one less window than
before."
    (yank-window window 'up))

  (define (yank-window-down window)
    "Moves WINDOW downwards until it overlaps with one less window than
before."
    (yank-window window 'down))

  ;;###autoload
  (define-command 'shrink-window-left shrink-window-left #:spec "%W")
  (define-command 'shrink-window-right shrink-window-right #:spec "%W")
  (define-command 'shrink-window-up shrink-window-up #:spec "%W")
  (define-command 'shrink-window-down shrink-window-down #:spec "%W")
  (define-command 'yank-window-left yank-window-left #:spec "%W")
  (define-command 'yank-window-right yank-window-right #:spec "%W")
  (define-command 'yank-window-up yank-window-up #:spec "%W")
  (define-command 'yank-window-down yank-window-down #:spec "%W")

;;; Implementation:

  (define (window-frame-rect window)
    "Returns the rectangle (left top right bottom) describing the frame
dimensions of WINDOW."
    (let* ((wpos (window-position window))
	   (wdim (window-frame-dimensions window))
	   (wleft (car wpos))
	   (wtop (cdr wpos)))
      (list wleft wtop (+ wleft (car wdim)) (+ wtop (cdr wdim)))))

  ;; I can never remember these!
  (define left car)
  (define top cadr)
  (define right caddr)
  (define bottom cadddr)

  (define (maybe-warp-pointer window old-rect direction maybe)
    (define (scale x x0 x1 x0new x1new)
      (round (/ (+ (* (- x x0) x1new)
		   (* (- x1 x) x0new))
		(- x1 x0))))
    (define (truncate-rect r)
      (list (max (left r) 0)
	    (max (top r) 0)
	    (min (right r) (screen-width))
	    (min (bottom r) (screen-height))))
    (case pack-warp-pointer
      ((always) (warp-cursor-to-window window))
      ((maybe)
       (when maybe
	 (let* ((owr (truncate-rect old-rect))
                (nwr (truncate-rect (window-frame-rect window)))
                (ppos (query-pointer))
                (xpos (car ppos))
                (ypos (cdr ppos)))
	   (case direction
	     ((left right)
	      (setq xpos (scale xpos (left owr) (right owr)
                                (left nwr) (right nwr))))
	     ((up down)
	      (setq ypos (scale ypos (top owr) (bottom owr)
                                (top nwr) (bottom nwr)))))
	   (warp-cursor xpos ypos))))))

  ;; Return the coordinate of the window intersection to shink or yank
  ;; to.  This will do for both shrinking and yanking although the
  ;; requirements are slightly different: e.g., a window that
  ;; completely surrounds the active window is irrelevant for
  ;; shrinking.
  (define (find-least-intersection window wr direction yank)
    (let* ((isect-coord (if yank
			    (case direction
			      ((left up) 0)
			      ((right) (screen-width))
			      ((down) (screen-height)))
			  (case direction
			    ((left) (left wr))
			    ((up) (top wr))
			    ((right) (right wr))
			    ((down) (bottom wr)))))
	   (isect-check (case direction
			  ((left)
			   (lambda (xr)
			     (and (< isect-coord (left xr) (right wr))
				  (setq isect-coord (left xr)))))
			  ((up)
			   (lambda (xr)
			     (and (< isect-coord (top xr) (bottom wr))
				  (setq isect-coord (top xr)))))
			  ((right)
			   (lambda (xr)
			     (and (< (left wr) (right xr) isect-coord)
				  (setq isect-coord (right xr)))))
			  ((down)
			   (lambda (xr)
			     (and (< (top wr) (bottom xr) isect-coord)
				  (setq isect-coord (bottom xr))))))))
      ;; If the window is partially (shrink or yank) or entirely (yank
      ;; only) outside the screen return the screen edge.
      (cond ((and (eq direction 'left)
		  (< isect-coord (screen-width) (right wr)))
	     (screen-width))
	    ((and (eq direction 'right) (< (left wr) 0 isect-coord))
	     0)
	    ((and (eq direction 'up)
		  (< isect-coord (screen-height) (bottom wr)))
	     (screen-height))
	    ((and (eq direction 'down) (< (top wr) 0 isect-coord))
	     0)
	    (t
	     (let ((win nil))
	       (mapc (lambda (x)
		       (and (not (eql x window))
			    (not (window-iconified-p x))
			    (window-appears-in-workspace-p x current-workspace)
			    (let ((xr (window-frame-rect x)))
			      (and (positivep (rect-2d-overlap* wr xr))
				   (isect-check xr)
				   (setq win x)))))
		     (managed-windows))
	       (and win isect-coord))))))

  (define (shrink-window window direction)
    "Shrinks WINDOW by moving the edge opposite to DIRECTION (left, right,
up or down) towards DIRECTION until it overlaps with one less window than
before."
    (let* ((wr (window-frame-rect window))
	   (isect-coord (find-least-intersection window wr direction nil))
	   (nleft (left wr))
	   (ntop (top wr))
	   (wdim (window-dimensions window))
	   (nwidth (car wdim))
	   (nheight (cdr wdim)))
      (when (and isect-coord
		 (let ((max-shrinkage (- (case direction
					   ((left right) nwidth)
					   ((up down) nheight))
					 shrink-window-minimum-size)))
		   (when (positivep max-shrinkage)
		     (case direction
		       ((left) (setq nwidth (- nwidth
					       (min max-shrinkage
						    (- (right wr)
						       isect-coord)))))
		       ((up) (setq nheight (- nheight
					      (min max-shrinkage
						   (- (bottom wr)
						      isect-coord)))))
		       ((right) (setq nwidth (- nwidth
						(min max-shrinkage
						     (- isect-coord
							(left wr))))))
		       ((down) (setq nheight (- nheight
						(min max-shrinkage
						     (- isect-coord
							(top wr))))))))))
	(let ((tem (cons nwidth nheight)))
	  (maximize-truncate-dims window tem)
	  (setq nwidth (car tem)
		nheight (cdr tem)))
	(case direction
	  ((right) (setq nleft (+ nleft (- (car wdim) nwidth))))
	  ((down) (setq ntop (+ ntop (- (cdr wdim) nheight)))))
	(let ((pointerw (query-pointer-window)))
	  (move-resize-window-to window nleft ntop nwidth nheight)
	  (maybe-warp-pointer window wr direction (eql window pointerw))))))

  (define (yank-window window direction)
    "Moves WINDOW towards DIRECTION (left, right, up or down) until
WINDOW overlaps with one less window than before."
    (let* ((wr (window-frame-rect window))
	   (isect-coord (find-least-intersection window wr direction t))
	   (nleft (left wr))
	   (ntop (top wr)))
      (and isect-coord
	   (case direction
	     ((left) (let ((max-move (- (right wr)
					yank-window-minimum-visible)))
		       (when (positivep max-move)
			 (setq nleft (- nleft
					(min max-move
					     (- (right wr) isect-coord)))))))
	     ((up) (let ((max-move (- (bottom wr)
				      yank-window-minimum-visible)))
		     (when (positivep max-move)
		       (setq ntop (- ntop
				     (min max-move
					  (- (bottom wr) isect-coord)))))))
	     ((right) (let ((max-pos (- (screen-width)
					yank-window-minimum-visible)))
			(when (< (left wr) max-pos)
			  (setq nleft (min max-pos isect-coord)))))
	     ((down) (let ((max-pos (- (screen-height)
				       yank-window-minimum-visible)))
		       (when (< (top wr) max-pos)
			 (setq ntop (min max-pos isect-coord))))))
	   (let ((pointerw (query-pointer-window)))
	     (move-window-to window nleft ntop)
	     (maybe-warp-pointer window wr
				 direction (eql window pointerw)))))))
