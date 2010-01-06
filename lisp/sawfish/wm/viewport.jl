;; viewport.jl -- virtual desktops

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
            viewport-at
	    window-viewport
            viewport-offset-coord
	    window-absolute-position
	    set-number-of-viewports
            rect-within-viewport-p
            rect-within-head-p
            window-head-any-viewport
	    viewport-minimum-size-changed
            viewport-honor-workspace-edges
	    viewport-windows)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.commands
	  sawfish.wm.workspace
	  sawfish.wm.custom
          sawfish.wm.util.rects
	  sawfish.wm.session.init)

  ;; Virtual workspaces are implemented by moving windows in and out of
  ;; the screen dimensions. E.g. moving to the left moves all windows one
  ;; screen-width to the right.

  (defgroup viewport "Viewport" :group workspace)

  (defcustom viewport-dimensions '(1 . 1)
    "The number of columns and rows of the virtual desktop: \\w"
    :group (workspace viewport)
    :type (pair (number 1) (number 1))
    :tooltip "This is meaningless if dynamic viewport is enabled."
    :after-set (lambda () (viewport-size-changed t)))

  (defcustom viewport-minimum-dimensions '(1 . 1)
    "Minimum number of columns and rows of virtual desktop (if boundary mode is dynamic): \\w"
    :group (workspace viewport)
    :type (pair (number 1) (number 1))
    :after-set (lambda () (viewport-minimum-size-changed)))

  (defcustom uniconify-to-current-viewport t
    "Windows uniconify to the current viewport."
    :type boolean
    :group (workspace viewport))

  (defcustom scroll-viewport-steps 1
    "When you go to another viewport, the bigger this value,
the more smoothly the screen is scrolled.
It is the number of steps for scrolling. The value 1 means no scroll, and
the change is instantaneous."
    :group (workspace viewport)
    :type number
    :range (1 . 50))

  (defcustom viewport-boundary-mode 'stop
    "Stop, wrap-around, or grow the virtual desktop when you go beyond virtual desktop edge."
    :group (workspace viewport)
    :type (choice wrap-around stop dynamic))

  (defvar workspace-viewport-data nil
    "Information about the viewport details of different workspaces.")

;;; raw viewport handling

  ;; The unit is pixel, NOT (col, row).
  ;; Position (0, 0) means the top-left of the workspace.
  (defvar viewport-x-offset 0)
  (defvar viewport-y-offset 0)

  (define (warp-viewport x y)
    "Move viewport to (x, y). The unit is pixel, not (col, row).
Position (0, 0) is the top-left of the workspace."
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

              ;; No need to warp windows not in the current workspace.
              ;; See viewport-leave-workspace-handler and
              ;; viewport-enter-workspace handler below.
              ((not
                (window-appears-in-workspace-p (car rest) current-workspace))
               (loop (cdr rest) inside outside))
	      ((window-outside-viewport-p (car rest))
	       (loop (cdr rest) inside (cons (car rest) outside)))

	      (t (loop (cdr rest) (cons (car rest) inside) outside))))

      (setq viewport-x-offset x)
      (setq viewport-y-offset y)))

  (define (viewport-honor-workspace-edges)
    "Whether or not to prevent the display from moving past the
current viewport boundaries.  Returns true if `viewport-boundary-mode'
is not set to 'dynamic."
    (not (eq viewport-boundary-mode 'dynamic)))

  (define (set-viewport x y)
    "Move viewport to (x, y). The unit is pixel, not (col, row).
Position (0, 0) is the top-left of the workspace.

Scroll is done whose steps are `scroll-viewport-steps'."
    (unless (= scroll-viewport-steps 1) ; fast skip if scroll is unwanted
      (let* ((xstep (quotient (- x viewport-x-offset) scroll-viewport-steps))
             (ystep (quotient (- y viewport-y-offset) scroll-viewport-steps))
             (step-count (if (= xstep ystep 0) 0 scroll-viewport-steps)))
        (while (> step-count 1)
          (warp-viewport (+ viewport-x-offset xstep)
                         (+ viewport-y-offset ystep))
          (setq step-count (1- step-count)))))
    (warp-viewport x y)
    (call-hook 'viewport-moved-hook))

  (define (viewport-before-exiting)
    ;; Reset all workspaces to viewport 0,0.  This is so that on
    ;; restart (or if another wm takes over) windows aren't in strange
    ;; places.
    (mapc (lambda (ws-data)
            (select-workspace (car ws-data))
            (set-screen-viewport 0 0))
          workspace-viewport-data))

  (add-hook 'before-exit-hook viewport-before-exiting t)

  (define (viewport-dynamic-resize)
    "Reset the size of the viewport to include the current screen as
well as any windows in the current workspace."
    (when (eq viewport-boundary-mode 'dynamic)
      (let ((windows
	     (filter-windows
	      (lambda (w)
		(window-in-workspace-p w current-workspace)))))
	(if windows
	    (let* ((width (screen-width))
                   (height (screen-height))
                   (points
                    (nconc
                     (mapcar (lambda (w)
                               (let ((pos (window-position w))
                                     (dims (window-frame-dimensions w)))
                                 (list (car pos)
                                       (cdr pos)
                                       (+ (car pos) (car dims))
                                       (+ (cdr pos) (cdr dims)))))
                             windows)
                     ;; Include the current screen:
                     `((0 0 ,(1- width) ,(1- height)))))
                   ;; The min/max values calculated below are relative to
                   ;; the old logical 0,0 point of the virtual desktop:
                   (old-x-origin (- viewport-x-offset))
                   (old-y-origin (- viewport-y-offset))
                   (x-min (- (apply min (mapcar car points)) old-x-origin))
                   (y-min (- (apply min (mapcar (lambda (e) (nth 1 e)) points))
                             old-y-origin))
                   (x-max (- (apply max (mapcar (lambda (e) (nth 2 e)) points))
                             old-x-origin))
                   (y-max (- (apply max (mapcar (lambda (e) (nth 3 e)) points))
                             old-y-origin))
                   ;; high-* values are the number of rows/columns above
                   ;; the old origin, low-* values the number below the
                   ;; old origin.
                   (high-rows (+ (quotient y-max height)
                                 (if (> (mod y-max height) 0)
                                     1
                                   0)))
                   (low-rows (+ (- (quotient y-min height))
                                (if (and (< y-min 0)
                                         (> (mod y-min height) 0))
                                    1
                                  0)))
                   (rows (+ low-rows high-rows))
                   (high-cols (+ (quotient x-max width)
                                 (if (> (mod x-max width) 0)
                                     1
                                   0)))
                   (low-cols (+ (- (quotient x-min width))
                                (if (and (< x-min 0)
                                         (> (mod x-min width) 0))
                                    1
                                  0)))
                   (cols (+ low-cols high-cols)))
	      (setq
               viewport-y-offset (- (- old-y-origin (* low-rows height)))
               viewport-x-offset (- (- old-x-origin (* low-cols width)))
	       viewport-dimensions (cons
				    (max cols
					 (car viewport-minimum-dimensions))
				    (max rows
					 (cdr viewport-minimum-dimensions)))))
	  (setq viewport-y-offset 0
		viewport-x-offset 0
		viewport-dimensions viewport-minimum-dimensions))
	(call-hook 'viewport-resized-hook))))

  ;; Resize virtual workspace on workspace switch or viewport move:
  (add-hook 'viewport-moved-hook
	    viewport-dynamic-resize)

  (add-hook 'after-initialization-hook
            viewport-dynamic-resize)

  (define (viewport-leave-workspace-handler ws)
    "On leaving a workspace, store information about the viewport
configuration so that it can be restored properly later.
`WS' is the workspace to leave."
    (let ((vp-data (list viewport-x-offset
                         viewport-y-offset
                         viewport-dimensions))
          (old-ent (assoc ws workspace-viewport-data)))
      (if old-ent
          (rplacd old-ent vp-data)
        (setq workspace-viewport-data
              (cons (cons ws vp-data)
                    workspace-viewport-data)))))

  (add-hook 'leave-workspace-hook
            viewport-leave-workspace-handler)

  (define (viewport-enter-workspace-handler ws)
    "Restore any saved data about the viewport for the new workspace.
When `viewport-boundary-mode' is not `dynamic', make sure that the new
viewport is within `viewport-dimensions'."
    (let ((vp-data (cdr (assoc ws workspace-viewport-data))))
      (if vp-data
          (let ((maybe-x-offset (car vp-data))
                (maybe-y-offset (nth 1 vp-data)))
            (if (eq viewport-boundary-mode 'dynamic)
                (setq viewport-dimensions (nth 2 vp-data)
                      viewport-x-offset maybe-x-offset
                      viewport-y-offset maybe-y-offset)
              ;; Do maybe-y-offset and maybe-x-offset fit within
              ;; current viewport-dimensions?
              (if (and (<= maybe-x-offset
                           (* (1- (cdr viewport-dimensions)) (screen-width)))
                       (<= maybe-y-offset
                           (* (1- (car viewport-dimensions)) (screen-height))))
                  (setq viewport-x-offset maybe-x-offset
                        viewport-y-offset maybe-y-offset)
                (setq viewport-x-offset 0
                      viewport-y-offset 0))))
        (setq viewport-x-offset 0
              viewport-y-offset 0))
      (viewport-size-changed)))

  (add-hook 'enter-workspace-hook
            viewport-enter-workspace-handler)

  ;; screen sized viewport handling

  (define (screen-viewport)
    "Gives the row and column of the current viewport."
    (cons (quotient viewport-x-offset (screen-width))
	  (quotient viewport-y-offset (screen-height))))

  ;; returns t if it actually moved the viewport
  (define (set-screen-viewport col row)
    (when (eq viewport-boundary-mode 'wrap-around)
      (setq col (mod col (car viewport-dimensions))
            row (mod row (cdr viewport-dimensions))))
    (when (or (eq viewport-boundary-mode 'dynamic)
	      (and (>= col 0) (< col (car viewport-dimensions))
		   (>= row 0) (< row (cdr viewport-dimensions))))
      (set-viewport (* col (screen-width))
                    (* row (screen-height)))
      t))

  (define (select-workspace-and-viewport space col row)
    (select-workspace space)
    (set-screen-viewport col row))

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
    "True if `window' is outside the workspace.  Note that this does
not check which workspace the windows is in; the window is outside the
workspace if it's position is not within any viewport."
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

  (define (window-outside-viewport-p window #!optional viewport)
    "True if no part of `window' is inside the current viewport.  If
`viewport' is specified check against that viewport rather than the
current one."
    (let* ((cur-vp (screen-viewport))
           (width (screen-width))
           (height (screen-height))
           (x-min (if viewport
                      (* (- (car viewport) (car cur-vp))
                         width)
                    0))
           (x-max (+ x-min width))
           (y-min (if viewport
                      (* (- (cdr viewport) (cdr cur-vp))
                         height)
                    0))
           (y-max (+ y-min height))
           (pos (window-position window))
           (dims (window-frame-dimensions window)))
      (or (<= (+ (car pos) (car dims)) x-min)
          (<= (+ (cdr pos) (cdr dims)) y-min)
          (>= (car pos) x-max)
          (>= (cdr pos) y-max))))

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

  (define (viewport-at x y)
    "Returns a cons cell consisting of the column and row of the
viewport containing the specified coordinates."
    (let ((x (+ x viewport-x-offset))
          (y (+ y viewport-y-offset)))
      ;; If x or y is negative the viewport must be adjusted by one.
      (cons (- (quotient x (screen-width))
               (if (< x 0)
                   1
                 0))
	    (- (quotient y (screen-height))
               (if (< y 0)
                   1
                 0)))))

  (define (window-viewport w)
    "Returns a cons cell consisting of the column and row of the
viewport containing (or mostly containing) `w'."
    (let* ((pos (window-position w))
           (dims (window-dimensions w)))
      (viewport-at (+ (car pos) (quotient (car dims) 2))
                   (+ (cdr pos) (quotient (cdr dims) 2)))))

  ;; From sawfish.wm.util.rects:
  ;; A rectangle is (LEFT TOP RIGHT BOTTOM [WEIGHT])
  ;; The left and top edges are considered part of the rectangle,
  ;; the right and bottom edges are not.

  (define (rect-within-viewport-p rect #!optional viewport)
    "Return t if `rect' is entirely inside the screen boundaries for
some viewport, regardless of whether it is the current viewport, or
even if the viewport currently exists.  If `viewport' is specified
check only against that viewport."
    (let ((vp (viewport-at (car rect) (nth 1 rect))))
      (when (or (null viewport) (equal vp viewport))
        (let ((offset (viewport-offset-coord vp)))
          (rect-wholly-within-rect (list (car offset)
                                         (cdr offset)
                                         (+ (car offset) (screen-width))
                                         (+ (cdr offset) (screen-height)))
                                   rect)))))

  ;; The next two functions should perhaps go in a "heads" module if
  ;; there were one.  But what makes them interesting is that they
  ;; must account for viewports, which is why I put them here.

  (define (rect-within-head-p rect #!optional head)
    "Return t if `rect' is entirely within some head on some
viewport. If `head' is provided `rect' must be within that head on
some viewport."
    (let* ((offset (viewport-offset-coord (viewport-at (car rect)
                                                 (nth 1 rect))))
           (left (- (car rect) (car offset)))
           (top (- (nth 1 rect) (cdr offset)))
           (right (- (nth 2 rect) (car offset)))
           (bottom (- (nth 3 rect) (cdr offset)))
           (head (or head (find-head left top)))
           (head-pos (head-offset head))
           (head-dims (head-dimensions head)))
      (rect-wholly-within-rect (list (car head-pos)
                                     (cdr head-pos)
                                     (+ (car head-pos) (car head-dims))
                                     (+ (cdr head-pos) (cdr head-dims)))
                               (list left top right bottom))))

  (define (window-head-any-viewport w)
    "Return the id of the head that would contain `w', if the viewport
`w' occupies were visible."
    (let* ((coords (window-position w))
           (dims (window-dimensions w))
           (center (cons (+ (car coords) (quotient (car dims) 2))
                         (+ (cdr coords) (quotient (cdr dims) 2))))
           (vp-offset (viewport-offset-coord (viewport-at (car center)
                                                    (cdr center)))))
      (find-head (- (car center) (car vp-offset))
                 (- (cdr center) (cdr vp-offset)))))

  (define (viewport-offset-coord vp)
    "Returns the offset from the current viewport to viewport `VP'
which is specified as (column . row). The return value is the cons
cell (x . y). The values are in pixel, and are negative if it lies at
left or above the current viewport.

`VP' can be outside of the workspace. If `VP' is nil, it is
understood as the current viewport, i.e., (0 . 0) will be returned."
    (if (consp vp)
        (let* ((cur-vp (screen-viewport)))
          (cons
           (* (- (car vp) (car cur-vp)) (screen-width))
           (* (- (cdr vp) (cdr cur-vp)) (screen-height))))
      '(0 . 0)))

  (define (window-absolute-position w #!optional disambiguate)
    "Returns the coordinates of the window as if the window's viewport
is selected. The return value is the cons cell (x . y)."
    ;; So, ignoring the side effect, it's roughly equal to
    ;; (set-screen-viewport (window-viewport w))
    ;; (window-position w)
    ;; The correct name should be "window-position-in-viewport".

    ;; The temporary hack 'disambiguate' is introduced in Sawfish 1.6.
    ;; It will be removed.

    ;; Two methods differ if the window lies across several vp slots.
    (if disambiguate
	;; method 1: always returns the same value.
	;; This is introduced in 1.6, and necessary for maximization.
	(let ((offset (viewport-offset-coord (window-viewport w)))
	      (coords (window-position w)))
	  (cons
	   (- (car coords) (car offset))
	   (- (cdr coords) (cdr offset))))
      ;; method 2: If the window is visible from the current viewport,
      ;; then simply window-position is returned.
      ;; Else, the viewport where the window's top-left lies is used.
      ;; This has existed for long time.
      ;; Perhaps this can be deleted, and check of visibility can be
      ;; separated, done by the caller.
      (let ((position (window-position w)))
	(if (window-outside-viewport-p w)
	    (cons (mod (+ (car position) viewport-x-offset) (screen-width))
		  (mod (+ (cdr position) viewport-y-offset) (screen-height)))
	  position))))

  (define (viewport-size-changed #!optional force)
    ;; This is called when the user requests a change (e.g., from the
    ;; gui, or via `set-number-of-viewports') as well as when the
    ;; desktop is switched.  If `force' is set it's considered to be
    ;; user-requested, and therefore mandatory that the
    ;; `viewport-dimensions' variable be respected.
    (when (or (< (car viewport-dimensions) (car viewport-minimum-dimensions))
	      (< (cdr viewport-dimensions) (cdr viewport-minimum-dimensions)))
      (if force
          (setq viewport-minimum-dimensions
                (cons (min (car viewport-dimensions)
                           (car viewport-minimum-dimensions))
                      (min (cdr viewport-dimensions)
                           (cdr viewport-minimum-dimensions))))
        (setq viewport-dimensions
              (cons (max (car viewport-dimensions)
                         (car viewport-minimum-dimensions))
                    (max (cdr viewport-dimensions)
                         (cdr viewport-minimum-dimensions)))))
      (when (eq viewport-boundary-mode 'dynamic)
	(viewport-dynamic-resize)))
    (unless (eq viewport-boundary-mode 'dynamic)
      ;; Not using dynmic viewports, so ensure that windows are within
      ;; the current virtual-workspace boundaries:
      (let ((port (screen-viewport)))
	(set-screen-viewport (min (car port) (1- (car viewport-dimensions)))
			     (min (cdr port) (1- (cdr viewport-dimensions))))
	(map-windows (lambda (w)
                       (when (and (window-outside-workspace-p w)
                                  (window-appears-in-workspace-p
                                   w current-workspace))
			 (move-window-to-current-viewport w))))
	(call-hook 'viewport-resized-hook))))

  (define (viewport-minimum-size-changed)
    (if (eq viewport-boundary-mode 'dynamic)
	(viewport-dynamic-resize)
      (when (or (< (car viewport-dimensions)
                   (car viewport-minimum-dimensions))
		(< (cdr viewport-dimensions)
                   (cdr viewport-minimum-dimensions)))
	(setq viewport-dimensions
	      (cons (max (car viewport-dimensions)
			 (car viewport-minimum-dimensions))
		    (max (cdr viewport-dimensions)
			 (cdr viewport-minimum-dimensions))))
	(viewport-size-changed))))

  (define (set-number-of-viewports width height)
    (setq viewport-dimensions (cons width height))
    (setq viewport-minimum-dimensions (cons width height))
    (if (eq viewport-boundary-mode 'dynamic)
	(viewport-dynamic-resize)
      (viewport-size-changed t)))

  (define (viewport-windows #!optional vp-col vp-row workspace
                            exclude-sticky exclude-iconified)
    "Provide a list of windows that are mapped to the specified viewport."
    (let* ((cur-vp (screen-viewport))
           (col (or vp-col (car cur-vp)))
           (row (or vp-row (cdr cur-vp)))
           (ws (or workspace current-workspace))
           (width (screen-width))
           (height (screen-height))
           (left (+ (- viewport-x-offset) (* col width)))
           (right (+ left (1- width)))
           (top (+ (- viewport-y-offset) (* row height)))
           (bottom (+ top (1- height))))
      (filter-windows (lambda (w)
                        (let ((pos (window-position w))
                              (dims (window-frame-dimensions w)))
                          (and (window-mapped-p w)
                               (not (window-get w 'ignored))
                               (if exclude-sticky
                                   (window-in-workspace-p w ws)
                                 (window-appears-in-workspace-p w ws))
                               (not (and exclude-iconified
                                         (window-get w 'iconified)))
                               (not (or (<= (+ (car pos) (car dims)) left)
                                        (<= (+ (cdr pos) (cdr dims)) top)
                                        (>= (car pos) right)
                                        (>= (cdr pos) bottom)))))))))

;;; commands

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
    "Move the window to the viewport on the left, and switch to that
viewport."
    (move-window-to-viewport-and-move-viewport w -1 0))

  (define (move-window-right w)
    "Move the window to the viewport on the right, and switch to that
viewport."
    (move-window-to-viewport-and-move-viewport w 1 0))

  (define (move-window-down w)
    "Move the window to the viewport below, and switch to that
viewport."
    (move-window-to-viewport-and-move-viewport w 0 1))

  (define (move-window-up w)
    "Move the window to the viewport above, and switch to that
viewport."
    (move-window-to-viewport-and-move-viewport w 0 -1))

  (define-command 'move-viewport-right move-viewport-right
    #:class 'default)
  (define-command 'move-viewport-left move-viewport-left
    #:class 'default)
  (define-command 'move-viewport-up move-viewport-up
    #:class 'default)
  (define-command 'move-viewport-down move-viewport-down
    #:class 'default)
  (define-command 'move-window-right move-window-right #:spec "%W"
    #:class 'default)
  (define-command 'move-window-left move-window-left #:spec "%W"
    #:class 'default)
  (define-command 'move-window-up move-window-up #:spec "%W"
    #:class 'default)
  (define-command 'move-window-down move-window-down #:spec "%W"
    #:class 'default)

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
