;; maximize.jl -- window maximization
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

(define-structure sawfish.wm.state.maximize

    (export window-maximized-p
	    window-maximized-horizontally-p
	    window-maximized-vertically-p
	    window-unmaximized-position
	    window-unmaximized-dimensions
	    window-maximizable-p
	    maximize-truncate-dims
	    maximize-find-workarea
	    maximize-window
	    unmaximize-window
	    maximize-window-vertically
	    maximize-window-horizontally
	    maximize-window-toggle
	    maximize-window-vertically-toggle
	    maximize-window-horizontally-toggle
	    maximize-fill-window
	    maximize-fill-window-vertically
	    maximize-fill-window-horizontally
	    maximize-fill-window-toggle
	    maximize-fill-window-vertically-toggle
	    maximize-fill-window-horizontally-toggle)

    (open rep
	  rep.system
	  sawfish.wm.util.edges
	  sawfish.wm.util.rects
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.gaol
	  sawfish.wm.session.init
	  sawfish.wm.workspace
	  sawfish.wm.util.display-window
	  sawfish.wm.util.stacking
	  sawfish.wm.misc)

  (define-structure-alias maximize sawfish.wm.state.maximize)

  ;; This sets the window property `unmaximized-geometry' of each
  ;; currently maximize window to `(X Y W H)', the saved geometry.

  (defcustom maximize-always-expands nil
    "Maximizing a window in one dimension must increase the size of that dimension."
    :group (min-max maximize)
    :user-level expert
    :type boolean)

  (defcustom maximize-raises t
    "Raise windows when they are maximized."
    :group (min-max maximize)
    :type boolean)

  (defcustom maximize-ignore-when-filling t
    "Let unmanaged windows be overlapped when filling windows."
    :group (min-max maximize)
    :user-level expert
    :type boolean)

  (defcustom maximize-avoid-avoided t
    "Don't cover `avoided' windows when maximizing."
    :group (min-max maximize)
    :user-level expert
    :type boolean)

  ;; called when a window is maximized, args (W #!optional DIRECTION)
  (defvar window-maximized-hook nil)

  ;; called when a window is un-maximized, args (W #!optional DIRECTION)
  (defvar window-unmaximized-hook nil)


;;; handling maximized state

  (define (window-maximized-p w)
    (window-get w 'unmaximized-geometry))

  (define (window-maximized-horizontally-p w)
    (window-get w 'maximized-horizontally))

  (define (window-maximized-vertically-p w)
    (window-get w 'maximized-vertically))

  (define (window-unmaximized-position w)
    (let ((coords (window-position w))
	  (old-geom (window-get w 'unmaximized-geometry)))
      (when (window-maximized-horizontally-p w)
	(rplaca coords (nth 0 old-geom)))
      (when (window-maximized-vertically-p w)
	(rplaca coords (nth 1 old-geom)))
      coords))

  (define (window-unmaximized-dimensions w)
    (let ((dims (window-dimensions w))
	  (old-geom (window-get w 'unmaximized-geometry)))
      (when (window-maximized-horizontally-p w)
	(rplaca dims (nth 2 old-geom)))
      (when (window-maximized-vertically-p w)
	(rplacd dims (nth 3 old-geom)))
      dims))

  (define (maximize-discard w #!optional horizontally vertically)
    (when horizontally
      (window-put w 'maximized-horizontally nil))
    (when vertically
      (window-put w 'maximized-vertically nil))
    (let ((dims (window-dimensions w))
	  (coords (window-position w))
	  (saved (window-get w 'unmaximized-geometry)))
      (when saved
	(unless (window-maximized-horizontally-p w)
	  (rplaca saved (car coords))
	  (rplaca (nthcdr 2 saved) (car dims)))
	(unless (window-maximized-vertically-p w)
	  (rplaca (cdr saved) (cdr coords))
	  (rplaca (nthcdr 3 saved) (cdr dims))))
      (when (and (not (window-maximized-vertically-p w))
		 (not (window-maximized-horizontally-p w)))
	(window-put w 'unmaximized-geometry nil))))

  (define (maximize-discard-move w directions)
    (maximize-discard w (memq 'horizontal directions)
		      (memq 'vertical directions)))

  (define (maximize-discard-resize w edges)
    (maximize-discard w (or (memq 'left edges) (memq 'right edges))
		      (or (memq 'top edges) (memq 'bottom edges))))


;;; 1D packing

  (defmacro edges-touching (start end edge)
    `(> (- (min ,end (nth 2 ,edge)) (max ,start (nth 1 ,edge))) 0))

  (define (expand-edges start end minimum maximum perp-1 perp-2 edges)
    (if maximize-always-expands
	(progn
	  (mapc (lambda (edge)
		  ;; EDGE is (PERP START END OPEN-P)
		  (if (nth 3 edge)
		      (when (and (< (car edge) maximum)
				 (>= (car edge) perp-2)
				 (> (car edge) minimum)
				 (edges-touching start end edge))
			(setq maximum (car edge)))
		    (when (and (> (car edge) minimum)
			       (<= (car edge) perp-1)
			       (< (car edge) maximum)
			       (edges-touching start end edge))
		      (setq minimum (car edge)))))
		edges)
	  (cons minimum maximum))
      (let ((opening (sort (mapcar car (filter (lambda (e)
						 (and (nth 3 e)
						      (edges-touching
						       start end e)))
					       edges))))
	    (closing (sort (mapcar car (filter (lambda (e)
						 (and (not (nth 3 e))
						      (edges-touching
						       start end e)))
					       edges))))
	    max-perp
	    (max-size 0))
	;; find the maximum gap between a closing and an opening edge which
	;; doesn't include any other opening edges
	(while (and opening closing)
	  (while (and opening (<= (car opening) (car closing)))
	    (setq opening (cdr opening)))
	  (when (and (> (- (or (car opening) maximum) (car closing)) max-size)
		     (or (not (nth 1 closing))
			 (>= (nth 1 closing) (or (car opening) maximum))))
	    (setq max-perp (car closing))
	    (setq max-size (- (car opening) (car closing))))
	  (setq closing (cdr closing)))
	(if max-perp
	    (cons max-perp (+ max-perp max-size))
	  (cons perp-1 perp-2)))))

  (define (do-horizontal w edges coords dims fdims)
    (let ((head-offset (current-head-offset w))
	  (head-dims (current-head-dimensions w)))
      (let ((x-span (expand-edges (cdr coords) (+ (cdr coords) (cdr fdims))
				  (car head-offset)
				  (+ (car head-offset) (car head-dims))
				  (car coords) (+ (car coords) (car fdims))
				  (car edges))))
	(rplaca coords (car x-span))
	(rplaca dims (- (- (cdr x-span) (car x-span))
			(- (car fdims) (car dims))))
	w)))

  (define (do-vertical w edges coords dims fdims)
    (let ((head-offset (current-head-offset w))
	  (head-dims (current-head-dimensions w)))
      (let ((y-span (expand-edges (car coords) (+ (car coords) (car fdims))
				  (cdr head-offset)
				  (+ (cdr head-offset) (cdr head-dims))
				  (cdr coords) (+ (cdr coords) (cdr fdims))
				  (cdr edges))))
	(rplacd coords (car y-span))
	(rplacd dims (- (- (cdr y-span) (car y-span))
			(- (cdr fdims) (cdr dims))))
	w)))


;;; 2D packing

  (define (find-max-rectangle avoided edges #!optional head)
    (let* ((grid (grid-from-edges (car edges) (cdr edges)))
	   rects)
      (setq rects (rectangles-from-grid
		   (sort (car grid)) (sort (cdr grid))
		   (lambda (rect)
		     ;; the rectangle mustn't overlap any avoided windows
		     ;; or span multiple heads, or be on a different head
		     ;; to that requested
		     (catch 'foo
		       (mapc (lambda (w)
			       (when (or (> (rect-2d-overlap
					     (window-frame-dimensions w)
					     (window-position w)
					     rect) 0)
					 (/= (rectangle-heads rect) 1)
					 (and head (/= head (find-head (car rect) (cadr rect)))))
				 (throw 'foo nil)))
			     avoided)
		       t))))

      ;; find the largest rectangle
      (let ((max-area 0)
	    (max-rect nil))
	(mapc (lambda (rect)
		(when (and (rect-wholly-visible-p rect)
			   (> (rectangle-area rect) max-area))
		  (setq max-area (rectangle-area rect))
		  (setq max-rect rect))) rects)
	max-rect)))

  (define (do-both window avoided edges coords dims fdims)
    (let ((max-rect (find-max-rectangle avoided edges (current-head window))))
      (when max-rect
	(rplaca coords (nth 0 max-rect))
	(rplacd coords (nth 1 max-rect))
	(rplaca dims (- (- (nth 2 max-rect) (nth 0 max-rect))
			(- (car fdims) (car dims))))
	(rplacd dims (- (- (nth 3 max-rect) (nth 1 max-rect))
			(- (cdr fdims) (cdr dims))))
	window)))


;;; size hints stuff

  (define (window-maximizable-p w #!optional direction hints)
    (unless hints
      (setq hints (window-size-hints w)))
    (let ((max-width (cdr (assq 'max-width hints)))
	  (max-height (cdr (assq 'max-height hints)))
	  (dims (window-dimensions w)))
      (catch 'out
	(when (and (or (null direction) (eq direction 'horizontal))
		   max-width
		   (>= (car dims) max-width))
	  (throw 'out nil))
	(when (and (or (null direction) (eq direction 'vertical))
		   max-height
		   (>= (cdr dims) max-height))
	  (throw 'out nil))
	t)))

  (define (maximize-truncate-dims w dims #!optional direction hints)
    (unless hints
      (setq hints (window-size-hints w)))
    (let ((x-base (or (cdr (or (assq 'base-width hints)
			       (assq 'min-width hints))) 1))
	  (x-inc (or (cdr (assq 'width-inc hints)) 1))
	  (y-base (or (cdr (or (assq 'base-height hints)
			       (assq 'min-height hints))) 1))
	  (y-inc (or (cdr (assq 'height-inc hints)) 1))
	  (x-max (cdr (assq 'max-width hints)))
	  (y-max (cdr (assq 'max-height hints)))
	  
	  (trunc (lambda (x inc base #!optional maximum)
		   (min (+ base (max 0 (- (- x base) (mod (- x base) inc))))
			(or maximum 65535)))))
      (when (or (null direction) (eq direction 'horizontal))
	(rplaca dims (trunc (car dims) x-inc x-base x-max)))
      (when (or (null direction) (eq direction 'vertical))
	(rplacd dims (trunc (cdr dims) y-inc y-base y-max)))
      dims))


;;; misc functions

  (define (maximize-find-workarea #!optional w)
    "Return the rectangle representing the largest rectangle on the screen that
doesn't overlap any avoided windows, or nil."
    (let* ((avoided (avoided-windows w))
	   (edges (get-visible-window-edges
		   #:with-ignored-windows t
		   #:windows avoided
		   #:include-heads (list (current-head)))))
      (find-max-rectangle avoided edges (current-head w))))


;;; commands

  (define (maximize-window w #!optional direction only-1d)
    "Maximize the dimensions of the window."
    (let ((unshade-selected-windows t))
      (display-window-without-focusing w))
    (let* ((coords (window-position w))
	   (dims (window-dimensions w))
	   (fdims (window-frame-dimensions w))
	   (hints (window-size-hints w))
	   (avoided (and maximize-avoid-avoided (avoided-windows w)))
	   (edges (get-visible-window-edges
		   #:with-ignored-windows t
		   #:windows avoided
		   #:include-heads (list (current-head)))))
      (when (window-maximizable-p w direction hints)
	(unless (window-get w 'unmaximized-geometry)
	  (window-put w 'unmaximized-geometry (list (car coords) (cdr coords)
						    (car dims) (cdr dims))))
	(cond ((null direction)
	       (if (not only-1d)
		   (do-both w avoided edges coords dims fdims)
		 (do-horizontal w edges coords dims fdims)
		 (do-vertical w edges coords dims fdims))
	       (window-put w 'maximized-horizontally t)
	       (window-put w 'maximized-vertically t))
	      ((eq direction 'horizontal)
	       (do-horizontal w edges coords dims fdims)
	       (window-put w 'maximized-horizontally t))
	      ((eq direction 'vertical)
	       (do-vertical w edges coords dims fdims)
	       (window-put w 'maximized-vertically t)))
	(maximize-truncate-dims w dims direction hints)
	(move-resize-window-to w (car coords) (cdr coords)
			       (car dims) (cdr dims))
	(when maximize-raises
	  (raise-window* w))
	(call-window-hook 'window-maximized-hook w (list direction))
	(call-window-hook 'window-state-change-hook w (list '(maximized))))))

  (define (unmaximize-window w #!optional direction)
    "Restore the dimensions of the window to its original, unmaximized, state."
    (let ((geom (window-get w 'unmaximized-geometry))
	  (coords (window-position w))
	  (dims (window-dimensions w)))
      (when geom
	(when (or (null direction) (eq direction 'horizontal))
	  (rplaca coords (nth 0 geom))
	  (rplaca dims (nth 2 geom))
	  (window-put w 'maximized-horizontally nil))
	(when (or (null direction) (eq direction 'vertical))
	  (rplacd coords (nth 1 geom))
	  (rplacd dims (nth 3 geom))
	  (window-put w 'maximized-vertically nil))
	(when (and (not (window-maximized-vertically-p w))
		   (not (window-maximized-horizontally-p w)))
	  (window-put w 'unmaximized-geometry nil))
	(move-resize-window-to w (car coords) (cdr coords)
			       (car dims) (cdr dims))
	(call-window-hook 'window-unmaximized-hook w (list direction))
	(call-window-hook 'window-state-change-hook w (list '(maximized))))))

  (define (maximize-window-vertically w)
    "Maximize the vertical dimension of the window."
    (maximize-window w 'vertical))

  (define (maximize-window-horizontally w)
    "Maximize the horizontal dimension of the window."
    (maximize-window w 'horizontal))

  (define (maximize-window-toggle w #!optional direction)
    "Toggle the state of the window between maximized and unmaximized."
    (if (window-maximized-p w)
	(unmaximize-window w direction)
      (maximize-window w direction)))

  (define (maximize-window-vertically-toggle w)
    "Toggle the state of the window between vertically maximized and
unmaximized."
    (if (window-maximized-vertically-p w)
	(unmaximize-window w 'vertical)
      (maximize-window w 'vertical)))

  (define (maximize-window-horizontally-toggle w)
    "Toggle the state of the window between horizontally maximized and
unmaximized."
    (if (window-maximized-horizontally-p w)
	(unmaximize-window w 'horizontal)
      (maximize-window w 'horizontal)))

  ;;###autoload
  (define-command 'maximize-window maximize-window #:spec "%W")
  (define-command 'unmaximize-window unmaximize-window #:spec "%W")
  (define-command 'maximize-window-vertically maximize-window-vertically #:spec "%W")
  (define-command 'maximize-window-horizontally maximize-window-horizontally #:spec "%W")
  (define-command 'maximize-window-toggle maximize-window-toggle #:spec "%W")
  (define-command 'maximize-window-horizontally-toggle maximize-window-horizontally-toggle #:spec "%W")
  (define-command 'maximize-window-vertically-toggle maximize-window-vertically-toggle #:spec "%W")


;;; fill commands

  (define (maximize-fill-window w #!optional direction)
    "Maximize the window without obscuring any other windows."
    (let ((avoid-by-default t)
	  (maximize-always-expands t)
	  (dont-avoid-ignored maximize-ignore-when-filling))
      (maximize-window w direction t)))

  (define (maximize-fill-window-vertically w)
    "Maximize the window vertically without obscuring any other windows."
    (maximize-fill-window w 'vertical))

  (define (maximize-fill-window-horizontally w)
    "Maximize the window horizontally without obscuring any other windows."
    (maximize-fill-window w 'horizontal))

  (define (maximize-fill-window-toggle w #!optional direction)
    "Toggle the state of the window between maximized-filled and unmaximized."
    (if (window-maximized-p w)
	(unmaximize-window w direction)
      (maximize-fill-window w direction)))

  (define (maximize-fill-window-vertically-toggle w)
    "Toggle the state of the window between vertically maximized-filled and
unmaximized."
    (if (window-maximized-vertically-p w)
	(unmaximize-window w 'vertical)
      (maximize-fill-window w 'vertical)))

  (define (maximize-fill-window-horizontally-toggle w)
    "Toggle the state of the window between horizontally maximized-filled and
unmaximized."
    (if (window-maximized-horizontally-p w)
	(unmaximize-window w 'horizontal)
      (maximize-fill-window w 'horizontal)))

  ;;###autoload
  (define-command 'maximize-fill-window maximize-fill-window #:spec "%W")
  (define-command 'maximize-fill-window-vertically maximize-fill-window-vertically #:spec "%W")
  (define-command 'maximize-fill-window-horizontally maximize-fill-window-horizontally #:spec "%W")
  (define-command 'maximize-fill-window-toggle maximize-fill-window-toggle #:spec "%W")
  (define-command 'maximize-fill-window-horizontally-toggle maximize-fill-window-horizontally-toggle #:spec "%W")
  (define-command 'maximize-fill-window-vertically-toggle maximize-fill-window-vertically-toggle #:spec "%W")


;;; initialisation

  (sm-add-saved-properties
   'unmaximized-geometry 'maximized-vertically 'maximized-horizontally)
  (add-swapped-properties
   'unmaximized-geometry 'maximized-vertically 'maximized-horizontally)

  (add-hook 'after-move-hook maximize-discard-move)
  (add-hook 'after-resize-hook maximize-discard-resize)

  (gaol-add window-maximized-p window-maximized-horizontally-p
	    window-maximized-vertically-p))
