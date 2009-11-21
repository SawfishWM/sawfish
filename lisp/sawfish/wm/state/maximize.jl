;; maximize.jl -- window maximization

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

(define-structure sawfish.wm.state.maximize

    (export window-maximized-p
	    window-maximized-horizontally-p
	    window-maximized-vertically-p
	    window-maximized-fullscreen-p
	    window-unmaximized-position
	    window-unmaximized-dimensions
	    window-maximizable-p
	    frame-part-movable-p
	    maximize-truncate-dims
	    maximize-find-workarea
	    window-locked-vertically-p
	    window-locked-horizontally-p
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
	    maximize-fill-window-horizontally-toggle
	    maximize-window-fullscreen
	    maximize-window-fullscreen-toggle
	    maximize-window-fullxinerama
	    maximize-window-fullxinerama-toggle)

    (open rep
	  rep.system
	  sawfish.wm.util.edges
	  sawfish.wm.util.rects
	  sawfish.wm.util.workarea
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.gaol
	  sawfish.wm.session.init
	  sawfish.wm.workspace
	  sawfish.wm.util.display-window
	  sawfish.wm.util.stacking
	  sawfish.wm.frames
	  sawfish.wm.misc
	  sawfish.wm.focus
          sawfish.wm.viewport
          sawfish.wm.state.shading
	  sawfish.wm.util.prompt)

  (define-structure-alias maximize sawfish.wm.state.maximize)

  (defvar maximize-always-expands nil
    "Maximizing a window in one dimension must increase the size of
that dimension.")

  (defvar maximize-raises t
    "Raise windows when they are maximized.")

  (defvar maximize-ignore-when-filling t
    "Let unmanaged windows be overlapped when filling windows.")

  (defvar maximize-avoid-avoided t
    "Don't cover `avoided' windows when maximizing.")

  (defcustom move-lock-when-maximized t
    "Lock position and size while windows are maximized."
    :type boolean
    :group min-max)

  ;; called when a window is maximized, args (W #!optional DIRECTION)
  (defvar window-maximized-hook nil)

  ;; called when a window is un-maximized, args (W #!optional DIRECTION)
  (defvar window-unmaximized-hook nil)

;;; handling maximized state

  (define (window-maximized-p w)
    (window-get w 'unmaximized-geometry))

  (define (window-maximized-fullscreen-p w)
    (window-get w 'maximized-fullscreen))

  (define (window-maximized-horizontally-p w)
    (window-get w 'maximized-horizontally))

  (define (window-maximized-vertically-p w)
    (window-get w 'maximized-vertically))

  (define (window-unmaximized-position w)
    (let ((coords (window-position w))
	  (old-geom (unmaximized-geometry w)))
      (when (window-maximized-horizontally-p w)
	(rplaca coords (nth 0 old-geom)))
      (when (window-maximized-vertically-p w)
	(rplacd coords (nth 1 old-geom)))
      coords))

  (define (window-unmaximized-dimensions w)
    (let ((dims (window-dimensions w))
	  (old-geom (unmaximized-geometry w)))
      (when (window-maximized-horizontally-p w)
	(rplaca dims (nth 2 old-geom)))
      (when (window-maximized-vertically-p w)
	(rplacd dims (nth 3 old-geom)))
      dims))

  ;; This sets the window property `unmaximized-geometry' of each
  ;; currently maximize window to `(X Y W H)', the saved geometry.
  (define (save-unmaximized-geometry w)
    (unless (window-get w 'unmaximized-geometry)
      (let* ((coords (window-relative-position w))
             (dims (window-dimensions w)))
	(window-put w 'unmaximized-geometry (list (car coords) (cdr coords)
						  (car dims) (cdr dims))))))

  (define (discard-unmaximized-geometry w)
    (window-put w 'unmaximized-geometry nil)
    (pop-window-type w 'sawfish.wm.state.maximize))

  (define (unmaximized-geometry w)
    (window-get w 'unmaximized-geometry))

  (define (maximize-discard w #!optional horizontally vertically)
    (when horizontally
      (window-put w 'maximized-horizontally nil))
    (when vertically
      (window-put w 'maximized-vertically nil))
    (let ((dims (window-dimensions w))
	  (coords (window-relative-position w))
	  (saved (unmaximized-geometry w)))
      (when saved
	(unless (window-maximized-horizontally-p w)
	  (rplaca saved (car coords))
	  (rplaca (nthcdr 2 saved) (car dims)))
	(unless (window-maximized-vertically-p w)
	  (rplaca (cdr saved) (cdr coords))
	  (rplaca (nthcdr 3 saved) (cdr dims))))
      (when (and (not (window-maximized-vertically-p w))
		 (not (window-maximized-horizontally-p w)))
	(discard-unmaximized-geometry w))))

  (define (maximize-discard-move w directions #!key successful)
    (when successful
      (maximize-discard w (memq 'horizontal directions)
			(memq 'vertical directions))))

  (define (maximize-discard-resize w edges #!key successful)
    (when successful
      (maximize-discard w (or (memq 'left edges) (memq 'right edges))
			(or (memq 'top edges) (memq 'bottom edges)))))

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
    (let* ((head (window-head-any-viewport w))
           (head-offset (head-offset head))
           (head-dims (head-dimensions head)))
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
    (let* ((head (window-head-any-viewport w))
           (head-offset (head-offset head))
           (head-dims (head-dimensions head)))
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

  (define (do-both window avoided edges coords dims fdims)
    (let ((max-rect (largest-rectangle-from-edges
		     edges
                     #:avoided avoided
                     #:head (window-head-any-viewport window))))
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
    (if (window-get w 'never-maximize)
	nil
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
	  t))))

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

  (define (maximize-find-workarea #!optional w #!key head head-fallback)
    "This function is deprecated. Use calculate-workarea instead."
    (declare (unused head-fallback))
    (calculate-workarea #:window w #:head head))

  (define (window-locked-vertically-p w)
    (and move-lock-when-maximized
	 (window-maximized-vertically-p w)))

  (define (window-locked-horizontally-p w)
    (and move-lock-when-maximized
	 (window-maximized-horizontally-p w)))

  (define (frame-part-movable-p w part)
    (if (not move-lock-when-maximized)
	t
      (let ((h-maximized (window-maximized-horizontally-p w))
	    (v-maximized (window-maximized-vertically-p w)))
	(case part
	  ((top-border bottom-border) (not v-maximized))
	  ((left-border right-border) (not h-maximized))
	  ((top-left-corner top-right-corner
                            bottom-left-corner bottom-right-corner title)
	   (not (and h-maximized v-maximized)))))))

;;; commands

  (define (maximize-window w #!optional direction only-1d)
    "Maximize the dimensions of the window."
    (when (window-get w 'shaded)
      (unshade-window w))
    (when (window-maximized-fullscreen-p w)
      (maximize-window-fullscreen w nil))
    (let* ((viewport (window-viewport w))
           (coords (window-position w))
           (head (window-head-any-viewport w))
           (dims (window-dimensions w))
           (fdims (window-frame-dimensions w))
           (hints (window-size-hints w))
           (avoided (and maximize-avoid-avoided (avoided-windows w)))
           (edges (get-visible-window-edges
                   #:with-ignored-windows t
                   #:windows avoided
                   #:include-heads (list head)
                   #:viewport viewport)))
      (when (window-maximizable-p w direction hints)
        (save-unmaximized-geometry w)
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
        (call-window-hook 'window-state-change-hook w
                          (list '(maximized))))))

  ;; does all unmaximizing except for changing the window properties and
  ;; calling the hooks
  (define (unmaximize-window-1 w #!optional direction before)
    (let ((vp-offset (viewport-offset-pixel (window-viewport w)))
          (geom (unmaximized-geometry w))
	  (coords (window-position w))
	  (dims (window-dimensions w)))
      (when geom
	(when (memq direction '(() fullscreen horizontal))
	  (rplaca coords (+ (nth 0 geom) (car vp-offset)))
	  (rplaca dims (nth 2 geom)))
	(when (memq direction '(() fullscreen vertical))
	  (rplacd coords (+ (nth 1 geom) (cdr vp-offset)))
	  (rplacd dims (nth 3 geom)))
	(when before
	  (before))
	(move-resize-window-to w (car coords) (cdr coords)
			       (car dims) (cdr dims)))))

  (define (unmaximize-window w #!optional direction)
    "Restore the dimensions of the window to its original,
unmaximized, state."
    (unmaximize-window-1 w direction
                         (lambda ()
                           (when (memq direction '(() fullscreen horizontal))
                             (window-put w 'maximized-horizontally nil))
                           (when (memq direction '(() fullscreen vertical))
                             (window-put w 'maximized-vertically nil))
                           (window-put w 'maximized-fullscreen nil)
                           (when (and (not (window-maximized-vertically-p w))
                                      (not (window-maximized-horizontally-p w))
                                      (not (window-maximized-fullscreen-p w)))
                             (discard-unmaximized-geometry w))))
    (warp-pointer-if-necessary w)
    (call-window-hook 'window-unmaximized-hook w (list direction))
    (call-window-hook 'window-state-change-hook w (list '(maximized))))

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
  (define-command 'maximize-window
    maximize-window #:spec "%W")
  (define-command 'unmaximize-window
    unmaximize-window #:spec "%W")
  (define-command 'maximize-window-vertically
    maximize-window-vertically #:spec "%W")
  (define-command 'maximize-window-horizontally
    maximize-window-horizontally #:spec "%W")
  (define-command 'maximize-window-toggle
    maximize-window-toggle #:spec "%W")
  (define-command 'maximize-window-horizontally-toggle
    maximize-window-horizontally-toggle #:spec "%W")
  (define-command 'maximize-window-vertically-toggle
    maximize-window-vertically-toggle #:spec "%W")

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
  (define-command 'maximize-fill-window
    maximize-fill-window #:spec "%W")
  (define-command 'maximize-fill-window-vertically
    maximize-fill-window-vertically #:spec "%W")
  (define-command 'maximize-fill-window-horizontally
    maximize-fill-window-horizontally #:spec "%W")
  (define-command 'maximize-fill-window-toggle
    maximize-fill-window-toggle #:spec "%W")
  (define-command 'maximize-fill-window-horizontally-toggle
    maximize-fill-window-horizontally-toggle #:spec "%W")
  (define-command 'maximize-fill-window-vertically-toggle
    maximize-fill-window-vertically-toggle #:spec "%W")

  ;; fullscreen commands

  (define (maximize-window-fullscreen w state)
    "Fullscreen maximize the window."
    (cond ((and state (not (window-maximized-fullscreen-p w)))
	   (when (window-maximizable-p w)
             (let* ((viewport (window-viewport w))
                    (vp-offset (viewport-offset-pixel viewport))
                    (head (window-head-any-viewport w))
                    (head-offset (head-offset head))
                    (head-dims (head-dimensions head)))
	       (save-unmaximized-geometry w)
	       (window-put w 'unmaximized-type (window-type w))
	       (push-window-type w 'unframed 'sawfish.wm.state.maximize)
	       (move-resize-window-to w
                                      (+ (car head-offset) (car vp-offset))
                                      (+ (cdr head-offset) (cdr vp-offset))
                                      (car head-dims) (cdr head-dims))
	       (raise-window* w)
	       (window-put w 'maximized-fullscreen t)
	       (window-put w 'maximized-vertically t)
	       (window-put w 'maximized-horizontally t)
	       (call-window-hook 'window-maximized-hook
				 w (list 'fullscreen))
	       (call-window-hook 'window-state-change-hook
				 w (list '(maximized))))))

	  ((and (not state) (window-maximized-fullscreen-p w))
	   (unmaximize-window w 'fullscreen))))

  (define (maximize-window-fullscreen-toggle w)
    "Toggle the state of the window between fullscreen maximized and
unmaximized."
    (maximize-window-fullscreen w (not (window-maximized-fullscreen-p w))))

  (define-command 'maximize-window-fullscreen
    maximize-window-fullscreen #:spec "%W
t")
  (define-command 'maximize-window-fullscreen-toggle
    maximize-window-fullscreen-toggle #:spec "%W")

  (define (maximize-window-fullxinerama w state)
    "Fullscreen maximize the window across all Xinerama screens."
    (cond ((and state (not (window-maximized-fullscreen-p w)))
	   (when (window-maximizable-p w)
	     (let ((screen-dims (screen-dimensions))
                   (vp-offset (viewport-offset-pixel (window-viewport w))))
	       (save-unmaximized-geometry w)
	       (window-put w 'unmaximized-type (window-type w))
	       (push-window-type w 'unframed 'sawfish.wm.state.maximize)
	       (move-resize-window-to w
                                      (car vp-offset)
                                      (cdr vp-offset)
                                      (+ (car screen-dims) (car vp-offset))
                                      (+ (cdr screen-dims) (cdr vp-offset)))
	       (raise-window* w)
	       (window-put w 'maximized-fullscreen t)
	       (window-put w 'maximized-vertically t)
	       (window-put w 'maximized-horizontally t)
	       (call-window-hook 'window-maximized-hook
				 w (list 'fullscreen))
	       (call-window-hook 'window-state-change-hook
				 w (list '(maximized))))))

	  ((and (not state) (window-maximized-fullscreen-p w))
	   (unmaximize-window w 'fullscreen))))

  (define (maximize-window-fullxinerama-toggle w)
    "Toggle the state of the window between fullscreen maximized
across all Xinerama and unmaximized."
    (maximize-window-fullxinerama w (not (window-maximized-fullscreen-p w))))

  (define-command 'maximize-window-fullxinerama
    maximize-window-fullxinerama #:spec "%W
t")
  
  (define-command 'maximize-window-fullxinerama-toggle
    maximize-window-fullxinerama-toggle #:spec "%W")

;;; initialisation

  (define (maximize-after-add-window w)
    (let ((vert (window-get w 'queued-vertical-maximize))
	  (horiz (window-get w 'queued-horizontal-maximize))
	  (full (window-get w 'queued-fullscreen-maximize))
          (fullxinerama (window-get w 'queued-fullxinerama-maximize)))
      (when (or vert horiz full fullxinerama)
	(when vert
	  (window-put w 'queued-vertical-maximize nil))
	(when horiz
	  (window-put w 'queued-horizontal-maximize nil))
	(when full
	  (window-put w 'queued-fullscreen-maximize nil))
        (when fullxinerama
          (window-put w 'queued-fullxinerama-maximize nil))
        (cond
         (full
          (maximize-window-fullscreen w t))
         (fullxinerama
          (maximize-window-fullxinerama w t))
         (t
          (maximize-window w (cond ((and vert horiz) nil)
                                   (vert 'vertical)
				   (horiz 'horizontal))))))))

  (add-hook 'after-add-window-hook maximize-after-add-window)

  ;; before exiting, return all windows to their unmaximized
  ;; geometries. But _don't_ change any of the properties (either
  ;; wm-local or X) that mark the window as being maximized
  (add-hook 'before-exit-hook (lambda () (map-windows unmaximize-window-1)))

  (define (check-if-maximizable w)
    (let ((maximizable (window-maximizable-p w)))
      (when (and (not maximizable)
		 (not (frame-class-removed-p w 'maximize-button)))
	(remove-frame-class w 'maximize-button)
	(window-put w 'maximize-removed-maximize-button t))
      (when (and maximizable
		 (window-get w 'maximize-removed-maximize-button))
	(add-frame-class w 'maximize-button)
	(window-put w 'maximize-removed-maximize-button nil))))

  (define (property-notify w prop type)
    (declare (unused type))
    (when (eq prop 'WM_NORMAL_HINTS)
      (check-if-maximizable w)))

  (add-hook 'property-notify-hook property-notify)

  (add-hook 'after-initialization-hook
	    (lambda ()
	      (map-windows check-if-maximizable)
	      ;; Don't install this hook until after all windows have
	      ;; initially been adopted, to avoid maximizing over
	      ;; avoided windows
	      (add-hook 'add-window-hook check-if-maximizable)))

  (add-hook 'sm-window-save-functions
	    (lambda (w)
	      (if (window-maximized-fullscreen-p w)
		  (list '(maximized-fullscreen . t))
		(nconc (and (window-maximized-horizontally-p w)
			    (list '(maximized-horizontally . t)))
		       (and (window-maximized-vertically-p w)
			    (list '(maximized-vertically . t)))))))

  (add-hook 'sm-restore-window-hook
	    (lambda (w alist)
	      (mapc (lambda (cell)
		      (let ((v (cdr (assq (car cell) alist))))
			(when v
			  (window-put w (cdr cell) v))))
		    '((maximized-fullscreen . queued-fullscreen-maximize)
		      (maximized-vertically . queued-vertical-maximize)
		      (maximized-horizontally . queued-horizontal-maximize)))))

  (sm-add-saved-properties 'unmaximized-geometry)

  (add-swapped-properties
   'unmaximized-geometry 'maximized-vertically
   'maximized-horizontally 'maximized-fullscreen)

  ;; This is now disabled - it doesn't really make sense for moving..
  ;; (add-hook 'after-move-hook maximize-discard-move)

  (add-hook 'after-resize-hook maximize-discard-resize)

  (gaol-add window-maximized-p window-maximized-horizontally-p
	    window-maximized-vertically-p))
