;; tile.jl --- Tile Sawfish windows in an intelligent (hah!) fashion
;;
;; Description: Handles tiling of all windows on the current workspace.
;; Windows are automatically sized and arranged to make best use of the
;; workspace without overlapping.
;;
;; Author: Mark Triggs <mst@dishevelled.net>
;; Maintainer: Christopher Roy Bratusek <nano@tuxfamily.org>

(define-structure sawfish.wm.edge.tile

    (export tile-windows
	    tile-windows-core
	    window-never-tile-p)

    (open rep
	  rep.data
	  rep.system
	  sawfish.wm.commands
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.state.iconify
	  sawfish.wm.state.maximize
	  sawfish.wm.state.ignored
	  sawfish.wm.commands.move-resize)

  (define-structure-alias tile sawfish.wm.edge.tile)

  (defvar min-height (/ (screen-height) 4)
    "For windows at least, wideness is preferable to tallness. So, the minimum
height specifies how short you are willing to tolerate your windows before
an extra column should be used.")

  ;; macro for tile-windows
  (defmacro pop (l) `(setq ,l (cdr ,l)))

  (define (window-never-tile-p w) (window-get w 'never-tile))

  (define (tile-windows-core edge while-moving)
    (interactive)
    (call-hook 'before-edge-action-hook (list 'tile-windows edge while-moving))
    (when while-moving
        (fake-release-window))
    (let ((windows (remove-if (or window-iconified-p
			          window-ignored-p
			          window-never-tile-p)
			      (or (filter-windows window-on-current-workspace-viewport-p)
			          (filter-windows window-on-current-head-viewport-p)))))
      (when (> (length windows) 1)
	(let* ((columns (do ((c 1 (1+ c)))
			    ((> (/ (screen-height) (/ (length windows) c))
				min-height)
			    c)))
	      (rows (ceiling (/ (length windows) columns)))
	      (width (floor (/ (screen-width) columns)))
	      (height (floor (/ (screen-height) rows))))
	  (let ((w windows))
	    (do ((x 0 (1+ x)))
		((or (>= x columns) (null w)) nil)
	      (do ((y 0 (1+ y)))
		  ((or (>= y rows) (null w) nil))
		(if (window-maximized-p (car w))
		    (unmaximize-window (car w)))
		(resize-window-frame-to (car w) width height)
		(move-window-to (car w) (+ (* x width) 2) (* y height))
		(pop w))))
	  ;; Resize the windows horizontally to use any remaining space.
	  (mapc (lambda (w)
		  (let ((old-width (window-width w))
			(old-height (window-height w))
			(new-width (- (screen-width) (window-x w))))
		    (resize-window-frame-to w new-width old-height)
		    (when (window-touching-p w)
		      ;; oops. Roll back to the original size
		      (resize-window-frame-to w old-width old-height))))
		windows)))
      (call-hook 'after-edge-action-hook (list 'tile-windows edge while-moving))))

  (define (tile-windows)
    (tile-windows-core nil nil))

  (define-command 'tile-windows tile-windows))
