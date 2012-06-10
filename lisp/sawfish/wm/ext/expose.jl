;; expose.jl --- Tile Sawfish windows in an intelligent (hah!) fashion
;;
;; Description: Handles tiling of all windows on the current workspace.
;; Windows are automatically sized and arranged to make best use of the
;; workspace without overlapping.
;;
;; Author: Mark Triggs <mst@dishevelled.net>
;; Maintainer: Christopher Roy Bratusek <nano@tuxfamily.org>

(define-structure sawfish.wm.ext.expose

    (export expose-windows
            expose-windows-vertically
            expose-windows-horizontally
  	    window-never-expose-p)

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

  (define-structure-alias expose sawfish.wm.ext.expose)

  (defvar min-height (/ (screen-height) 4)
    "For windows at least, wideness is preferable to tallness. So, the minimum
height specifies how short you are willing to tolerate your windows before
an extra column should be used.")

  (define (window-never-expose-p w) (window-get w 'never-expose))

  (defvar top-panel-height 0
   "This var will be considered when using tile commands offered by hqw-util")

  (defvar bottom-panel-height 0
    "This var will be considered when using tile commands offered by hqw-util")

  (define (expose-windows-vertically)
    (lambda ()
      (let (
            (wins (workspace-windows current-workspace)))
         (expose-windows wins t
                    #:top-left-corner `(0 . ,top-panel-height)
                    #:height-for-tile (- (screen-height) (+ top-panel-height bottom-panel-height))))))

  (define (expose-windows-horizontally)
    (lambda ()
      (let (
            (wins (workspace-windows current-workspace)))
         (expose-windows wins nil
                    #:top-left-corner `(0 . ,top-panel-height)
                    #:height-for-tile (- (screen-height)
                                         (+ top-panel-height bottom-panel-height))))))

  (define (expose-windows wins #!optional
                      vertically
                      #!key
                      (width-for-tile (screen-width))
                      (height-for-tile (screen-height))
                      (top-left-corner '(0 . 0)))
   "To tile windows."
   (let* (
         (windows (remove-if (or window-iconified-p
                                 window-never-expose-p
                                 window-ignored-p)
                               (or (filter-windows window-on-current-workspace-viewport-p)
                                   (filter-windows window-on-current-head-viewport-p))))
         (len (length windows))
         (border-width (- (car (window-frame-dimensions (car windows)))
                          (car (window-dimensions (car windows)))))
         (border-height (- (cdr (window-frame-dimensions (car windows)))
                           (cdr (window-dimensions (car windows)))))
         (width-step (ceiling (/  width-for-tile
                                  len)))
         (height-step (ceiling (/ height-for-tile
                                  len))))
      (do (
           (w (car windows) (progn (setq windows (cdr windows)) (car windows)))
           (x (car top-left-corner) (if vertically
                                          x
                                       (setq x (+ x width-step))))
           (y (cdr top-left-corner) (if vertically
                                          (setq y (+ y height-step))
                                       y))
           (win-width (if vertically
                            (- width-for-tile border-width)
                         (- width-step border-width)))
           (win-height (if vertically
                             (- height-step border-height)
                           (- height-for-tile border-height)))
           )
            ((null wins) )
         (move-resize-window-to w x y win-width win-height))))

  (define-command 'expose-windows expose-windows))
