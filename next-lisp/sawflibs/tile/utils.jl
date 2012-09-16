;; Utilities for window placement and tiling

(define-structure sawflibs.tile.utils
    (export current-workspace
            window-workspace
            workspace-windows
            window-type
            window-x
            window-y
            window-width
            window-height
            scr-height
            scr-width
            push-window
            resize-frame-to
            input-focus
            focus-window
            align-workspace-windows
            notify)
    (open rep
          rep.io.timers
          sawfish.wm.stacking
          sawfish.wm.state.ignored
	  sawfish.wm.windows
	  sawfish.wm.frames
	  sawfish.wm.placement
	  sawfish.wm.workspace
          sawfish.wm.util.window-order
          sawfish.wm.misc)

  (define (window-workspace w) (car (window-get w 'workspaces)))

  (define (workspace-windows #!optional ignore)
    (remove-if (lambda (w) (or (equal w ignore) (window-ignored-p w)))
               (window-order current-workspace)))

  (define (window-x w) (car (window-position w)))
  (define (window-y w) (cdr (window-position w)))
  (define (window-width w) (car (window-frame-dimensions w)))
  (define (window-height w) (cdr (window-frame-dimensions w)))
  (define (scr-height #!optional (tm 0) (bm 0)) (- (screen-height) tm bm))
  (define (scr-width #!optional (l 0) (r 0) (g 0)) (- (screen-width) l r g))

  (define (resize-frame-to w width height)
    (let ((width-offset (- (car (window-frame-dimensions w))
                           (car (window-dimensions w))))
          (height-offset (- (cdr (window-frame-dimensions w))
                            (cdr (window-dimensions w)))))
      (resize-window-to w (- width width-offset) (- height height-offset))))

  (define (push-window w x y width height)
    (resize-frame-to w width height)
    (move-window-to w x y)
    (window-order-push w))

  (define (focus-window w) (window-order-push w))

  (define (align-workspace-windows)
    (interactive)
    (let* ((fw (input-focus))
           (windows (workspace-windows fw))
           (x (window-x fw))
           (y (window-y fw)))
      (mapc (lambda (w)
              (push-window w x y (window-width w) (window-height w)))
            windows)))

  (define %hide-timer (make-timer (lambda (tm) (display-message nil))))

  (define (notify fmt #!rest args)
    (display-message (apply format (cons nil (cons fmt args))))
    (set-timer %hide-timer 1)))

