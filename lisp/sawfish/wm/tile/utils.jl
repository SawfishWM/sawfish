;; Utilities for window placement and tiling

(define-structure sawfish.wm.tile.utils
    (export current-workspace
            window-workspace
            tileable-workspace-windows
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
            notify
            take
            group-by)
    (open rep
          rep.io.timers
          sawfish.wm.stacking
          sawfish.wm.state.ignored
	  sawfish.wm.state.iconify
	  sawfish.wm.ext.expose
	  sawfish.wm.windows
	  sawfish.wm.frames
	  sawfish.wm.placement
	  sawfish.wm.workspace
          sawfish.wm.util.window-order
          sawfish.wm.misc)

  (define (window-workspace w) (car (window-get w 'workspaces)))

  (define (tileable-workspace-windows #!optional ignore)
    (remove-if (lambda (w)
		 (or (equal w ignore)
		     (dock-window-p w)
		     (window-iconified-p w)
		     ;; window-matcher
		     (window-never-expose-p w)
		     ;; for pager and stuff
		     (not (window-visible-p w))
		     (window-ignored-p w)))
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
    (resize-frame-to w (inexact->exact width) (inexact->exact height))
    (move-window-to w (inexact->exact x) (inexact->exact y))
    (window-order-push w))

  (define (focus-window w) (window-order-push w))

  (define (align-workspace-windows)
    (interactive)
    (let* ((fw (input-focus))
           (windows (tileable-workspace-windows fw))
           (x (window-x fw))
           (y (window-y fw)))
      (mapc (lambda (w)
              (push-window w x y (window-width w) (window-height w)))
            windows)))

  (define %hide-timer (make-timer (lambda () (display-message nil))))

  (define (notify fmt #!rest args)
    (display-message (apply format (cons nil (cons fmt args))))
    (set-timer %hide-timer 1))

  (define (take n l)
    (cond ((null l) '())
          ((<= n 0) '())
          ((= 1 n) (list (car l)))
          (t (cons (car l) (take (- n 1) (cdr l))))))

  (define (group-by ws n)
    (cond ((null ws) ws)
          ((< n 1) (list ws))
          ((<= (length ws) n) (list ws))
          (t (cons (take n ws) (group-by (nthcdr n ws) n))))))

