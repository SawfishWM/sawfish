

(define-structure nokogiri-widgets/workspace-geometry ()

    (open rep
	  gtk
	  gnome-canvas
	  gnome-canvas-pixbuf
	  gdk-pixbuf
	  nokogiri-widget
	  nokogiri-layout)

  (defconst canvas-width 100)
  (defconst canvas-height 100)

  (defconst monitor-pixbuf-file "/usr/share/pixmaps/monitor.png")	;XXX

  (define monitor-pixbuf (gdk-pixbuf-new-from-file monitor-pixbuf-file))

  (define (make-workspace-geometry-item changed-callback &optional doc)
    (let* ((canvas (gnome-canvas-new))
	   (vbox (gtk-vbox-new nil box-spacing))
	   (hbox (gtk-hbox-new nil box-spacing))
	   canvas-items widget)

      (define (erase-canvas)
	(mapc gtk-object-destroy canvas-items)
	(setq canvas-items '()))

      (define (draw-line x1 y1 x2 y2)
	(let ((item (gnome-canvas-item-new (gnome-canvas-root canvas)
					   'GnomeCanvasLine
					   `(points ,(list x1 y1 x2 y2)
					     fill_color "black"
					     line_style on-off-dash))))
	  (setq canvas-items (cons item canvas-items))
	  item))

      (define (draw-rect x1 y1 x2 y2)
	(let ((item (gnome-canvas-item-new (gnome-canvas-root canvas)
					   'GnomeCanvasRect
					   (list 'x1 x1 'y1 y1
						 'x2 x2 'y2 y2
						 'fill_color "grey90"
						 'outline_color "black"))))
	  (setq canvas-items (cons item canvas-items))
	  item))

      (define (draw-image pixbuf x y)
	(let ((item (gnome-canvas-item-new (gnome-canvas-root canvas)
					   'GnomeCanvasPixbuf
					   (list 'pixbuf pixbuf
						 'x x 'y y))))
	  (setq canvas-items (cons item canvas-items))
	  item))

      (define (draw-canvas)
	(let* ((dims (widget-ref widget))
	       (spaces (car dims))
	       (port-dims (cdr dims)))
	  (let* ((space-step 10)
		 (space-size (- canvas-width (* (1- spaces) space-step)))
		 (x-step (floor (/ space-size (car port-dims))))
		 (y-step (floor (/ space-size (cdr port-dims)))))
	    (do ((i (1- spaces) (1- i)))
		((< i 0))
	      (draw-rect (* i space-step)
			 (* i space-step)
			 (+ (* i space-step) space-size)
			 (+ (* i space-step) space-size)))
	    (do ((i 1 (1+ i)))
		((= i (car port-dims)))
	      (draw-line (* i x-step) 0
			 (* i x-step) space-size))
	    (do ((i 1 (1+ i)))
		((= i (cdr port-dims)))
	      (draw-line 0 (* i y-step)
			 space-size (* i y-step)))
	    (draw-image (gdk-pixbuf-scale-simple monitor-pixbuf
						 (- x-step 4) (- y-step 4)
						 'bilinear) 2 2))))

      (define (update-canvas)
	(erase-canvas)
	(draw-canvas))

      (define (changed)
	(call-callback changed-callback)
	(update-canvas))

      (setq widget (make-widget
		    `(pair (labelled ,(_ "Workspaces:")
				     (number 1))
			   (pair (labelled ,(_ "X:") (number 1))
				 (labelled ,(_ "Y:") (number 1))) t)
		    changed))

      (gtk-widget-set-usize canvas (+ canvas-width 10) (+ canvas-height 10))
      (gnome-canvas-set-scroll-region canvas 0 0 canvas-width canvas-height)

      (when doc
	(gtk-container-add vbox (make-label (remove-newlines doc))))
      (gtk-container-add vbox (widget-gtk-widget widget))
      (gtk-container-add hbox vbox)
      (gtk-container-add hbox canvas)
      (gtk-widget-show-all hbox)
      (draw-canvas)

      (lambda (op)
	(case op
	  ((gtk-widget) hbox)
	  ((set) (lambda (x)
		   (widget-set widget x)
		   (update-canvas)))
	  ((clear) (lambda (x)
		     (widget-clear widget)
		     (update-canvas)))
	  (t (widget op))))))

  (define-widget-type 'workspace-geometry make-workspace-geometry-item)
  (widget-accepts-doc-string 'workspace-geometry))
