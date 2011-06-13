;; preview-image-selector.jl
;; Based on file.jl
;; Author: Lucas Pandolfo
;; Modified by: Christopher Roy Bratusek <nano@tuxfamily.org>

(define-structure sawfish.gtk.widgets.image

    (export )

    (open rep
	  rep.system
	  rep.io.files
          gui.gtk-2.gtk
          sawfish.gtk.widget)

  (define (make-image-item changed-callback)
    (let* ((box (gtk-table-new 1 2 nil))
	   (selector (gtk-file-chooser-button-new '() 'open))
	   (selector-preview (gtk-image-new))
	   (image-preview (gtk-image-new)))

      (gtk-container-set-border-width box 0)

      (gtk-table-attach box selector 0 1 0 1 'shrink 'shrink 0 0)
      (gtk-table-attach box image-preview 0 1 1 2  'shrink 'shrink 0 -12)

      (gtk-widget-set-size-request image-preview 150 150)
      (gtk-widget-set-size-request selector 150 -1)

      (when changed-callback
  	(g-signal-connect
	  selector "file-set" (make-signal-callback changed-callback)))

      (g-signal-connect selector "update-preview" 
			(lambda (w) 
			  (let* ((filename (gtk-file-chooser-get-preview-filename w))
				 (pixbuf (gdk-pixbuf-new-from-file-at-scale filename 150 -1 t)))
			    (if pixbuf
				(progn (gtk-image-set-from-pixbuf selector-preview pixbuf)
				       (gtk-file-chooser-set-preview-widget-active selector t))
				(gtk-file-chooser-set-preview-widget-active selector nil)))))

      (gtk-file-chooser-set-preview-widget selector selector-preview)

      (g-signal-connect selector "file-set" (lambda () 
					    (gtk-image-set-from-pixbuf image-preview (gdk-pixbuf-new-from-file-at-scale
										       (gtk-file-chooser-get-filename selector) 150 -1 t))))
      (gtk-widget-show box)

      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (gtk-file-chooser-select-filename selector (or (and (file-exists-p x) x) ""))
		   (when (and (stringp x) x)
		     (gtk-image-set-from-pixbuf image-preview
		       (gdk-pixbuf-new-from-file-at-scale x 150 -1 t)))))
	  ((clear) (lambda ()
		     (gtk-image-clear image-preview)
		     (gtk-file-chooser-select-filename selector "")))
	  ((ref) (lambda ()
		   (gtk-file-chooser-get-filename selector)))
	  ((gtk-widget) box)
	  ((validp) (lambda (x) (stringp x)))))))

  (define-widget-type 'image make-image-item))
