; microGUI/theme.jl

;; Copyright (C) 1999 Ryan Lovett <ryan@ocf.berkeley.edu>

;; This theme was based on the QNX Photon microGUI found at www.qnx.com
;; (screenshots at http://www.qnx.com/amiga/delivering.html). Many image
;; components were taken from the minEguE enlightenment theme by Ben
;; FrantzDale (frantzdb@admin.arhs.net). The theme and the rest of the images
;; were put together by Ryan (ryan@ocf.berkeley.edu).

;; hacked for lexical scope by John Harper

(let*
    ;; Update window title pixel length
    ((title-width
      (lambda (w)
	(let
	    ((w-width (car (window-dimensions w))))
	  (max 0 (min (- w-width 100) (text-width (window-name w)))))))
     
     ;; 2x6
     (bottom-images (make-image "bottom.png"))

     ;; 6x27
     (top-left-images (list (make-image "top_left_inactive.png")
			    (make-image "top_left.png")))
     (top-left-shaped-images (list (make-image "top_left_inactive_s.png")
				   (make-image "top_left_s.png")))

     ;; 19x19
     (top-blue-images (list (make-image "top_blue_inactive.png")
			    (make-image "top_blue.png")))

     ;; 12x19
     (top-curves-images (list (make-image "top_curves_inactive.png")
			      (make-image "top_curves.png")))

     ;; 19x19
     (top-grey-images (make-image "top_grey.png"))

     ;; 7x19
     (top-right-images (make-image "top_right.png"))
     (top-right-shaped-images (make-image "top_right_s.png"))

     ;; 6x6
     (bottom-left-images (make-image "bl.png"))

     ;; 6x6
     (bottom-right-images (make-image "br.png"))

     ;; 17x16
     (close-images (list (make-image "close_normal.png")
			 (make-image "close_active.png") nil
			 (make-image "close_clicked.png")))

     ;; 17x16
     (maximize-images (list (make-image "maximize_normal.png")
			    (make-image "maximize_active.png") nil
			    (make-image "maximize_clicked.png")))

     ;; 17x16
     (iconify-images (list (make-image "minimize_normal.png")
			   (make-image "minimize_active.png") nil
			   (make-image "minimize_clicked.png")))

     ;; 17x16
     (menu-images (list (make-image "menu_normal.png")
			(make-image "menu_active.png") nil
			(make-image "menu_clicked.png")))

     ;; 6x19
     (left-images (make-image "left.png"))

     ;; 6x19
     (right-images (make-image "right.png"))

     (top-images (flip-image-diagonally (copy-image left-images)))

     ;; 18x17
     (t-close-images (list (make-image "t_close_normal.png")
			   (make-image "t_close_active.png") nil
			   (make-image "t_close_clicked.png")))
     ;; 5x19
     (t-left-images (make-image "t_left.png"))

     ;; 18x10
     (t-right-images (make-image "t_right.png"))

     ;; 19x5
     (t-top-images (make-image "t_top.png"))

     ;; 19x5
     (t-bottom-images (make-image "t_bottom.png"))

     ;; 4x4
     (t-top-left-images (make-image "t_top_left.png"))

     ;; 4x4
     (t-bottom-left-images (make-image "t_bottom_left.png"))

     ;; 17x4
     (t-top-right-images (make-image "t_top_right.png"))

     ;; 17x4
     (t-bottom-right-images (make-image "t_bottom_right.png"))

     (text-colors '("grey85" "white"))

     ;; frame layout

     (frame `(((background . ,top-left-images)
	       (left-edge . -6)
	       (top-edge . -19)
	       (class . top-left-corner))

	      ;; top blue
	      ((background . ,top-blue-images)
	       (foreground . ,text-colors)
	       (text . ,window-name)
	       (x-justify . 4)
	       (y-justify . center)
	       (top-edge . -19)
	       (left-edge . 19)
	       (width . ,(lambda (w) (+ (title-width w) 13)))
	       (class . title))

	      ;; menu button
	      ((background . ,menu-images)
	       (top-edge . -19)
	       (left-edge . 0)
	       (class . menu-button))

	      ;; top curves
	      ((background . ,top-curves-images)
	       (left-edge . ,(lambda (w) (+ (title-width w) 32)))
	       (top-edge . -19)
	       (class . title))

	      ;; top grey
	      ((background . ,top-grey-images)
	       (top-edge . -19)
	       (left-edge . ,(lambda (w) (+ (title-width w) 44)))
	       (right-edge . 52)
	       (class . title))

	      ;; left border
	      ((background . ,left-images)
	       (left-edge . -6)
	       (top-edge . 8)
	       (bottom-edge . 0)
	       (class . left-border))

	      ;; top-right corner
	      ((background . ,top-right-images)
	       (right-edge . -6)
	       (top-edge . -19)
	       (class . top-right-corner))

	      ;; right border
	      ((background . ,right-images)
	       (right-edge . -6)
	       (top-edge . 0)
	       (bottom-edge . 0)
	       (class . right-border))

	      ;; bottom border
	      ((background . ,bottom-images)
	       (left-edge . 0)
	       (right-edge . 0)
	       (bottom-edge . -6)
	       (class . bottom-border))

	      ;; bottom-left corner
	      ((background . ,bottom-left-images)
	       (left-edge . -6)
	       (bottom-edge . -6)
	       (class . bottom-left-corner))

	      ;; bottom-right corner
	      ((background . ,bottom-right-images)
	       (right-edge . -6)
	       (bottom-edge . -6)
	       (class . bottom-right-corner))

	      ;; iconify button
	      ((background . ,iconify-images)
	       (right-edge . 35)
	       (top-edge . -19)
	       (class . iconify-button))

	      ;; maximize button
	      ((background . ,maximize-images)
	       (right-edge . 18)
	       (top-edge . -19)
	       (class . maximize-button))

	      ;; delete button
	      ((background . ,close-images)
	       (right-edge . 1)
	       (top-edge . -19)
	       (class . close-button))))

       (shaped-frame `(((background . ,top-left-shaped-images)
			(left-edge . -6)
			(top-edge . -19)
			(height . 19)
			(class . top-left-corner))

		       ;; top blue
		       ((background . ,top-blue-images)
			(foreground . ,text-colors)
			(text . ,window-name)
			(x-justify . 4)
			(y-justify . center)
			(top-edge . -19)
			(left-edge . 19)
			(width . ,(lambda (w) (+ (title-width w) 13)))
			(class . title))

		       ;; menu button
		       ((background . ,menu-images)
			(top-edge . -19)
			(left-edge . 0)
			(class . menu-button))

		       ;; top curves
		       ((background . ,top-curves-images)
			(left-edge . ,(lambda (w) (+ (title-width w) 32)))
			(top-edge . -19)
			(class . title))

		       ;; top grey
		       ((background . ,top-grey-images)
			(top-edge . -19)
			(left-edge . ,(lambda (w) (+ (title-width w) 44)))
			(right-edge . 52)
			(class . title))

		       ;; top-right corner
		       ((background . ,top-right-shaped-images)
			(right-edge . -6)
			(top-edge . -19)
			(height . 19)
			(class . top-right-corner))

		       ;; iconify button
		       ((background . ,iconify-images)
			(right-edge . 35)
			(top-edge . -19)
			(class . iconify-button))

		       ;; maximize button
		       ((background . ,maximize-images)
			(right-edge . 18)
			(top-edge . -19)
			(class . maximize-button))

		       ;; delete button
		       ((background . ,close-images)
			(right-edge . 1)
			(top-edge . -19)
			(class . close-button))))

       (transient-frame `(((background . ,t-top-left-images)
			   (left-edge . -5)
			   (top-edge . -5)
			   (class . top-left-corner))

			  ;;top-right corner
			  ((background . ,t-top-right-images)
			   (right-edge . -18)
			   (top-edge . -5)
			   (class . top-right-corner))

			  ;;title border
			  ((background . ,t-top-images)
			   (left-edge . -1)
			   (right-edge . -1)
			   (top-edge . -5)
			   (class . top-border))

			  ;; left border
			  ((background . ,t-left-images)
			   (left-edge . -5)
			   (top-edge . -1)
			   (bottom-edge . -1)
			   (class . left-border))

			  ;; right border
			  ((background . ,t-right-images)
			   (right-edge . -18)
			   (top-edge . -1)
			   (bottom-edge . -1)
			   (class . title))

			  ;; bottom border
			  ((background . ,t-bottom-images)
			   (left-edge . -1)
			   (right-edge . -1)
			   (bottom-edge . -5)
			   (class . bottom-border))

			  ;; bottom-left corner
			  ((background . ,t-bottom-left-images)
			   (left-edge . -5)
			   (bottom-edge . -5)
			   (class . bottom-left-corner))

			  ;; bottom-right corner
			  ((background . ,t-bottom-right-images)
			   (right-edge . -18)
			   (bottom-edge . -5)
			   (class . bottom-right-corner))

			  ;; delete button
			  ((background . ,t-close-images)
			   (right-edge . -18)
			   (top-edge . 1)
			   (class . close-button))))

       (shaped-transient-frame `(((background . ,t-top-right-images)
				  (right-edge . -18)
				  (top-edge . -5)
				  (class . top-right-corner))

				 ;; right border
				 ((background . ,t-right-images)
				  (right-edge . -18)
				  (top-edge . -1)
				  (bottom-edge . -1)
				  (class . title))

				 ;; bottom-right corner
				 ((background . ,t-bottom-right-images)
				  (right-edge . -18)
				  (bottom-edge . -5)
				  (class . bottom-right-corner))

				 ;; delete button
				 ((background . ,t-close-images)
				  (right-edge . -18)
				  (top-edge . 1)
				  (class . close-button)))))
  
  (add-frame-style 'microGUI
		   (lambda (w type)
		     (case type
		       ((default) frame)
		       ((transient) transient-frame)
		       ((shaped) shaped-frame)
		       ((shaped-transient) shaped-transient-frame))))

  (call-after-property-changed
   'WM_NAME (lambda ()
	      (rebuild-frames-with-style 'microGUI))))
