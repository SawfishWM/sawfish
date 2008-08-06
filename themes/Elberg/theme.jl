; This USED to be the theme.jl from the microGUI theme by
; Ryan Lovett, Ben FrantzDale, and John Harper.  Big props to them, and
; here's to hoping someone will write a decent HOWTO for sawfish-themer
; because I really, _really_ suck at scheme coding.

; This theme is GPL, unless Ryan, Ben, or John have a problem with that.
; I just got tired of there not being many non-hideous themes for Sawfish.

(let*
    ;; Update window title pixel length
    ((title-width
      (lambda (w)
	(let
	    ((w-width (car (window-dimensions w))))
	  (max 0 (min (- w-width 100) (text-width (window-name w)))))))
     
     ;; 6x19 - upper left corner
     (upper-left-images (list (make-image "i-ul.png")
                              (make-image "a-ul.png")))
     (upper-left-shaped-images (list (make-image "i-uls.png")
				     (make-image "a-uls.png")))

     ;; 16x19 - upper left menu button
     (menu-images (list (make-image "i-th.png")
			(make-image "a-th.png")
                        (make-image "p-th.png")
			(make-image "c-th.png")))

     ;; 11x19 - upper left grey to green border
     (top-lefthollow-images (list (make-image "i-t1.png")
		  	          (make-image "a-t1.png")))

     ;; 3x19 - upper green background
     (top-hollow-images (list (make-image "i-t2.png")
		  	      (make-image "a-t2.png")))

     ;; 11x19 - upper right green to grey border
     (top-righthollow-images (list (make-image "i-t3.png")
		       	           (make-image "a-t3.png")))

     ;; 3x19 - upper grey background
     (top-grey-images (list (make-image "i-t0.png")
                                 (make-image "a-t0.png")))

     ;; 17x16 - iconify icon
     (iconify-images (list (make-image "i-ti.png")
			   (make-image "a-ti.png")
			   (make-image "p-ti.png")
			   (make-image "c-ti.png")))

     ;; 17x16 - maximize icon
     (maximize-images (list (make-image "i-tm.png")
			    (make-image "a-tm.png")
			    (make-image "p-tm.png")
			    (make-image "c-tm.png")))

     ;; 17x16 - close icon
     (close-images (list (make-image "i-tx.png")
			 (make-image "a-tx.png")
			 (make-image "p-tx.png")
			 (make-image "c-tx.png")))

     ;; 18x6 - upper right corner
     (upper-right-images (list (make-image "i-ur.png")
                               (make-image "a-ur.png")))
     (upper-right-shaped-images (list (make-image "i-urs.png")
				      (make-image "a-urs.png")))


     ;; 6x18 - left side
     (left-side-images (list (make-image "i-ls.png")
                             (make-image "a-ls.png")))
     ;; 6x18 - right side
     (right-side-images (list (make-image "i-rs.png")
                              (make-image "a-rs.png")))

     ;; 6x6 - lower left corner
     (bottom-left-images (list (make-image "i-ll.png")
                               (make-image "a-ll.png")))
     ;; 18x6 - lower ledge
     (bottom-images (list (make-image "i-bot.png")
                          (make-image "a-bot.png")))
     ;; 6x6 - lower right corner
     (bottom-right-images (list (make-image "i-lr.png")
                                (make-image "a-lr.png")))

     ;; 4x4
     (t-upper-left-images (list (make-image "i-tul.png")
                                (make-image "a-tul.png")))
     ;; 11x4
     (t-upper-side-images (list (make-image "i-ttop.png")
                                (make-image "a-ttop.png")))
     ;; 17x4
     (t-upper-right-images (list (make-image "i-tur.png")
                                 (make-image "a-tur.png")))
     (t-upper-right-shaped-images (list (make-image "i-turs.png")
                                        (make-image "a-turs.png")))

     ;; 4x17
     (t-left-images (list (make-image "i-tls.png")
                          (make-image "a-tls.png")))

     ;; 17x16
     (t-close-images (list (make-image "i-ttx.png")
			   (make-image "a-ttx.png")
			   (make-image "p-ttx.png")
			   (make-image "c-ttx.png")))

     ;; 17x3
     (t-right-images (list (make-image "i-tt0.png")
                           (make-image "a-tt0.png")))

     ;; 4x4
     (t-bottom-left-images (list (make-image "i-tll.png")
                                 (make-image "a-tll.png")))
     ;; 11x4
     (t-bottom-side-images (list (make-image "i-tbot.png")
                                 (make-image "a-tbot.png")))
     ;; 17x4
     (t-bottom-right-images (list (make-image "i-tlr.png")
                                  (make-image "a-tlr.png")))
     (t-bottom-right-shaped-images (list (make-image "i-tlrs.png")
                                         (make-image "a-tlrs.png")))


     (text-colors '("grey85" "white"))

     ;; frame layout

     (frame `(((background . ,upper-left-images)
	       (left-edge . -6)
	       (top-edge . -19)
	       (class . top-left-corner))

	      ;; menu button
	      ((background . ,menu-images)
	       (top-edge . -19)
	       (left-edge . 0)
	       (class . menu-button))

	      ;; top curves
	      ((background . ,top-lefthollow-images)
	       (top-edge . -19)
               (left-edge . 16)
	       (class . title))

	      ;; top green
	      ((background . ,top-hollow-images)
	       (foreground . ,text-colors)
	       (text . ,window-name)
	       (x-justify . 4)
	       (y-justify . center)
	       (top-edge . -19)
	       (left-edge . 27)
	       (width . ,(lambda (w) (+ (title-width w) 13)))
	       (class . title))

	      ;; top curves
	      ((background . ,top-righthollow-images)
	       (left-edge . ,(lambda (w) (+ (title-width w) 37)))
	       (top-edge . -19)
	       (class . title))

	      ;; top grey
	      ((background . ,top-grey-images)
	       (left-edge . ,(lambda (w) (+ (title-width w) 48)))
	       (top-edge . -19)
	       (right-edge . 48)
	       (class . title))

	      ;; left border
	      ((background . ,left-side-images)
	       (left-edge . -6)
	       (top-edge . 0)
	       (bottom-edge . 0)
	       (class . left-border))

	      ;; top-right corner
	      ((background . ,upper-right-images)
	       (right-edge . -6)
	       (top-edge . -19)
	       (class . top-right-corner))

	      ;; right border
	      ((background . ,right-side-images)
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
	       (right-edge . 32)
	       (top-edge . -19)
	       (class . iconify-button))

	      ;; maximize button
	      ((background . ,maximize-images)
	       (right-edge . 16)
	       (top-edge . -19)
	       (class . maximize-button))

	      ;; delete button
	      ((background . ,close-images)
	       (right-edge . 0)
	       (top-edge . -19)
	       (class . close-button))))

       (shaped-frame `(((background . ,upper-left-shaped-images)
			(left-edge . -6)
			(top-edge . -19)
			(height . 19)
			(class . top-left-corner))

		       ;; menu button
		       ((background . ,menu-images)
			(top-edge . -19)
			(left-edge . 0)
			(class . menu-button))

                       ;; top curves
                       ((background . ,top-lefthollow-images)
                        (top-edge . -19)
                        (left-edge . 16) 
                        (class . title))

		       ;; Title text area
		       ((background . ,top-hollow-images)
			(foreground . ,text-colors)
			(text . ,window-name)
			(x-justify . 4)
			(y-justify . center)
			(top-edge . -19)
			(left-edge . 27)
			(width . ,(lambda (w) (+ (title-width w) 13)))
			(class . title))

		       ;; top curves
		       ((background . ,top-righthollow-images)
			(left-edge . ,(lambda (w) (+ (title-width w) 37)))
			(top-edge . -19)
			(class . title))

		       ;; top grey
		       ((background . ,top-grey-images)
			(left-edge . ,(lambda (w) (+ (title-width w) 48)))
			(top-edge . -19)
			(right-edge . 48)
			(class . title))

		       ;; top-right corner
		       ((background . ,upper-right-shaped-images)
			(right-edge . -6)
			(top-edge . -19)
			(height . 19)
			(class . top-right-corner))

		       ;; iconify button
		       ((background . ,iconify-images)
			(right-edge . 32)
			(top-edge . -19)
			(class . iconify-button))

		       ;; maximize button
		       ((background . ,maximize-images)
			(right-edge . 16)
			(top-edge . -19)
			(class . maximize-button))

		       ;; delete button
		       ((background . ,close-images)
			(right-edge . 0)
			(top-edge . -19)
			(class . close-button))))

       (transient-frame `(((background . ,t-upper-left-images)
			   (left-edge . -4)
			   (top-edge . -4)
			   (class . top-left-corner))

			  ;;top-right corner
			  ((background . ,t-upper-right-images)
			   (right-edge . -17)
			   (top-edge . -4)
			   (class . top-right-corner))

			  ;;title border
			  ((background . ,t-upper-side-images)
			   (left-edge . -1)
			   (right-edge . -1)
			   (top-edge . -4)
			   (class . top-border))

			  ;; left border
			  ((background . ,t-left-images)
			   (left-edge . -4)
			   (top-edge . -1)
			   (bottom-edge . -1)
			   (class . left-border))

			  ;; right border
			  ((background . ,t-right-images)
			   (right-edge . -17)
			   (top-edge . -1)
			   (bottom-edge . -1)
			   (class . title))

			  ;; bottom border
			  ((background . ,t-bottom-side-images)
			   (left-edge . -1)
			   (right-edge . -1)
			   (bottom-edge . -4)
			   (class . bottom-border))

			  ;; bottom-left corner
			  ((background . ,t-bottom-left-images)
			   (left-edge . -4)
			   (bottom-edge . -4)
			   (class . bottom-left-corner))

			  ;; bottom-right corner
			  ((background . ,t-bottom-right-images)
			   (right-edge . -17)
			   (bottom-edge . -4)
			   (class . bottom-right-corner))

			  ;; delete button
			  ((background . ,t-close-images)
			   (right-edge . -17)
			   (top-edge . 1)
			   (class . close-button))))

       (shaped-transient-frame `(((background . ,t-upper-right-shaped-images)
				  (right-edge . -17)
				  (top-edge . -4)
				  (class . top-right-corner))

				 ;; right border
				 ((background . ,t-right-images)
				  (right-edge . -17)
				  (top-edge . -1)
				  (bottom-edge . -1)
				  (class . title))

				 ;; bottom-right corner
				 ((background . ,t-bottom-right-shaped-images)
				  (right-edge . -17)
				  (bottom-edge . -4)
				  (class . bottom-right-corner))

				 ;; delete button
				 ((background . ,t-close-images)
				  (right-edge . -17)
				  (top-edge . 1)
				  (class . close-button)))))
  
  (add-frame-style 'Elberg
		   (lambda (w type)
		     (case type
		       ((default) frame)
		       ((transient) transient-frame)
		       ((shaped) shaped-frame)
		       ((shaped-transient) shaped-transient-frame))))

  (call-after-property-changed
   'WM_NAME (lambda ()
	      (rebuild-frames-with-style 'Elberg))))
