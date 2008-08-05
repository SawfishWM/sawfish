; get-S-tabbed/theme.jl

;; Based off of my arctic Enlightenment theme....


(let*
  (
    ;(font (get-font "-*-lucida-medium-r-normal-*-*-100-*-*-p-*-iso8859-1"))
    ;(font (get-font "Sans 9"))
    (font default-font)
    (font-colors (list "grey50" "white"))
    
	(title-images (list (make-image "title-bar-inactive.png")
                            (make-image "title-bar-active.png")))

        (title-left-images (list (set-image-border
	                         (make-image "title-left-inactive.png") 10 10 0 0)
	                         (set-image-border
				 (make-image "title-left-active.png") 10 10 0 0)))

        (title-left-f-images (list (set-image-border
	                         (make-image "title-left-inactive-f.png") 10 10 3 1)
	                         (set-image-border
				 (make-image "title-left-active-f.png") 10 10 3 1)))

        (title-left-l-images (list (make-image "title-left-inactive-l.png")
				 (make-image "title-left-active-l.png")))

        (title-left-m-images (list (set-image-border
	                         (make-image "title-left-inactive-m.png") 10 10 0 0)
	                         (set-image-border
				 (make-image "title-left-active-m.png") 10 10 0 0)))

        (title-left-r-images (list (make-image "title-left-inactive-r.png")
				 (make-image "title-left-active-r.png")))

        (iconify-images (list (make-image "iconify-normal.png")
			      nil nil
                              (make-image "iconify-clicked.png")))

     	(maximize-images (list (make-image "maximize-normal.png")
                               nil nil
  	 		       (make-image "maximize-clicked.png")))

        (menu-images (list (make-image "menu-normal.png")
	                            nil nil
				    (make-image "menu-clicked.png")))

     	(close-images (list (make-image "close-normal.png")
			    nil nil
                            (make-image "close-clicked.png")))

        (title-right (set-image-border(make-image "title-right.png")10 10 10 10))
        (title-right-l (set-image-border(make-image "title-right-l.png") 3 0 10 10))
        (title-right-r (set-image-border(make-image "title-right-r.png") 0 3 10 10))
     	(border-top (make-image "border_top.png"))
     	(border-bottom (make-image "border_bottom.png"))
     	(border-right (make-image "border_right.png"))
     	(border-left (make-image "border_left.png"))

     	(corner-tl (make-image "corner_top_left.png"))
     	(corner-tr (make-image "corner_top_right.png"))
        (corner-tl-2 (make-image "corner_top_left_2.png"))
	(corner-tr-2 (make-image "corner_top_right_2.png"))
        (corner-bl (make-image "corner_bottom_left.png"))
     	(corner-br (make-image "corner_bottom_right.png"))
        (corner-bl-2 (make-image "corner_bottom_left_2.png"))
	(corner-br-2 (make-image "corner_bottom_right_2.png"))

     	(shaped-frame `(

		((background . ,title-left-images)
		 (foreground . ,font-colors)
		 (font . ,font)
		 (text . ,window-name)
		 (x-justify . center)
		 (y-justify . center)
		 (top-edge . -16)
		 (left-edge . 15)
		 (right-edge . 40)
		 (class . title))
     	             	 
	))

	(shaped-transient-frame `(
  
      		((background . ,title-images)
      		 (foreground . ,font-colors)
      		 (font . ,font)
      		 (text . ,window-name)
      		 (x-justify . center)
      		 (y-justify . center)
      		 (top-edge . -12)
      		 (left-edge . 0)
      		 (right-edge . 0)
      		 (class . title))

                 ((background . ,menu-images)
		   (top-edge . -12)
		   (left-edge . 1)
		   (class . menu-button))
	      
     		((background . ,iconify-images)
      		 (top-edge . -12)
      		 (right-edge . 26) 
      		 (class . iconify-button))

     		((background . ,maximize-images)
      		 (top-edge . -12)
      		 (right-edge . 13)
      		 (class . maximize-button))

     		((background . ,close-images)
      		 (top-edge . -12)
      		 (right-edge . 1)
      		 (class . close-button))

     		((background . ,border-left)
      		 (top-edge . -12)
      		 ;;(bottom-edge . 0)
      		 (left-edge . -2)   
      		 (class . left-border))

     		((background . ,border-right)
      		 (top-edge . -12)
      		 ;;(bottom-edge . 0)
      		 (right-edge . -2)  
      		 (class . right-border))

     		((background . ,border-top)
      		 (top-edge . -14)
      		 (right-edge . 0)
      		 (left-edge . 0) 
      		 (class . top-border))

     		((background . ,border-bottom)
      		 (top-edge . 0)
      		 (right-edge . 0)
      		 (left-edge . 0) 
      		 (class . bottom-border))

         	((background . ,corner-tl)
      		 (top-edge . -14)
      		 (left-edge . -2)
      		 (class . top-left-corner))

     		((background . ,corner-tr)
      		 (top-edge . -14)
      		 (right-edge . -2)
      		 (class . top-right-corner))

                ((background . ,corner-bl-2)
		 (top-edge . 0)
		 (left-edge . -2)
		 (class . bottom-left-corner))
								   
                ((background . ,corner-br-2)
                 (top-edge . 0)
		 (right-edge . -2)
		 (class . bottom-right-corner))
						       


))

	(transient-frame `(	      
				
     		((background . ,border-left)
      		 (top-edge . -2)
      		 (bottom-edge . 0)
      		 (left-edge . -2) 
      		 (class . left-border))

     		((background . ,border-right)
      		 (top-edge . -2)
      		 (bottom-edge . 0)
      		 (right-edge . -2)
      		 (class . right-border))

	     	((background . ,border-top)
      		 (top-edge . -2)
      		 (right-edge . 0)
      		 (left-edge . 0) 
      		 (class . title))

     		((background . ,border-bottom)
      		 (bottom-edge . -2)
      		 (right-edge . 0)  
      		 (left-edge . 0)   
      		 (class . bottom-border))

     		((background . ,corner-tl-2)
      		 (top-edge . -2)
      		 (left-edge . -2)
      		 (class . top-left-corner))

     		((background . ,corner-tr-2)
      		 (top-edge . -2)
      		 (right-edge . -2)
      		 (class . top-right-corner))

     		((background . ,corner-bl)
      		 (bottom-edge . -2)
      		 (left-edge . -2)  
      		 (class . bottom-left-corner))

     		((background . ,corner-br)
      		 (bottom-edge . -2)
      		 (right-edge . -2) 
      		 (class . bottom-right-corner))
	))
	
	(frame `(
		; TODO: re-add this part
     		;((background . ,title-right)
      		; (top-edge . -14)
      		; (left-edge . -3)
		; (right-edge . -3)
      		; (class . title))
      
		;; left part of title background
     		((background . ,title-right-l)
      		 (top-edge . -14)
      		 (left-edge . -3)
		 (width . 18)
      		 (class . title))
      
		;; right part of title background
     		((background . ,title-right-r)
      		 (top-edge . -14)
      		 (width . 42)
		 (right-edge . -3)
      		 (class . title))
      
		;; tab
     		;((background . ,title-left-f-images)
     		((background . ,title-left-m-images)
      		 (foreground . ,font-colors)
      		 (font . ,font)
      		 (text . ,window-name)
      		 (x-justify . center)
      		 (y-justify . center)
      		 (top-edge . -16)
      		 ;(left-edge . 15)
      		 ;(right-edge . 40)
      		 (class . tab))
		 
		;; tab
     		((background . ,title-left-l-images)
      		 (top-edge . -16)
      		 (class . tab-l))
		 
		;; tab
     		((background . ,title-left-r-images)
      		 (top-edge . -16)
      		 (class . tab-r))
		 
		((background . ,menu-images)
		 (top-edge . -12)
		 (left-edge . 1)
                 (class . menu-button))
		
		((background . ,iconify-images)
      		 (top-edge . -12)
      		 (right-edge . 26) 
      		 (class . iconify-button))

                ((background . ,maximize-images)
		  (top-edge . -12)
		  (right-edge . 13)
		  (class . maximize-button))

     		((background . ,close-images)
      		 (top-edge . -12)
      		 (right-edge . 1)
      		 (class . close-button))

     		((background . ,border-left)
      		 (top-edge . 0)
      		 (bottom-edge . 0)
      		 (left-edge . -2)   
      		 (class . left-border))

     		((background . ,border-right)
      		 (top-edge . 0)
      		 (bottom-edge . 0)
      		 (right-edge . -2)  
      		 (class . right-border))

     		((background . ,border-bottom)
      		 (bottom-edge . 0)
      		 (right-edge . 0)
      		 (left-edge . 0) 
      		 (class . bottom-border))

                ((background . ,corner-bl)
		 (bottom-edge . 0)
		 (left-edge . -2)
		 (class . bottom-left-corner))
								   
                ((background . ,corner-br)
                 (bottom-edge . 0)
		 (right-edge . -2)
		 (class . bottom-right-corner))
						       
	))
	
  )
  (add-frame-style 'get-S-tabbed
    	(lambda (w type)   
          (case type
	     ((default) frame)
	     ((transient) transient-frame)
	     ((shaped) shaped-frame)
	     ((shaped-transient) shaped-transient-frame))))

  ; TODO: figure out how to store this info in this file correctly
  ;(custom-set-typed-variable'tab-left-dec-width '5 'number)
  ;(custom-set-typed-variable'tab-right-dec-width '5 'number)
  ;(custom-set-typed-variable'tab-left-margin '14 'number)
  ;(custom-set-typed-variable'tab-right-margin '38 'number)

  )
