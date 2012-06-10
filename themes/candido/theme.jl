;; Name    : candido. Based on metacity candido theme.
;; Author  : Sergey Sharybin <sharybin@nm.ru>
;; License : GPL-2

(let*
  (
    (iclose  (list (make-image "close.png")       nil (make-image "close_active.png") nil))
    (imax    (list (make-image "max.png")         nil (make-image "max_active.png")   nil))
    (imin    (list (make-image "min.png")         nil (make-image "min_active.png")   nil))
    (ititle  (list (make-image "title.png")       nil nil nil))
    (imenu   (list (make-image "menu.png")        nil (make-image "menu_active.png")  nil))
    (ibl     (list (make-image "bl.png")          nil nil nil))
    (ibr     (list (make-image "br.png")          nil nil nil))
    (iright  (list (make-image "right.png")       nil nil nil))
    (ileft   (list (make-image "left.png")        nil nil nil))
    (ibottom (list (make-image "bottom.png")      nil nil nil))
    (ibtbg   (list (make-image "btbg.png")        nil nil nil))
    (imenubg (list (make-image "menubg.png")      nil nil nil))

    (default-frame
     `(
        ((background . ,ibtbg)  (right-edge . -4) (width . 81)        (height . 19) (top-edge . -19) (class . title))
        ((background . ,imenubg)(left-edge . -4)  (width . 21)        (height . 19) (top-edge . -19) (class . title))

        ((background . ,iclose)  (right-edge . 1)                   (width . 26)               (top-edge . -19)                    (class . close-button))
        ((background . ,imax)    (right-edge . 26)                  (width . 26)               (top-edge . -19)                    (class . maximize-button))
        ((background . ,imin)    (right-edge . 51)                  (width . 26)               (top-edge . -19)                    (class . iconify-button))
        ((background . ,imenu)                     (left-edge . 0)  (width . 13)               (top-edge . -15)                    (class . menu-button))
        ((background . ,iright)  (right-edge . -4)                  (width . 4)                (top-edge . 0)   (bottom-edge . 0)  (class . right-border))
        ((background . ,ileft)                     (left-edge . -4) (width . 4)                (top-edge . 0)   (bottom-edge . 0)  (class . left-border))
        ((background . ,ibottom) (right-edge . 0)  (left-edge . 0)  (height . 4)               (bottom-edge . -4) (class . bottom-border))
        ((background . ,ibl)                       (left-edge . -4) (width . 4)  (height . 4)                   (bottom-edge . -4) (class . bottom-left-corner))
        ((background . ,ibr)     (right-edge . -4)                  (width . 4)  (height . 4)                   (bottom-edge . -4) (class . bottom-right-corner))

        ((background . ,ititle)  (right-edge . 77) (left-edge . 17)              (height . 19) (top-edge . -19) 
         (foreground . ("grey" "black"))  (text . ,window-name) (x-justify . center) (y-justify . center) (class . title))
      )
    )

    (shaded-frame
     `(
        ((background . ,ibtbg)  (right-edge . -4) (width . 81)        (height . 19) (top-edge . -19) (class . title))
        ((background . ,imenubg)(left-edge . -4)  (width . 21)        (height . 19) (top-edge . -19) (class . title))

        ((background . ,iclose)  (right-edge . 1)                   (width . 26)               (top-edge . -19)                    (class . close-button))
        ((background . ,imax)    (right-edge . 26)                  (width . 26)               (top-edge . -19)                    (class . maximize-button))
        ((background . ,imin)    (right-edge . 51)                  (width . 26)               (top-edge . -19)                    (class . iconify-button))
        ((background . ,imenu)                     (left-edge . 0)  (width . 13)               (top-edge . -15)                    (class . menu-button))
        ((background . ,ibottom) (right-edge . 0)  (left-edge . 0)  (height . 4)               (top-edge . 0) (class . bottom-border))
        ((background . ,ibl)                       (left-edge . -4) (width . 4)  (height . 4)  (top-edge . 0) (class . bottom-left-corner))
        ((background . ,ibr)     (right-edge . -4)                  (width . 4)  (height . 4)  (top-edge . 0) (class . bottom-right-corner))

        ((background . ,ititle)  (right-edge . 77) (left-edge . 17)              (height . 19) (top-edge . -19) 
         (foreground . ("grey" "black"))  (text . ,window-name) (x-justify . center) (y-justify . center) (class . title))

      )
    )

    (transident-frame
     `(
        ((background . ,ibtbg)  (right-edge . -4) (width . 81)        (height . 19) (top-edge . -19) (class . title))
        ((background . ,imenubg)(left-edge . -4)  (width . 21)        (height . 19) (top-edge . -19) (class . title))

        ((background . ,iclose)  (right-edge . 1)                   (width . 26)               (top-edge . -19)                    (class . close-button))
        ((background . ,imin)    (right-edge . 26)                  (width . 26)               (top-edge . -19)                    (class . iconify-button))
        ((background . ,imenu)                     (left-edge . 0)  (width . 13)               (top-edge . -15)                    (class . menu-button))
        ((background . ,iright)  (right-edge . -4)                  (width . 4)                (top-edge . 0)   (bottom-edge . 0)  (class . right-border))
        ((background . ,ileft)                     (left-edge . -4) (width . 4)                (top-edge . 0)   (bottom-edge . 0)  (class . left-border))
        ((background . ,ibottom) (right-edge . 0)  (left-edge . 0)  (height . 4)               (bottom-edge . -4) (class . bottom-border))
        ((background . ,ibl)                       (left-edge . -4) (width . 4)  (height . 4)                   (bottom-edge . -4) (class . bottom-left-corner))
        ((background . ,ibr)     (right-edge . -4)                  (width . 4)  (height . 4)                   (bottom-edge . -4) (class . bottom-right-corner))

        ((background . ,ititle)  (right-edge . 77) (left-edge . 17)              (height . 19) (top-edge . -19) 
         (foreground . ("grey" "black"))  (text . ,window-name) (x-justify . center) (y-justify . center) (class . title))

      )
    )

    (shaded-transident-frame
     `(
        ((background . ,ibtbg)  (right-edge . -4) (width . 81)        (height . 19) (top-edge . -19) (class . title))
        ((background . ,imenubg)(left-edge . -4)  (width . 21)        (height . 19) (top-edge . -19) (class . title))

        ((background . ,iclose)  (right-edge . 1)                   (width . 26)               (top-edge . -19)                    (class . close-button))
        ((background . ,imin)    (right-edge . 26)                  (width . 26)               (top-edge . -19)                    (class . iconify-button))
        ((background . ,imenu)                     (left-edge . 0)  (width . 13)               (top-edge . -15)                    (class . menu-button))
        ((background . ,ibottom) (right-edge . 0)  (left-edge . 0)  (height . 4)               (top-edge . 0) (class . bottom-border))
        ((background . ,ibl)                       (left-edge . -4) (width . 4)  (height . 4)  (top-edge . 0) (class . bottom-left-corner))
        ((background . ,ibr)     (right-edge . -4)                  (width . 4)  (height . 4)  (top-edge . 0) (class . bottom-right-corner))

        ((background . ,ititle)  (right-edge . 77) (left-edge . 17)              (height . 19) (top-edge . -19) 
         (foreground . ("grey" "black"))  (text . ,window-name) (x-justify . center) (y-justify . center) (class . title))

      )
    )
  )

  (add-frame-style 'candido 
    (lambda (w type) 
      (case type 
        ((default) default-frame)
        ((transient) transident-frame)
        ((shaped) shaded-frame)
        ((shaped-transient) shaded-transident-frame)
      )
    )
  )
)
