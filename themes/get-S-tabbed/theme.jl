;; get-S-tabbed/theme.jl

;; Based off of my arctic Enlightenment theme....

(define font default-font)
(define font-colors (list "grey50" "white"))


(define title-images
  (list (make-image "title-bar-inactive.png")
        (make-image "title-bar-active.png")))

(define title-left-images
  (list (set-image-border
         (make-image "title-left-inactive.png") 10 10 0 0)
        (set-image-border
         (make-image "title-left-active.png") 10 10 0 0)))

(define title-left-f-images
  (list (set-image-border
         (make-image "title-left-inactive-f.png") 10 10 3 1)
        (set-image-border
         (make-image "title-left-active-f.png") 10 10 3 1)))

(define title-left-l-images
  (list (make-image "title-left-inactive-l.png")
        (make-image "title-left-active-l.png")))

(define title-left-m-images
  (list (set-image-border
         (make-image "title-left-inactive-m.png") 10 10 0 0)
        (set-image-border
         (make-image "title-left-active-m.png") 10 10 0 0)))

(define title-left-r-images
  (list (make-image "title-left-inactive-r.png")
        (make-image "title-left-active-r.png")))

(define iconify-images
  (list (make-image "iconify-normal.png")
        nil nil
        (make-image "iconify-clicked.png")))

(define maximize-images
  (list (make-image "maximize-normal.png")
        nil nil
        (make-image "maximize-clicked.png")))

(define menu-images
  (list (make-image "menu-normal.png")
        nil nil
        (make-image "menu-clicked.png")))

(define close-images
  (list (make-image "close-normal.png")
        nil nil
        (make-image "close-clicked.png")))

(define title-right
  (set-image-border(make-image "title-right.png")10 10 10 10))
(define title-right-l
  (set-image-border(make-image "title-right-l.png") 3 0 10 10))
(define title-right-r
  (set-image-border(make-image "title-right-r.png") 0 3 10 10))
(define border-top (make-image "border_top.png"))
(define border-bottom (make-image "border_bottom.png"))
(define border-right (make-image "border_right.png"))
(define border-left (make-image "border_left.png"))

(define corner-tl (make-image "corner_top_left.png"))
(define corner-tr (make-image "corner_top_right.png"))
(define corner-tl-2 (make-image "corner_top_left_2.png"))
(define corner-tr-2 (make-image "corner_top_right_2.png"))
(define corner-bl (make-image "corner_bottom_left.png"))
(define corner-br (make-image "corner_bottom_right.png"))
(define corner-bl-2 (make-image "corner_bottom_left_2.png"))
(define corner-br-2 (make-image "corner_bottom_right_2.png"))

(define shaped-frame 
  `(((background . ,title-left-images)
     (foreground . ,font-colors)
     (font . ,font)
     (text . ,window-name)
     (x-justify . center)
     (y-justify . center)
     (top-edge . -16)
     (left-edge . 15)
     (right-edge . 40)
     (class . title))))

(define shaped-transient-frame 
  `(((background . ,title-images)
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
     (class . bottom-right-corner))))

(define transient-frame 
  `(((background . ,border-left)
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
     (class . bottom-right-corner))))

(define frame 
  `(
    ;; TODO: re-add this part
    ;;((background . ,title-right)
    ;; (top-edge . -14)
    ;; (left-edge . -3)
    ;; (right-edge . -3)
    ;; (class . title))
    
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
    ;;((background . ,title-left-f-images)
    ((background . ,title-left-m-images)
     (foreground . ,font-colors)
     (font . ,font)
     (text . ,window-name)
     (x-justify . center)
     (y-justify . center)
     (top-edge . -16)
     ;;(left-edge . 15)
     ;;(right-edge . 40)
     (class . tabbar-horizontal))
    
    ;; tab
    ((background . ,title-left-l-images)
     (top-edge . -16)
     (class . tabbar-horizontal-left-edge))
    
    ;; tab
    ((background . ,title-left-r-images)
     (top-edge . -16)
     (class . tabbar-horizontal-right-edge))
    
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
     (class . bottom-right-corner))))

(define create-frames
  (lambda ()
    (let ((left-d-w 5)
          (right-d-w 5)
          (left-m 14)
          (rigth-m 38)
          (left-m-t 0)
          (right-m-t 0))
      (require 'sawfish.wm.tabs.tab)
      (set-tab-adjustments #:theme-left-dec-width left-d-w #:theme-right-dec-width right-d-w #:theme-left-margin left-m #:theme-right-margin rigth-m
                           #:theme-left-margin-transient left-m-t #:theme-right-margin-transient right-m-t))))

(create-frames)

(add-frame-style 'get-S-tabbed
                 (lambda (w type)
                   (case type
                         ((default) frame)
                         ((transient) transient-frame)
                         ((shaped) shaped-frame)
                         ((shaped-transient) shaped-transient-frame))))

(define (create-frames-only w)
    (when (eq (window-get w 'current-frame-style) 'get-S-tabbed)
      (create-frames)))

;; Create only frames, don't rebuild-frame/reframe-window.
;; Tabthemes will reframe/rebuild windows call from tabgroup.jl.
;; 
(call-after-state-changed '(title-position) create-frames-only)
