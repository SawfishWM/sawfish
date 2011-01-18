;; This USED to be the theme.jl from the microGUI theme by
;; Ryan Lovett, Ben FrantzDale, and John Harper.  Big props to them, and
;; here's to hoping someone will write a decent HOWTO for sawfish-themer
;; because I really, _really_ suck at scheme coding.

;; This theme is GPL, unless Ryan, Ben, or John have a problem with that.
;; I just got tired of there not being many non-hideous themes for Sawfish.

;; Modified to add tabbed windowing support by Scott Scriven.


;;(define title-width
;;  (lambda (w)
;;    (let
;;        ((w-width (car (window-dimensions w))))
;;      (max 0 (min (- w-width 100) (text-width (window-name w)))))))

;; 6x19 - upper left corner
(define upper-left-images (list (make-image "i-ul.png")
                                (make-image "a-ul.png")))
(define upper-left-shaped-images (list (make-image "i-uls.png")
                                (make-image "a-uls.png")))

;; 16x19 - upper left menu button
(define menu-images 
  (list (make-image "i-th.png")
        (make-image "a-th.png")
        (make-image "p-th.png")
        (make-image "c-th.png")))

;; 11x19 - upper left grey to green border
(define top-lefthollow-images 
  (list (make-image "i-t1.png")
        (make-image "a-t1.png")))

;; 3x19 - upper green background
(define top-hollow-images 
  (list (make-image "i-t2.png")
        (make-image "a-t2.png")))

;; 11x19 - upper right green to grey border
(define top-righthollow-images 
  (list (make-image "i-t3.png")
        (make-image "a-t3.png")))

;; 3x19 - upper grey background
(define top-grey-images 
  (list (make-image "i-t0.png")
        (make-image "a-t0.png")))

;; 17x16 - iconify icon
(define iconify-images 
  (list (make-image "i-ti.png")
        (make-image "a-ti.png")
        (make-image "p-ti.png")
        (make-image "c-ti.png")))

;; 17x16 - maximize icon
(define maximize-images 
  (list (make-image "i-tm.png")
        (make-image "a-tm.png")
        (make-image "p-tm.png")
        (make-image "c-tm.png")))

;; 17x16 - close icon
(define close-images 
  (list (make-image "i-tx.png")
        (make-image "a-tx.png")
        (make-image "p-tx.png")
        (make-image "c-tx.png")))

;; 18x6 - upper right corner
(define upper-right-images 
  (list (make-image "i-ur.png")
        (make-image "a-ur.png")))

(define upper-right-shaped-images 
  (list (make-image "i-urs.png")
        (make-image "a-urs.png")))

;; 6x18 - left side
(define left-side-images 
  (list (make-image "i-ls.png")
        (make-image "a-ls.png")))

;; 6x18 - right side
(define right-side-images 
  (list (make-image "i-rs.png")
        (make-image "a-rs.png")))

;; 6x6 - lower left corner
(define bottom-left-images 
  (list (make-image "i-ll.png")
        (make-image "a-ll.png")))

;; 18x6 - lower ledge
(define bottom-images 
  (list (make-image "i-bot.png")
        (make-image "a-bot.png")))

;; 6x6 - lower right corner
(define bottom-right-images 
  (list (make-image "i-lr.png")
        (make-image "a-lr.png")))

;; 4x4
(define t-upper-left-images 
  (list (make-image "i-tul.png")
        (make-image "a-tul.png")))

;; 11x4
(define t-upper-side-images 
  (list (make-image "i-ttop.png")
        (make-image "a-ttop.png")))

;; 17x4
(define t-upper-right-images 
  (list (make-image "i-tur.png")
        (make-image "a-tur.png")))

(define t-upper-right-shaped-images 
  (list (make-image "i-turs.png")
        (make-image "a-turs.png")))

;; 4x17
(define t-left-images 
  (list (make-image "i-tls.png")
        (make-image "a-tls.png")))

;; 17x16
(define t-close-images 
  (list (make-image "i-ttx.png")
        (make-image "a-ttx.png")
        (make-image "p-ttx.png")
        (make-image "c-ttx.png")))

;; 17x3
(define t-right-images 
  (list (make-image "i-tt0.png")
        (make-image "a-tt0.png")))

;; 4x4
(define t-bottom-left-images 
  (list (make-image "i-tll.png")
        (make-image "a-tll.png")))

;; 11x4
(define t-bottom-side-images 
  (list (make-image "i-tbot.png")
        (make-image "a-tbot.png")))

;; 17x4
(define t-bottom-right-images 
  (list (make-image "i-tlr.png")
        (make-image "a-tlr.png")))

(define t-bottom-right-shaped-images 
  (list (make-image "i-tlrs.png")
        (make-image "a-tlrs.png")))

(define text-colors (list "grey50" "white"))


;; frame layout

(define frame
  `(((background . ,upper-left-images)
     (left-edge . -6)
     (top-edge . -19)
     (class . top-left-corner))
    
    ;; menu button
    ((background . ,menu-images)
     (top-edge . -19)
     (left-edge . 0)
     (class . menu-button))
    
    ;; top curves
    ;;((background . ,top-lefthollow-images)
    ;; (top-edge . -19)
    ;; ;(left-edge . 16)
    ;; (left-edge . 0)
    ;; (class . title))
    
    ;; top green
    ;;((background . ,top-hollow-images)
    ;; (foreground . ,text-colors)
    ;; (text . ,window-name)
    ;; (x-justify . 4)
    ;; (y-justify . center)
    ;; (top-edge . -19)
    ;; ;(left-edge . 27)
    ;; (left-edge . 11)
    ;; (width . ,(lambda (w) (+ (title-width w) 13)))
    ;; ;(right-edge . 27)
    ;; (class . title))
    
    ;; top curves
    ;;((background . ,top-righthollow-images)
    ;; ;(left-edge . ,(lambda (w) (+ (title-width w) 37)))
    ;; (left-edge . ,(lambda (w) (+ (title-width w) 21)))
    ;; ;(right-edge . 16)
    ;; (top-edge . -19)
    ;; (class . title))
    
    ;; top grey
    ;;((background . ,top-grey-images)
    ;; ;(left-edge . ,(lambda (w) (+ (title-width w) 48)))
    ;; (left-edge . ,(lambda (w) (+ (title-width w) 32)))
    ;; (top-edge . -19)
    ;; ;(right-edge . 48)
    ;; (right-edge . 16)
    ;; (class . title))
    
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
    ;;((background . ,iconify-images)
    ;; (right-edge . 32)
    ;; (top-edge . -19)
    ;; (class . iconify-button))
    
    ;; maximize button
    ;;((background . ,maximize-images)
    ;; (right-edge . 16)
    ;; (top-edge . -19)
    ;; (class . maximize-button))
    
    ;; delete button
    ((background . ,close-images)
     (right-edge . 0)
     (top-edge . -19)
     (class . close-button))
    
    ;; tab left curve
    ((background . ,top-lefthollow-images)
     (top-edge . -19)
     (class . tabbar-horizontal-left-edge))
    
    ;; tab
    ((background . ,top-hollow-images)
     (foreground . ,text-colors)
     (top-edge . -19)
     ;;(left-edge . 0)
     (height . 19)
     (text . ,window-name)
     ;;(x-justify . 4)
     ;;(y-justify . center)
     (class . tabbar-horizontal))
    
    ;; tab right curve
    ((background . ,top-righthollow-images)
     (top-edge . -19)
     (class . tabbar-horizontal-right-edge))))

(define shaped-frame 
  `(((background . ,upper-left-shaped-images)
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
    ;;((background . ,top-lefthollow-images)
    ;;(top-edge . -19)
    ;;(left-edge . 16)
    ;;(class . title))
    
    ;; Title text area
    ;;((background . ,top-hollow-images)
    ;;(foreground . ,text-colors)
    ;;(text . ,window-name)
    ;;(x-justify . 4)
    ;;(y-justify . center)
    ;;(top-edge . -19)
    ;;(left-edge . 27)
    ;;(width . ,(lambda (w) (+ (title-width w) 13)))
    ;;(class . title))
    
    ;; top curves
    ;;((background . ,top-righthollow-images)
    ;;(left-edge . ,(lambda (w) (+ (title-width w) 37)))
    ;;(top-edge . -19)
    ;;(class . title))
    
    ;; top grey
    ;;((background . ,top-grey-images)
    ;;(left-edge . ,(lambda (w) (+ (title-width w) 48)))
    ;;(top-edge . -19)
    ;;(right-edge . 48)
    ;;(class . title))
    
    ;; top-right corner
    ((background . ,upper-right-shaped-images)
     (right-edge . -6)
     (top-edge . -19)
     (height . 19)
     (class . top-right-corner))
    
    ;; iconify button
    ;;((background . ,iconify-images)
    ;;(right-edge . 32)
    ;;(top-edge . -19)
    ;;(class . iconify-button))
    
    ;; maximize button
    ;;((background . ,maximize-images)
    ;;(right-edge . 16)
    ;;(top-edge . -19)
    ;;(class . maximize-button))
    
    ;; delete button
    ((background . ,close-images)
     (right-edge . 0)
     (top-edge . -19)
     (class . close-button))
    
    ;; tab left curve
    ((background . ,top-lefthollow-images)
     (top-edge . -19)
     (class . tabbar-horizontal-left-edge))
    
    ;; tab
    ((background . ,top-hollow-images)
     (foreground . ,text-colors)
     (top-edge . -19)
     ;;(left-edge . 0)
     (height . 19)
     (text . ,window-name)
     ;;(x-justify . 4)
     ;;(y-justify . center)
     (class . tabbar-horizontal))
    
    ;; tab right curve
    ((background . ,top-righthollow-images)
     (top-edge . -19)
     (class . tabbar-horizontal-right-edge))))


(define transient-frame 
  `(((background . ,t-upper-left-images)
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


(define shaped-transient-frame 
  `(((background . ,t-upper-right-shaped-images)
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
     (class . close-button))))

(define create-frames
  (lambda ()
    (let ((Elberg-left-d-w 11)
          (Elberg-right-d-w 11)
          (Elberg-left-m 16)
          (Elberg-rigth-m 16)
          (Elberg-left-m-t 0)
          (Elberg-right-m-t 0))
      (set-tab-adjustments #:theme-left-dec-width Elberg-left-d-w #:theme-right-dec-width Elberg-right-d-w #:theme-left-margin 
                           Elberg-left-m #:theme-right-margin Elberg-rigth-m #:theme-left-margin-transient Elberg-left-m-t 
                           #:theme-right-margin-transient Elberg-right-m-t))))

(create-frames)
  
(add-frame-style 'Elberg-tabbed
                 (lambda (w type)
                   (case type
                         ((default) frame)
                         ((transient) transient-frame)
                         ((shaped) shaped-frame)
                         ((shaped-transient) shaped-transient-frame))))

(define (create-frames-only w)
    (when (eq (window-get w 'current-frame-style) 'Elberg-tabbed)
      (create-frames)))

;; Create only frames, don't rebuild-frame/reframe-window.
;; Tabthemes will reframe/rebuild windows call from tabgroup.jl.
(call-after-state-changed '(title-position) create-frames-only)
