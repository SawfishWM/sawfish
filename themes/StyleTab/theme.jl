;; StyleTab

(define theme-name 'StyleTab)

;;need hash tables for icon cache 
;;
(require 'rep.data.tables)

;; Defcustom and defgroup
(defgroup StyleTab:group "StyleTab"
  :group appearance)

(defgroup StyleTab:settings-group "Settings"
  :group (appearance StyleTab:group))

(defgroup StyleTab:buttons-group "buttons"
  :group (appearance StyleTab:group))

(defgroup StyleTab:top-buttons-group "Top Titlebar Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group))

(defgroup StyleTab:bottom-buttons-group "Bottom Titlebar Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group))

(defgroup StyleTab:left-buttons-group "Left Titlebar Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group))

(defgroup StyleTab:right-buttons-group "Right Titlebar Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group))

(defgroup StyleTab:top-left-buttons-group "Top Titlebar Left Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:top-buttons-group))

(defgroup StyleTab:top-right-buttons-group "Top Titlebar Right Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:top-buttons-group))

(defgroup StyleTab:bottom-left-buttons-group "Bottom Titlebar Left Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:bottom-buttons-group))

(defgroup StyleTab:bottom-right-buttons-group "Bottom Titlebar Right Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:bottom-buttons-group))

(defgroup StyleTab:left-top-buttons-group "Left Titlebar Top Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:left-buttons-group))

(defgroup StyleTab:left-bottom-buttons-group "Left Titlebar Bottom Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:left-buttons-group))

(defgroup StyleTab:right-top-buttons-group "Right Titlebar Top Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:right-buttons-group))

(defgroup StyleTab:right-bottom-buttons-group "Right Titlebar Bottom Buttons"
  :group (appearance StyleTab:group StyleTab:buttons-group StyleTab:right-buttons-group))

(defcustom styletab:style 'Dark "Frame and button style."
  :group (appearance StyleTab:group StyleTab:settings-group)
  :options (Reduce Dark DarkColor Silver SilverColor Smoothly)
  :type symbol)

(defcustom styletab:titlebar-place 'top "Titlebar default place."
  :group (appearance StyleTab:group StyleTab:settings-group)
  :options (top bottom left right)
  :type symbol)

(defcustom styletab:title-dimension 24 "Height of title border. Default 24"
  :group (appearance StyleTab:group StyleTab:settings-group)
  :type (range (16 . 32)))

(defcustom styletab:borders-dimension 4 "Width of window border. Default 4"
  :group (appearance StyleTab:group StyleTab:settings-group)
  :type (range (0 . 10)))

(defcustom styletab:custom-button-width nil "Customize buttons width. (Don't use styles defaults.)"
  :group (appearance StyleTab:group StyleTab:settings-group)
  :type boolean)

(defcustom styletab:button-width 0 "Width of Buttons. Default 0"
  :group (appearance StyleTab:group StyleTab:settings-group)
  :type (range (-4 . 4))
  :depends styletab:custom-button-width)

(mapc
 (lambda (arg)
   (let ((type-list ;; ":type" in defcustom
          (append '(v-and)
                  (make-list 10
			     ;; Here, `list' is necessary. If you
			     ;; replace it with a quote, the configurator
			     ;; crashes.
                             (list 'v-and '(choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                                                   send-to-next lock raise-lower move-resize rename frame-type)
                                   '(boolean "Also show in transients"))))))
     (eval
      (macroexpand
       `(defcustom ,(car arg) ;; name
          ,(cadr arg) ;; default value
          ,(caddr arg) ;; doc
          :group
         ,`(appearance StyleTab:group StyleTab:buttons-group
                       ,(cadddr arg) ,(car (cddddr arg)))
         :type ,type-list
         ))))) ;; end of lambda
 '( ;; list to pass to mapc
   (styletab:top-left-buttons
    '((menu t) (frame-type t) (sticky nil) (shade nil) (space nil)
      (raise-lower nil) (lock nil) (move-resize nil))
    "Top Titlebar Left Buttons (from left to right)"
    StyleTab:top-buttons-group StyleTab:top-left-buttons-group)
   (styletab:top-right-buttons
    '((close t) (maximize t) (minimize nil) (space nil)
      (send-to-next nil) (send-to-prev nil))
    "Top Titlebar Right Buttons (from right to left)"
    StyleTab:top-buttons-group StyleTab:top-right-buttons-group)
   (styletab:bottom-left-buttons
    '((menu t) (frame-type t) (sticky nil) (shade nil) (space nil)
      (raise-lower nil) (lock nil) (move-resize nil))
    "Bottom Titlebar Left Buttons (from left to right)"
    StyleTab:bottom-buttons-group StyleTab:bottom-left-buttons-group)
   (styletab:bottom-right-buttons
    '((close t) (maximize t) (minimize nil) (space nil)
      (send-to-next nil) (send-to-prev nil))
    "Bottom Titlebar Right Buttons (from right to left)"
    StyleTab:bottom-buttons-group StyleTab:bottom-right-buttons-group)
   (styletab:left-top-buttons
    '((close t) (maximize t) (minimize t) (sticky nil) (lock nil))
    "Left Titlebar Top Buttons (from top to bottom)"
    StyleTab:left-buttons-group StyleTab:left-top-buttons-group)
   (styletab:left-bottom-buttons
    '((menu t) (frame-type t))
    "Left Titlebar Bottom Buttons (from bottom to top)"
    StyleTab:left-buttons-group StyleTab:left-bottom-buttons-group)
   (styletab:right-top-buttons
    '((close t) (maximize t) (minimize t) (sticky nil) (lock nil))
    "Right Titlebar Top Buttons (from top to bottom)"
    StyleTab:right-buttons-group StyleTab:right-top-buttons-group)
   (styletab:right-bottom-buttons
    '((menu t) (frame-type t))
    "Right Titlebar Bottom Buttons (from bottom to top)"
    StyleTab:right-buttons-group StyleTab:right-bottom-buttons-group)
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; styles settings 

;;(define styletab-style styletab:style)

(define title-colors-images
  (lambda ()
      (case styletab:style
            ((Reduce) (title-colors-reduce))
            ((Dark) (title-colors-dark))
            ((DarkColor) (title-colors-dark))
            ((Silver) (title-colors-silver))
            ((SilverColor) (title-colors-silver))
            ((Smoothly) (title-colors-smoothly)))))

(define title-colors-reduce
  (lambda ()
    `((focused . "#E5E5E5") (highlighted . "#FDFDFD") (clicked . "#FDFDFD") (inactive . "#B1B1B1") (inactive-highlighted . "#CBCBCB")
      (inactive-clicked . "#CBCBCB"))))

(define title-colors-dark
  (lambda ()
    `((focused . "#F2F2F2") (highlighted . "#FEFEFE") (clicked . "#FEFEFE") (inactive . "#CBCBCB") (inactive-highlighted . "#D8D8D8")
      (inactive-clicked . "#D8D8D8"))))

(define title-colors-smoothly
  (lambda ()
    `((focused . "#000000") (highlighted . "#333333") (clicked . "#333333") (inactive . "#666666") (inactive-highlighted . "#777777")
      (inactive-clicked . "#777777"))))

(define title-colors-silver
  (lambda ()
    `((focused . "#000000") (highlighted . "#333333") (clicked . "#333333") (inactive . "#4C4C4C") (inactive-highlighted . "#666666")
      (inactive-clicked . "#666666"))))


(define button-width-custom
  (lambda ()
    (if (eq styletab:custom-button-width t)
        (button-width-set)
      (case styletab:style
            ((Reduce) (button-width-reduce))
            ((Dark) (button-width-dark))
            ((DarkColor) (button-width-dark))
            ((Silver) (button-width-silver))
            ((SilverColor) (button-width-silver))
            ((Smoothly) (button-width-smoothly))))))

(define button-width-add
  (lambda ()
    (if (eq styletab:custom-button-width nil)
        (button-width-zero)
      (case styletab:style
            ((Reduce) (button-width-reduce))
            ((Dark) (button-width-dark))
            ((DarkColor) (button-width-dark))
            ((Silver) (button-width-silver))
            ((SilverColor) (button-width-silver))
            ((Smoothly) (button-width-smoothly))))))

(define button-width-set (lambda () (+ styletab:button-width (button-width-add))))
(define button-width-zero (lambda () 0))
(define button-width-reduce (lambda () 0))
(define button-width-dark (lambda () 8))
(define button-width-silver (lambda () -4))
(define button-width-smoothly (lambda () 0))

(define tabbar-right-edge-width
  (lambda ()
    (case styletab:style
          ((Reduce) 6)
          ((Dark) 3)
          ((DarkColor) 3)
          ((Silver) 3)
          ((SilverColor) 3)
          ((Smoothly) 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; frame-class, keys bindings

(define (rotate-tab src dest)
  (let ((w (current-event-window))
        (wins (tab-group-window-index (current-event-window)))
        pos-x pos-y fdim framew framehigh dim-x dim-y current-title type)
    (if (not (window-get w 'title-position))
        (case styletab:titlebar-place
              ((top) (setq current-title 'top))
              ((bottom) (setq current-title 'bottom))
              ((left) (setq current-title 'left))
              ((right) (setq current-title 'right)))
      (setq current-title (window-get w 'title-position)))
    (setq type (window-get w 'type))
    (setq pos-x (car (window-position w)))
    (setq pos-y (cdr (window-position w)))
    (setq dim-x (car (window-dimensions w)))
    (setq dim-y (cdr (window-dimensions w)))
    (setq fdim (window-frame-dimensions w))
    (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ dim-x dim-y styletab:title-dimension)) 3))

    (when (not (eq type 'unframed))
      (if (window-get w 'shaded) (unshade-window w))
      (if (eq src 'horiz)
          (if (eq dest 'opposite)
              (progn
                (if (eq current-title 'top)
                    (setq dest 'bottom)
                  (setq dest 'top))
                (when (>= (+ pos-y dim-y styletab:title-dimension framew) (screen-height))
                  (setq pos-y (- (screen-height) dim-y styletab:title-dimension framew)))
                (when (<= pos-y 0) (setq pos-y 0)))
            ;; To left or right
            (if (or (eq (window-get w 'type) 'default)
                    (eq (window-get w 'type) 'transient))
                (setq framehigh (+ (- styletab:title-dimension (* styletab:borders-dimension 2)) framew))
              (setq framehigh (+ styletab:title-dimension framew)))
            (setq dim-x (- dim-x framehigh))
            (setq dim-y (+ dim-y framehigh))
            (when (and (eq dest 'left) (<= pos-x 0))
              (setq pos-x 0))
            (when (and (eq dest 'right)
                       (>= (+ pos-x dim-x framehigh framew framew) (screen-width)))
              (setq pos-x (- (screen-width) dim-x styletab:title-dimension framew))))
        ;; vert
        (if (eq dest 'opposite)
            (progn
              (if (eq current-title 'left)
                  (progn
                    (setq dest 'right)
                    (when (>= (+ pos-x dim-x) (screen-width))
                      (setq pos-x (- (screen-width) dim-x styletab:title-dimension framew))))
                (setq dest 'left)
                (when (<= pos-x 0) (setq pos-x 0))))
          ;; To top or bottom
          (if (or (eq (window-get w 'type) 'default)
                  (eq (window-get w 'type) 'transient))
              (setq framehigh (- styletab:title-dimension (* styletab:borders-dimension 2)))
            (setq framehigh styletab:title-dimension))
          (setq dim-x (+ dim-x framehigh framew))
          (setq dim-y (- dim-y framehigh framew))
          (when (>= (+ pos-y dim-y styletab:title-dimension framew) (screen-height))
            (setq pos-y (- (screen-height) dim-y styletab:title-dimension framew)))
          (when (<= pos-y 0) (setq pos-y 0))))

      (mapcar (lambda (w)
                (window-put w 'title-position dest)) wins)
      (call-window-hook 'window-state-change-hook w (list '(title-position)))
      (mapcar (lambda (w)
                (reframe-window w)
                (move-window-to w pos-x pos-y)
                (resize-window-to w dim-x dim-y)) wins))))

(define (tabbartotop)
  "Move tab-bar to top."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'left)
            (eq (window-get w 'title-position) 'right))
        (rotate-tab 'vert 'top))
    (if (eq (window-get w 'title-position) 'bottom)
        (rotate-tab 'horiz 'opposite))))

(define (tabbartobottom)
  "Move tab-bar to bottom."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'left)
            (eq (window-get w 'title-position) 'right))
        (rotate-tab 'vert 'bottom))
    (if (eq (window-get w 'title-position) 'top)
        (rotate-tab 'horiz 'opposite))))

(define (tabbartoleft)
  "Move tab-bar to left."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'left))
    (if (eq (window-get w 'title-position) 'right)
        (rotate-tab 'vert 'opposite))))

(define (tabbartoright)
  "Move tab-bar to right."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'right))
    (if (eq (window-get w 'title-position) 'left)
        (rotate-tab 'vert 'opposite))))

(define (tabbartoggle)
  " Move tab-bar to the opposite side. (Swap top & bottom, or left & right)"
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'opposite)
      (rotate-tab 'vert 'opposite))))

(define-command-gaol 'tabbar-toggle tabbartoggle)
(define-command-gaol 'tabbar-to-top tabbartotop)
(define-command-gaol 'tabbar-to-bottom tabbartobottom)
(define-command-gaol 'tabbar-to-left tabbartoleft)
(define-command-gaol 'tabbar-to-right tabbartoright)

(def-frame-class tabbar-horizontal-left-edge ()
  (bind-keys tabbar-horizontal-left-edge-keymap
             "Button1-Off" 'tabbar-to-left
             "Button2-Off" 'tabbar-toggle
             "Button3-Off" 'tabbar-to-right))

(def-frame-class tabbar-vertical-top-edge ()
  (bind-keys tabbar-vertical-top-edge-keymap
             "Button1-Off" 'tabbar-to-top
             "Button2-Off" 'tabbar-toggle
             "Button3-Off" 'tabbar-to-bottom))


(define (f-type dest)
  (let ((w (current-event-window))
        (wins (tab-group-window-index (current-event-window)))
        pos-x pos-y dim-x dim-y cur new current-title)
    (if (not (window-get w 'title-position))
        (case styletab:titlebar-place
              ((top) (setq current-title 'top))
              ((bottom) (setq current-title 'bottom))
              ((left) (setq current-title 'left))
              ((right) (setq current-title 'right)))
      (setq current-title (window-get w 'title-position)))
    (setq pos-x (car (window-position w)))
    (setq pos-y (cdr (window-position w)))
    (setq dim-x (car (window-dimensions w)))
    (setq dim-y (cdr (window-dimensions w)))
    (setq cur (window-get w 'type))
    (if (window-get w 'shaded) (unshade-window w))

    (when (eq dest 'def-tra)
      (if (eq cur 'default)
          (setq new 'transient)
        (setq new 'default))
      (when (or (eq cur 'shaped)
                (eq cur 'utility))
        (setq new 'default)
        (setq dim-x (- dim-x (* styletab:borders-dimension 2)))
        (setq dim-y (- dim-y styletab:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (+ dim-x styletab:borders-dimension))
          (setq dim-y (- dim-y styletab:borders-dimension))))
      (when (eq cur 'shaped-transient)
        (setq new 'transient)
        (setq dim-x (- dim-x (* styletab:borders-dimension 2)))
        (setq dim-y (- dim-y styletab:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (+ dim-x styletab:borders-dimension))
          (setq dim-y (- dim-y styletab:borders-dimension))))
      (when (eq cur 'unframed)
        (setq new 'default)
        (when (or (eq current-title 'top)
                  (eq current-title 'bottom))
          (setq dim-x (- dim-x (* styletab:borders-dimension 2)))
          (setq dim-y (- dim-y styletab:borders-dimension styletab:title-dimension)))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-y (- dim-y (* styletab:borders-dimension 2)))
          (setq dim-x (- dim-x styletab:borders-dimension styletab:title-dimension)))))

    (when (eq dest 'sha-tra)
      (if (or (eq cur 'shaped)
              (eq cur 'utility))
          (setq new 'shaped-transient)
        (setq new 'shaped))
      (when (eq cur 'default)
        (setq new 'shaped)
        (setq dim-x (+ dim-x (* styletab:borders-dimension 2)))
        (setq dim-y (+ dim-y styletab:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (- dim-x styletab:borders-dimension))
          (setq dim-y (+ dim-y styletab:borders-dimension))))
      (when (eq cur 'transient)
        (setq new 'shaped-transient)
        (setq dim-x (+ dim-x (* styletab:borders-dimension 2)))
        (setq dim-y (+ dim-y styletab:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (- dim-x styletab:borders-dimension))
          (setq dim-y (+ dim-y styletab:borders-dimension))))
      (when (eq cur 'unframed)
        (setq new 'shaped)
        (if (or (eq current-title 'top)
                (eq current-title 'bottom))
            (setq dim-y (- dim-y styletab:title-dimension))
          (setq dim-x (- dim-x styletab:title-dimension)))))

    (when (eq dest 'unf-def)
      (when (or (eq cur 'default)
                (eq cur 'transient))
        (setq new 'unframed)
        (when (or (eq current-title 'top)
                  (eq current-title 'bottom))
          (setq dim-x (+ dim-x (* styletab:borders-dimension 2)))
          (setq dim-y (+ dim-y styletab:borders-dimension styletab:title-dimension)))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-y (+ dim-y (* styletab:borders-dimension 2)))
          (setq dim-x (+ dim-x styletab:borders-dimension styletab:title-dimension))))
      (when (or (eq cur 'shaped)
                (eq cur 'shaped-transient)
                (eq cur 'utility))
        (setq new 'unframed)
        (if (or (eq current-title 'top)
                (eq current-title 'bottom))
            (setq dim-y (+ dim-y styletab:title-dimension))
          (setq dim-x (+ dim-x styletab:title-dimension))))
      (when (eq cur 'unframed)
        (setq new 'shaped-transient)
        (if (or (eq current-title 'top)
                (eq current-title 'bottom))
            (setq dim-y (- dim-y styletab:title-dimension))
          (setq dim-x (- dim-x styletab:title-dimension)))))
    (when (not (eq cur new))
      (mapcar (lambda (w)
                (window-put w 'type new)
                (reframe-window w)
                (move-window-to w pos-x pos-y)
                (resize-window-to w dim-x dim-y)) wins))))


(define (set-frame-default-and-default/transient)
  "Set frametype to `default' and toggle transient-ness with resize."
  (f-type 'def-tra))

(define (set-frame-unframed-and-unframed/shaped-transient)
  "Set frametype to 'unframed' and toggle transient-ness with resize"
  (f-type 'unf-def))

(define (set-frame-shaped-and-shaped/shaped-transient)
  "Set frametype to shaped and toggle transient-ness with resize"
  (f-type 'sha-tra))

(define-command-gaol 'set-frame-default-and-default/transient-toggle set-frame-default-and-default/transient)
(define-command-gaol 'set-frame-unframed-and-unframed/shaped-transient-toggle set-frame-unframed-and-unframed/shaped-transient)
(define-command-gaol 'set-frame-shaped-and-shaped/shaped-transient-toggle set-frame-shaped-and-shaped/shaped-transient)

(def-frame-class frame-type-button ()
  (bind-keys frame-type-button-keymap
             "Button1-Off" 'set-frame-default-and-default/transient-toggle
             "Button2-Off" 'set-frame-unframed-and-unframed/shaped-transient-toggle
             "Button3-Off" 'set-frame-shaped-and-shaped/shaped-transient-toggle))

(defvar prev-button-keymap
  (bind-keys (make-keymap)
			 "Button3-Off" 'send-to-next-workspace
             "Button2-Click" 'popup-workspace-list
             "Button1-Off" 'send-to-previous-workspace))
(defvar next-button-keymap
  (bind-keys (make-keymap)
             "Button3-Off" 'send-to-previous-workspace
             "Button2-Click" 'popup-workspace-list
             "Button1-Off" 'send-to-next-workspace))
(define-frame-class 'prev-button '((keymap . prev-button-keymap)))
(define-frame-class 'next-button '((keymap . next-button-keymap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; make images 

;; button/icon table
;;
(define styletab-icon-cache (make-weak-table eq-hash eq))

;; frames/title table
;;
(define styletab-frame-cache (make-weak-table eq-hash eq))

(define (window-icon w)
  (or (table-ref styletab-icon-cache w)
      (let ((icon (window-icon-image w)))
        (if icon
            (let ((scaled (scale-image icon (- styletab:title-dimension 7) (- styletab:title-dimension 7))))
              (table-set styletab-icon-cache w scaled)
              scaled)
          (scale-image top-frame-icon-title-images (- styletab:title-dimension 7) (- styletab:title-dimension 7))))))

(define top-frame-icon-title-images 
  (make-image (concat (symbol-name  styletab:style) "/" "top-frame-icon-title-images-f.png")))

(define (get-frame-image img)
  (or
   (table-ref styletab-frame-cache img)
   (let ((image
          (make-image img)))
     (table-set styletab-frame-cache img image)
     image)))

(define make-border-image
  (lambda (w)
    `((focused . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-f.png")))
      (inactive . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-i.png"))))))

(define top-border-images 
  (lambda (w) (make-border-image (concat w "-frame-top-border"))))
(define top-left-corner-images 
  (lambda (w) (make-border-image (concat w "-frame-top-left-corner"))))
(define top-left-corner-shaped-images 
  (lambda (w) (make-border-image (concat w "-frame-top-left-corner-shaped"))))
(define top-right-corner-images
  (lambda (w) (make-border-image (concat w "-frame-top-right-corner"))))
(define top-right-corner-shaped-images
  (lambda (w) (make-border-image (concat w "-frame-top-right-corner-shaped"))))
(define tab-left-icon-images 
  (lambda (w) (make-border-image (concat w "-frame-tab-left-icon"))))
(define tab-images 
  (lambda (w) (make-border-image (concat w "-frame-tab"))))
(define tab-right-images 
  (lambda (w) (make-border-image (concat w "-frame-tab-right"))))
(define tab-bottom-icon-images
  (lambda (w) (make-border-image (concat w "-frame-tab-bottom-icon"))))
(define tab-top-images
  (lambda (w) (make-border-image (concat w "-frame-tab-top"))))
(define title-images 
  (lambda (w) (make-border-image (concat w "-frame-title"))))
(define left-border-images
  (lambda (w) (make-border-image (concat w "-frame-left-border"))))
(define right-border-images
  (lambda (w) (make-border-image (concat w "-frame-right-border"))))
(define bottom-left-corner-images
  (lambda (w) (make-border-image (concat w "-frame-bottom-left-corner"))))
(define bottom-border-images
  (lambda (w) (make-border-image (concat w "-frame-bottom-border"))))
(define bottom-right-corner-images
  (lambda (w) (make-border-image (concat w "-frame-bottom-right-corner"))))
(define bottom-border-cursor-images
  (lambda (w) (make-border-image (concat w "-frame-bottom-border-cursor"))))
(define bottom-left-corner-shaped-images
  (lambda (w) (make-border-image (concat w "-frame-bottom-left-corner-shaped"))))
(define bottom-right-corner-shaped-images
  (lambda (w) (make-border-image (concat w "-frame-bottom-right-corner-shaped"))))

(define make-button-image
  (lambda (w)
    `((focused . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-f.png")))
      (highlighted . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-h.png")))
      (clicked . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-c.png")))
      (inactive . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-i.png")))
      (inactive-highlighted . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-ih.png")))
      (inactive-clicked . ,(get-frame-image (concat (symbol-name  styletab:style) "/" w "-ic.png"))))))

(define button-images
  (lambda (w x) (make-button-image (concat w "-frame-" x "-button"))))

(define top-frame-shade-button-images (make-button-image "top-frame-shade-button"))
(define bottom-frame-shade-button-images (make-button-image "bottom-frame-shade-button"))
(define left-frame-shade-button-images (make-button-image "left-frame-shade-button"))
(define right-frame-shade-button-images (make-button-image "right-frame-shade-button"))
(define top-frame-unshade-button-images (make-button-image "top-frame-unshade-button"))
(define bottom-frame-unshade-button-images (make-button-image "bottom-frame-unshade-button"))
(define left-frame-unshade-button-images (make-button-image "left-frame-unshade-button"))
(define right-frame-unshade-button-images (make-button-image "right-frame-unshade-button"))
(define top-frame-sticky-button-images (make-button-image "top-frame-sticky-button"))
(define bottom-frame-sticky-button-images (make-button-image "bottom-frame-sticky-button"))
(define left-frame-sticky-button-images (make-button-image "left-frame-sticky-button"))
(define right-frame-sticky-button-images (make-button-image "right-frame-sticky-button"))
(define top-frame-unsticky-button-images (make-button-image "top-frame-unsticky-button"))
(define bottom-frame-unsticky-button-images (make-button-image "bottom-frame-unsticky-button"))
(define left-frame-unsticky-button-images (make-button-image "left-frame-unsticky-button"))
(define right-frame-unsticky-button-images (make-button-image "right-frame-unsticky-button"))
(define top-frame-maximize-button-images (make-button-image "top-frame-maximize-button"))
(define bottom-frame-maximize-button-images (make-button-image "bottom-frame-maximize-button"))
(define left-frame-maximize-button-images (make-button-image "left-frame-maximize-button"))
(define right-frame-maximize-button-images (make-button-image "right-frame-maximize-button"))
(define top-frame-unmaximize-button-images (make-button-image "top-frame-unmaximize-button"))
(define bottom-frame-unmaximize-button-images (make-button-image "bottom-frame-unmaximize-button"))
(define left-frame-unmaximize-button-images (make-button-image "left-frame-unmaximize-button"))
(define right-frame-unmaximize-button-images (make-button-image "right-frame-unmaximize-button"))
(define top-frame-lock-button-images (make-button-image "top-frame-lock-button"))
(define bottom-frame-lock-button-images (make-button-image "bottom-frame-lock-button"))
(define left-frame-lock-button-images (make-button-image "left-frame-lock-button"))
(define right-frame-lock-button-images (make-button-image "right-frame-lock-button"))
(define top-frame-unlock-button-images (make-button-image "top-frame-unlock-button"))
(define bottom-frame-unlock-button-images (make-button-image "bottom-frame-unlock-button"))
(define left-frame-unlock-button-images (make-button-image "left-frame-unlock-button"))
(define right-frame-unlock-button-images (make-button-image "right-frame-unlock-button"))
(define top-frame-prev-button-images (make-button-image "top-frame-prev-button"))
(define bottom-frame-prev-button-images (make-button-image "bottom-frame-prev-button"))
(define left-frame-prev-button-images (make-button-image "left-frame-prev-button"))
(define right-frame-prev-button-images (make-button-image "right-frame-prev-button"))
(define top-frame-prev-last-button-images (make-button-image "top-frame-prev-last-button"))
(define bottom-frame-prev-last-button-images (make-button-image "bottom-frame-prev-last-button"))
(define left-frame-prev-last-button-images (make-button-image "left-frame-prev-last-button"))
(define right-frame-prev-last-button-images (make-button-image "right-frame-prev-last-button"))
(define top-frame-next-button-images (make-button-image "top-frame-next-button"))
(define bottom-frame-next-button-images (make-button-image "bottom-frame-next-button"))
(define left-frame-next-button-images (make-button-image "left-frame-next-button"))
(define right-frame-next-button-images (make-button-image "right-frame-next-button"))
(define top-frame-next-last-button-images (make-button-image "top-frame-next-last-button"))
(define bottom-frame-next-last-button-images (make-button-image "bottom-frame-next-last-button"))
(define left-frame-next-last-button-images (make-button-image "left-frame-next-last-button"))
(define right-frame-next-last-button-images (make-button-image "right-frame-next-last-button"))
(define top-frame-raise-lower-button-images (make-button-image "top-frame-raise-lower-button"))
(define bottom-frame-raise-lower-button-images (make-button-image "bottom-frame-raise-lower-button"))
(define left-frame-raise-lower-button-images (make-button-image "left-frame-raise-lower-button"))
(define right-frame-raise-lower-button-images (make-button-image "right-frame-raise-lower-button"))
(define top-frame-ontop-button-images (make-button-image "top-frame-ontop-button"))
(define bottom-frame-ontop-button-images (make-button-image "bottom-frame-ontop-button"))
(define left-frame-ontop-button-images (make-button-image "left-frame-ontop-button"))
(define right-frame-ontop-button-images (make-button-image "right-frame-ontop-button"))
(define top-frame-unontop-button-images (make-button-image "top-frame-unontop-button"))
(define bottom-frame-unontop-button-images (make-button-image "bottom-frame-unontop-button"))
(define left-frame-unontop-button-images (make-button-image "left-frame-unontop-button"))
(define right-frame-unontop-button-images (make-button-image "right-frame-unontop-button"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; buttons, colors settings 

(define top-frame-sticky-image-set
  (lambda (w)
    (if (window-get w 'sticky)
        top-frame-unsticky-button-images
      top-frame-sticky-button-images)))

(define bottom-frame-sticky-image-set
  (lambda (w)
    (if (window-get w 'sticky)
        bottom-frame-unsticky-button-images
      bottom-frame-sticky-button-images)))

(define left-frame-sticky-image-set
  (lambda (w)
    (if (window-get w 'sticky)
        left-frame-unsticky-button-images
      left-frame-sticky-button-images)))

(define right-frame-sticky-image-set
  (lambda (w)
    (if (window-get w 'sticky)
        right-frame-unsticky-button-images
      right-frame-sticky-button-images)))

(define top-frame-shade-image-set
  (lambda (w)
    (if (window-get w 'shaded)
        top-frame-unshade-button-images
      top-frame-shade-button-images)))

(define bottom-frame-shade-image-set
  (lambda (w)
    (if (window-get w 'shaded)
        bottom-frame-unshade-button-images
      bottom-frame-shade-button-images)))

(define left-frame-shade-image-set
  (lambda (w)
    (if (window-get w 'shaded)
        left-frame-unshade-button-images
      left-frame-shade-button-images)))

(define right-frame-shade-image-set
  (lambda (w)
    (if (window-get w 'shaded)
        right-frame-unshade-button-images
      right-frame-shade-button-images)))

(define top-frame-maximize-image-set
  (lambda (w)
    (if (window-get w 'unmaximized-geometry)
        top-frame-unmaximize-button-images
      top-frame-maximize-button-images)))

(define bottom-frame-maximize-image-set
  (lambda (w)
    (if (window-get w 'unmaximized-geometry)
        bottom-frame-unmaximize-button-images
      bottom-frame-maximize-button-images)))

(define left-frame-maximize-image-set
  (lambda (w)
    (if (window-get w 'unmaximized-geometry)
        left-frame-unmaximize-button-images
      left-frame-maximize-button-images)))

(define right-frame-maximize-image-set
  (lambda (w)
    (if (window-get w 'unmaximized-geometry)
        right-frame-unmaximize-button-images
      right-frame-maximize-button-images)))

(defmacro window-in-workspace-p (w space)
  `(memq ,space (window-get ,w 'workspaces)))

(define (get-first-workspace) 1)

(define top-frame-prev-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-first-workspace) 1))
            (window-get w 'sticky))
        top-frame-prev-last-button-images
      top-frame-prev-button-images)))

(define bottom-frame-prev-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-first-workspace) 1))
            (window-get w 'sticky))
        bottom-frame-prev-last-button-images
      bottom-frame-prev-button-images)))

(define left-frame-prev-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-first-workspace) 1))
            (window-get w 'sticky))
        left-frame-prev-last-button-images
      left-frame-prev-button-images)))

(define right-frame-prev-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-first-workspace) 1))
            (window-get w 'sticky))
        right-frame-prev-last-button-images
      right-frame-prev-button-images)))

(define (get-last-workspace)
  (aref
   (nth 2 (get-x-property 'root '_NET_NUMBER_OF_DESKTOPS)) 0))

(define top-frame-next-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-last-workspace) 1))
            (window-get w 'sticky))
        top-frame-next-last-button-images
      top-frame-next-button-images)))

(define bottom-frame-next-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-last-workspace) 1))
            (window-get w 'sticky))
        bottom-frame-next-last-button-images
      bottom-frame-next-button-images)))

(define left-frame-next-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-last-workspace) 1))
            (window-get w 'sticky))
        left-frame-next-last-button-images
      left-frame-next-button-images)))

(define right-frame-next-image-set
  (lambda (w)
    (if (or (window-in-workspace-p w (- (get-last-workspace) 1))
            (window-get w 'sticky))
        right-frame-next-last-button-images
      right-frame-next-button-images)))

(define top-frame-lock-image-set
  (lambda (w)
    (if (window-get w 'fixed-position)
        top-frame-unlock-button-images
      top-frame-lock-button-images)))

(define bottom-frame-lock-image-set
  (lambda (w)
    (if (window-get w 'fixed-position)
        bottom-frame-unlock-button-images
      bottom-frame-lock-button-images)))

(define left-frame-lock-image-set
  (lambda (w)
    (if (window-get w 'fixed-position)
        left-frame-unlock-button-images
      left-frame-lock-button-images)))

(define right-frame-lock-image-set
  (lambda (w)
    (if (window-get w 'fixed-position)
        right-frame-unlock-button-images
      right-frame-lock-button-images)))

(define top-frame-raise-lower-image-set
  (lambda (w)
    (if (= (window-get w 'depth) 0)
        top-frame-raise-lower-button-images
      (if (> (window-get w 'depth) 0)
          top-frame-ontop-button-images
        top-frame-unontop-button-images))))

(define bottom-frame-raise-lower-image-set
  (lambda (w)
    (if (= (window-get w 'depth) 0)
        bottom-frame-raise-lower-button-images
      (if (> (window-get w 'depth) 0)
          bottom-frame-ontop-button-images
        bottom-frame-unontop-button-images))))

(define left-frame-raise-lower-image-set
  (lambda (w)
    (if (= (window-get w 'depth) 0)
        left-frame-raise-lower-button-images
      (if (> (window-get w 'depth) 0)
          left-frame-ontop-button-images
        left-frame-unontop-button-images))))

(define right-frame-raise-lower-image-set
  (lambda (w)
    (if (= (window-get w 'depth) 0)
        right-frame-raise-lower-button-images
      (if (> (window-get w 'depth) 0)
          right-frame-ontop-button-images
        right-frame-unontop-button-images))))

(define frame-width
  (lambda (w)
    (if (or (eq (window-type w) 'default)
			(eq (window-type w) 'transient))
        styletab:borders-dimension
      0)))

(define frame-edge
  (lambda (w)
    (if (or (eq (window-type w) 'default)
            (eq (window-type w) 'transient))
        (- styletab:borders-dimension)
      0)))

(define title-height (lambda (w) styletab:title-dimension)) 
(define title-height-s (lambda (w) (- styletab:title-dimension 2)))
(define title-edge (lambda (w) (- styletab:title-dimension)))
(define title-edge-s (lambda (w) (- (- styletab:title-dimension 2))))
(define top-frame-button-width (lambda (w) (+ styletab:title-dimension (button-width-custom))))
(define bottom-frame-button-width (lambda (w) (+ styletab:title-dimension (button-width-custom))))
(define left-frame-button-height (lambda (w) (+ styletab:title-dimension (button-width-custom))))
(define right-frame-button-height (lambda (w) (+ styletab:title-dimension (button-width-custom))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; frames 

(define top-frame-default-border-corner-group 
  `(((class . top-left-corner)
     (background . ,(top-left-corner-images "top"))
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
	((class . top-right-corner)
     (background . ,(top-right-corner-images "top"))
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(title-images "top"))
     (right-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . 2))))

(define bottom-frame-default-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,(bottom-left-corner-images "bottom"))
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,(bottom-right-corner-images "bottom"))
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(title-images "bottom"))
     (right-edge . 0)
     (bottom-edge . ,title-edge)
     (height . ,title-height-s)
     (width . 2))))

(define left-frame-default-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,(bottom-left-corner-images "left"))
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-left-corner)
     (background . ,(top-left-corner-images "left"))
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))))

(define right-frame-default-border-corner-group
  `(((class . bottom-right-corner)
     (background . ,(bottom-right-corner-images "right"))
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-right-corner)
     (background . ,(top-right-corner-images "right"))
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))))

(define top-frame-border-group
  `(((class . left-border)
     (background . ,(left-border-images "top"))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (top-edge . 0)
     (width . ,frame-width)
     (bottom-edge . 0))
	((class . bottom-left-corner)
     (background . ,(bottom-left-corner-images "top"))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-border)
     (background . ,(bottom-border-images "top"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-right-corner)
     (background . ,(bottom-right-corner-images "top"))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . right-border)
     (background . ,(right-border-images "top"))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))))

(define bottom-frame-border-group
  `(((class . left-border)
     (background . ,(left-border-images "bottom"))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (bottom-edge . 0)
     (width . ,frame-width)
     (top-edge . 0))
    ((class . top-left-corner)
     (background . ,(top-left-corner-images "bottom"))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-border)
     (background . ,(top-border-images "bottom"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-right-corner)
     (background . ,(top-right-corner-images "bottom"))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . right-border)
     (background . ,(right-border-images "bottom"))
     (cursor . sb_h_double_arrow)
     (bottom-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (top-edge . 0))))

(define left-frame-border-group
  `(((class . bottom-border)
     (background . ,(bottom-border-images "left"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-right-corner)
     (background . ,(bottom-right-corner-images "left"))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . right-border)
     (background . ,(right-border-images "left"))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-right-corner)
     (background . ,(top-right-corner-images "left"))
     (top-edge . ,frame-edge)
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,(top-border-images "left"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define right-frame-border-group
  `(((class . bottom-border)
     (background . ,(bottom-border-images "right"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-left-corner)
     (background . ,(bottom-left-corner-images "right"))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . left-border)
     (background . ,(left-border-images "right"))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (left-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-left-corner)
     (background . ,(top-left-corner-images "right"))
     (top-edge . ,frame-edge)
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,(top-border-images "right"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define top-frame-title-group
  `(((class . top-border)
     (background . ,(top-border-images "top"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (top-edge . ,title-edge)
     (right-edge . 0)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- styletab:title-dimension 12)))
     (y-justify . center)
     (background . ,(tab-images "top"))
     (foreground . ,title-colors-images)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(tab-left-icon-images "top"))
     (cursor . hand2)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . 2)
     (x-justify . 5))
    ((class . tabbar-horizontal-right-edge)
     (background . ,(tab-right-images "top"))
     (width . ,tabbar-right-edge-width)
     (height . ,title-height-s)
     (top-edge . ,title-edge-s))))

(define bottom-frame-title-group
  `(((class . title)
     (background . ,(bottom-border-images "bottom"))
     (left-edge . 0)
     (bottom-edge . -2)
     (right-edge . 0)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- styletab:title-dimension 12)))
     (y-justify . center)
     (background . ,(tab-images "bottom"))
     (foreground . ,title-colors-images)
     (bottom-edge . ,title-edge)
     (height . ,title-height-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(tab-left-icon-images "bottom"))
     (cursor . hand2)
     (bottom-edge . ,title-edge)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . 2)
     (x-justify . 5))
    ((class . tabbar-horizontal-right-edge)
     (background . ,(tab-right-images "bottom"))
     (width . ,tabbar-right-edge-width)
     (height . ,title-height-s)
     (bottom-edge . ,title-edge))))

(define left-frame-title-group
  `(((class . left-border)
     (background . ,(left-border-images "left"))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (left-edge . ,title-edge)
     (bottom-edge . 0)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(tab-top-images "left"))
     (cursor . hand2)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (left-edge . ,title-edge-s)
     (y-justify . 4)
     (x-justify . 2))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,(tab-images "left"))
     (left-edge . ,title-edge-s)
     (width . ,title-height-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(tab-bottom-icon-images "left"))
     (left-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . 1)
     (x-justify . 2))))

(define right-frame-title-group
  `(((class . right-border)
     (background . ,(right-border-images "right"))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,title-edge)
     (bottom-edge . 0)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(tab-top-images "right"))
     (cursor . hand2)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (right-edge . ,title-edge-s)
     (y-justify . 4)
     (x-justify . 4))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,(tab-images "right"))
     (right-edge . ,title-edge-s)
     (width . ,title-height-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(tab-bottom-icon-images "right"))
     (right-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . 1)
     (x-justify . 4))))

(define bottom-frame-title-cursor-images
  `(((class . bottom-border)
     (background . ,(bottom-border-cursor-images "bottom"))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (bottom-edge . ,title-edge)
     (right-edge . 0)
     (height . 2))))

(define top-frame-close-button
  `((class . close-button)
    (background . ,(button-images "top" "close"))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-close-button
  `((class . close-button)
    (background . ,(button-images "bottom" "close"))
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-close-button
  `((class . close-button)
    (background . ,(button-images "left" "close"))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-close-button
  `((class . close-button)
    (background . ,(button-images "right" "close"))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-menu-button
  `((class . menu-button)
    (background . ,(button-images "top" "menu"))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-menu-button
  `((class . menu-button)
    (background . ,(button-images "bottom" "menu"))
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-menu-button
  `((class . menu-button)
    (background . ,(button-images "left" "menu"))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-menu-button
  `((class . menu-button)
    (background . ,(button-images "right" "menu"))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-iconify-button
  `((class . iconify-button)
    (background . ,(button-images "top" "iconify"))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-iconify-button
  `((class . iconify-button)
    (background . ,(button-images "bottom" "iconify"))
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-iconify-button
  `((class . iconify-button)
    (background . ,(button-images "left" "iconify"))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-iconify-button
  `((class . iconify-button)
    (background . ,(button-images "right" "iconify"))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-maximize-button
  `((class . maximize-button)
    (background . ,top-frame-maximize-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-maximize-button
  `((class . maximize-button)
    (background . ,bottom-frame-maximize-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-maximize-button
  `((class . maximize-button)
    (background . ,left-frame-maximize-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-maximize-button
  `((class . maximize-button)
    (background . ,right-frame-maximize-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-shade-button
  `((class . shade-button)
    (background . ,top-frame-shade-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-shade-button
  `((class . shade-button)
    (background . ,bottom-frame-shade-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-shade-button
  `((class . shade-button)
    (background . ,left-frame-shade-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-shade-button
  `((class . shade-button)
    (background . ,right-frame-shade-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-sticky-button
  `((class . sticky-button)
    (background . ,top-frame-sticky-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-sticky-button
  `((class . sticky-button)
    (background . ,bottom-frame-sticky-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-sticky-button
  `((class . sticky-button)
    (background . ,left-frame-sticky-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-sticky-button
  `((class . sticky-button)
    (background . ,right-frame-sticky-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-space-button
  `((class . title)
    (background . ,(title-images "top"))
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-space-button
  `((class . title)
    (background . ,(title-images "bottom"))
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-space-button
  `((class . title)
    (background . ,(title-images "left"))
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-space-button
  `((class . title)
    (background . ,(title-images "right"))
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-prev-button
  `((class . prev-button)
    (background . ,top-frame-prev-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-prev-button
  `((class . prev-button)
    (background . ,bottom-frame-prev-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-prev-button
  `((class . prev-button)
    (background . ,left-frame-prev-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-prev-button
  `((class . prev-button)
    (background . ,right-frame-prev-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-next-button
  `((class . next-button)
    (background . ,top-frame-next-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-next-button
  `((class . next-button)
    (background . ,bottom-frame-next-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-next-button
  `((class . next-button)
    (background . ,left-frame-next-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-next-button
  `((class . next-button)
    (background . ,right-frame-next-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-lock-button
  `((class . lock-button)
    (background . ,top-frame-lock-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-lock-button
  `((class . lock-button)
    (background . ,bottom-frame-lock-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-lock-button
  `((class . lock-button)
    (background . ,left-frame-lock-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-lock-button
  `((class . lock-button)
    (background . ,right-frame-lock-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,top-frame-raise-lower-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,bottom-frame-raise-lower-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,left-frame-raise-lower-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,right-frame-raise-lower-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define  top-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(button-images "top" "move-resize"))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define  bottom-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(button-images "bottom" "move-resize"))
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define  left-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(button-images "left" "move-resize"))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define  right-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(button-images "right" "move-resize"))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-rename-button
  `((class . rename-button)
    (background . ,(button-images "top" "rename"))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-rename-button
  `((class . rename-button)
    (background . ,(button-images "bottom" "rename"))
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-rename-button
  `((class . rename-button)
    (background . ,(button-images "left" "rename"))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-rename-button
  `((class . rename-button)
    (background . ,(button-images "right" "rename"))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(button-images "top" "frame-type"))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(button-images "bottom" "frame-type"))
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(button-images "left" "frame-type"))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(button-images "right" "frame-type"))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-shaped-border-corner-group
  `(((class . top-left-corner)
     (background . ,(top-left-corner-shaped-images "top"))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
	((class . top-right-corner)
     (background . ,(top-right-corner-shaped-images "top"))
     (cursor . sb_h_double_arrow)
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(title-images "top"))
     (right-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . 2))))

(define bottom-frame-shaped-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,(bottom-left-corner-shaped-images "bottom"))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,(bottom-right-corner-shaped-images "bottom"))
     (cursor . sb_h_double_arrow)
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(title-images "bottom"))
     (right-edge . 0)
     (bottom-edge . ,title-edge)
     (height . ,title-height-s)
     (width . 2))))

(define left-frame-shaped-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,(bottom-left-corner-shaped-images "left"))
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-left-corner)
     (background . ,(top-left-corner-shaped-images "left"))
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))))

(define right-frame-shaped-border-corner-group
  `(((class . bottom-right-corner)
     (background . ,(bottom-right-corner-shaped-images "right"))
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-right-corner)
     (background . ,(top-right-corner-shaped-images "right"))
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))))

(define top-button-alist
  `((close  . ,top-frame-close-button)
    (menu   . ,top-frame-menu-button)
    (minimize   . ,top-frame-iconify-button)
    (maximize   . ,top-frame-maximize-button)
    (shade  . ,top-frame-shade-button)
    (sticky . ,top-frame-sticky-button)
    (space . ,top-frame-space-button)
    (send-to-prev . ,top-frame-prev-button)
    (send-to-next . ,top-frame-next-button)
    (lock . ,top-frame-lock-button)
    (raise-lower . ,top-frame-raise-lower-button)
    (move-resize . ,top-frame-move-resize-button)
    (rename . ,top-frame-rename-button)
    (frame-type . ,top-frame-frame-type-button)
    (\(none\)   . ,nil)))

(define bottom-button-alist
  `((close  . ,bottom-frame-close-button)
    (menu   . ,bottom-frame-menu-button)
    (minimize   . ,bottom-frame-iconify-button)
    (maximize   . ,bottom-frame-maximize-button)
    (shade  . ,bottom-frame-shade-button)
    (sticky . ,bottom-frame-sticky-button)
    (space . ,bottom-frame-space-button)
    (send-to-prev . ,bottom-frame-prev-button)
    (send-to-next . ,bottom-frame-next-button)
    (lock . ,bottom-frame-lock-button)
    (raise-lower . ,bottom-frame-raise-lower-button)
    (move-resize . ,bottom-frame-move-resize-button)
    (rename . ,bottom-frame-rename-button)
    (frame-type . ,bottom-frame-frame-type-button)
    (\(none\)   . ,nil)))

(define left-button-alist
  `((close  . ,left-frame-close-button)
    (menu   . ,left-frame-menu-button)
    (minimize   . ,left-frame-iconify-button)
    (maximize   . ,left-frame-maximize-button)
    (shade  . ,left-frame-shade-button)
    (sticky . ,left-frame-sticky-button)
    (space . ,left-frame-space-button)
    (send-to-prev . ,left-frame-prev-button)
    (send-to-next . ,left-frame-next-button)
    (lock . ,left-frame-lock-button)
    (raise-lower . ,left-frame-raise-lower-button)
    (move-resize . ,left-frame-move-resize-button)
    (rename . ,left-frame-rename-button)
    (frame-type . ,left-frame-frame-type-button)
    (\(none\)   . ,nil)))

(define right-button-alist
  `((close  . ,right-frame-close-button)
    (menu   . ,right-frame-menu-button)
    (minimize   . ,right-frame-iconify-button)
    (maximize   . ,right-frame-maximize-button)
    (shade  . ,right-frame-shade-button)
    (sticky . ,right-frame-sticky-button)
    (space . ,right-frame-space-button)
    (send-to-prev . ,right-frame-prev-button)
    (send-to-next . ,right-frame-next-button)
    (lock . ,right-frame-lock-button)
    (raise-lower . ,right-frame-raise-lower-button)
    (move-resize . ,right-frame-move-resize-button)
    (rename . ,right-frame-rename-button)
    (frame-type . ,right-frame-frame-type-button)
    (\(none\)   . ,nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; build themes

(define normal-frame nil)
(define shaped-frame nil)
(define transient-frame nil)
(define shaped-transient-frame nil)

;; first run use top title
;;
(define current-title 'top)

(define create-frames
  ;; ripped from Anonymous
  ;;
  (lambda ()
    (let* (
           (button-alist
            (case current-title
                  ((top) top-button-alist)
                  ((bottom) bottom-button-alist)
                  ((left) left-button-alist)
                  ((right) right-button-alist)))

           ;; turns one cons cell (btn-name . show-in-transients) into a button
           ;; definition, adding the button position
           ;;
           (make-button
            (lambda (is-trans btn edge pos)
              (let ((btn-def  (cdr (assq (car btn) button-alist)))
                    (btn-in-trans (last btn)))
                (if (or (null btn-def) (and is-trans (not btn-in-trans)))
                    nil
                  (cons (cons edge pos) btn-def)))))

           ;; turns the list of cons cells (btn-name . show-in-transients) into
           ;; a list of button definitions, adding the button positions
           ;;
           (make-button-list
            (lambda (is-trans btn-list edge pos-start pos-inc)
              (let loop ((rest btn-list) (pos pos-start) (result ()))
                (if (null rest)
                    result
                  (let ((new-btn (make-button is-trans (car rest) edge pos)))
                    (if (null new-btn)
                        (loop (cdr rest) pos result)
                      (loop (cdr rest) (+ pos pos-inc) (append (list new-btn) result))))))))

		   (top-frame-normal-buttons-left 
            (make-button-list nil styletab:top-left-buttons 'left-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (top-frame-normal-buttons-right
            (make-button-list nil styletab:top-right-buttons 'right-edge 0 (+ styletab:title-dimension (button-width-custom))))
           (top-frame-transient-buttons-left
            (make-button-list t styletab:top-left-buttons 'left-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (top-frame-transient-buttons-right
            (make-button-list t styletab:top-right-buttons 'right-edge 0 (+ styletab:title-dimension (button-width-custom))))

           (bottom-frame-normal-buttons-left 
            (make-button-list nil styletab:bottom-left-buttons 'left-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (bottom-frame-normal-buttons-right
            (make-button-list nil styletab:bottom-right-buttons 'right-edge 0 (+ styletab:title-dimension (button-width-custom))))
           (bottom-frame-transient-buttons-left
            (make-button-list t styletab:bottom-left-buttons 'left-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (bottom-frame-transient-buttons-right
            (make-button-list t styletab:bottom-right-buttons 'right-edge 0 (+ styletab:title-dimension (button-width-custom))))

           (left-frame-normal-buttons-left 
            (make-button-list nil styletab:left-bottom-buttons 'bottom-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (left-frame-normal-buttons-right
            (make-button-list nil styletab:left-top-buttons 'top-edge 0 (+ styletab:title-dimension (button-width-custom))))
           (left-frame-transient-buttons-left
            (make-button-list t styletab:left-bottom-buttons 'bottom-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (left-frame-transient-buttons-right
            (make-button-list t styletab:left-top-buttons 'top-edge 0 (+ styletab:title-dimension (button-width-custom))))

           (right-frame-normal-buttons-left 
            (make-button-list nil styletab:right-bottom-buttons 'bottom-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (right-frame-normal-buttons-right
            (make-button-list nil styletab:right-top-buttons 'top-edge 0 (+ styletab:title-dimension (button-width-custom))))
           (right-frame-transient-buttons-left
            (make-button-list t styletab:right-bottom-buttons 'bottom-edge  0 (+ styletab:title-dimension (button-width-custom))))
           (right-frame-transient-buttons-right
            (make-button-list t styletab:right-top-buttons 'top-edge 0 (+ styletab:title-dimension (button-width-custom)))))

      (require 'sawfish.wm.tabs.tab)
      (when (eq current-title 'top)
        (let ((top-left-d-w 11)
              (top-right-d-w (tabbar-right-edge-width))
              (top-left-m
               (if (numberp (cdr (car (car top-frame-normal-buttons-left))))
                   (+ (cdr (car (car top-frame-normal-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (top-right-m
               (if (numberp (cdr (car (car top-frame-normal-buttons-right))))
                   (+ (cdr (car (car top-frame-normal-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0))
              (top-left-m-t
               (if (numberp (cdr (car (car top-frame-transient-buttons-left))))
                   (+ (cdr (car (car top-frame-transient-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (top-right-m-t
               (if (numberp (cdr (car (car top-frame-transient-buttons-right))))
                   (+ (cdr (car (car top-frame-transient-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0)))
          (set-tab-adjustments #:theme-left-dec-width top-left-d-w #:theme-right-dec-width top-right-d-w #:theme-left-margin top-left-m
                               #:theme-right-margin top-right-m #:theme-left-margin-transient top-left-m-t
                               #:theme-right-margin-transient top-right-m-t))
        (setq normal-frame
              (append top-frame-default-border-corner-group top-frame-title-group top-frame-normal-buttons-left 
                      top-frame-border-group top-frame-normal-buttons-right))
        (setq shaped-frame
              (append top-frame-shaped-border-corner-group top-frame-title-group top-frame-normal-buttons-left 
                      top-frame-normal-buttons-right))
        (setq transient-frame
              (append top-frame-default-border-corner-group top-frame-title-group top-frame-transient-buttons-left 
                      top-frame-border-group top-frame-transient-buttons-right))
        (setq shaped-transient-frame
              (append top-frame-shaped-border-corner-group top-frame-title-group top-frame-transient-buttons-left
                      top-frame-transient-buttons-right)))

      (when (eq current-title 'bottom)
        (let ((bottom-left-d-w 11)
              (bottom-right-d-w (tabbar-right-edge-width))
              (bottom-left-m
               (if (numberp (cdr (car (car bottom-frame-normal-buttons-left))))
                   (+ (cdr (car (car bottom-frame-normal-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (bottom-right-m
               (if (numberp (cdr (car (car bottom-frame-normal-buttons-right))))
                   (+ (cdr (car (car bottom-frame-normal-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0))
              (bottom-left-m-t
               (if (numberp (cdr (car (car bottom-frame-transient-buttons-left))))
                   (+ (cdr (car (car bottom-frame-transient-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (bottom-right-m-t
               (if (numberp (cdr (car (car bottom-frame-transient-buttons-right))))
                   (+ (cdr (car (car bottom-frame-transient-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0)))
          (set-tab-adjustments #:theme-left-dec-width bottom-left-d-w #:theme-right-dec-width bottom-right-d-w #:theme-left-margin bottom-left-m
                               #:theme-right-margin bottom-right-m #:theme-left-margin-transient bottom-left-m-t
                               #:theme-right-margin-transient bottom-right-m-t))
        (setq normal-frame
              (append bottom-frame-default-border-corner-group bottom-frame-title-group bottom-frame-normal-buttons-left 
                      bottom-frame-border-group bottom-frame-normal-buttons-right bottom-frame-title-cursor-images))
        (setq shaped-frame
              (append bottom-frame-shaped-border-corner-group bottom-frame-title-group bottom-frame-normal-buttons-left 
                      bottom-frame-normal-buttons-right bottom-frame-title-cursor-images))
        (setq transient-frame
              (append bottom-frame-default-border-corner-group bottom-frame-title-group bottom-frame-transient-buttons-left
                      bottom-frame-border-group bottom-frame-transient-buttons-right bottom-frame-title-cursor-images))
        (setq shaped-transient-frame
              (append bottom-frame-shaped-border-corner-group bottom-frame-title-group bottom-frame-transient-buttons-left 
                      bottom-frame-transient-buttons-right bottom-frame-title-cursor-images)))
      
      (when (eq current-title 'left)
        (let ((left-left-d-w 11)
              (left-right-d-w (- styletab:title-dimension 2))
              (left-left-m
               (if (numberp (cdr (car (car left-frame-normal-buttons-left))))
                   (+ (cdr (car (car left-frame-normal-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (left-right-m
               (if (numberp (cdr (car (car left-frame-normal-buttons-right))))
                   (+ (cdr (car (car left-frame-normal-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0))
              (left-left-m-t
               (if (numberp (cdr (car (car left-frame-transient-buttons-left))))
                   (+ (cdr (car (car left-frame-transient-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (left-right-m-t
               (if (numberp (cdr (car (car left-frame-transient-buttons-right))))
                   (+ (cdr (car (car left-frame-transient-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0)))
          (set-tab-adjustments #:theme-left-dec-width left-left-d-w #:theme-right-dec-width left-right-d-w #:theme-left-margin left-left-m
                               #:theme-right-margin left-right-m #:theme-left-margin-transient left-left-m-t
                               #:theme-right-margin-transient left-right-m-t))
        (setq normal-frame
              (append left-frame-title-group left-frame-normal-buttons-left left-frame-default-border-corner-group 
                      left-frame-border-group left-frame-normal-buttons-right))
        (setq shaped-frame
              (append left-frame-title-group left-frame-normal-buttons-left left-frame-shaped-border-corner-group 
                      left-frame-normal-buttons-right))
        (setq transient-frame
              (append left-frame-title-group left-frame-transient-buttons-left left-frame-default-border-corner-group 
                      left-frame-border-group left-frame-transient-buttons-right))
        (setq shaped-transient-frame
              (append left-frame-title-group left-frame-transient-buttons-left left-frame-shaped-border-corner-group 
                      left-frame-transient-buttons-right)))
      
      (when (eq current-title 'right)
        (let ((right-left-d-w 11)
              (right-right-d-w (- styletab:title-dimension 2))
              (right-left-m
               (if (numberp (cdr (car (car right-frame-normal-buttons-left))))
                   (+ (cdr (car (car right-frame-normal-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (right-right-m
               (if (numberp (cdr (car (car right-frame-normal-buttons-right))))
                   (+ (cdr (car (car right-frame-normal-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0))
              (right-left-m-t
               (if (numberp (cdr (car (car right-frame-transient-buttons-left))))
                   (+ (cdr (car (car right-frame-transient-buttons-left))) (+ styletab:title-dimension (button-width-custom))) 0))
              (right-right-m-t
               (if (numberp (cdr (car (car right-frame-transient-buttons-right))))
                   (+ (cdr (car (car right-frame-transient-buttons-right))) (+ styletab:title-dimension (button-width-custom))) 0)))
          (set-tab-adjustments #:theme-left-dec-width right-left-d-w #:theme-right-dec-width right-right-d-w #:theme-left-margin right-left-m
                               #:theme-right-margin right-right-m #:theme-left-margin-transient right-left-m-t
                               #:theme-right-margin-transient right-right-m-t))
        (setq normal-frame
              (append right-frame-title-group right-frame-normal-buttons-left right-frame-default-border-corner-group 
                      right-frame-border-group right-frame-normal-buttons-right))
        (setq shaped-frame
              (append right-frame-title-group right-frame-normal-buttons-left right-frame-shaped-border-corner-group 
                      right-frame-normal-buttons-right))
        (setq transient-frame
              (append right-frame-title-group right-frame-transient-buttons-left right-frame-default-border-corner-group 
                      right-frame-border-group right-frame-transient-buttons-right))
        (setq shaped-transient-frame
              (append right-frame-title-group right-frame-transient-buttons-left right-frame-shaped-border-corner-group 
                      right-frame-transient-buttons-right))))))

(create-frames)

;; At last! We create the actual theme
;;
(add-frame-style theme-name
                 (lambda (w type)
                   (case type
                         ((default)             normal-frame)
                         ((shaped)              shaped-frame)
                         ((transient)           transient-frame)
                         ((shaped-transient)    shaped-transient-frame)
                         ((utility)             normal-frame)
                         ((shaded-utility)      normal-frame)
                         ((unframed)            nil-frame))))

(define (current-title-w w)
  (if (not (window-get w 'title-position))
      (case styletab:titlebar-place
            ((top) (setq current-title 'top))
            ((bottom) (setq current-title 'bottom))
            ((left) (setq current-title 'left))
            ((right) (setq current-title 'right)))
    (setq current-title (window-get w 'title-position)))
  (create-frames))

(define (create-frames-only w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (current-title-w w)))

(define (reframe-windows style)
  (map-windows
   (lambda (w)
     (when (eq (window-get w 'current-frame-style) style)
       (current-title-w w)
       (reframe-window w)))))

(define reframe-all
  (lambda ()
    (reframe-windows theme-name)))

(define (reframe-one w)
  (when (not (window-get w 'tabbed))
    (when (eq (window-get w 'current-frame-style) theme-name)
      (current-title-w w)
      (rebuild-frame w))))

;; also reset icon cache
;;
(define clear-cache-reframe
  (lambda ()
    (setq styletab-icon-cache (make-weak-table eq-hash eq))
    (reframe-all)))

;; also reset all cache 
;;
(define (clear-cache-reload-frame-style)
  (setq styletab-icon-cache (make-weak-table eq-hash eq))
  (setq styletab-frame-cache (make-weak-table eq-hash eq))
  (reload-frame-style theme-name))

(call-after-state-changed '(sticky fixed-position stacking) reframe-one)
(call-after-state-changed '(title-position) create-frames-only)
(add-hook 'remove-from-workspace-hook reframe-one)

(custom-set-property 'styletab:custom-colors ':after-set reframe-all)
(custom-set-property 'styletab:style ':after-set clear-cache-reload-frame-style)
(custom-set-property 'styletab:title-dimension ':after-set clear-cache-reframe)
(custom-set-property 'styletab:custom-button-width ':after-set clear-cache-reframe)
(custom-set-property 'styletab:button-width ':after-set clear-cache-reframe)
(custom-set-property 'styletab:borders-dimension ':after-set reframe-all)
(custom-set-property 'styletab:titlebar-place ':after-set reframe-all)
(custom-set-property 'styletab:top-left-buttons ':after-set reframe-all)
(custom-set-property 'styletab:top-right-buttons ':after-set reframe-all)
(custom-set-property 'styletab:bottom-left-buttons ':after-set reframe-all)
(custom-set-property 'styletab:bottom-right-buttons ':after-set reframe-all)
(custom-set-property 'styletab:left-top-buttons ':after-set reframe-all)
(custom-set-property 'styletab:left-bottom-buttons ':after-set reframe-all)
(custom-set-property 'styletab:right-top-buttons ':after-set reframe-all)
(custom-set-property 'styletab:right-bottom-buttons ':after-set reframe-all)
