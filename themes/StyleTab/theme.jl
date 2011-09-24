;; StyleTab

(define theme-name 'StyleTab)

;;need hash tables for icon cache 
(require 'rep.data.tables)

;; recolor imanges
(require 'sawfish.wm.util.recolor-image)

;; Defcustom and defgroup
(defgroup StyleTab:group "StyleTab"
  :group appearance)

(defgroup StyleTab:color-group "colors"
  :group (appearance StyleTab:group))

(defgroup StyleTab:top-buttons-group "Top Titlebar Buttons"
  :group (appearance StyleTab:group))

(defgroup StyleTab:bottom-buttons-group "Bottom Titlebar Buttons"
  :group (appearance StyleTab:group))

(defgroup StyleTab:left-buttons-group "Left Titlebar Buttons"
  :group (appearance StyleTab:group))

(defgroup StyleTab:right-buttons-group "Right Titlebar Buttons"
  :group (appearance StyleTab:group))

(defcustom styletab-c:do-reload nil "Do reload. Will reload the theme if switch on/off.
Essential if you change the title hight or botton size.
Fix/hack for foreground button, but makes botton ugly."
  :group (appearance StyleTab:group)
  :type boolean)

(defcustom styletab-c:styles 'Default "Frame and button style."
  :group (appearance StyleTab:group)
  :type symbol
  :options (Default))
;;  :options (Default Reduce Glass WixDa Smoothly)

(defcustom styletab-c:titlebar-place 'top "Titlebar default place."
  :group (appearance StyleTab:group)
  :type symbol
  :options (top bottom left right))

(defcustom styletab-c:title-dimension 24 "Height of title border. "
  :group (appearance StyleTab:group)
  :type number
  :range (16 . 32))

(defcustom styletab-c:borders-dimension 4 "Width of window border."
  :group (appearance StyleTab:group)
  :type number
  :range (0 . 10))

(defcustom styletab-c:custom-button-width nil "Customize buttons width."
  :group (appearance StyleTab:group)
  :type boolean)

(defcustom styletab-c:button-width 0 "Width of Buttons."
  :group (appearance StyleTab:group)
  :type number
  :depends styletab-c:custom-button-width
  :range (-4 . 4))

(defcustom styletab-c:proposals 'Pink "Proposals."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type symbol
  :options (Brown Dark Darkblue Blue Pink Green)
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:hightlight-tabbar nil "Also hightlighted tabbars."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :after-set (lambda () (botton-color-changed recolor-tab)))

(defcustom styletab-c:custom-frame-colors nil "Customize frame color/brightness."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:focus-frame-color (get-color "#780000")
  "Focus frame color."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type color
  :depends styletab-c:custom-frame-colors
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:unfocus-frame-color (get-color "#780000")
  "Inactive frame color"
  :group (appearance StyleTab:group StyleTab:color-group)
  :type color
  :depends styletab-c:custom-frame-colors
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:inactive-dimout 2 "Dimout inactive frame."
  :group (appearance StyleTab:group StyleTab:color-group)
  :depends styletab-c:custom-frame-colors
  :type number
  :range (0 . 5)
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:active-hightlight-brighten 2 "Focus window brightness mouse over buttons."
  :group (appearance StyleTab:group StyleTab:color-group)
  :depends styletab-c:custom-frame-colors
  :type number
  :range (-5 . 5)
  :after-set (lambda () (bright-changed)))

(defcustom styletab-c:inactive-hightlight-brighten 2 "Inactive windows brightness mouse over buttons."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type number
  :depends styletab-c:custom-frame-colors
  :range (-5 . 5)
  :after-set (lambda () (bright-changed)))

(defcustom styletab-c:hightlight-close nil
  "Hightlighted close button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-close-button)))

(defcustom styletab-c:hightlight-close-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-close
  :after-set (lambda () (botton-color-changed recolor-close-button)))

(defcustom styletab-c:hightlight-maximize nil
  "Hightlighted maximize button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-maximize-button)))

(defcustom styletab-c:hightlight-maximize-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-maximize
  :after-set (lambda () (botton-color-changed recolor-maximize-button)))

(defcustom styletab-c:hightlight-iconify nil
  "Hightlighted minimize button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-iconify-button)))

(defcustom styletab-c:hightlight-iconify-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-iconify
  :after-set (lambda () (botton-color-changed recolor-iconify-button)))

(defcustom styletab-c:hightlight-shade nil
  "Hightlighted shade button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-shade-button)))

(defcustom styletab-c:hightlight-shade-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-shade
  :after-set (lambda () (botton-color-changed recolor-shade-button)))

(defcustom styletab-c:hightlight-sticky nil
  "Hightlighted sticky button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-sticky-button)))

(defcustom styletab-c:hightlight-sticky-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-sticky
  :after-set (lambda () (botton-color-changed recolor-sticky-button)))

(defcustom styletab-c:hightlight-menu nil
  "Hightlighted menu button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-menu-button)))

(defcustom styletab-c:hightlight-menu-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-menu
  :after-set (lambda () (botton-color-changed recolor-menu-button)))

(defcustom styletab-c:hightlight-frame-type nil
  "Hightlighted frame type button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-frame-type-button)))

(defcustom styletab-c:hightlight-frame-type-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-frame-type
  :after-set (lambda () (botton-color-changed recolor-frame-type-button)))

(defcustom styletab-c:hightlight-lock nil
  "Hightlighted lock button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-lock-button)))

(defcustom styletab-c:hightlight-lock-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-lock
  :after-set (lambda () (botton-color-changed recolor-lock-button)))

(defcustom styletab-c:hightlight-move-resize nil
  "Hightlighted move/resize button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-move-resize-button)))

(defcustom styletab-c:hightlight-move-resize-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-move-resize
  :after-set (lambda () (botton-color-changed recolor-move-resize-button)))

(defcustom styletab-c:hightlight-raise-lower nil
  "Hightlighted raise/lower button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-raise-lower-button)))

(defcustom styletab-c:hightlight-raise-lower-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-raise-lower
  :after-set (lambda () (botton-color-changed recolor-raise-lower-button)))

(defcustom styletab-c:hightlight-next nil
  "Hightlighted next workspace button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-next-button)))

(defcustom styletab-c:hightlight-next-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-next
  :after-set (lambda () (botton-color-changed recolor-next-button)))

(defcustom styletab-c:hightlight-prev nil
  "Hightlighted previous workspace button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-prev-button)))

(defcustom styletab-c:hightlight-prev-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-prev
  :after-set (lambda () (botton-color-changed recolor-prev-button)))

(defcustom styletab-c:hightlight-rename nil
  "Hightlighted rename button."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-rename-button)))

(defcustom styletab-c:hightlight-rename-all nil "Always use."
  :group (appearance StyleTab:group StyleTab:color-group)
  :type boolean
  :depends styletab-c:hightlight-rename
  :after-set (lambda () (botton-color-changed recolor-rename-button)))

(mapc
 (lambda (arg)
   (let ((type-list ;; ":type" in defcustom
          (append '(v-and)
                  (make-list 10
                             ;; Here, `list' is necessary. If you
                             ;; replace it with a quote, the configurator
                             ;; crashes.
                             (list 'h-and '(choice \(none\) close menu maximize minimize shade sticky space send-to-prev
                                                   send-to-next lock raise-lower move-resize rename frame-type)
                                   '(boolean "Also show in transients"))))))
     (eval
      (macroexpand
       `(defcustom ,(car arg) ;; name
          ,(cadr arg) ;; default value
          ,(caddr arg) ;; doc
          :group
          ,`(appearance StyleTab:group
                        ,(cadddr arg))
          :type ,type-list
          ))))) ;; end of lambda
 '( ;; list to pass to mapc
   (styletab-c:top-left-buttons
    '((menu t) (sticky nil) (shade nil))
    "Top Titlebar Left Buttons (from left to right) \\top"
    StyleTab:top-buttons-group)
   (styletab-c:top-right-buttons
    '((close t) (maximize t) (minimize nil))
    "Top Titlebar Right Buttons (from right to left) \\top"
    StyleTab:top-buttons-group)
   (styletab-c:bottom-left-buttons
    '((menu t) (sticky nil) (shade nil))
    "Bottom Titlebar Left Buttons (from left to right) \\top"
    StyleTab:bottom-buttons-group)
   (styletab-c:bottom-right-buttons
    '((close t) (maximize t) (minimize nil))
    "Bottom Titlebar Right Buttons (from right to left) \\top"
    StyleTab:bottom-buttons-group)
   (styletab-c:left-top-buttons
    '((close t) (maximize t) (minimize nil))
    "Left Titlebar Top Buttons (from top to bottom) \\top"
    StyleTab:left-buttons-group)
   (styletab-c:left-bottom-buttons
    '((menu t) (sticky nil) (shade nil))
    "Left Titlebar Bottom Buttons (from bottom to top) \\top"
    StyleTab:left-buttons-group)
   (styletab-c:right-top-buttons
    '((close t) (maximize t) (minimize nil))
    "Right Titlebar Top Buttons (from top to bottom) \\top"
    StyleTab:right-buttons-group)
   (styletab-c:right-bottom-buttons
    '((menu t) (sticky nil) (shade nil))
    "Right Titlebar Bottom Buttons (from bottom to top) \\top"
    StyleTab:right-buttons-group)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; styles settings 

(define proposals-colors
  (lambda ()
    (case styletab-c:proposals
          ;; format "color" "dimout inactive frame" "brighte mouse over button active" "brighte mouse over button inactive"
          ((Dark) (list "#000033" '40 '100 '100))
          ((Darkblue) (list "#00006E" '30 '70 '70))
          ((Brown) (list "#780000" '40 '40 '50))
          ((Blue) (list "#0000B4" '30 '40 '50))
          ((Pink) (list "#7F0081" '50 '50 '50))
          ((Green) (list "#006400" '30 '60 '60)))))

(define title-colors-images
  (lambda ()
    (case styletab-c:styles
          ((Default) (title-colors-default))
          ((Reduce) (title-colors-reduce))
          ((Glass) (title-colors-glass))
          ((WixDa) (title-colors-wixda))
          ((Smoothly) (title-colors-smoothly)))))

(define title-colors-default
  (lambda ()
    `((focused . "#F2F2F2") (highlighted . "#FEFEFE") (clicked . "#FEFEFE") (inactive . "#CBCBCB") (inactive-highlighted . "#D8D8D8")
      (inactive-clicked . "#D8D8D8"))))

(define title-colors-reduce
  (lambda ()
    `((focused . "#E5E5E5") (highlighted . "#FDFDFD") (clicked . "#FDFDFD") (inactive . "#B1B1B1") (inactive-highlighted . "#CBCBCB")
      (inactive-clicked . "#CBCBCB"))))

(define title-colors-glass
  (lambda ()
    `((focused . "#E5E5E5") (highlighted . "#FDFDFD") (clicked . "#FDFDFD") (inactive . "#2B2B2B") (inactive-highlighted . "#000000")
      (inactive-clicked . "#000000"))))

(define title-colors-wixda
  (lambda ()
    `((focused . "#000000") (highlighted . "#333333") (clicked . "#333333") (inactive . "#4C4C4C") (inactive-highlighted . "#666666")
      (inactive-clicked . "#666666"))))

(define title-colors-smoothly
  (lambda ()
    `((focused . "#000000") (highlighted . "#333333") (clicked . "#333333") (inactive . "#666666") (inactive-highlighted . "#777777")
      (inactive-clicked . "#777777"))))

(define button-width-custom
  (lambda ()
    (if (eq styletab-c:custom-button-width t)
        (button-width-set)
      (case styletab-c:styles
            ((Default) (button-width-default))
            ((Reduce) (button-width-reduce))
            ((Glass) (button-width-glass))
            ((WixDa) (button-width-wixda))
            ((Smoothly) (button-width-smoothly))))))

(define button-width-add
  (lambda ()
    (if (eq styletab-c:custom-button-width nil)
        (button-width-zero)
      (case styletab-c:styles
            ((Default) (button-width-default))
            ((Reduce) (button-width-reduce))
            ((Glass) (button-width-glass))
            ((WixDa) (button-width-wixda))
            ((Smoothly) (button-width-smoothly))))))

(define button-width-set (lambda () (+ styletab-c:button-width (button-width-add))))
(define button-width-zero (lambda () 0))
(define button-width-default (lambda () 8))
(define button-width-reduce (lambda () 0))
(define button-width-glass (lambda () 0))
(define button-width-wixda (lambda () -4))
(define button-width-smoothly (lambda () 0))

;; end of tabtext
(define tabbar-right-edge-width
  (lambda ()
    (case styletab-c:styles
          ((Default) 3)
          ((Reduce) 6)
          ((Glass) 3)
          ((WixDa) 3)
          ((Smoothly) 3))))

;; edge of first buttons left/right
(define button-left-edge
  (lambda ()
    (case styletab-c:styles
          ((Default) 1)
          ((Reduce) 1)
          ((Glass) 1)
          ((WixDa) 1)
          ((Smoothly) 1))))

(define button-right-edge
  (lambda ()
    (case styletab-c:styles
          ((Default) 2)
          ((Reduce) 2)
          ((Glass) 1)
          ((WixDa) 1)
          ((Smoothly) 3))))

(define icon-edge
  (lambda ()
    (case styletab-c:styles
          ((Default) 2)
          ((Reduce) 2)
          ((Glass) 1)
          ((WixDa) 1)
          ((Smoothly) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; frame-class, keys bindings

(define (rotate-tab src dest)
  (let ((w (current-event-window))
        (wins (tab-group-window-index (current-event-window)))
        pos-x pos-y fdim framew framehigh dim-x dim-y current-title type)
    (if (not (window-get w 'title-position))
        (case styletab-c:titlebar-place
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
    (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ dim-x dim-y styletab-c:title-dimension)) 3))

    (when (not (eq type 'unframed))
      (if (window-get w 'shaded) (unshade-window w))
      (if (eq src 'horiz)
          (if (eq dest 'opposite)
              (progn
                (if (eq current-title 'top)
                    (setq dest 'bottom)
                  (setq dest 'top))
                (when (>= (+ pos-y dim-y styletab-c:title-dimension framew) (screen-height))
                  (setq pos-y (- (screen-height) dim-y styletab-c:title-dimension framew)))
                (when (<= pos-y 0) (setq pos-y 0)))
            ;; To left or right
            (if (or (eq (window-get w 'type) 'default)
                    (eq (window-get w 'type) 'transient))
                (setq framehigh (+ (- styletab-c:title-dimension (* styletab-c:borders-dimension 2)) framew))
              (setq framehigh (+ styletab-c:title-dimension framew)))
            (setq dim-x (- dim-x framehigh))
            (setq dim-y (+ dim-y framehigh))
            (when (and (eq dest 'left) (<= pos-x 0))
              (setq pos-x 0))
            (when (and (eq dest 'right)
                       (>= (+ pos-x dim-x framehigh framew framew) (screen-width)))
              (setq pos-x (- (screen-width) dim-x styletab-c:title-dimension framew))))
        ;; vert
        (if (eq dest 'opposite)
            (progn
              (if (eq current-title 'left)
                  (progn
                    (setq dest 'right)
                    (when (>= (+ pos-x dim-x) (screen-width))
                      (setq pos-x (- (screen-width) dim-x styletab-c:title-dimension framew))))
                (setq dest 'left)
                (when (<= pos-x 0) (setq pos-x 0))))
          ;; To top or bottom
          (if (or (eq (window-get w 'type) 'default)
                  (eq (window-get w 'type) 'transient))
              (setq framehigh (- styletab-c:title-dimension (* styletab-c:borders-dimension 2)))
            (setq framehigh styletab-c:title-dimension))
          (setq dim-x (+ dim-x framehigh framew))
          (setq dim-y (- dim-y framehigh framew))
          (when (>= (+ pos-y dim-y styletab-c:title-dimension framew) (screen-height))
            (setq pos-y (- (screen-height) dim-y styletab-c:title-dimension framew)))
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
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'left)
            (eq (window-get w 'title-position) 'right))
        (rotate-tab 'vert 'top))
    (if (eq (window-get w 'title-position) 'bottom)
        (rotate-tab 'horiz 'opposite))))

(define (tabbartobottom)
  "Move tab-bar to bottom."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'left)
            (eq (window-get w 'title-position) 'right))
        (rotate-tab 'vert 'bottom))
    (if (eq (window-get w 'title-position) 'top)
        (rotate-tab 'horiz 'opposite))))

(define (tabbartoleft)
  "Move tab-bar to left."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'left))
    (if (eq (window-get w 'title-position) 'right)
        (rotate-tab 'vert 'opposite))))

(define (tabbartoright)
  "Move tab-bar to right."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'right))
    (if (eq (window-get w 'title-position) 'left)
        (rotate-tab 'vert 'opposite))))

(define (tabbartoggle)
  "Move tab-bar to the opposite side. (Swap top & bottom, or left & right)"
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
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
        (case styletab-c:titlebar-place
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
        (setq dim-x (- dim-x (* styletab-c:borders-dimension 2)))
        (setq dim-y (- dim-y styletab-c:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (+ dim-x styletab-c:borders-dimension))
          (setq dim-y (- dim-y styletab-c:borders-dimension))))
      (when (eq cur 'shaped-transient)
        (setq new 'transient)
        (setq dim-x (- dim-x (* styletab-c:borders-dimension 2)))
        (setq dim-y (- dim-y styletab-c:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (+ dim-x styletab-c:borders-dimension))
          (setq dim-y (- dim-y styletab-c:borders-dimension))))
      (when (eq cur 'unframed)
        (setq new 'default)
        (when (or (eq current-title 'top)
                  (eq current-title 'bottom))
          (setq dim-x (- dim-x (* styletab-c:borders-dimension 2)))
          (setq dim-y (- dim-y styletab-c:borders-dimension styletab-c:title-dimension)))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-y (- dim-y (* styletab-c:borders-dimension 2)))
          (setq dim-x (- dim-x styletab-c:borders-dimension styletab-c:title-dimension)))))

    (when (eq dest 'sha-tra)
      (if (or (eq cur 'shaped)
              (eq cur 'utility))
          (setq new 'shaped-transient)
        (setq new 'shaped))
      (when (eq cur 'default)
        (setq new 'shaped)
        (setq dim-x (+ dim-x (* styletab-c:borders-dimension 2)))
        (setq dim-y (+ dim-y styletab-c:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (- dim-x styletab-c:borders-dimension))
          (setq dim-y (+ dim-y styletab-c:borders-dimension))))
      (when (eq cur 'transient)
        (setq new 'shaped-transient)
        (setq dim-x (+ dim-x (* styletab-c:borders-dimension 2)))
        (setq dim-y (+ dim-y styletab-c:borders-dimension))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-x (- dim-x styletab-c:borders-dimension))
          (setq dim-y (+ dim-y styletab-c:borders-dimension))))
      (when (eq cur 'unframed)
        (setq new 'shaped)
        (if (or (eq current-title 'top)
                (eq current-title 'bottom))
            (setq dim-y (- dim-y styletab-c:title-dimension))
          (setq dim-x (- dim-x styletab-c:title-dimension)))))

    (when (eq dest 'unf-def)
      (when (or (eq cur 'default)
                (eq cur 'transient))
        (setq new 'unframed)
        (when (or (eq current-title 'top)
                  (eq current-title 'bottom))
          (setq dim-x (+ dim-x (* styletab-c:borders-dimension 2)))
          (setq dim-y (+ dim-y styletab-c:borders-dimension styletab-c:title-dimension)))
        (when (not (or (eq current-title 'top)
                       (eq current-title 'bottom)))
          (setq dim-y (+ dim-y (* styletab-c:borders-dimension 2)))
          (setq dim-x (+ dim-x styletab-c:borders-dimension styletab-c:title-dimension))))
      (when (or (eq cur 'shaped)
                (eq cur 'shaped-transient)
                (eq cur 'utility))
        (setq new 'unframed)
        (if (or (eq current-title 'top)
                (eq current-title 'bottom))
            (setq dim-y (+ dim-y styletab-c:title-dimension))
          (setq dim-x (+ dim-x styletab-c:title-dimension))))
      (when (eq cur 'unframed)
        (setq new 'shaped-transient)
        (if (or (eq current-title 'top)
                (eq current-title 'bottom))
            (setq dim-y (- dim-y styletab-c:title-dimension))
          (setq dim-x (- dim-x styletab-c:title-dimension)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; make images/recolor 

;; button/icon table
(define styletab-c-icon-cache (make-weak-table eq-hash eq))

;; frames/title table
(define styletab-c-frame-cache (make-weak-table eq-hash eq))

(define (window-icon w)
  (or (table-ref styletab-c-icon-cache w)
      (let ((icon (window-icon-image w)))
        (if icon
            (let ((scaled (scale-image icon (- styletab-c:title-dimension 7) (- styletab-c:title-dimension 7))))
              (table-set styletab-c-icon-cache w scaled)
              scaled)
          (scale-image top-frame-icon-title-images (- styletab-c:title-dimension 7) (- styletab-c:title-dimension 7))))))

(defun brighten-color (color percentage)
  (let* ((red (car (color-rgb color)))
         (green (cadr (color-rgb color)))
         (blue (car (cddr (color-rgb color))))
         (red-delta (quotient (* red percentage) 100))
         (green-delta (quotient (* green percentage) 100))
         (blue-delta (quotient (* blue percentage) 100)))
    (get-color-rgb (min (+ red-delta red) 65535)
                   (min (+ green-delta green) 65535)
                   (min (+ blue-delta blue) 65535))))

(defun darken-color (color percentage)
  (let* ((red (car (color-rgb color)))
         (green (cadr (color-rgb color)))
         (blue (car (cddr (color-rgb color))))
         (red-delta (quotient (* red percentage) 100))
         (green-delta (quotient (* green percentage) 100))
         (blue-delta (quotient (* blue percentage) 100)))
    (get-color-rgb (max (- red red-delta) 0)
                   (max (- green green-delta) 0)
                   (max (- blue blue-delta) 0))))

(defun do-recolor (img color)
  (let* ((recolorer
          (make-image-recolorer color
                                #:zero-channel blue-channel
                                #:index-channel green-channel)))
    (recolorer img)))

(define (get-recolor-dark dimout color)
  (darken-color color dimout))

(define (get-recolor-bright bright color)
  (brighten-color color bright))

(define (do-make-image img)
  (or
   (table-ref styletab-c-frame-cache img)
   (let ((image
          (make-image img)))
     (table-set styletab-c-frame-cache img image)
     image)))

(define make-border-image
  (lambda (w)
    `((focused . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-f.png")))
      (inactive . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-f.png")))))) 

(define make-tab-image
  (lambda (w)
    `((focused . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-f.png")))
      (highlighted . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-f.png")))
      (inactive . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-i.png")))
      (inactive-highlighted . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-i.png"))))))

(define make-base-button-image
  (lambda (w)
    `((focused . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-button-f.png")))
      (highlighted . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-button-f.png")))
      (clicked . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-button-c.png")))
      (inactive . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-button-f.png")))
      (inactive-highlighted . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-button-f.png")))
      (inactive-clicked . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-button-c.png"))))))

;; Scale foreground botton here.
;; I have no idea how to scale
;; a botton after the botton was 
;; create with make-image, (scale-image
;; then not work like recolor-image)
;; so theme or sawfish must restart/reload 
;; if titel or botton sitz change but it
;; makes the forground ugly
(define scale-w nil)
(define scale-h nil)
(define make-hight-button-image
  (lambda (w x)
    (if (or (= w '"top")
            (= w '"bottom"))
        (progn (setq scale-w (+ styletab-c:title-dimension (button-width-custom)))
               (setq scale-h (- styletab-c:title-dimension 4)))
      (progn (setq scale-w (- styletab-c:title-dimension 4))
             (setq scale-h (+ styletab-c:title-dimension (button-width-custom)))))
    `((focused . ,(scale-image (do-make-image (concat (symbol-name styletab-c:styles) 
                                                                  "/" w "-frame-" x "-button-f.png")) scale-w scale-h))
      (highlighted . ,(scale-image (do-make-image (concat (symbol-name styletab-c:styles) 
                                                                      "/" w "-frame-" x "-button-f.png")) scale-w scale-h))
      (clicked . ,(scale-image (do-make-image (concat (symbol-name styletab-c:styles) 
                                                                  "/" w "-frame-" x "-button-c.png")) scale-w scale-h))
      (inactive . ,(scale-image (do-make-image (concat (symbol-name styletab-c:styles) 
                                                                   "/" w "-frame-" x "-button-i.png")) scale-w scale-h))
      (inactive-highlighted . ,(scale-image (do-make-image (concat (symbol-name styletab-c:styles) 
                                                                               "/" w "-frame-" x "-button-ih.png")) scale-w scale-h))
      (inactive-clicked . ,(scale-image (do-make-image (concat (symbol-name styletab-c:styles) 
                                                                           "/" w "-frame-" x "-button-ic.png")) scale-w scale-h))))
  (reframe-all))
;;(define make-hight-button-image
;;  (lambda (w x)
;;    `((focused . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-f.png")))
;;      (highlighted . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-f.png")))
;;      (clicked . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-c.png")))
;;      (inactive . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-i.png")))
;;      (inactive-highlighted . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-ih.png")))
;;      (inactive-clicked . ,(do-make-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-ic.png"))))))

;; frames/tabbar
(define top-frame-icon-title-images 
  (do-make-image (concat (symbol-name styletab-c:styles) "/" "top-frame-icon-title-images-f.png")))

(define tabbar-horizontal-images
  (mapcar (lambda (w)  (mapcar (lambda (x) (make-tab-image 
                                            (concat x "-" w))) (list "top" "bottom"))) (list "frame-tab-left-icon" "frame-tab" "frame-tab-right")))
(define tabbar-vertical-images
  (mapcar (lambda (w)  (mapcar (lambda (x) (make-tab-image 
                                            (concat x "-" w))) (list "left" "right"))) (list "frame-tab-top" "frame-tab" "frame-tab-bottom-icon")))
(define title-cursor-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-title-cursor"))) (list "top" "bottom" "left" "right")))
(define title-nocursor-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-title-nocursor"))) (list "top" "bottom" "left" "right")))
(define top-border-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-top-border"))) (list "bottom" "left" "right")))
(define top-left-corner-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-top-left-corner"))) (list "top" "bottom" "left" "right")))
(define top-right-corner-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-top-right-corner"))) (list "top" "bottom" "left" "right")))
(define top-left-corner-shaped-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-top-left-corner-shaped"))) (list "top" "left")))
(define top-right-corner-shaped-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-top-right-corner-shaped"))) (list "top" "right")))
(define title-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-title"))) (list "top" "bottom" "left" "right")))
(define left-border-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-left-border"))) (list "top" "bottom" "right")))
(define right-border-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-right-border"))) (list "top" "bottom" "left")))
(define bottom-left-corner-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-bottom-left-corner"))) (list "top" "bottom" "left" "right")))
(define bottom-border-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-bottom-border"))) (list "top" "left" "right")))
(define bottom-right-corner-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-bottom-right-corner"))) (list "top" "bottom" "left" "right")))
(define bottom-left-corner-shaped-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-bottom-left-corner-shaped"))) (list "bottom" "left")))
(define bottom-right-corner-shaped-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-bottom-right-corner-shaped"))) (list "bottom" "right")))

;; buttons
(define base-button-images
  (mapcar (lambda (w) (make-base-button-image w)) (list "top" "bottom" "left" "right")))
(define space-button-images
  (mapcar (lambda (w) (make-border-image (concat w "-frame-title"))) (list "top" "bottom" "left" "right")))
(define close-button-images
  (mapcar (lambda (w) (make-hight-button-image w "close")) (list "top" "bottom" "left" "right")))
(define menu-button-images
  (mapcar (lambda (w) (make-hight-button-image w "menu")) (list "top" "bottom" "left" "right")))
(define iconify-button-images
  (mapcar (lambda (w) (make-hight-button-image w "iconify")) (list "top" "bottom" "left" "right")))
(define move-resize-button-images
  (mapcar (lambda (w) (make-hight-button-image w "move-resize")) (list "top" "bottom" "left" "right")))
(define rename-button-images
  (mapcar (lambda (w) (make-hight-button-image w "rename")) (list "top" "bottom" "left" "right")))
(define frame-type-button-images
  (mapcar (lambda (w) (make-hight-button-image w "frame-type")) (list "top" "bottom" "left" "right")))
(define maximize-button-images 
  (mapcar (lambda (w) (mapcar (lambda (x) (make-hight-button-image x w)) (list "top" "bottom" "left" "right"))) (list "maximize" "unmaximize")))
(define shade-button-images 
  (mapcar (lambda (w) (mapcar (lambda (x) (make-hight-button-image x w)) (list "top" "bottom" "left" "right"))) (list "shade" "unshade")))
(define sticky-button-images 
  (mapcar (lambda (w) (mapcar (lambda (x) (make-hight-button-image x w)) (list "top" "bottom" "left" "right"))) (list "sticky" "unsticky")))
(define lock-button-images 
  (mapcar (lambda (w) (mapcar (lambda (x) (make-hight-button-image x w)) (list "top" "bottom" "left" "right"))) (list "lock" "unlock")))
(define prev-button-images 
  (mapcar (lambda (w) (mapcar (lambda (x) (make-hight-button-image x w)) (list "top" "bottom" "left" "right"))) (list "prev" "prev-last")))
(define next-button-images 
  (mapcar (lambda (w) (mapcar (lambda (x) (make-hight-button-image x w)) (list "top" "bottom" "left" "right"))) (list "next" "next-last")))
(define raise-lower-button-images 
  (mapcar (lambda (w) (mapcar (lambda (x) (make-hight-button-image x w)) (list "top" "bottom" "left" "right"))) (list "raise-lower" "ontop" "unontop")))

(define (recolor-base)
  (mapcar (lambda (w)
            (mapcar (lambda (x)
                      (do-recolor (cdr (nth 0 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-dark 0 styletab-c:focus-frame-color)
                                    (get-recolor-dark 0 (get-color (nth 0 (proposals-colors))))))
                      (do-recolor (cdr (nth 1 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-dark (* styletab-c:inactive-dimout 20) styletab-c:unfocus-frame-color)
                                    (get-recolor-dark (nth 1 (proposals-colors)) (get-color (nth 0 (proposals-colors))))))) w))
          (list title-cursor-images title-nocursor-images top-border-images top-left-corner-images top-right-corner-images
                top-left-corner-shaped-images top-right-corner-shaped-images title-images left-border-images right-border-images
                bottom-left-corner-images bottom-border-images bottom-right-corner-images bottom-left-corner-shaped-images
                bottom-right-corner-shaped-images space-button-images))

  (mapcar (lambda (w)
            (mapcar (lambda (x)
                      (mapcar (lambda (y)
                                (do-recolor (cdr (nth y x))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-dark 0 styletab-c:focus-frame-color)
                                              (get-recolor-dark 0 (get-color (nth 0 (proposals-colors)))))))
                              (list '0 '1 '2))
                      (mapcar (lambda (y)
                                (do-recolor (cdr (nth y x))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-dark (* styletab-c:inactive-dimout 20) styletab-c:unfocus-frame-color)
                                              (get-recolor-dark (nth 1 (proposals-colors)) (get-color (nth 0 (proposals-colors)))))))
                              (list '3 '4 '5))) w))
          (list base-button-images)))

(define (recolor-tab)
  (mapcar (lambda (w)
            (mapcar (lambda (x)
                      (mapcar (lambda (y)
                                (do-recolor (cdr (nth 0 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-dark 0 styletab-c:focus-frame-color)
                                              (get-recolor-dark 0 (get-color (nth 0 (proposals-colors))))))
                                (do-recolor (cdr (nth 1 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-bright (if (eq styletab-c:hightlight-tabbar t) 
                                                                        (/ (* styletab-c:active-hightlight-brighten 20) 2)
                                                                      0)
                                                                    styletab-c:focus-frame-color)
                                              (get-recolor-bright (if (eq styletab-c:hightlight-tabbar t) 
                                                                      (/ (nth 2 (proposals-colors)) 2)
                                                                    0)
                                                                  (get-color (nth 0 (proposals-colors))))))
                                (do-recolor (cdr (nth 2 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-dark (* styletab-c:inactive-dimout 20) styletab-c:unfocus-frame-color)
                                              (get-recolor-dark (nth 1 (proposals-colors)) (get-color (nth 0 (proposals-colors))))))
                                (do-recolor (cdr (nth 3 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-bright 
                                                 (if (eq styletab-c:hightlight-tabbar t) 
                                                     (- (/ (* styletab-c:inactive-hightlight-brighten 20) 2) (* styletab-c:inactive-dimout 20))
                                                   (- (* styletab-c:inactive-dimout 20)))
                                                 styletab-c:unfocus-frame-color)
                                              (get-recolor-bright (if (eq styletab-c:hightlight-tabbar t) 
                                                                      (- (/ (nth 3 (proposals-colors)) 2) (nth 1 (proposals-colors)))
                                                                    (- (nth 1 (proposals-colors))))
                                                                  (get-color (nth 0 (proposals-colors))))))) x)) w)) 
          (list tabbar-horizontal-images tabbar-vertical-images)))

(define (recolor-botton-static-sing button color always)
  (mapcar (lambda (w)
            (mapcar (lambda (x)
                      (do-recolor (cdr (nth 0 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-dark 0 (if (and always color) color styletab-c:focus-frame-color))
                                    (get-recolor-dark 0 (if (and always color) color (get-color (nth 0 (proposals-colors)))))))
                      (do-recolor (cdr (nth 1 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-bright (* styletab-c:active-hightlight-brighten 20) (if color color styletab-c:focus-frame-color))
                                    (get-recolor-bright (nth 2 (proposals-colors)) (if color color (get-color (nth 0 (proposals-colors)))))))
                      (do-recolor (cdr (nth 2 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-bright (* styletab-c:active-hightlight-brighten 20) (if color color styletab-c:focus-frame-color))
                                    (get-recolor-bright (nth 2 (proposals-colors)) (if color color (get-color (nth 0 (proposals-colors)))))))
                      (do-recolor (cdr (nth 3 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-dark (* styletab-c:inactive-dimout 20) (if (and always color) color styletab-c:unfocus-frame-color))
                                    (get-recolor-dark (nth 1 (proposals-colors)) (if (and always color) color (get-color (nth 0 (proposals-colors)))))))
                      (do-recolor (cdr (nth 4 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-bright (- (* styletab-c:inactive-hightlight-brighten 20) (* styletab-c:inactive-dimout 20))
                                                          (if color color styletab-c:unfocus-frame-color))
                                    (get-recolor-bright (- (nth 3 (proposals-colors)) (nth 1 (proposals-colors)))
                                                        (if color color (get-color (nth 0 (proposals-colors)))))))
                      (do-recolor (cdr (nth 5 x))
                                  (if (eq styletab-c:custom-frame-colors t)
                                      (get-recolor-bright (- (* styletab-c:inactive-hightlight-brighten 20) (* styletab-c:inactive-dimout 20))
                                                          (if color color styletab-c:unfocus-frame-color))
                                    (get-recolor-bright (- (nth 3 (proposals-colors)) (nth 1 (proposals-colors)))
                                                        (if color color (get-color (nth 0 (proposals-colors)))))))) w))
          (list button)))

(define (recolor-botton-dynamic-sing button color always)
  (mapcar (lambda (w)
            (mapcar (lambda (x)
                      (mapcar (lambda (y)
                                (do-recolor (cdr (nth 0 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-dark 0 (if (and always color) color styletab-c:focus-frame-color))
                                              (get-recolor-dark 0 (if (and always color) color (get-color (nth 0 (proposals-colors)))))))
                                (do-recolor (cdr (nth 1 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-bright (* styletab-c:active-hightlight-brighten 20) 
                                                                    (if color color styletab-c:focus-frame-color))
                                              (get-recolor-bright (nth 2 (proposals-colors)) (if color color (get-color (nth 0 (proposals-colors)))))))
                                (do-recolor (cdr (nth 2 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-bright (* styletab-c:active-hightlight-brighten 20) 
                                                                    (if color color styletab-c:focus-frame-color))
                                              (get-recolor-bright (nth 2 (proposals-colors)) (if color color (get-color (nth 0 (proposals-colors)))))))
                                (do-recolor (cdr (nth 3 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-dark (* styletab-c:inactive-dimout 20) 
                                                                  (if (and always color) color styletab-c:unfocus-frame-color))
                                              (get-recolor-dark (nth 1 (proposals-colors)) 
                                                                (if (and always color) color (get-color (nth 0 (proposals-colors)))))))
                                (do-recolor (cdr (nth 4 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-bright (- (* styletab-c:inactive-hightlight-brighten 20) (* styletab-c:inactive-dimout 20))
                                                                    (if color color styletab-c:unfocus-frame-color))
                                              (get-recolor-bright (- (nth 3 (proposals-colors)) (nth 1 (proposals-colors)))
                                                                  (if color color (get-color (nth 0 (proposals-colors)))))))
                                (do-recolor (cdr (nth 5 y))
                                            (if (eq styletab-c:custom-frame-colors t)
                                                (get-recolor-bright (- (* styletab-c:inactive-hightlight-brighten 20) (* styletab-c:inactive-dimout 20))
                                                                    (if color color styletab-c:unfocus-frame-color))
                                              (get-recolor-bright (- (nth 3 (proposals-colors)) (nth 1 (proposals-colors)))
                                                                  (if color color (get-color (nth 0 (proposals-colors)))))))) x)) w))
          (list button)))

(define (recolor-close-button)
  (recolor-botton-static-sing close-button-images styletab-c:hightlight-close (if (eq styletab-c:hightlight-close-all t) t)))
(define (recolor-menu-button)
  (recolor-botton-static-sing menu-button-images styletab-c:hightlight-menu (if (eq styletab-c:hightlight-menu-all t) t)))
(define (recolor-iconify-button)
  (recolor-botton-static-sing iconify-button-images styletab-c:hightlight-iconify (if (eq styletab-c:hightlight-iconify-all t) t)))
(define (recolor-move-resize-button)
  (recolor-botton-static-sing move-resize-button-images styletab-c:hightlight-move-resize (if (eq styletab-c:hightlight-move-resize-all t) t)))
(define (recolor-rename-button)
  (recolor-botton-static-sing rename-button-images styletab-c:hightlight-rename (if (eq styletab-c:hightlight-rename-all t) t)))
(define (recolor-frame-type-button)
  (recolor-botton-static-sing frame-type-button-images styletab-c:hightlight-frame-type (if (eq styletab-c:hightlight-frame-type-all t) t)))
(define (recolor-maximize-button)
  (recolor-botton-dynamic-sing maximize-button-images styletab-c:hightlight-maximize (if (eq styletab-c:hightlight-maximize-all t) t)))
(define (recolor-shade-button)
  (recolor-botton-dynamic-sing shade-button-images styletab-c:hightlight-shade (if (eq styletab-c:hightlight-shade-all t) t)))
(define (recolor-sticky-button)
  (recolor-botton-dynamic-sing sticky-button-images styletab-c:hightlight-sticky (if (eq styletab-c:hightlight-sticky-all t) t)))
(define (recolor-lock-button)
  (recolor-botton-dynamic-sing lock-button-images styletab-c:hightlight-lock (if (eq styletab-c:hightlight-lock-all t) t)))
(define (recolor-prev-button)
  (recolor-botton-dynamic-sing prev-button-images styletab-c:hightlight-prev (if (eq styletab-c:hightlight-prev-all t) t)))
(define (recolor-next-button)
  (recolor-botton-dynamic-sing next-button-images styletab-c:hightlight-next (if (eq styletab-c:hightlight-next-all t) t)))
(define (recolor-raise-lower-button)
  (recolor-botton-dynamic-sing raise-lower-button-images styletab-c:hightlight-raise-lower (if (eq styletab-c:hightlight-raise-lower-all t) t)))

(define (recolor-all-buttons)
  (recolor-close-button) (recolor-menu-button) (recolor-iconify-button) (recolor-move-resize-button) (recolor-rename-button) 
  (recolor-frame-type-button) (recolor-maximize-button) (recolor-shade-button) (recolor-sticky-button) (recolor-lock-button)
  (recolor-prev-button) (recolor-next-button) (recolor-raise-lower-button))

(define (recolor-all)
  (recolor-base) (recolor-tab) (recolor-all-buttons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; frames classes
(defmacro window-in-workspace-p (w space)
  `(memq ,space (window-get ,w 'workspaces)))

(define (get-first-workspace) 1)

(define (get-last-workspace)
  (aref
   (nth 2 (get-x-property 'root '_NET_NUMBER_OF_DESKTOPS)) 0))

(define frame-width
  (lambda (w)
    (if (or (eq (window-type w) 'default)
			(eq (window-type w) 'transient))
        styletab-c:borders-dimension
      0)))

(define frame-edge
  (lambda (w)
    (if (or (eq (window-type w) 'default)
            (eq (window-type w) 'transient))
        (- styletab-c:borders-dimension)
      0)))

(define title-height (lambda (w) styletab-c:title-dimension)) 
(define title-height-s (lambda (w) (- styletab-c:title-dimension 4)))
(define title-edge (lambda (w) (- styletab-c:title-dimension)))
(define title-edge-s (lambda (w) (- (- styletab-c:title-dimension 2))))
(define top-frame-button-width (lambda (w) (+ styletab-c:title-dimension (button-width-custom))))
(define bottom-frame-button-width (lambda (w) (+ styletab-c:title-dimension (button-width-custom))))
(define left-frame-button-height (lambda (w) (+ styletab-c:title-dimension (button-width-custom))))
(define right-frame-button-height (lambda (w) (+ styletab-c:title-dimension (button-width-custom))))

(define top-frame-default-border-corner-group
  `(((class . title)
     (background . ,(nth 0 title-images))
     (left-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . top-left-corner)
     (background . ,(nth 0 top-left-corner-images))
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . top-right-corner)
     (background . ,(nth 0 top-right-corner-images))
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(nth 0 title-images))
     (right-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define bottom-frame-default-border-corner-group
  `(((class . title)
     (background . ,(nth 1 title-images))
     (left-edge . 0)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . bottom-left-corner)
     (background . ,(nth 1 bottom-left-corner-images))
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,(nth 1 bottom-right-corner-images))
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(nth 1 title-images))
     (right-edge . 0)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define left-frame-default-border-corner-group
  `(((class . title)
     (background . ,(nth 2 title-images))
     (top-edge . 0)
     (left-edge . ,title-edge-s)
     (height . ,button-right-edge)
     (width . ,title-height-s))
    ((class . bottom-left-corner)
     (background . ,(nth 2 bottom-left-corner-images))
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-left-corner)
     (background . ,(nth 2 top-left-corner-images))
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . title)
     (background . ,(nth 2 title-images))
     (bottom-edge . 0)
     (left-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))))

(define right-frame-default-border-corner-group
  `(((class . title)
     (background . ,(nth 3 title-images))
     (top-edge . 0)
     (right-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))
    ((class . bottom-right-corner)
     (background . ,(nth 3 bottom-right-corner-images))
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-right-corner)
     (background . ,(nth 3 top-right-corner-images))
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . title)
     (background . ,(nth 3 title-images))
     (bottom-edge . 0)
     (right-edge . ,title-edge-s)
     (height . ,button-right-edge)
     (width . ,title-height-s))))

(define top-frame-border-group
  `(((class . left-border)
     (background . ,(nth 0 left-border-images))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (top-edge . 0)
     (width . ,frame-width)
     (bottom-edge . 0))
	((class . bottom-left-corner)
     (background . ,(nth 0 bottom-left-corner-images))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-border)
     (background . ,(nth 0 bottom-border-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-right-corner)
     (background . ,(nth 0 bottom-right-corner-images))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . right-border)
     (background . ,(nth 0 right-border-images))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))))

(define bottom-frame-border-group
  `(((class . left-border)
     (background . ,(nth 1 left-border-images))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (bottom-edge . 0)
     (width . ,frame-width)
     (top-edge . 0))
    ((class . top-left-corner)
     (background . ,(nth 1 top-left-corner-images))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-border)
     (background . ,(nth 0 top-border-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-right-corner)
     (background . ,(nth 1 top-right-corner-images))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . right-border)
     (background . ,(nth 1 right-border-images))
     (cursor . sb_h_double_arrow)
     (bottom-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (top-edge . 0))))

(define left-frame-border-group
  `(((class . bottom-border)
     (background . ,(nth 1 bottom-border-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-right-corner)
     (background . ,(nth 2 bottom-right-corner-images))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . right-border)
     (background . ,(nth 2 right-border-images))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-right-corner)
     (background . ,(nth 2 top-right-corner-images))
     (top-edge . ,frame-edge)
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,(nth 1 top-border-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define right-frame-border-group
  `(((class . bottom-border)
     (background . ,(nth 2 bottom-border-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-left-corner)
     (background . ,(nth 3 bottom-left-corner-images))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . left-border)
     (background . ,(nth 2 left-border-images))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (left-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-left-corner)
     (background . ,(nth 3 top-left-corner-images))
     (top-edge . ,frame-edge)
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,(nth 2 top-border-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define top-frame-title-group
  `(((class . top-border)
     (background . ,(nth 0 title-cursor-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (top-edge . ,title-edge)
     (right-edge . 0)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- styletab-c:title-dimension 12)))
     (y-justify . center)
     (background . ,(nth 0 (nth 1 tabbar-horizontal-images)))
     (foreground . ,title-colors-images)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(nth 0 (nth 0 tabbar-horizontal-images)))
     (cursor . hand2)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,(lambda (w) (+ 2 (title-height-s w))))
     (y-justify . 2)
     (x-justify . ,(lambda (w) (+ 3 (icon-edge w)))))
    ((class . tabbar-horizontal-right-edge)
     (background . ,(nth 0 (nth 2 tabbar-horizontal-images)))
     (width . ,tabbar-right-edge-width)
     (height . ,title-height-s)
     (top-edge . ,title-edge-s))
    ((class . title)
     (background . ,(nth 0 title-nocursor-images))
     (left-edge . 0)
     (top-edge . -2)
     (right-edge . 0)
     (height . 2))))

(define bottom-frame-title-group
  `(((class . title)
     (background . ,(nth 1 title-nocursor-images))
     (left-edge . 0)
     (bottom-edge . -2)
     (right-edge . 0)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- styletab-c:title-dimension 12)))
     (y-justify . center)
     (background . ,(nth 0 (nth 1 tabbar-horizontal-images)))
     (foreground . ,title-colors-images)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(nth 0 (nth 0 tabbar-horizontal-images)))
     (cursor . hand2)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,(lambda (w) (+ 2 (title-height-s w))))
     (y-justify . 2)
     (x-justify . ,(lambda (w) (+ 3 (icon-edge w)))))
    ((class . tabbar-horizontal-right-edge)
     (background . ,(nth 0 (nth 2 tabbar-horizontal-images)))
     (width . ,tabbar-right-edge-width)
     (height . ,title-height-s)
     (bottom-edge . ,title-edge-s))
    ((class . bottom-border)
     (background . ,(nth 1 title-cursor-images))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (bottom-edge . ,title-edge)
     (right-edge . 0)
     (height . 2))))

(define left-frame-title-group
  `(((class . left-border)
     (background . ,(nth 2 title-cursor-images))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (left-edge . ,title-edge)
     (bottom-edge . 0)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(nth 0 (nth 0 tabbar-vertical-images)))
     (cursor . hand2)
     (height . ,(lambda (w) (+ 2 (title-height-s w))))
     (width . ,title-height-s)
     (left-edge . ,title-edge-s)
     (y-justify . ,(lambda (w) (+ 2 (icon-edge w))))
     (x-justify . 2))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,(nth 0 (nth 1 tabbar-vertical-images)))
     (left-edge . ,title-edge-s)
     (width . ,title-height-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(nth 0 (nth 2 tabbar-vertical-images)))
     (left-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . ,(lambda (w) (- (+ 0 (icon-edge w)))))
     (x-justify . 2))
    ((class . title)
     (background . ,(nth 2 title-nocursor-images))
     (top-edge . 0)
     (left-edge . -2)
     (bottom-edge . 0)
     (width . 2))))

(define right-frame-title-group
  `(((class . right-border)
     (background . ,(nth 3 title-cursor-images))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,title-edge)
     (bottom-edge . 0)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(nth 1 (nth 0 tabbar-vertical-images)))
     (cursor . hand2)
     (height . ,(lambda (w) (+ 2 (title-height-s w))))
     (width . ,title-height-s)
     (right-edge . ,title-edge-s)
     (y-justify . ,(lambda (w) (+ 2 (icon-edge w))))
     (x-justify . 2))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,(nth 1 (nth 1 tabbar-vertical-images)))
     (right-edge . ,title-edge-s)
     (width . ,title-height-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(nth 1 (nth 2 tabbar-vertical-images)))
     (right-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . ,(lambda (w) (- (+ 0 (icon-edge w)))))
     (x-justify . 2))
    ((class . title)
     (background . ,(nth 3 title-nocursor-images))
     (top-edge . 0)
     (right-edge . -2)
     (bottom-edge . 0)
     (width . 2))))

(define top-frame-close-button
  `((class . close-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(nth 0 close-button-images))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-close-button
  `((class . close-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(nth 1 close-button-images))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-close-button
  `((class . close-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(nth 2 close-button-images))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-close-button
  `((class . close-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(nth 3 close-button-images))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-menu-button
  `((class . menu-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(nth 0 menu-button-images))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-menu-button
  `((class . menu-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(nth 1 menu-button-images))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-menu-button
  `((class . menu-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(nth 2 menu-button-images))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-menu-button
  `((class . menu-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(nth 3 menu-button-images))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-iconify-button
  `((class . iconify-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(nth 0 iconify-button-images))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-iconify-button
  `((class . iconify-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(nth 1 iconify-button-images))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-iconify-button
  `((class . iconify-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(nth 2 iconify-button-images))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-iconify-button
  `((class . iconify-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(nth 3 iconify-button-images))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-maximize-button
  `((class . maximize-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (nth 0 (nth 1 maximize-button-images)) (nth 0 (nth 0 maximize-button-images)))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-maximize-button
  `((class . maximize-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (nth 1 (nth 1 maximize-button-images)) (nth 1 (nth 0 maximize-button-images)))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-maximize-button
  `((class . maximize-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (nth 2 (nth 1 maximize-button-images)) (nth 2 (nth 0 maximize-button-images)))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-maximize-button
  `((class . maximize-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (nth 3 (nth 1 maximize-button-images)) (nth 3 (nth 0 maximize-button-images)))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-shade-button
  `((class . shade-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) (nth 0 (nth 1 shade-button-images)) (nth 0 (nth 0 shade-button-images)))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-shade-button
  `((class . shade-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) (nth 1 (nth 1 shade-button-images)) (nth 1 (nth 0 shade-button-images)))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-shade-button
  `((class . shade-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) (nth 2 (nth 1 shade-button-images)) (nth 2 (nth 0 shade-button-images)))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-shade-button
  `((class . shade-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) (nth 3 (nth 1 shade-button-images)) (nth 3 (nth 0 shade-button-images)))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-sticky-button
  `((class . sticky-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'sticky) (nth 0 (nth 1 sticky-button-images)) (nth 0 (nth 0 sticky-button-images)))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-sticky-button
  `((class . sticky-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'sticky) (nth 1 (nth 1 sticky-button-images)) (nth 1 (nth 0 sticky-button-images)))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-sticky-button
  `((class . sticky-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'sticky) (nth 2 (nth 1 sticky-button-images)) (nth 2 (nth 0 sticky-button-images)))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-sticky-button
  `((class . sticky-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'sticky) (nth 3 (nth 1 sticky-button-images)) (nth 3 (nth 0 sticky-button-images)))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-space-button
  `((class . title)
    (background . ,(nth 0 space-button-images))
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-space-button
  `((class . title)
    (background . ,(nth 1 space-button-images))
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-space-button
  `((class . title)
    (background . ,(nth 2 space-button-images))
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-space-button
  `((class . title)
    (background . ,(nth 3 space-button-images))
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky))
                                   (nth 0 (nth 1 prev-button-images)) (nth 0 (nth 0 prev-button-images)))))    
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky))
                                   (nth 1 (nth 1 prev-button-images)) (nth 1 (nth 0 prev-button-images)))))    
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky))
                                   (nth 2 (nth 1 prev-button-images)) (nth 2 (nth 0 prev-button-images)))))    
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky))
                                   (nth 3 (nth 1 prev-button-images)) (nth 3 (nth 0 prev-button-images)))))    
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-next-button
  `((class . next-workspace-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky))
                                   (nth 0 (nth 1 next-button-images)) (nth 0 (nth 0 next-button-images)))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-next-button
  `((class . next-workspace-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky))
                                   (nth 1 (nth 1 next-button-images)) (nth 1 (nth 0 next-button-images)))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-next-button
  `((class . next-workspace-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky))
                                   (nth 2 (nth 1 next-button-images)) (nth 2 (nth 0 next-button-images)))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-next-button
  `((class . next-workspace-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky))
                                   (nth 3 (nth 1 next-button-images)) (nth 3 (nth 0 next-button-images)))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-lock-button
  `((class . lock-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) (nth 0 (nth 1 lock-button-images)) (nth 0 (nth 0 lock-button-images)))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-lock-button
  `((class . lock-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) (nth 1 (nth 1 lock-button-images)) (nth 1 (nth 0 lock-button-images)))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-lock-button
  `((class . lock-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) (nth 2 (nth 1 lock-button-images)) (nth 2 (nth 0 lock-button-images)))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-lock-button
  `((class . lock-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) (nth 3 (nth 1 lock-button-images)) (nth 3 (nth 0 lock-button-images)))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (nth 0 (nth 0 raise-lower-button-images)) 
                                 (if (> (window-get w 'depth) 0) (nth 0 (nth 1 raise-lower-button-images)) (nth 0 (nth 2 raise-lower-button-images))))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (nth 1 (nth 0 raise-lower-button-images)) 
                                 (if (> (window-get w 'depth) 0) (nth 1 (nth 1 raise-lower-button-images)) (nth 1 (nth 2 raise-lower-button-images))))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (nth 2 (nth 0 raise-lower-button-images)) 
                                 (if (> (window-get w 'depth) 0) (nth 2 (nth 1 raise-lower-button-images)) (nth 2 (nth 2 raise-lower-button-images))))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (nth 3 (nth 0 raise-lower-button-images)) 
                                 (if (> (window-get w 'depth) 0) (nth 3 (nth 1 raise-lower-button-images)) (nth 3 (nth 2 raise-lower-button-images))))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define  top-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(nth 0 move-resize-button-images))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define  bottom-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(nth 1 move-resize-button-images))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define  left-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(nth 2 move-resize-button-images))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define  right-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(nth 3 move-resize-button-images))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-rename-button
  `((class . rename-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(nth 0 rename-button-images))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-rename-button
  `((class . rename-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(nth 1 rename-button-images))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-rename-button
  `((class . rename-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(nth 2 rename-button-images))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-rename-button
  `((class . rename-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(nth 3 rename-button-images))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(nth 0 base-button-images))
    (foreground . ,(nth 0 frame-type-button-images))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(nth 1 base-button-images))
    (foreground . ,(nth 1 frame-type-button-images))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(nth 2 base-button-images))
    (foreground . ,(nth 2 frame-type-button-images))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(nth 3 base-button-images))
    (foreground . ,(nth 3 frame-type-button-images))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-shaped-border-corner-group
  `(((class . title)
     (background . ,(nth 0 title-images))
     (left-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . top-left-corner)
     (background . ,(nth 0 top-left-corner-shaped-images))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . top-right-corner)
     (background . ,(nth 0 top-right-corner-shaped-images))
     (cursor . sb_h_double_arrow)
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(nth 0 title-images))
     (right-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define bottom-frame-shaped-border-corner-group
  `(((class . title)
     (background . ,(nth 1 title-images))
     (left-edge . 0)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . bottom-left-corner)
     (background . ,(nth 0 bottom-left-corner-shaped-images))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,(nth 0 bottom-right-corner-shaped-images))
     (cursor . sb_h_double_arrow)
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . title)
     (background . ,(nth 1 title-images))
     (right-edge . 0)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define left-frame-shaped-border-corner-group
  `(((class . title)
     (background . ,(nth 2 title-images))
     (top-edge . 0)
     (left-edge . ,title-edge-s)
     (height . ,button-right-edge)
     (width . ,title-height-s))
    ((class . bottom-left-corner)
     (background . ,(nth 1 bottom-left-corner-shaped-images))
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-left-corner)
     (background . ,(nth 1 top-left-corner-shaped-images))
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . title)
     (background . ,(nth 2 title-images))
     (bottom-edge . 0)
     (left-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))))

(define right-frame-shaped-border-corner-group
  `(((class . title)
     (background . ,(nth 3 title-images))
     (top-edge . 0)
     (right-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))
    ((class . bottom-right-corner)
     (background . ,(nth 1 bottom-right-corner-shaped-images))
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . top-right-corner)
     (background . ,(nth 1 top-right-corner-shaped-images))
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . title)
     (background . ,(nth 3 title-images))
     (bottom-edge . 0)
     (right-edge . ,title-edge-s)
     (height . ,button-right-edge)
     (width . ,title-height-s))))

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
(define button-alist nil)
(define current-title styletab-c:titlebar-place)
(define top-normal-buttons-left nil)
(define top-transient-buttons-left nil)
(define top-normal-buttons-right nil)
(define top-transient-buttons-right nil)
(define bottom-normal-buttons-left nil)
(define bottom-transient-buttons-left nil)
(define bottom-normal-buttons-right nil)
(define bottom-transient-buttons-right nil)
(define left-normal-buttons-top nil)
(define left-transient-buttons-top nil)
(define left-normal-buttons-bottom nil)
(define left-transient-buttons-bottom nil)
(define right-normal-buttons-top nil)
(define right-transient-buttons-top nil)
(define right-normal-buttons-bottom nil)
(define right-transient-buttons-bottom nil)
(define top-tab-adjustments nil)
(define bottom-tab-adjustments nil)
(define left-tab-adjustments nil)
(define right-tab-adjustments nil)

(define make-buttons
  ;; ripped from Anonymous
  (lambda ()
    (let* (
           ;; turns one cons cell (btn-name . show-in-transients) into a button
           ;; definition, adding the button position
           (make-button
            (lambda (is-trans btn edge pos)
              (let ((btn-def  (cdr (assq (car btn) button-alist)))
                    (btn-in-trans (last btn)))
                (if (or (null btn-def) (and is-trans (not btn-in-trans)))
                    nil
                  (cons (cons edge pos) btn-def)))))

           ;; turns the list of cons cells (btn-name . show-in-transients) into
           ;; a list of button definitions, adding the button positions
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
            (setq button-alist top-button-alist)
            (make-button-list nil styletab-c:top-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (top-frame-normal-buttons-right
            (make-button-list nil styletab-c:top-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (top-frame-transient-buttons-left
            (make-button-list t styletab-c:top-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (top-frame-transient-buttons-right
            (make-button-list t styletab-c:top-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))

           (bottom-frame-normal-buttons-left 
            (setq button-alist bottom-button-alist)
            (make-button-list nil styletab-c:bottom-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (bottom-frame-normal-buttons-right
            (make-button-list nil styletab-c:bottom-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (bottom-frame-transient-buttons-left
            (make-button-list t styletab-c:bottom-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (bottom-frame-transient-buttons-right
            (make-button-list t styletab-c:bottom-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))

           (left-frame-normal-buttons-bottom 
            (setq button-alist left-button-alist)
            (make-button-list nil styletab-c:left-bottom-buttons 'bottom-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (left-frame-normal-buttons-top
            (make-button-list nil styletab-c:left-top-buttons 'top-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (left-frame-transient-buttons-bottom
            (make-button-list t styletab-c:left-bottom-buttons 'bottom-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (left-frame-transient-buttons-top
            (make-button-list t styletab-c:left-top-buttons 'top-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))

           (right-frame-normal-buttons-bottom 
            (setq button-alist right-button-alist)
            (make-button-list nil styletab-c:right-bottom-buttons 'bottom-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (right-frame-normal-buttons-top
            (make-button-list nil styletab-c:right-top-buttons 'top-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (right-frame-transient-buttons-bottom
            (make-button-list t styletab-c:right-bottom-buttons 'bottom-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
           (right-frame-transient-buttons-top
            (make-button-list t styletab-c:right-top-buttons 'top-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom)))))

      (let ((top-left-d-w 11)
            (top-right-d-w (tabbar-right-edge-width))
            (top-left-m
             (if (numberp (cdr (car (car top-frame-normal-buttons-left))))
                 (+ (cdr (car (car top-frame-normal-buttons-left))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (top-right-m
             (if (numberp (cdr (car (car top-frame-normal-buttons-right))))
                 (+ (cdr (car (car top-frame-normal-buttons-right))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (top-left-m-t
             (if (numberp (cdr (car (car top-frame-transient-buttons-left))))
                 (+ (cdr (car (car top-frame-transient-buttons-left))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (top-right-m-t
             (if (numberp (cdr (car (car top-frame-transient-buttons-right))))
                 (+ (cdr (car (car top-frame-transient-buttons-right))) (+ styletab-c:title-dimension (button-width-custom))) 0)))
        (setq top-normal-buttons-left top-frame-normal-buttons-left)
        (setq top-transient-buttons-left top-frame-transient-buttons-left)
        (setq top-normal-buttons-right top-frame-normal-buttons-right)
        (setq top-transient-buttons-right top-frame-transient-buttons-right)
        (setq top-tab-adjustments (list top-left-d-w top-right-d-w top-left-m top-right-m top-left-m-t top-right-m-t)))

      (let ((bottom-left-d-w 11)
            (bottom-right-d-w (tabbar-right-edge-width))
            (bottom-left-m
             (if (numberp (cdr (car (car bottom-frame-normal-buttons-left))))
                 (+ (cdr (car (car bottom-frame-normal-buttons-left))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (bottom-right-m
             (if (numberp (cdr (car (car bottom-frame-normal-buttons-right))))
                 (+ (cdr (car (car bottom-frame-normal-buttons-right))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (bottom-left-m-t
             (if (numberp (cdr (car (car bottom-frame-transient-buttons-left))))
                 (+ (cdr (car (car bottom-frame-transient-buttons-left))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (bottom-right-m-t
             (if (numberp (cdr (car (car bottom-frame-transient-buttons-right))))
                 (+ (cdr (car (car bottom-frame-transient-buttons-right))) (+ styletab-c:title-dimension (button-width-custom))) 0)))
        (setq bottom-normal-buttons-left bottom-frame-normal-buttons-left)
        (setq bottom-transient-buttons-left bottom-frame-transient-buttons-left)
        (setq bottom-normal-buttons-right bottom-frame-normal-buttons-right)
        (setq bottom-transient-buttons-right bottom-frame-transient-buttons-right)
        (setq bottom-tab-adjustments (list bottom-left-d-w bottom-right-d-w bottom-left-m bottom-right-m bottom-left-m-t bottom-right-m-t)))
      
      (let ((left-left-d-w 11)
            (left-right-d-w (- styletab-c:title-dimension 2))
            (left-left-m
             (if (numberp (cdr (car (car left-frame-normal-buttons-bottom))))
                 (+ (cdr (car (car left-frame-normal-buttons-bottom))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (left-right-m
             (if (numberp (cdr (car (car left-frame-normal-buttons-top))))
                 (+ (cdr (car (car left-frame-normal-buttons-top))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (left-left-m-t
             (if (numberp (cdr (car (car left-frame-transient-buttons-bottom))))
                 (+ (cdr (car (car left-frame-transient-buttons-bottom))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (left-right-m-t
             (if (numberp (cdr (car (car left-frame-transient-buttons-top))))
                 (+ (cdr (car (car left-frame-transient-buttons-top))) (+ styletab-c:title-dimension (button-width-custom))) 0)))
        (setq left-normal-buttons-top left-frame-normal-buttons-top)
        (setq left-transient-buttons-top left-frame-transient-buttons-top)
        (setq left-normal-buttons-bottom left-frame-normal-buttons-bottom)
        (setq left-transient-buttons-bottom left-frame-transient-buttons-bottom)
        (setq left-tab-adjustments (list left-left-d-w left-right-d-w left-left-m left-right-m left-left-m-t left-right-m-t)))

      (let ((right-left-d-w 11)
            (right-right-d-w (- styletab-c:title-dimension 2))
            (right-left-m
             (if (numberp (cdr (car (car right-frame-normal-buttons-bottom))))
                 (+ (cdr (car (car right-frame-normal-buttons-bottom))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (right-right-m
             (if (numberp (cdr (car (car right-frame-normal-buttons-top))))
                 (+ (cdr (car (car right-frame-normal-buttons-top))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (right-left-m-t
             (if (numberp (cdr (car (car right-frame-transient-buttons-bottom))))
                 (+ (cdr (car (car right-frame-transient-buttons-bottom))) (+ styletab-c:title-dimension (button-width-custom))) 0))
            (right-right-m-t
             (if (numberp (cdr (car (car right-frame-transient-buttons-top))))
                 (+ (cdr (car (car right-frame-transient-buttons-top))) (+ styletab-c:title-dimension (button-width-custom))) 0)))
        (setq right-normal-buttons-top right-frame-normal-buttons-top)
        (setq right-transient-buttons-top right-frame-transient-buttons-top)
        (setq right-normal-buttons-bottom right-frame-normal-buttons-bottom)
        (setq right-transient-buttons-bottom right-frame-transient-buttons-bottom)
        (setq right-tab-adjustments (list right-left-d-w right-right-d-w right-left-m right-right-m right-left-m-t right-right-m-t))))))

(define make-frame
  (lambda ()
    (require 'sawfish.wm.tabs.tab)
    (when (eq current-title 'top)
      (update-title-x-offsets `(,(- styletab-c:title-dimension 12) . 0))
      (set-tab-adjustments #:theme-left-dec-width (nth 0 top-tab-adjustments) #:theme-right-dec-width (nth 1 top-tab-adjustments)
                           #:theme-left-margin (nth 2 top-tab-adjustments) #:theme-right-margin (nth 3 top-tab-adjustments)
                           #:theme-left-margin-transient (nth 4 top-tab-adjustments) #:theme-right-margin-transient (nth 5 top-tab-adjustments))
      (setq normal-frame
            (append top-frame-title-group top-frame-default-border-corner-group top-normal-buttons-left
                    top-frame-border-group top-normal-buttons-right))
      (setq shaped-frame
            (append top-frame-title-group top-frame-shaped-border-corner-group top-normal-buttons-left
                    top-normal-buttons-right))
      (setq transient-frame
            (append top-frame-title-group top-frame-default-border-corner-group top-transient-buttons-left
                    top-frame-border-group top-transient-buttons-right))
      (setq shaped-transient-frame
            (append top-frame-title-group top-frame-shaped-border-corner-group top-transient-buttons-left
                    top-transient-buttons-right)))

    (when (eq current-title 'bottom)
      (update-title-x-offsets `(,(- styletab-c:title-dimension 12) . 0))
      (set-tab-adjustments #:theme-left-dec-width (nth 0 bottom-tab-adjustments) #:theme-right-dec-width (nth 1 bottom-tab-adjustments)
                           #:theme-left-margin (nth 2 bottom-tab-adjustments) #:theme-right-margin (nth 3 bottom-tab-adjustments)
                           #:theme-left-margin-transient (nth 4 bottom-tab-adjustments) #:theme-right-margin-transient (nth 5 bottom-tab-adjustments))
      (setq normal-frame
            (append bottom-frame-title-group bottom-frame-default-border-corner-group bottom-normal-buttons-left
                    bottom-frame-border-group bottom-normal-buttons-right))
      (setq shaped-frame
            (append bottom-frame-title-group bottom-frame-shaped-border-corner-group bottom-normal-buttons-left
                    bottom-normal-buttons-right))
      (setq transient-frame
            (append bottom-frame-title-group bottom-frame-default-border-corner-group bottom-transient-buttons-left
                    bottom-frame-border-group bottom-transient-buttons-right))
      (setq shaped-transient-frame
            (append bottom-frame-title-group bottom-frame-shaped-border-corner-group bottom-transient-buttons-left
                    bottom-transient-buttons-right)))

    (when (eq current-title 'left)
      (update-title-x-offsets '(11 . -11))
      (set-tab-adjustments #:theme-left-dec-width (nth 0 left-tab-adjustments) #:theme-right-dec-width (nth 1 left-tab-adjustments)
                           #:theme-left-margin (nth 2 left-tab-adjustments) #:theme-right-margin (nth 3 left-tab-adjustments)
                           #:theme-left-margin-transient (nth 4 left-tab-adjustments) #:theme-right-margin-transient (nth 5 left-tab-adjustments))
      (setq normal-frame
            (append left-frame-title-group left-normal-buttons-bottom left-frame-default-border-corner-group
                    left-frame-border-group left-normal-buttons-top))
      (setq shaped-frame
            (append left-frame-title-group left-normal-buttons-bottom left-frame-shaped-border-corner-group
                    left-normal-buttons-top))
      (setq transient-frame
            (append left-frame-title-group left-transient-buttons-bottom left-frame-default-border-corner-group
                    left-frame-border-group left-transient-buttons-top))
      (setq shaped-transient-frame
            (append left-frame-title-group left-transient-buttons-bottom left-frame-shaped-border-corner-group
                    left-transient-buttons-top)))

    (when (eq current-title 'right)
      (update-title-x-offsets '(11 . -11))
      (set-tab-adjustments #:theme-left-dec-width (nth 0 right-tab-adjustments) #:theme-right-dec-width (nth 1 right-tab-adjustments)
                           #:theme-left-margin (nth 2 right-tab-adjustments) #:theme-right-margin (nth 3 right-tab-adjustments)
                           #:theme-left-margin-transient (nth 4 right-tab-adjustments) #:theme-right-margin-transient (nth 5 right-tab-adjustments))
      (setq normal-frame
            (append right-frame-title-group right-normal-buttons-bottom right-frame-default-border-corner-group
                    right-frame-border-group right-normal-buttons-top))
      (setq shaped-frame
            (append right-frame-title-group right-normal-buttons-bottom right-frame-shaped-border-corner-group
                    right-normal-buttons-top))
      (setq transient-frame
            (append right-frame-title-group right-transient-buttons-bottom right-frame-default-border-corner-group
                    right-frame-border-group right-transient-buttons-top))
      (setq shaped-transient-frame
            (append right-frame-title-group right-transient-buttons-bottom right-frame-shaped-border-corner-group
                    right-transient-buttons-top)))))

(define (current-title-w w)
  (if (not (window-get w 'title-position))
      (case styletab-c:titlebar-place
            ((top) (setq current-title 'top))
            ((bottom) (setq current-title 'bottom))
            ((left) (setq current-title 'left))
            ((right) (setq current-title 'right)))
    (setq current-title (window-get w 'title-position)))
  (make-buttons)
  (make-frame))

(define (create-frames-only w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (current-title-w w)))

(define (reframe-windows style)
  (map-windows
   (lambda (w)
     (when (eq (window-get w 'current-frame-style) style)
       (current-title-w w)
       (reframe-window w)))))

(define reframe-buttons
  (lambda ()
    (make-buttons)
    (reframe-windows theme-name)))

(define reframe-all
  (lambda ()
    (make-frame)
    (reframe-windows theme-name)))

(define (reframe-one w)
  (when (not (window-get w 'tabbed))
    (when (eq (window-get w 'current-frame-style) theme-name)
      (current-title-w w)
      (rebuild-frame w))))

;; reframe-all and reset icon cache
(define clear-icon-cache-reframe
  (lambda ()
    (setq styletab-c-icon-cache (make-weak-table eq-hash eq))
    (reframe-all)))

;; reload-frame-style and reset icon and frame cache 
(define (clear-cache-reload-frame-style)
  (setq styletab-c-icon-cache (make-weak-table eq-hash eq))
  (setq styletab-c-frame-cache (make-weak-table eq-hash eq))
  (reload-frame-style theme-name))

(define (bright-changed)
  (recolor-tab)
  (recolor-all-buttons)
  (reframe-windows-with-style theme-name))

(define (color-changed)
  (recolor-all)
  (reframe-windows-with-style theme-name))

(define (botton-color-changed botton)
  (botton)
  (reframe-windows theme-name))

(define (initialize-theme)
  (recolor-all)
  (make-buttons)
  (make-frame)
  (reframe-windows-with-style theme-name))

(initialize-theme)

;; At last! We create the actual theme
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


(call-after-state-changed '(sticky fixed-position stacking maximized) reframe-one)
(call-after-state-changed '(title-position) create-frames-only)
(add-hook 'remove-from-workspace-hook reframe-one)

;;(custom-set-property 'styletab-c:styles ':after-set clear-cache-reload-frame-style)
(custom-set-property 'styletab-c:title-dimension ':after-set clear-icon-cache-reframe)
(custom-set-property 'styletab-c:custom-button-width ':after-set clear-icon-cache-reframe)
(custom-set-property 'styletab-c:button-width ':after-set clear-icon-cache-reframe)
(custom-set-property 'styletab-c:borders-dimension ':after-set reframe-all)
(custom-set-property 'styletab-c:titlebar-place ':after-set reframe-all)
(custom-set-property 'styletab-c:top-left-buttons ':after-set reframe-buttons)
(custom-set-property 'styletab-c:top-right-buttons ':after-set reframe-buttons)
(custom-set-property 'styletab-c:bottom-left-buttons ':after-set reframe-buttons)
(custom-set-property 'styletab-c:bottom-right-buttons ':after-set reframe-buttons)
(custom-set-property 'styletab-c:left-top-buttons ':after-set reframe-buttons)
(custom-set-property 'styletab-c:left-bottom-buttons ':after-set reframe-buttons)
(custom-set-property 'styletab-c:right-top-buttons ':after-set reframe-buttons)
(custom-set-property 'styletab-c:right-bottom-buttons ':after-set reframe-buttons)

(custom-set-property 'styletab-c:do-reload ':after-set clear-cache-reload-frame-style)
