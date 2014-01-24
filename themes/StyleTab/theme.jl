;; StyleTab

(define theme-name 'StyleTab)

;;need hash tables for icon cache 
(require 'rep.data.tables)

;; recolor imanges
(require 'sawfish.wm.util.recolor-image)

;; Defcustom and defgroup
(defgroup StyleTab:group "StyleTab"
  :group appearance)

(defgroup StyleTab:top-buttons-group "Top Titlebar Buttons"
  :group (appearance StyleTab:group))

(defgroup StyleTab:bottom-buttons-group "Bottom Titlebar Buttons"
  :group (appearance StyleTab:group))

(defgroup StyleTab:left-buttons-group "Left Titlebar Buttons"
  :group (appearance StyleTab:group))

(defgroup StyleTab:right-buttons-group "Right Titlebar Buttons"
  :group (appearance StyleTab:group))

(defcustom styletab-c:titlebar-place 'top "Titlebar default place."
  :group (appearance StyleTab:group)
  :type symbol
  :options (top bottom left right))

(defcustom styletab-c:title-dimension 24 "Height of title border."
  :group (appearance StyleTab:group)
  :type symbol
  :options (16 18 20 22 24 26 28 30 32))

(defcustom styletab-c:borders-dimension 4 "Width of window border."
  :group (appearance StyleTab:group)
  :type symbol
  :options (0 2 4 6 8))

(defcustom styletab-c:custom-button-width nil "Customize buttons width."
  :group (appearance StyleTab:group)
  :type boolean)

(defcustom styletab-c:button-width 0 "Width of Buttons."
  :group (appearance StyleTab:group)
  :type symbol
  :depends styletab-c:custom-button-width
  :options (1 2 3 4 0 -1 -2 -3 -4))

(defcustom styletab-c:styles 'Default "Frame and button style."
  :group (appearance StyleTab:group)
  :type symbol
  :options (Default Reduce Glass WixDa Smoothly))

(defcustom styletab-c:proposals 'Pink "Color proposals."
  :group (appearance StyleTab:group)
  :type symbol
  :options (Default Reduce Glass WixDa Smoothly Brown Darkblue Blue Pink Green)
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:tabbar-marked t "Colorize tab/titelbar if it is to be added as a tab."
  :group (appearance StyleTab:group)
  :type boolean
  :after-set (lambda () (botton-color-changed recolor-tab)))

(defcustom styletab-c:tabbar-marked-color (get-color "#DF0000")
  "Color of tab/titelbar when it was marked."
  :group (appearance StyleTab:group)
  :type color
  :depends styletab-c:tabbar-marked
  :after-set (lambda () (botton-color-changed recolor-tab)))

(defcustom styletab-c:hightlight-tabbar nil "Also hightlighted tab/titelbar."
  :group (appearance StyleTab:group)
  :type boolean
  :after-set (lambda () (botton-color-changed recolor-tab)))

(defcustom styletab-c:custom-frame-colors nil "Customize frame color/brightness (Don't use color proposals)."
  :group (appearance StyleTab:group)
  :type boolean
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:focus-frame-color (get-color "#780000")
  "Focus frame color."
  :group (appearance StyleTab:group)
  :type color
  :depends styletab-c:custom-frame-colors
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:unfocus-frame-color (get-color "#780000")
  "Inactive frame color"
  :group (appearance StyleTab:group)
  :type color
  :depends styletab-c:custom-frame-colors
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:inactive-dimout 2 "Dimout inactive frame."
  :group (appearance StyleTab:group)
  :depends styletab-c:custom-frame-colors
  :type symbol
  :options (0 1 2 3 4 5)
  :after-set (lambda () (color-changed)))

(defcustom styletab-c:active-hightlight-brighten 2 "Focus window brightness mouse over buttons."
  :group (appearance StyleTab:group)
  :depends styletab-c:custom-frame-colors
  :type symbol
  :options (1 2 3 4 5 0 -1 -2 -3 -4 -5)
  :after-set (lambda () (bright-changed)))

(defcustom styletab-c:inactive-hightlight-brighten 2 "Inactive windows brightness mouse over buttons."
  :group (appearance StyleTab:group)
  :type symbol
  :depends styletab-c:custom-frame-colors
  :options (1 2 3 4 5 0 -1 -2 -3 -4 -5)
  :after-set (lambda () (bright-changed)))

(defcustom styletab-c:hightlight-close nil
  "Colorize the close button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-close-button)))

(defcustom styletab-c:hightlight-close-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-close
  :after-set (lambda () (botton-color-changed recolor-close-button)))

(defcustom styletab-c:hightlight-maximize nil
  "Colorize the maximize button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-maximize-button)))

(defcustom styletab-c:hightlight-maximize-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-maximize
  :after-set (lambda () (botton-color-changed recolor-maximize-button)))

(defcustom styletab-c:hightlight-iconify nil
  "Colorize the minimize button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-iconify-button)))

(defcustom styletab-c:hightlight-iconify-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-iconify
  :after-set (lambda () (botton-color-changed recolor-iconify-button)))

(defcustom styletab-c:hightlight-shade nil
  "Colorize the shade button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-shade-button)))

(defcustom styletab-c:hightlight-shade-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-shade
  :after-set (lambda () (botton-color-changed recolor-shade-button)))

(defcustom styletab-c:hightlight-sticky nil
  "Colorize the sticky button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-sticky-button)))

(defcustom styletab-c:hightlight-sticky-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-sticky
  :after-set (lambda () (botton-color-changed recolor-sticky-button)))

(defcustom styletab-c:hightlight-menu nil
  "Colorize the menu button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-menu-button)))

(defcustom styletab-c:hightlight-menu-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-menu
  :after-set (lambda () (botton-color-changed recolor-menu-button)))

(defcustom styletab-c:hightlight-frame-type nil
  "Colorize the frame type button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-frame-type-button)))

(defcustom styletab-c:hightlight-frame-type-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-frame-type
  :after-set (lambda () (botton-color-changed recolor-frame-type-button)))

(defcustom styletab-c:hightlight-lock nil
  "Colorize the lock button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-lock-button)))

(defcustom styletab-c:hightlight-lock-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-lock
  :after-set (lambda () (botton-color-changed recolor-lock-button)))

(defcustom styletab-c:hightlight-move-resize nil
  "Colorize the move/resize button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-move-resize-button)))

(defcustom styletab-c:hightlight-move-resize-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-move-resize
  :after-set (lambda () (botton-color-changed recolor-move-resize-button)))

(defcustom styletab-c:hightlight-raise-lower nil
  "Colorize the raise/lower button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-raise-lower-button)))

(defcustom styletab-c:hightlight-raise-lower-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-raise-lower
  :after-set (lambda () (botton-color-changed recolor-raise-lower-button)))

(defcustom styletab-c:hightlight-next nil
  "Colorize the next workspace button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-next-button)))

(defcustom styletab-c:hightlight-next-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-next
  :after-set (lambda () (botton-color-changed recolor-next-button)))

(defcustom styletab-c:hightlight-prev nil
  "Colorize the previous workspace button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-prev-button)))

(defcustom styletab-c:hightlight-prev-all nil "Always colorize."
  :group (appearance StyleTab:group)
  :type boolean
  :depends styletab-c:hightlight-prev
  :after-set (lambda () (botton-color-changed recolor-prev-button)))

(defcustom styletab-c:hightlight-rename nil
  "Colorize the rename button."
  :group (appearance StyleTab:group)
  :type (optional color)
  :after-set (lambda () (botton-color-changed recolor-rename-button)))

(defcustom styletab-c:hightlight-rename-all nil "Always colorize."
  :group (appearance StyleTab:group)
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
                                   '(boolean "Also show in transients."))))))
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
    "Top Titlebar Left Buttons (from left to right). \\top"
    StyleTab:top-buttons-group)
   (styletab-c:top-right-buttons
    '((close t) (maximize t) (minimize nil))
    "Top Titlebar Right Buttons (from right to left). \\top"
    StyleTab:top-buttons-group)
   (styletab-c:bottom-left-buttons
    '((menu t) (sticky nil) (shade nil))
    "Bottom Titlebar Left Buttons (from left to right). \\top"
    StyleTab:bottom-buttons-group)
   (styletab-c:bottom-right-buttons
    '((close t) (maximize t) (minimize nil))
    "Bottom Titlebar Right Buttons (from right to left). \\top"
    StyleTab:bottom-buttons-group)
   (styletab-c:left-top-buttons
    '((close t) (maximize t) (minimize nil))
    "Left Titlebar Top Buttons (from top to bottom). \\top"
    StyleTab:left-buttons-group)
   (styletab-c:left-bottom-buttons
    '((menu t) (sticky nil) (shade nil))
    "Left Titlebar Bottom Buttons (from bottom to top). \\top"
    StyleTab:left-buttons-group)
   (styletab-c:right-top-buttons
    '((close t) (maximize t) (minimize nil))
    "Right Titlebar Top Buttons (from top to bottom). \\top"
    StyleTab:right-buttons-group)
   (styletab-c:right-bottom-buttons
    '((menu t) (sticky nil) (shade nil))
    "Right Titlebar Bottom Buttons (from bottom to top). \\top"
    StyleTab:right-buttons-group)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; styles settings 

(define proposals-colors
  (lambda ()
    (case styletab-c:proposals
          ;; format "color" "dimout inactive frame" "brighte mouse over button active" "brighte mouse over button inactive"
          ((Default) (list "#000033" '40 '100 '100))
          ((Reduce) (list "#000000" '0 '100 '100))
          ((Glass) (list "#5E5E70" '60 '40 '60))
          ((WixDa) (list "#6E6D8F" '20 '40 '60))
          ((Smoothly) (list "#75759E" '20 '40 '40))
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
    `((focused . "#F2F2F2") (highlighted . "#FFFFFF") (clicked . "#FFFFFF") (inactive . "#D9D9D9") (inactive-highlighted . "#E6E6E6")
      (inactive-clicked . "#E6E6E6"))))

(define title-colors-wixda
  (lambda ()
    `((focused . "#262626") (highlighted . "#000000") (clicked . "#000000") (inactive . "#404040") (inactive-highlighted . "#333333")
      (inactive-clicked . "#333333"))))

(define title-colors-smoothly
  (lambda ()
    `((focused . "#333333") (highlighted . "#000000") (clicked . "#000000") (inactive . "#666666") (inactive-highlighted . "#444444")
      (inactive-clicked . "#444444"))))

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
          ((Default) 0)
          ((Reduce) 0)
          ((Glass) 0)
          ((WixDa) 0)
          ((Smoothly) 0))))

(define button-right-edge
  (lambda ()
    (case styletab-c:styles
          ((Default) 1)
          ((Reduce) 1)
          ((Glass) 0)
          ((WixDa) 0)
          ((Smoothly) 2))))

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

      (reframe-window w)
      (move-window-to w pos-x pos-y)
      (resize-window-to w dim-x dim-y)
      (tab-refresh-group w 'move))))

(define (tab/titelbartotop)
  "Move tab/titelbar to top, with resize (StyleTab Option)."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'left)
            (eq (window-get w 'title-position) 'right))
        (rotate-tab 'vert 'top))
    (if (eq (window-get w 'title-position) 'bottom)
        (rotate-tab 'horiz 'opposite))))

(define (tab/titelbartobottom)
  "Move tab/titelbar to bottom, with resize (StyleTab Option)."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'left)
            (eq (window-get w 'title-position) 'right))
        (rotate-tab 'vert 'bottom))
    (if (eq (window-get w 'title-position) 'top)
        (rotate-tab 'horiz 'opposite))))

(define (tab/titelbartoleft)
  "Move tab/titelbar to left, with resize (StyleTab Option)."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'left))
    (if (eq (window-get w 'title-position) 'right)
        (rotate-tab 'vert 'opposite))))

(define (tab/titelbartoright)
  "Move tab/titelbar to right, with resize (StyleTab Option)."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'right))
    (if (eq (window-get w 'title-position) 'left)
        (rotate-tab 'vert 'opposite))))

(define (tab/titelbartoggle)
  "Move tab/titelbar to the opposite side, with resize (StyleTab Option)."
  (let ((w (current-event-window)))
    (if (not (window-get w 'title-position))
        (window-put w 'title-position styletab-c:titlebar-place))
    (if (or (eq (window-get w 'title-position) 'top)
            (eq (window-get w 'title-position) 'bottom))
        (rotate-tab 'horiz 'opposite)
      (rotate-tab 'vert 'opposite))))

(define-command-gaol 'tab/titelbar-toggle tab/titelbartoggle)
(define-command-gaol 'tab/titelbar-to-top tab/titelbartotop)
(define-command-gaol 'tab/titelbar-to-bottom tab/titelbartobottom)
(define-command-gaol 'tab/titelbar-to-left tab/titelbartoleft)
(define-command-gaol 'tab/titelbar-to-right tab/titelbartoright)

(def-frame-class tabbar-horizontal-left-edge ()
  (bind-keys tabbar-horizontal-left-edge-keymap
             "Button1-Off" 'tab/titelbar-to-left
             "Button2-Off" 'tab/titelbar-toggle
             "Button3-Off" 'tab/titelbar-to-right))

(def-frame-class tabbar-vertical-top-edge ()
  (bind-keys tabbar-vertical-top-edge-keymap
             "Button1-Off" 'tab/titelbar-to-top
             "Button2-Off" 'tab/titelbar-toggle
             "Button3-Off" 'tab/titelbar-to-bottom))

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
                (resize-window-to w dim-x dim-y)) wins)
      (tab-refresh-group w 'move))))

(define (set-frame-default-and-default/transient)
  "Set frametype to `default' and toggle transient-ness, with resize (StyleTab Option)."
  (f-type 'def-tra))

(define (set-frame-unframed-and-unframed/shaped-transient)
  "Set frametype to 'unframed' and toggle transient-ness, with resize (StyleTab Option)."
  (f-type 'unf-def))

(define (set-frame-shaped-and-shaped/shaped-transient)
  "Set frametype to shaped and toggle transient-ness, with resize (StyleTab Option)."
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
(define styletab-c-frame-cache (make-table equal-hash equal))

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

(define (get-recolor-dark dimout color)
  (darken-color color dimout))

(define (get-recolor-bright bright color)
  (brighten-color color bright))

(define (do-recolor img color)
  (let ((recolorer
         (make-image-recolorer color
                               #:zero-channel blue-channel
                               #:index-channel green-channel)))
    (recolorer img)
    img))

(define (do-make-get-image img)
  (or
   (table-ref styletab-c-frame-cache img)
   (let ((image
          (make-image img)))
     (table-set styletab-c-frame-cache img image)
     image)))

(define (base-tables-images w)
  (let ((focus (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-f.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark 0 styletab-c:focus-frame-color)
                             (get-recolor-dark 0 (get-color (nth 0 (proposals-colors)))))))
        (inact (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-i.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark (* styletab-c:inactive-dimout 20) styletab-c:unfocus-frame-color)
                             (get-recolor-dark (nth 1 (proposals-colors)) (get-color (nth 0 (proposals-colors))))))))
    (table-unset styletab-c-frame-cache w)
    (table-set styletab-c-frame-cache w `((focused . ,focus) (inactive . ,inact)))))

(define (tab-tables-images w)
  (let ((focus (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-f.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark 0 styletab-c:focus-frame-color)
                             (get-recolor-dark 0 (get-color (nth 0 (proposals-colors)))))))
        (focus-m (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-marked-f.png"))
                             (if (eq styletab-c:tabbar-marked t)
                                 styletab-c:tabbar-marked-color
                               (if (eq styletab-c:custom-frame-colors t)
                                   (get-recolor-bright 20 styletab-c:focus-frame-color)
                                 (get-recolor-bright 20 (get-color (nth 0 (proposals-colors))))))))
        (highl (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-h.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-bright 
                                (if (eq styletab-c:hightlight-tabbar t) 
                                    (/ (* styletab-c:active-hightlight-brighten 20) 2) 0)
                                styletab-c:focus-frame-color) (get-recolor-bright 
                                                               (if (eq styletab-c:hightlight-tabbar t) 
                                                                   (/ (nth 2 (proposals-colors)) 2) 0)
                                                               (get-color (nth 0 (proposals-colors)))))))
        (highl-m (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-marked-h.png"))
                             (if (eq styletab-c:tabbar-marked t)
                                 (if (eq styletab-c:hightlight-tabbar t)
                                     (get-recolor-bright 20 styletab-c:tabbar-marked-color)
                                   styletab-c:tabbar-marked-color)
                               (if (eq styletab-c:custom-frame-colors t)
                                   (get-recolor-bright 
                                    (if (eq styletab-c:hightlight-tabbar t) 
                                        (/ (* styletab-c:active-hightlight-brighten 40) 2) 20)
                                    styletab-c:focus-frame-color) (get-recolor-bright 
                                                                   (if (eq styletab-c:hightlight-tabbar t) 
                                                                       (/ (nth 2 (proposals-colors)) 2) 0)
                                                                   (get-recolor-bright 20 (get-color (nth 0 (proposals-colors)))))))))
        (inact (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-i.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark (* styletab-c:inactive-dimout 20) styletab-c:unfocus-frame-color)
                             (get-recolor-dark (nth 1 (proposals-colors)) (get-color (nth 0 (proposals-colors)))))))
        (inact-m (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-marked-i.png"))
                             (if (eq styletab-c:tabbar-marked t)
                                 styletab-c:tabbar-marked-color
                               (if (eq styletab-c:custom-frame-colors t)
                                   (get-recolor-bright 20 styletab-c:focus-frame-color)
                                 (get-recolor-bright 20 (get-color (nth 0 (proposals-colors))))))))
        (in-hi (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-ih.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-bright 
                                (if (eq styletab-c:hightlight-tabbar t) 
                                    (- (/ (* styletab-c:inactive-hightlight-brighten 20) 2) (* styletab-c:inactive-dimout 20))
                                  (- (* styletab-c:inactive-dimout 20)))
                                styletab-c:unfocus-frame-color)
                             (get-recolor-bright (if (eq styletab-c:hightlight-tabbar t) 
                                                     (- (/ (nth 3 (proposals-colors)) 2) (nth 1 (proposals-colors)))
                                                   (- (nth 1 (proposals-colors))))
                                                 (get-color (nth 0 (proposals-colors)))))))
        (in-hi-m (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-marked-ih.png"))
                             (if (eq styletab-c:tabbar-marked t)
                                 (if (eq styletab-c:hightlight-tabbar t)
                                     (get-recolor-bright 20 styletab-c:tabbar-marked-color)
                                   styletab-c:tabbar-marked-color)
                               (if (eq styletab-c:custom-frame-colors t)
                                   (get-recolor-bright 
                                    (if (eq styletab-c:hightlight-tabbar t) 
                                        (/ (* styletab-c:active-hightlight-brighten 40) 2) 20)
                                    styletab-c:focus-frame-color) (get-recolor-bright 
                                                                   (if (eq styletab-c:hightlight-tabbar t) 
                                                                       (/ (nth 2 (proposals-colors)) 2) 0)
                                                                   (get-recolor-bright 20 (get-color (nth 0 (proposals-colors))))))))))
    (table-unset styletab-c-frame-cache w)
    (table-set styletab-c-frame-cache w `((focused . ,focus) (highlighted . ,highl) (inactive . ,inact) (inactive-highlighted . ,in-hi)))
    (table-unset styletab-c-frame-cache (concat w "-m"))
    (table-set styletab-c-frame-cache 
               (concat w "-m") `((focused . ,focus-m) (highlighted . ,highl-m) (inactive . ,inact-m) (inactive-highlighted . ,in-hi-m)))))

(define (base-button-tables-images w)
  (let ((focus (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-f.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark 0 styletab-c:focus-frame-color)
                             (get-recolor-dark 0 (get-color (nth 0 (proposals-colors)))))))
        (in-cl (do-recolor (do-make-get-image  (concat (symbol-name styletab-c:styles) "/" w "-c.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark (* styletab-c:inactive-dimout 20) styletab-c:unfocus-frame-color)
                             (get-recolor-dark (nth 1 (proposals-colors)) (get-color (nth 0 (proposals-colors)))))))
        (click (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-c.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark 0 styletab-c:focus-frame-color)
                             (get-recolor-dark 0 (get-color (nth 0 (proposals-colors)))))))
        (inact (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-i.png"))
                           (if (eq styletab-c:custom-frame-colors t)
                               (get-recolor-dark (* styletab-c:inactive-dimout 20) styletab-c:unfocus-frame-color)
                             (get-recolor-dark (nth 1 (proposals-colors)) (get-color (nth 0 (proposals-colors))))))))
    (table-unset styletab-c-frame-cache w)
    (table-set styletab-c-frame-cache w  `((focused . ,focus) (clicked . ,click) (inactive . ,inact) (inactive-clicked . ,in-cl)))))

(define scale-w nil)
(define scale-h nil)
(define (button-tables-images w x color always)
  (if (or (equal w '"top")
          (equal w '"bottom"))
      (progn (setq scale-w (+ styletab-c:title-dimension (button-width-custom)))
             (setq scale-h (- styletab-c:title-dimension 4)))
    (progn (setq scale-w (- styletab-c:title-dimension 4))
           (setq scale-h (+ styletab-c:title-dimension (button-width-custom)))))
  (let ((focus (scale-image (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-f.png"))
                                        (if (eq styletab-c:custom-frame-colors t)
                                            (get-recolor-dark 0 (if (and always color) color styletab-c:focus-frame-color))
                                          (get-recolor-dark 0 (if (and always color) color (get-color (nth 0 (proposals-colors))))))) scale-w scale-h))
        (highl (scale-image (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-h.png"))
                                        (if (eq styletab-c:custom-frame-colors t)
                                            (get-recolor-bright (* styletab-c:active-hightlight-brighten 20)
                                                                (if color color styletab-c:focus-frame-color))
                                          (get-recolor-bright (nth 2 (proposals-colors))
                                                              (if color color (get-color (nth 0 (proposals-colors))))))) scale-w scale-h))
        (click (scale-image (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-c.png"))
                                        (if (eq styletab-c:custom-frame-colors t)
                                            (get-recolor-bright (* styletab-c:active-hightlight-brighten 20)
                                                                (if color color styletab-c:focus-frame-color))
                                          (get-recolor-bright (nth 2 (proposals-colors))
                                                              (if color color (get-color (nth 0 (proposals-colors))))))) scale-w scale-h))
        (inact (scale-image (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-i.png"))
                                        (if (eq styletab-c:custom-frame-colors t)
                                            (get-recolor-dark (* styletab-c:inactive-dimout 20)
                                                              (if (and always color) color styletab-c:unfocus-frame-color))
                                          (get-recolor-dark (nth 1 (proposals-colors))
                                                            (if (and always color) color (get-color (nth 0 (proposals-colors))))))) scale-w scale-h))
        (in-hi (scale-image (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-ih.png"))
                                        (if (eq styletab-c:custom-frame-colors t)
                                            (get-recolor-bright (- (* styletab-c:inactive-hightlight-brighten 20) (* styletab-c:inactive-dimout 20))
                                                                (if color color styletab-c:unfocus-frame-color))
                                          (get-recolor-bright (- (nth 3 (proposals-colors)) (nth 1 (proposals-colors)))
                                                              (if color color (get-color (nth 0 (proposals-colors))))))) scale-w scale-h))
        (in-cl (scale-image (do-recolor (do-make-get-image (concat (symbol-name styletab-c:styles) "/" w "-frame-" x "-button-ic.png"))
                                        (if (eq styletab-c:custom-frame-colors t)
                                            (get-recolor-bright (- (* styletab-c:inactive-hightlight-brighten 20) (* styletab-c:inactive-dimout 20))
                                                                (if color color styletab-c:unfocus-frame-color))
                                          (get-recolor-bright (- (nth 3 (proposals-colors)) (nth 1 (proposals-colors)))
                                                              (if color color (get-color (nth 0 (proposals-colors))))))) scale-w scale-h)))
    (table-unset styletab-c-frame-cache w)
    (table-set styletab-c-frame-cache (concat w "-frame-" x "-button") `((focused . ,focus) (highlighted . ,highl) (clicked . ,click) (inactive . ,inact)
                                                                         (inactive-highlighted . ,in-hi) (inactive-clicked . ,in-cl)))))

;; frames/tabbar
(define top-frame-icon-title-images
  (make-image (concat (symbol-name styletab-c:styles) "/" "top-frame-icon-title-images-f.png")))

(define (tabbar-horizontal-images)
  (mapcar (lambda (w) (mapcar (lambda (x)
                                (tab-tables-images (concat x "-" w))) (list "top" "bottom"))) (list "frame-tab-left-icon" "frame-tab" "frame-tab-right")))
(define (tabbar-vertical-images)
  (mapcar (lambda (w) (mapcar (lambda (x)
                                (tab-tables-images (concat x "-" w))) (list "left" "right"))) (list "frame-tab-top" "frame-tab" "frame-tab-bottom-icon")))
(define (title-cursor-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-title-cursor"))) (list "top" "bottom" "left" "right")))
(define (title-nocursor-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-title-nocursor"))) (list "top" "bottom" "left" "right")))
(define (top-border-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-top-border"))) (list "bottom" "left" "right")))
(define (top-left-corner-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-top-left-corner"))) (list "top" "bottom" "left" "right")))
(define (top-right-corner-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-top-right-corner"))) (list "top" "bottom" "left" "right")))
(define (top-left-corner-shaped-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-top-left-corner-shaped"))) (list "top" "left")))
(define (top-right-corner-shaped-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-top-right-corner-shaped"))) (list "top" "right")))
(define (title-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-title"))) (list "top" "bottom" "left" "right")))
(define (left-border-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-left-border"))) (list "top" "bottom" "right")))
(define (right-border-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-right-border"))) (list "top" "bottom" "left")))
(define (bottom-left-corner-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-bottom-left-corner"))) (list "top" "bottom" "left" "right")))
(define (bottom-border-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-bottom-border"))) (list "top" "left" "right")))
(define (bottom-right-corner-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-bottom-right-corner"))) (list "top" "bottom" "left" "right")))
(define (bottom-left-corner-shaped-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-bottom-left-corner-shaped"))) (list "bottom" "left")))
(define (bottom-right-corner-shaped-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-bottom-right-corner-shaped"))) (list "bottom" "right")))

;; buttons
(define (base-button-images)
  (mapcar (lambda (w) (base-button-tables-images (concat w "-frame-button"))) (list "top" "bottom" "left" "right")))
(define (space-button-images)
  (mapcar (lambda (w) (base-tables-images (concat w "-frame-title"))) (list "top" "bottom" "left" "right")))
(define (close-button-images)
  (mapcar (lambda (w) (button-tables-images w "close" styletab-c:hightlight-close
                                            (if (eq styletab-c:hightlight-close-all t) t))) (list "top" "bottom" "left" "right")))
(define (menu-button-images)
  (mapcar (lambda (w) (button-tables-images w "menu" styletab-c:hightlight-menu
                                            (if (eq styletab-c:hightlight-menu-all t) t))) (list "top" "bottom" "left" "right")))
(define (iconify-button-images)
  (mapcar (lambda (w) (button-tables-images w "iconify" styletab-c:hightlight-iconify
                                            (if (eq styletab-c:hightlight-iconify-all t) t))) (list "top" "bottom" "left" "right")))
(define (move-resize-button-images)
  (mapcar (lambda (w) (button-tables-images w "move-resize" styletab-c:hightlight-move-resize
                                            (if (eq styletab-c:hightlight-move-resize-all t) t))) (list "top" "bottom" "left" "right")))
(define (rename-button-images)
  (mapcar (lambda (w) (button-tables-images w "rename" styletab-c:hightlight-rename
                                            (if (eq styletab-c:hightlight-rename-all t) t))) (list "top" "bottom" "left" "right")))
(define (frame-type-button-images)
  (mapcar (lambda (w) (button-tables-images w "frame-type" styletab-c:hightlight-frame-type
                                            (if (eq styletab-c:hightlight-frame-type-all t) t))) (list "top" "bottom" "left" "right")))
(define (maximize-button-images)
  (mapcar (lambda (w) (mapcar (lambda (x) (button-tables-images x w styletab-c:hightlight-maximize
                                                                (if (eq styletab-c:hightlight-maximize-all t) t)))
                              (list "top" "bottom" "left" "right"))) (list "maximize" "unmaximize")))
(define (shade-button-images)
  (mapcar (lambda (w) (mapcar (lambda (x) (button-tables-images x w styletab-c:hightlight-shade
                                                                (if (eq styletab-c:hightlight-shade-all t) t)))
                              (list "top" "bottom" "left" "right"))) (list "shade" "unshade")))
(define (sticky-button-images)
  (mapcar (lambda (w) (mapcar (lambda (x) (button-tables-images x w styletab-c:hightlight-sticky
                                                                (if (eq styletab-c:hightlight-sticky-all t) t)))
                              (list "top" "bottom" "left" "right"))) (list "sticky" "unsticky")))
(define (lock-button-images)
  (mapcar (lambda (w) (mapcar (lambda (x) (button-tables-images x w styletab-c:hightlight-lock
                                                                (if (eq styletab-c:hightlight-lock-all t) t)))
                              (list "top" "bottom" "left" "right"))) (list "lock" "unlock")))
(define (prev-button-images)
  (mapcar (lambda (w) (mapcar (lambda (x) (button-tables-images x w styletab-c:hightlight-prev
                                                                (if (eq styletab-c:hightlight-prev-all t) t)))
                              (list "top" "bottom" "left" "right"))) (list "prev" "prev-last")))
(define (next-button-images)
  (mapcar (lambda (w) (mapcar (lambda (x) (button-tables-images x w styletab-c:hightlight-next
                                                                (if (eq styletab-c:hightlight-next-all t) t)))
                              (list "top" "bottom" "left" "right"))) (list "next" "next-last")))
(define (raise-lower-button-images)
  (mapcar (lambda (w) (mapcar (lambda (x) (button-tables-images x w styletab-c:hightlight-raise-lower
                                                                (if (eq styletab-c:hightlight-raise-lower-all t) t)))
                              (list "top" "bottom" "left" "right"))) (list "raise-lower" "ontop" "unontop")))
(define (recolor-base)
  (title-cursor-images) (title-nocursor-images) (top-border-images) (top-left-corner-images) (top-right-corner-images) (top-left-corner-shaped-images)
  (top-right-corner-shaped-images) (title-images) (left-border-images) (right-border-images) (bottom-left-corner-images) (bottom-border-images)
  (bottom-right-corner-images) (bottom-left-corner-shaped-images) (bottom-right-corner-shaped-images) (base-button-images) (space-button-images))

(define (recolor-tab)
  (tabbar-horizontal-images) (tabbar-vertical-images))

(define (recolor-close-button)
  (close-button-images))
(define (recolor-menu-button)
  (menu-button-images))
(define (recolor-iconify-button)
  (iconify-button-images))
(define (recolor-move-resize-button)
  (move-resize-button-images))
(define (recolor-rename-button)
  (rename-button-images))
(define (recolor-frame-type-button)
  (frame-type-button-images))
(define (recolor-maximize-button)
  (maximize-button-images))
(define (recolor-shade-button)
  (shade-button-images))
(define (recolor-sticky-button)
  (sticky-button-images))
(define (recolor-lock-button)
  (lock-button-images))
(define (recolor-prev-button)
  (prev-button-images))
(define (recolor-next-button)
  (next-button-images))
(define (recolor-raise-lower-button)
  (raise-lower-button-images))

(define (recolor-all-buttons)
  (recolor-close-button) (recolor-menu-button) (recolor-iconify-button) (recolor-move-resize-button) 
  (recolor-rename-button) (recolor-frame-type-button) (recolor-maximize-button) (recolor-shade-button) 
  (recolor-sticky-button) (recolor-lock-button) (recolor-prev-button) (recolor-next-button) (recolor-raise-lower-button))

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

(define sharped-edge
  (lambda (w)
    (if (or (eq (window-get w 'type) 'shaped) 
            (eq (window-get w 'type) 'shaped-transient))
        styletab-c:borders-dimension
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
  `(((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-title")))
     (left-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-top-left-corner")))
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-top-right-corner")))
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-title")))
     (right-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define bottom-frame-default-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-title")))
     (left-edge . 0)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-bottom-left-corner")))
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-bottom-right-corner")))
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-title")))
     (right-edge . 0)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define left-frame-default-border-corner-group
  `(((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-title")))
     (top-edge . 0)
     (left-edge . ,title-edge-s)
     (height . ,button-right-edge)
     (width . ,title-height-s))
    ((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-top-left-corner")))
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-bottom-left-corner")))
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-title")))
     (bottom-edge . 0)
     (left-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))))

(define right-frame-default-border-corner-group
  `(((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-title")))
     (top-edge . 0)
     (right-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))
    ((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-top-right-corner")))
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-bottom-right-corner")))
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-height))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-title")))
     (bottom-edge . 0)
     (right-edge . ,title-edge-s)
     (height . ,button-right-edge)
     (width . ,title-height-s))))

(define top-frame-border-group
  `(((class . left-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-left-border")))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (top-edge . 0)
     (width . ,frame-width)
     (bottom-edge . 0))
	((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-bottom-left-corner")))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-bottom-border")))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-bottom-right-corner")))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . right-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-right-border")))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))))

(define bottom-frame-border-group
  `(((class . left-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-left-border")))
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (bottom-edge . 0)
     (width . ,frame-width)
     (top-edge . 0))
    ((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-top-left-corner")))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-top-border")))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-top-right-corner")))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . right-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-right-border")))
     (cursor . sb_h_double_arrow)
     (bottom-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (top-edge . 0))))

(define left-frame-border-group
  `(((class . bottom-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-bottom-border")))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-bottom-right-corner")))
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . right-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-right-border")))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-top-right-corner")))
     (top-edge . ,frame-edge)
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-top-border")))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define right-frame-border-group
  `(((class . bottom-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-bottom-border")))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-bottom-left-corner")))
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . left-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-left-border")))
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (left-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-top-left-corner")))
     (top-edge . ,frame-edge)
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-top-border")))
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define top-frame-title-group
  `(((class . top-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-title-cursor")))
     (cursor . sb_v_double_arrow)
     (left-edge . ,sharped-edge)
     (top-edge . ,title-edge)
     (right-edge . ,sharped-edge)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- styletab-c:title-dimension 12)))
     (y-justify . ,(lambda (w) (+ (/ styletab-c:title-dimension 2) -7)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"top-frame-tab-m")
                                  (table-ref styletab-c-frame-cache '"top-frame-tab"))))
     (foreground . ,title-colors-images)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"top-frame-tab-left-icon-m")
                                  (table-ref styletab-c-frame-cache '"top-frame-tab-left-icon"))))
     (cursor . hand2)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,(lambda (w) (+ 2 (title-height-s w))))
     (y-justify . 2)
     (x-justify . ,(lambda (w) (+ 3 (icon-edge w)))))
    ((class . tabbar-horizontal-right-edge)
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"top-frame-tab-right-m")
                                  (table-ref styletab-c-frame-cache '"top-frame-tab-right"))))
     (width . ,tabbar-right-edge-width)
     (height . ,title-height-s)
     (top-edge . ,title-edge-s))
    ((class . title)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-title-nocursor")))
     (left-edge . ,sharped-edge)
     (top-edge . -2)
     (right-edge . ,sharped-edge)
     (height . 2))))

(define bottom-frame-title-group
  `(((class . title)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-title-nocursor")))
     (left-edge . ,sharped-edge)
     (bottom-edge . -2)
     (right-edge . ,sharped-edge)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- styletab-c:title-dimension 12)))
     (y-justify . ,(lambda (w) (+ (/ styletab-c:title-dimension 2) -7)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"bottom-frame-tab-m")
                                  (table-ref styletab-c-frame-cache '"bottom-frame-tab"))))
     (foreground . ,title-colors-images)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"bottom-frame-tab-left-icon-m")
                                  (table-ref styletab-c-frame-cache '"bottom-frame-tab-left-icon"))))
     (cursor . hand2)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,(lambda (w) (+ 2 (title-height-s w))))
     (y-justify . 2)
     (x-justify . ,(lambda (w) (+ 3 (icon-edge w)))))
    ((class . tabbar-horizontal-right-edge)
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"bottom-frame-tab-right-m")
                                  (table-ref styletab-c-frame-cache '"bottom-frame-tab-right"))))
     (width . ,tabbar-right-edge-width)
     (height . ,title-height-s)
     (bottom-edge . ,title-edge-s))
    ((class . bottom-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-title-cursor")))
     (cursor . sb_v_double_arrow)
     (left-edge . ,sharped-edge)
     (bottom-edge . ,title-edge)
     (right-edge . ,sharped-edge)
     (height . 2))))

(define left-frame-title-group
  `(((class . left-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-title-cursor")))
     (cursor . sb_h_double_arrow)
     (top-edge . ,sharped-edge)
     (left-edge . ,title-edge)
     (bottom-edge . ,sharped-edge)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"left-frame-tab-top-m")
                                  (table-ref styletab-c-frame-cache '"left-frame-tab-top"))))
     (cursor . hand2)
     (height . ,(lambda (w) (+ 2 (title-height-s w))))
     (width . ,title-height-s)
     (left-edge . ,title-edge-s)
     (y-justify . ,(lambda (w) (+ 2 (icon-edge w))))
     (x-justify . 2))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"left-frame-tab-m")
                                  (table-ref styletab-c-frame-cache '"left-frame-tab"))))
     (left-edge . ,title-edge-s)
     (width . ,title-height-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"left-frame-tab-bottom-icon-m")
                                  (table-ref styletab-c-frame-cache '"left-frame-tab-bottom-icon"))))
     (left-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . ,(lambda (w) (- (+ 0 (icon-edge w)))))
     (x-justify . 2))
    ((class . title)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-title-nocursor")))
     (top-edge . ,sharped-edge)
     (left-edge . -2)
     (bottom-edge . ,sharped-edge)
     (width . 2))))

(define right-frame-title-group
  `(((class . right-border)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-title-cursor")))
     (cursor . sb_h_double_arrow)
     (top-edge . ,sharped-edge)
     (right-edge . ,title-edge)
     (bottom-edge . ,sharped-edge)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"right-frame-tab-top-m")
                                  (table-ref styletab-c-frame-cache '"right-frame-tab-top"))))
     (cursor . hand2)
     (height . ,(lambda (w) (+ 2 (title-height-s w))))
     (width . ,title-height-s)
     (right-edge . ,title-edge-s)
     (y-justify . ,(lambda (w) (+ 2 (icon-edge w))))
     (x-justify . 2))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"right-frame-tab-m")
                                  (table-ref styletab-c-frame-cache '"right-frame-tab"))))
     (right-edge . ,title-edge-s)
     (width . ,title-height-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,(lambda (w) (if (window-get w 'marked)
                                    (table-ref styletab-c-frame-cache '"right-frame-tab-bottom-icon-m")
                                  (table-ref styletab-c-frame-cache '"right-frame-tab-bottom-icon"))))
     (right-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,title-height-s)
     (y-justify . ,(lambda (w) (- (+ 0 (icon-edge w)))))
     (x-justify . 2))
    ((class . title)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-title-nocursor")))
     (top-edge . ,sharped-edge)
     (right-edge . -2)
     (bottom-edge . ,sharped-edge)
     (width . 2))))

(define top-frame-close-button
  `((class . close-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-close-button")))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-close-button
  `((class . close-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-close-button")))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-close-button
  `((class . close-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-close-button")))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-close-button
  `((class . close-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-close-button")))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-menu-button
  `((class . menu-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-menu-button")))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-menu-button
  `((class . menu-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-menu-button")))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-menu-button
  `((class . menu-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-menu-button")))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-menu-button
  `((class . menu-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-menu-button")))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-iconify-button
  `((class . iconify-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-iconify-button")))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-iconify-button
  `((class . iconify-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-iconify-button")))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-iconify-button
  `((class . iconify-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-iconify-button")))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-iconify-button
  `((class . iconify-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-iconify-button")))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-maximize-button
  `((class . maximize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (table-ref styletab-c-frame-cache '"top-frame-unmaximize-button") 
                                 (table-ref styletab-c-frame-cache '"top-frame-maximize-button"))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-maximize-button
  `((class . maximize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (table-ref styletab-c-frame-cache '"bottom-frame-unmaximize-button") 
                                 (table-ref styletab-c-frame-cache '"bottom-frame-maximize-button"))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-maximize-button
  `((class . maximize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (table-ref styletab-c-frame-cache '"left-frame-unmaximize-button") 
                                 (table-ref styletab-c-frame-cache '"left-frame-maximize-button"))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-maximize-button
  `((class . maximize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'unmaximized-geometry) 
                                   (table-ref styletab-c-frame-cache '"right-frame-unmaximize-button") 
                                 (table-ref styletab-c-frame-cache '"right-frame-maximize-button"))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-shade-button
  `((class . shade-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) 
                                   (table-ref styletab-c-frame-cache '"top-frame-unshade-button") 
                                 (table-ref styletab-c-frame-cache '"top-frame-shade-button"))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-shade-button
  `((class . shade-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) 
                                   (table-ref styletab-c-frame-cache '"bottom-frame-unshade-button") 
                                 (table-ref styletab-c-frame-cache '"bottom-frame-shade-button"))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-shade-button
  `((class . shade-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) 
                                   (table-ref styletab-c-frame-cache '"left-frame-unshade-button") 
                                 (table-ref styletab-c-frame-cache '"left-frame-shade-button"))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-shade-button
  `((class . shade-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'shaded) 
                                   (table-ref styletab-c-frame-cache '"right-frame-unshade-button") 
                                 (table-ref styletab-c-frame-cache '"right-frame-shade-button"))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-sticky-button
  `((class . sticky-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-get w 'sticky)
                                       (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"top-frame-unsticky-button") 
                                 (table-ref styletab-c-frame-cache '"top-frame-sticky-button"))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-sticky-button
  `((class . sticky-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-get w 'sticky) 
                                       (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"bottom-frame-unsticky-button") 
                                 (table-ref styletab-c-frame-cache '"bottom-frame-sticky-button"))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-sticky-button
  `((class . sticky-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-get w 'sticky) 
                                       (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"left-frame-unsticky-button") 
                                 (table-ref styletab-c-frame-cache '"left-frame-sticky-button"))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-sticky-button
  `((class . sticky-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-get w 'sticky) 
                                       (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"right-frame-unsticky-button") 
                                 (table-ref styletab-c-frame-cache '"right-frame-sticky-button"))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-space-button
  `((class . title)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-title")))
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-space-button
  `((class . title)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-title")))
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-space-button
  `((class . title)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-title")))
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-space-button
  `((class . title)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-title")))
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"top-frame-prev-last-button")   
                                 (table-ref styletab-c-frame-cache '"top-frame-prev-button"))))    
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"bottom-frame-prev-last-button")   
                                 (table-ref styletab-c-frame-cache '"bottom-frame-prev-button"))))    
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"left-frame-prev-last-button")   
                                 (table-ref styletab-c-frame-cache '"left-frame-prev-button"))))    
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-prev-button
  `((class . previous-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-first-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"right-frame-prev-last-button")   
                                 (table-ref styletab-c-frame-cache '"right-frame-prev-button"))))    
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-next-button
  `((class . next-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"top-frame-next-last-button")   
                                 (table-ref styletab-c-frame-cache '"top-frame-next-button"))))    
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-next-button
  `((class . next-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"bottom-frame-next-last-button")   
                                 (table-ref styletab-c-frame-cache '"bottom-frame-next-button"))))    
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-next-button
  `((class . next-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"left-frame-next-last-button")   
                                 (table-ref styletab-c-frame-cache '"left-frame-next-button"))))    
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-next-button
  `((class . next-workspace-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (if (or (window-in-workspace-p w (- (get-last-workspace) 1)) (window-get w 'sticky) (window-get w 'sticky-viewport))
                                   (table-ref styletab-c-frame-cache '"right-frame-next-last-button")   
                                 (table-ref styletab-c-frame-cache '"right-frame-next-button"))))    
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-lock-button
  `((class . lock-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) 
                                   (table-ref styletab-c-frame-cache '"top-frame-unlock-button") 
                                 (table-ref styletab-c-frame-cache '"top-frame-lock-button"))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-lock-button
  `((class . lock-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) 
                                   (table-ref styletab-c-frame-cache '"bottom-frame-unlock-button") 
                                 (table-ref styletab-c-frame-cache '"bottom-frame-lock-button"))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-lock-button
  `((class . lock-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) 
                                   (table-ref styletab-c-frame-cache '"left-frame-unlock-button") 
                                 (table-ref styletab-c-frame-cache '"left-frame-lock-button"))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-lock-button
  `((class . lock-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (if (window-get w 'fixed-position) 
                                   (table-ref styletab-c-frame-cache '"right-frame-unlock-button") 
                                 (table-ref styletab-c-frame-cache '"right-frame-lock-button"))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (table-ref styletab-c-frame-cache '"top-frame-raise-lower-button") 
                                 (if (> (window-get w 'depth) 0) 
                                     (table-ref styletab-c-frame-cache '"top-frame-ontop-button")
                                   (table-ref styletab-c-frame-cache '"top-frame-unontop-button")))))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (table-ref styletab-c-frame-cache '"bottom-frame-raise-lower-button") 
                                 (if (> (window-get w 'depth) 0) 
                                     (table-ref styletab-c-frame-cache '"bottom-frame-ontop-button")
                                   (table-ref styletab-c-frame-cache '"bottom-frame-unontop-button")))))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (table-ref styletab-c-frame-cache '"left-frame-raise-lower-button") 
                                 (if (> (window-get w 'depth) 0) 
                                     (table-ref styletab-c-frame-cache '"left-frame-ontop-button")
                                   (table-ref styletab-c-frame-cache '"left-frame-unontop-button")))))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (if (= (window-get w 'depth) 0) (table-ref styletab-c-frame-cache '"right-frame-raise-lower-button") 
                                 (if (> (window-get w 'depth) 0) 
                                     (table-ref styletab-c-frame-cache '"right-frame-ontop-button")
                                   (table-ref styletab-c-frame-cache '"right-frame-unontop-button")))))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define  top-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-move-resize-button")))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define  bottom-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-move-resize-button")))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define  left-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-move-resize-button")))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define  right-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-move-resize-button")))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-rename-button
  `((class . rename-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-rename-button")))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-rename-button
  `((class . rename-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-rename-button")))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-rename-button
  `((class . rename-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-rename-button")))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-rename-button
  `((class . rename-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-rename-button")))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-frame-type-button")))
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-frame-type-button")))
    (cursor . hand2)
    (bottom-edge . ,title-edge-s)
    (height . ,title-height-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-frame-type-button")))
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-height-s)))

(define right-frame-frame-type-button
  `((class . frame-type-button)
    (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-button")))
    (foreground . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-frame-type-button")))
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-height-s)))

(define top-frame-shaped-border-corner-group
  `(((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-title")))
     (left-edge . ,sharped-edge)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . top-left-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"top-frame-top-left-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"top-frame-top-left-corner"))))
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-height)
     (width . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                   (eq (window-get w 'type) 'shaped-transient))
                               styletab-c:borders-dimension
                             (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge)))))
    ((class . top-right-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"top-frame-top-right-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"top-frame-top-right-corner"))))
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                   (eq (window-get w 'type) 'shaped-transient))
                               styletab-c:borders-dimension
                             (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge)))))
    ((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"top-frame-title")))
     (right-edge . ,sharped-edge)
     (top-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define bottom-frame-shaped-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-title")))
     (left-edge . ,sharped-edge)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-left-edge))
    ((class . bottom-left-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"bottom-frame-bottom-left-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"bottom-frame-bottom-left-corner"))))
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-height)
     (width . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                   (eq (window-get w 'type) 'shaped-transient))
                               styletab-c:borders-dimension
                             (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge)))))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"bottom-frame-bottom-right-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"bottom-frame-bottom-right-corner"))))
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-height)
     (width . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                   (eq (window-get w 'type) 'shaped-transient))
                               styletab-c:borders-dimension
                             (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge)))))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"bottom-frame-title")))
     (right-edge . ,sharped-edge)
     (bottom-edge . ,title-edge-s)
     (height . ,title-height-s)
     (width . ,button-right-edge))))

(define left-frame-shaped-border-corner-group
  `(((class . top-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-title")))
     (top-edge . ,sharped-edge)
     (left-edge . ,title-edge-s)
     (height . ,button-right-edge)
     (width . ,title-height-s))
    ((class . top-left-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"left-frame-top-left-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"left-frame-top-left-corner"))))
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                    (eq (window-get w 'type) 'shaped-transient))
                                styletab-c:borders-dimension
                              (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge))))
     (width . ,title-height))
    ((class . bottom-left-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"left-frame-bottom-left-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"left-frame-bottom-left-corner"))))
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                    (eq (window-get w 'type) 'shaped-transient))
                                styletab-c:borders-dimension
                              (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge))))
     (width . ,title-height))
    ((class . bottom-left-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"left-frame-title")))
     (bottom-edge . ,sharped-edge)
     (left-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))))

(define right-frame-shaped-border-corner-group
  `(((class . top-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-title")))
     (top-edge . ,sharped-edge)
     (right-edge . ,title-edge-s)
     (height . ,button-left-edge)
     (width . ,title-height-s))
    ((class . top-right-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"right-frame-top-right-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"right-frame-top-right-corner"))))
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                    (eq (window-get w 'type) 'shaped-transient))
                                styletab-c:borders-dimension
                              (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge))))
     (width . ,title-height))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (if (window-get w 'shaded) 
                                    (table-ref styletab-c-frame-cache '"right-frame-bottom-right-corner-shaped")
                                  (table-ref styletab-c-frame-cache '"right-frame-bottom-right-corner"))))
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,(lambda (w) (if (or (eq (window-get w 'type) 'shaped) 
                                    (eq (window-get w 'type) 'shaped-transient))
                                styletab-c:borders-dimension
                              (if (window-get w 'shaded) styletab-c:borders-dimension sharped-edge))))
     (width . ,title-height))
    ((class . bottom-right-corner)
     (background . ,(lambda (w) (table-ref styletab-c-frame-cache '"right-frame-title")))
     (bottom-edge . ,sharped-edge)
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

(define frame nil)
(define button-alist nil)
(define current-title styletab-c:titlebar-place)
(define recolor-lock t)
(define current-type nil)

;; botton list table
(define styletab-c-botton-cache (make-table equal-hash equal))

(define (make-buttons)
  (setq styletab-c-botton-cache (make-weak-table equal-hash equal))
  ;; ripped from Anonymous
  (let* (;; turns one cons cell (btn-name . show-in-transients) into a button
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

         (setalist (setq button-alist top-button-alist))
         (top-normal-buttons-left
          (make-button-list nil styletab-c:top-left-buttons 'left-edge (+ (button-left-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (top-normal-buttons-left-s
          (make-button-list nil styletab-c:top-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (top-normal-buttons-right
          (make-button-list nil styletab-c:top-right-buttons 'right-edge (+ (button-right-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (top-normal-buttons-right-s
          (make-button-list nil styletab-c:top-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (top-transient-buttons-left
          (make-button-list t styletab-c:top-left-buttons 'left-edge (+ (button-left-edge) styletab-c:borders-dimension)
                            (+ styletab-c:title-dimension (button-width-custom))))
         (top-transient-buttons-left-s
          (make-button-list t styletab-c:top-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (top-transient-buttons-right
          (make-button-list t styletab-c:top-right-buttons 'right-edge (+ (button-right-edge) styletab-c:borders-dimension)
                            (+ styletab-c:title-dimension (button-width-custom))))
         (top-transient-buttons-right-s
          (make-button-list t styletab-c:top-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))

         (setalist (setq button-alist bottom-button-alist))
         (bottom-normal-buttons-left
          (make-button-list nil styletab-c:bottom-left-buttons 'left-edge (+ (button-left-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (bottom-normal-buttons-left-s
          (make-button-list nil styletab-c:bottom-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (bottom-normal-buttons-right
          (make-button-list nil styletab-c:bottom-right-buttons 'right-edge (+ (button-right-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (bottom-normal-buttons-right-s
          (make-button-list nil styletab-c:bottom-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (bottom-transient-buttons-left
          (make-button-list t styletab-c:bottom-left-buttons 'left-edge (+ (button-left-edge) styletab-c:borders-dimension)
                            (+ styletab-c:title-dimension (button-width-custom))))
         (bottom-transient-buttons-left-s
          (make-button-list t styletab-c:bottom-left-buttons 'left-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (bottom-transient-buttons-right
          (make-button-list t styletab-c:bottom-right-buttons 'right-edge (+ (button-right-edge) styletab-c:borders-dimension)
                            (+ styletab-c:title-dimension (button-width-custom))))
         (bottom-transient-buttons-right-s
          (make-button-list t styletab-c:bottom-right-buttons 'right-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))

         (setalist (setq button-alist left-button-alist))
         (left-normal-buttons-top
          (make-button-list nil styletab-c:left-top-buttons 'top-edge (+ (button-right-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (left-normal-buttons-top-s
          (make-button-list nil styletab-c:left-top-buttons 'top-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (left-normal-buttons-bottom 
          (make-button-list nil styletab-c:left-bottom-buttons 'bottom-edge (+ (button-left-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (left-normal-buttons-bottom-s
          (make-button-list nil styletab-c:left-bottom-buttons 'bottom-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (left-transient-buttons-top
          (make-button-list t styletab-c:left-top-buttons 'top-edge (+ (button-right-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (left-transient-buttons-top-s
          (make-button-list t styletab-c:left-top-buttons 'top-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (left-transient-buttons-bottom
          (make-button-list t styletab-c:left-bottom-buttons 'bottom-edge (+ (button-left-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (left-transient-buttons-bottom-s
          (make-button-list t styletab-c:left-bottom-buttons 'bottom-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         
         (setalist (setq button-alist right-button-alist))
         (right-normal-buttons-top
          (make-button-list nil styletab-c:right-top-buttons 'top-edge (+ (button-left-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (right-normal-buttons-top-s
          (make-button-list nil styletab-c:right-top-buttons 'top-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (right-normal-buttons-bottom 
          (make-button-list nil styletab-c:right-bottom-buttons 'bottom-edge (+ (button-right-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (right-normal-buttons-bottom-s
          (make-button-list nil styletab-c:right-bottom-buttons 'bottom-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (right-transient-buttons-top
          (make-button-list t styletab-c:right-top-buttons 'top-edge (+ (button-left-edge) styletab-c:borders-dimension)
                            (+ styletab-c:title-dimension (button-width-custom))))
         (right-transient-buttons-top-s
          (make-button-list t styletab-c:right-top-buttons 'top-edge (button-left-edge) (+ styletab-c:title-dimension (button-width-custom))))
         (right-transient-buttons-bottom
          (make-button-list t styletab-c:right-bottom-buttons 'bottom-edge (+ (button-right-edge) styletab-c:borders-dimension) 
                            (+ styletab-c:title-dimension (button-width-custom))))
         (right-transient-buttons-bottom-s
          (make-button-list t styletab-c:right-bottom-buttons 'bottom-edge (button-right-edge) (+ styletab-c:title-dimension (button-width-custom)))))
    
    (table-set styletab-c-botton-cache "top-normal-buttons-left" top-normal-buttons-left)
    (table-set styletab-c-botton-cache "top-normal-buttons-left-s" top-normal-buttons-left-s)
    (table-set styletab-c-botton-cache "top-normal-buttons-right" top-normal-buttons-right)
    (table-set styletab-c-botton-cache "top-normal-buttons-right-s" top-normal-buttons-right-s)
    (table-set styletab-c-botton-cache "top-transient-buttons-left" top-transient-buttons-left)
    (table-set styletab-c-botton-cache "top-transient-buttons-left-s" top-transient-buttons-left-s)
    (table-set styletab-c-botton-cache "top-transient-buttons-right" top-transient-buttons-right)
    (table-set styletab-c-botton-cache "top-transient-buttons-right-s" top-transient-buttons-right-s)
    (table-set styletab-c-botton-cache "bottom-normal-buttons-left" bottom-normal-buttons-left)
    (table-set styletab-c-botton-cache "bottom-normal-buttons-left-s" bottom-normal-buttons-left-s)
    (table-set styletab-c-botton-cache "bottom-normal-buttons-right" bottom-normal-buttons-right)
    (table-set styletab-c-botton-cache "bottom-normal-buttons-right-s" bottom-normal-buttons-right-s)
    (table-set styletab-c-botton-cache "bottom-transient-buttons-left" bottom-transient-buttons-left)
    (table-set styletab-c-botton-cache "bottom-transient-buttons-left-s" bottom-transient-buttons-left-s)
    (table-set styletab-c-botton-cache "bottom-transient-buttons-right" bottom-transient-buttons-right)
    (table-set styletab-c-botton-cache "bottom-transient-buttons-right-s" bottom-transient-buttons-right-s)
    (table-set styletab-c-botton-cache "left-normal-buttons-top" left-normal-buttons-top)
    (table-set styletab-c-botton-cache "left-normal-buttons-top-s" left-normal-buttons-top-s)
    (table-set styletab-c-botton-cache "left-normal-buttons-bottom" left-normal-buttons-bottom)
    (table-set styletab-c-botton-cache "left-normal-buttons-bottom-s" left-normal-buttons-bottom-s)
    (table-set styletab-c-botton-cache "left-transient-buttons-top" left-transient-buttons-top)
    (table-set styletab-c-botton-cache "left-transient-buttons-top-s" left-transient-buttons-top-s)
    (table-set styletab-c-botton-cache "left-transient-buttons-bottom" left-transient-buttons-bottom)
    (table-set styletab-c-botton-cache "left-transient-buttons-bottom-s" left-transient-buttons-bottom-s)
    (table-set styletab-c-botton-cache "right-normal-buttons-top" right-normal-buttons-top)
    (table-set styletab-c-botton-cache "right-normal-buttons-top-s" right-normal-buttons-top-s)
    (table-set styletab-c-botton-cache "right-normal-buttons-bottom" right-normal-buttons-bottom)
    (table-set styletab-c-botton-cache "right-normal-buttons-bottom-s" right-normal-buttons-bottom-s)
    (table-set styletab-c-botton-cache "right-transient-buttons-top" right-transient-buttons-top)
    (table-set styletab-c-botton-cache "right-transient-buttons-top-s" right-transient-buttons-top-s)
    (table-set styletab-c-botton-cache "right-transient-buttons-bottom" right-transient-buttons-bottom)
    (table-set styletab-c-botton-cache "right-transient-buttons-bottom-s" right-transient-buttons-bottom-s)))

(define (adjustments-tabbar w)
  (let ((tab-adjustments (table-ref styletab-c-botton-cache w)))
    (if (numberp (cdr (car (car tab-adjustments))))
        (+ (cdr (car (car tab-adjustments))) (+ styletab-c:title-dimension (button-width-custom))) 0)))

(define (check-sharped w)
  (if (or (eq current-type 'shaped)
          (eq current-type 'shaped-transient))
      w (concat w "-s")))

(define (make-frame w type current-title)
  (setq current-type (window-get w 'type))
  (require 'sawfish.wm.tabs.tab)
  (when (eq current-title 'top)
    (update-title-x-offsets `(,(- styletab-c:title-dimension 12) . 0))
    (set-tab-adjustments #:theme-left-dec-width 11 #:theme-right-dec-width (tabbar-right-edge-width)
                         #:theme-left-margin (adjustments-tabbar (check-sharped '"top-normal-buttons-left"))
                         #:theme-right-margin (adjustments-tabbar (check-sharped '"top-normal-buttons-right"))
                         #:theme-left-margin-transient (adjustments-tabbar (check-sharped '"top-transient-buttons-left"))
                         #:theme-right-margin-transient (adjustments-tabbar (check-sharped '"top-transient-buttons-right")))
    (when (eq type 'normal-frame)
      (setq frame (append top-frame-title-group top-frame-default-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"top-normal-buttons-left"))
                          top-frame-border-group (table-ref styletab-c-botton-cache (check-sharped '"top-normal-buttons-right")))))
    (when (eq type 'transient-frame)
      (setq frame (append top-frame-title-group top-frame-default-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"top-transient-buttons-left"))
                          top-frame-border-group (table-ref styletab-c-botton-cache (check-sharped '"top-transient-buttons-right")))))
    (when (eq type 'shaped-frame)
      (setq frame (append top-frame-title-group top-frame-shaped-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"top-normal-buttons-left"))
                          (table-ref styletab-c-botton-cache (check-sharped '"top-normal-buttons-right")))))
    (when (eq type 'shaped-transient-frame)
      (setq frame (append top-frame-title-group top-frame-shaped-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"top-transient-buttons-left"))
                          (table-ref styletab-c-botton-cache (check-sharped '"top-transient-buttons-right"))))))
  
  (when (eq current-title 'bottom)
    (update-title-x-offsets `(,(- styletab-c:title-dimension 12) . 0))
    (set-tab-adjustments #:theme-left-dec-width 11 #:theme-right-dec-width (tabbar-right-edge-width)
                         #:theme-left-margin (adjustments-tabbar (check-sharped '"bottom-normal-buttons-left"))
                         #:theme-right-margin (adjustments-tabbar (check-sharped '"bottom-normal-buttons-right"))
                         #:theme-left-margin-transient (adjustments-tabbar (check-sharped '"bottom-transient-buttons-left"))
                         #:theme-right-margin-transient (adjustments-tabbar (check-sharped '"bottom-transient-buttons-right")))
    (when (eq type 'normal-frame)
      (setq frame (append bottom-frame-title-group bottom-frame-default-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"bottom-normal-buttons-left"))
                          bottom-frame-border-group (table-ref styletab-c-botton-cache (check-sharped '"bottom-normal-buttons-right")))))
    (when (eq type 'transient-frame)
      (setq frame (append bottom-frame-title-group bottom-frame-default-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"bottom-transient-buttons-left"))
                          bottom-frame-border-group (table-ref styletab-c-botton-cache (check-sharped '"bottom-transient-buttons-right")))))
    (when (eq type 'shaped-frame)
      (setq frame (append bottom-frame-title-group bottom-frame-shaped-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"bottom-normal-buttons-left"))
                          (table-ref styletab-c-botton-cache (check-sharped '"bottom-normal-buttons-right")))))
    (when (eq type 'shaped-transient-frame)
      (setq frame (append bottom-frame-title-group bottom-frame-shaped-border-corner-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"bottom-transient-buttons-left"))
                          (table-ref styletab-c-botton-cache (check-sharped '"bottom-transient-buttons-right"))))))
  
  (when (eq current-title 'left)
    (update-title-x-offsets '(11 . -11))
    (set-tab-adjustments #:theme-left-dec-width 11 #:theme-right-dec-width (- styletab-c:title-dimension 2)
                         #:theme-left-margin (adjustments-tabbar (check-sharped '"left-normal-buttons-bottom"))
                         #:theme-right-margin (adjustments-tabbar (check-sharped '"left-normal-buttons-top"))
                         #:theme-left-margin-transient (adjustments-tabbar (check-sharped '"left-transient-buttons-bottom"))
                         #:theme-right-margin-transient (adjustments-tabbar (check-sharped '"left-transient-buttons-top")))
    (when (eq type 'normal-frame)
      (setq frame (append left-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"left-normal-buttons-bottom"))
                          left-frame-default-border-corner-group left-frame-border-group 
                          (table-ref styletab-c-botton-cache (check-sharped '"left-normal-buttons-top")))))
    (when (eq type 'transient-frame)
      (setq frame (append left-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"left-transient-buttons-bottom")) 
                          left-frame-default-border-corner-group 
                          left-frame-border-group (table-ref styletab-c-botton-cache (check-sharped '"left-transient-buttons-top")))))
    (when (eq type 'shaped-frame)
      (setq frame (append left-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"left-normal-buttons-bottom"))
                          left-frame-shaped-border-corner-group (table-ref styletab-c-botton-cache (check-sharped '"left-normal-buttons-top")))))
    (when (eq type 'shaped-transient-frame)
      (setq frame (append left-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"left-transient-buttons-bottom"))
                          left-frame-shaped-border-corner-group (table-ref styletab-c-botton-cache (check-sharped '"left-transient-buttons-top"))))))
  
  (when (eq current-title 'right)
    (update-title-x-offsets '(11 . -11))
    (set-tab-adjustments #:theme-left-dec-width 11 #:theme-right-dec-width (- styletab-c:title-dimension 2)
                         #:theme-left-margin (adjustments-tabbar (check-sharped '"right-normal-buttons-bottom"))
                         #:theme-right-margin (adjustments-tabbar (check-sharped '"right-normal-buttons-top"))
                         #:theme-left-margin-transient (adjustments-tabbar (check-sharped '"right-transient-buttons-bottom"))
                         #:theme-right-margin-transient (adjustments-tabbar (check-sharped '"right-transient-buttons-top")))
    (when (eq type 'normal-frame)
      (setq frame (append right-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"right-normal-buttons-bottom")) 
                          right-frame-default-border-corner-group
                          right-frame-border-group (table-ref styletab-c-botton-cache (check-sharped '"right-normal-buttons-top")))))
    (when (eq type 'transient-frame)
      (setq frame (append right-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"right-transient-buttons-bottom")) 
                          right-frame-default-border-corner-group
                          right-frame-border-group (table-ref styletab-c-botton-cache (check-sharped '"right-transient-buttons-top")))))
    (when (eq type 'shaped-frame)
      (setq frame (append right-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"right-normal-buttons-bottom"))
                          right-frame-shaped-border-corner-group
                          (table-ref styletab-c-botton-cache (check-sharped '"right-normal-buttons-top")))))
    (when (eq type 'shaped-transient-frame)
      (setq frame (append right-frame-title-group (table-ref styletab-c-botton-cache (check-sharped '"right-transient-buttons-bottom")) 
                          right-frame-shaped-border-corner-group
                          (table-ref styletab-c-botton-cache (check-sharped '"right-transient-buttons-top"))))))
  frame)

(define (make-buttons-reframe-with-style)
  (make-buttons) 
  (reframe-windows-with-style theme-name))

(define (reframe-with-style)
  (reframe-windows-with-style theme-name))

(define (reframe-one w)
  (when (not (window-get w 'tabbed))
    (when (eq (window-get w 'current-frame-style) theme-name)
      (reframe-window w))))

(define (reframe-marked w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (reframe-window w)))

;; reframe-with-style, resize bottons
;; reset icon cache
(define (clear-icon-cache-reframe)
  (when recolor-lock
    (setq recolor-lock nil)
    (recolor-all-buttons)
    (setq styletab-c-icon-cache (make-weak-table eq-hash eq))
    (make-buttons) 
    (reframe-with-style)
    (setq recolor-lock t)))

(define (color-changed)
  (when recolor-lock
    (setq recolor-lock nil)
    (recolor-all)
    (reframe-with-style)
    (setq recolor-lock t)))

(define (bright-changed)
  (when recolor-lock
    (setq recolor-lock nil)
    (recolor-tab)
    (recolor-all-buttons)
    (reframe-with-style)
    (setq recolor-lock t)))

(define (botton-color-changed botton)
  (when recolor-lock
    (setq recolor-lock nil)
    (botton)
    (reframe-with-style)
    (setq recolor-lock t)))

(define (reload-frame-style-reframe)
  (reload-frame-style theme-name)
  (reframe-with-style))

(define (frame-style-name w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (set-tab-theme-name #:frame-style-supported-tabs theme-name)))

(define (get-frame w type)
  (let ((current-title 
         (if (not (window-get w 'title-position))
             (case styletab-c:titlebar-place
                   ((top)    'top)
                   ((bottom) 'bottom)
                   ((left)   'left)
                   ((right)  'right))
           (window-get w 'title-position))))
    (case type
          ((default)
           (make-frame w 'normal-frame current-title))
          ((utility)
           (make-frame w 'normal-frame current-title))
          ((shaded-utility)
           (make-frame w 'normal-frame current-title))
          ((transient)
           (make-frame w 'transient-frame current-title))
          ((shaped)
           (make-frame w 'shaped-frame current-title))
          ((shaped-transient)
           (make-frame w 'shaped-transient-frame current-title)))))

;; initialize theme
(color-changed)
(make-buttons)

(add-frame-style theme-name get-frame)

(call-after-state-changed '(tab-theme-name) frame-style-name)
(call-after-state-changed '(marked) reframe-marked)
(call-after-state-changed '(maximized sticky fixed-position stacking) reframe-one)
(add-hook 'remove-from-workspace-hook reframe-one)

(custom-set-property 'styletab-c:styles ':after-set reload-frame-style-reframe)
(custom-set-property 'styletab-c:title-dimension ':after-set clear-icon-cache-reframe)
(custom-set-property 'styletab-c:custom-button-width ':after-set clear-icon-cache-reframe)
(custom-set-property 'styletab-c:button-width ':after-set clear-icon-cache-reframe)
(custom-set-property 'styletab-c:borders-dimension ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:titlebar-place ':after-set reframe-with-style)
(custom-set-property 'styletab-c:top-left-buttons ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:top-right-buttons ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:bottom-left-buttons ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:bottom-right-buttons ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:left-top-buttons ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:left-bottom-buttons ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:right-top-buttons ':after-set make-buttons-reframe-with-style)
(custom-set-property 'styletab-c:right-bottom-buttons ':after-set make-buttons-reframe-with-style)
