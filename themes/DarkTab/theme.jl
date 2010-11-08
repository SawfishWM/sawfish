;; theme file

(define theme-name 'DarkTab)

;;need hash tables for icon cache 
;;
(require 'rep.data.tables)

(defgroup DarkTab:group "DarkTab"
  :group appearance)

(defgroup DarkTab:settings-group "Settings"
  :group (appearance DarkTab:group))

(defgroup DarkTab:buttons-group "buttons"
  :group (appearance DarkTab:group))

(defgroup DarkTab:top-buttons-group "Top Titlebar Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group))

(defgroup DarkTab:bottom-buttons-group "Bottom Titlebar Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group))

(defgroup DarkTab:left-buttons-group "Left Titlebar Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group))

(defgroup DarkTab:right-buttons-group "Right Titlebar Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group))

(defgroup DarkTab:top-left-buttons-group "Top Titlebar Left Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:top-buttons-group))

(defgroup DarkTab:top-right-buttons-group "Top Titlebar Right Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:top-buttons-group))

(defgroup DarkTab:bottom-left-buttons-group "Bottom Titlebar Left Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:bottom-buttons-group))

(defgroup DarkTab:bottom-right-buttons-group "Bottom Titlebar Right Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:bottom-buttons-group))

(defgroup DarkTab:left-top-buttons-group "Left Titlebar Top Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:left-buttons-group))

(defgroup DarkTab:left-bottom-buttons-group "Left Titlebar Bottom Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:left-buttons-group))

(defgroup DarkTab:right-top-buttons-group "Right Titlebar Top Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:right-buttons-group))

(defgroup DarkTab:right-bottom-buttons-group "Right Titlebar Bottom Buttons"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:right-buttons-group))

(defcustom darktab:titlebar-place 'top "Titlebar default place."
  :group (appearance DarkTab:group DarkTab:settings-group)
  :options (top bottom left right)
  :type symbol)

(defcustom darktab:title-dimension 24 "Hight of title border. Default 24"
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type number
  :range (16 . 32))

(defcustom darktab:borders-dimension 4 "Width of window border. Default 4"
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type number
  :range (0 . 10))

(defcustom darktab:focused-color "#FEFEFE"
  "Focused title text color."
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type color)

(defcustom darktab:highlighted-color "#E5E5E5"
  "Highlighted title text color."
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type color)

(defcustom darktab:clicked-color "#CBCBCB"
  "Clicked title text color."
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type color)

(defcustom darktab:inactive-color "#B2B2B2"
  "Inactive title text color."
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type color)

(defcustom darktab:inactive-highlighted-color "#989898"
  "Inactive Highlighted title text color."
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type color)

(defcustom darktab:inactive-clicked "#808080"
  "Inactive Clicked title text color."
  :group (appearance DarkTab:group DarkTab:settings-group)
  :type color)

(defcustom darktab:top-left-buttons `((menu ,t) (shade ,t) (sticky ,nil))
  "Top Titlebar Left Buttons (from left to right)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:top-buttons-group DarkTab:top-left-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))

(defcustom darktab:top-right-buttons `((close ,t) (maximize ,nil) (minimize ,nil))
  "Top Titlebar Right Buttons (from right to left)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:top-buttons-group DarkTab:top-right-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))

(defcustom darktab:bottom-left-buttons `((menu ,t) (shade ,t) (sticky ,nil))
  "Bottom Titlebar Left Buttons (from left to right)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:bottom-buttons-group DarkTab:bottom-left-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))

(defcustom darktab:bottom-right-buttons `((close ,t) (maximize ,nil) (minimize ,nil))
  "Bottom Titlebar Right Buttons (from right to left)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:bottom-buttons-group DarkTab:bottom-right-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))

(defcustom darktab:left-top-buttons `((menu ,t) (shade ,t) (sticky ,nil))
  "Left Titlebar Top Buttons (from top to bottom)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:left-buttons-group DarkTab:left-top-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))

(defcustom darktab:left-bottom-buttons `((close ,t) (maximize ,nil) (minimize ,nil))
  "Left Titlebar Bottom Buttons (from bottom to top)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:left-buttons-group DarkTab:left-bottom-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))

(defcustom darktab:right-top-buttons `((menu ,t) (shade ,t) (sticky ,nil))
  "Right Titlebar Top Buttons (from top to bottom)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:right-buttons-group DarkTab:right-top-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))

(defcustom darktab:right-bottom-buttons `((close ,t) (maximize ,nil) (minimize ,nil))
  "Right Titlebar Bottom Buttons (from bottom to top)"
  :group (appearance DarkTab:group DarkTab:buttons-group DarkTab:right-buttons-group DarkTab:right-bottom-buttons-group)
  :type (v-and (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))
               (v-and (choice \(none\) close menu maximize minimize shade sticky space send-to-prev 
                              send-to-next lock raise-lower move-resize rename frame-typ)
                      (boolean "Show in transients"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frame-class, keys bindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-frame-class tabbar-horizontal-left-edge ()
  (bind-keys
   tabbar-horizontal-left-edge-keymap "Button1-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (unshade-window (current-event-window))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim (window-dimensions (current-event-window)))
       (setq fdim (window-frame-dimensions (current-event-window)))
       (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ (car dim) (cdr dim) darktab:title-dimension)) 3))
       (if (eq current-title 'right)
           (setq framehigh 0)
         (if (or (eq (window-get (current-event-window)
                                 'type) 'default)
                 (eq (window-get (current-event-window)
                                 'type) 'transient))
             (setq framehigh (+ (- darktab:title-dimension (* darktab:borders-dimension 2)) framew))
           (setq framehigh (+ darktab:title-dimension framew))))
       (setq dim-x (- (car (window-dimensions (current-event-window)))
                      framehigh))
       (setq dim-y (+ (cdr (window-dimensions (current-event-window)))
                      framehigh))
       (if (<= pos-x 0) (setq pos-x 0))
       (map-window-group 
        (lambda (x)
          (window-put x 'title-position 'left)) (current-event-window))
       (call-window-hook 'window-state-change-hook (current-event-window) (list '(title-position)))
       (move-window-to (current-event-window) pos-x pos-y)
       (resize-window-to (current-event-window) dim-x dim-y))))
  (bind-keys
   tabbar-horizontal-left-edge-keymap "Button2-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (unshade-window (current-event-window))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim-x (car (window-dimensions (current-event-window))))
       (setq dim-y (cdr (window-dimensions (current-event-window))))
       (setq fdim (window-frame-dimensions (current-event-window)))
       (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ dim-x dim-y darktab:title-dimension)) 3))

       (setq type (window-get (current-event-window) 'type))

       (if (>= (+ pos-y dim-y darktab:title-dimension framew) (screen-height))
           (setq pos-y (- (screen-height) dim-y darktab:title-dimension framew)))
       (if (<= pos-y 0) (setq pos-y 0))
       (if (eq current-title 'top)
           (map-window-group
            (lambda (x)
              (window-put x 'title-position 'bottom)) (current-event-window))
         (map-window-group
          (lambda (x)
            (window-put x 'title-position 'top)) (current-event-window)))
       (call-window-hook 'window-state-change-hook (current-event-window) (list '(title-position)))
       (move-window-to (current-event-window) pos-x pos-y)
       (resize-window-to (current-event-window) dim-x dim-y))))
  (bind-keys
   tabbar-horizontal-left-edge-keymap "Button3-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (unshade-window (current-event-window))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim (window-dimensions (current-event-window)))
       (setq fdim (window-frame-dimensions (current-event-window)))
       (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ (car dim) (cdr dim) darktab:title-dimension)) 3))
       (if (eq current-title 'left)
           (setq framehigh 0)
         (if (or (eq (window-get (current-event-window)
                                 'type) 'default)
                 (eq (window-get (current-event-window)
                                 'type) 'transient))
             (setq framehigh (+ (- darktab:title-dimension (* darktab:borders-dimension 2)) framew))
           (setq framehigh (+ darktab:title-dimension framew))))
       (setq dim-x (- (car (window-dimensions (current-event-window)))
                      framehigh))
       (setq dim-y (+ (cdr (window-dimensions (current-event-window)))
                      framehigh))
       (if (>= (+ pos-x dim-x framehigh framew framew) (screen-width))
           (setq pos-x (- (screen-width) dim-x darktab:title-dimension framew)))
       (map-window-group
        (lambda (x)
          (window-put x 'title-position 'right)) (current-event-window))
       (call-window-hook 'window-state-change-hook (current-event-window) (list '(title-position)))
       (move-window-to (current-event-window) pos-x pos-y)
       (resize-window-to (current-event-window) dim-x dim-y)))))

(def-frame-class tabbar-vertical-top-edge ()
  (bind-keys
   tabbar-vertical-top-edge-keymap "Button1-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (unshade-window (current-event-window))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim-x (car (window-dimensions (current-event-window))))
       (setq dim-y (cdr (window-dimensions (current-event-window))))
       (setq dim (window-dimensions (current-event-window)))
       (setq fdim (window-frame-dimensions (current-event-window)))
       (when (not (eq current-title 'left))
         (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ (car dim) (cdr dim) darktab:title-dimension)) 3))
         (if (eq current-title 'right)
             (setq framehigh 0)
           (setq framehigh (+ (- darktab:title-dimension (* darktab:borders-dimension 2)) framew)))
         (setq dim-x (- (car (window-dimensions (current-event-window)))
                        framehigh))
         (setq dim-y (+ (cdr (window-dimensions (current-event-window)))
                        framehigh))
         (if (<= pos-x 0) (setq pos-x 0))
         (map-window-group
          (lambda (x)
            (window-put x 'title-position 'left)) (current-event-window)))
       (when (eq current-title 'left)
         (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ dim-x dim-y darktab:title-dimension)) 3))
         (if (or (eq (window-get (current-event-window)
                                 'type) 'default)
                 (eq (window-get (current-event-window)
                                 'type) 'transient))
             (setq framehigh (- darktab:title-dimension (* darktab:borders-dimension 2)))
           (setq framehigh darktab:title-dimension))
         (setq dim-x (+ dim-x framehigh framew))
         (setq dim-y (- dim-y framehigh framew))
         (if (>= (+ pos-y dim-y darktab:title-dimension framew) (screen-height))
             (setq pos-y (- (screen-height) dim-y darktab:title-dimension framew)))
         (if (<= pos-y 0) (setq pos-y 0))
         (map-window-group
          (lambda (x)
            (window-put x 'title-position 'bottom)) (current-event-window)))
       (call-window-hook 'window-state-change-hook (current-event-window) (list '(title-position)))
       (move-window-to (current-event-window) pos-x pos-y)
       (resize-window-to (current-event-window) dim-x dim-y))))
  (bind-keys
   tabbar-vertical-top-edge-keymap "Button2-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (unshade-window (current-event-window))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim-x (car (window-dimensions (current-event-window))))
       (setq dim-y (cdr (window-dimensions (current-event-window))))
       (setq fdim (window-frame-dimensions (current-event-window)))
       (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ dim-x dim-y darktab:title-dimension)) 3))
       (if (or (eq (window-get (current-event-window)
                               'type) 'default)
               (eq (window-get (current-event-window)
                               'type) 'transient))
           (setq framehigh (- darktab:title-dimension (* darktab:borders-dimension 2)))
         (setq framehigh darktab:title-dimension))
       (setq dim-x (+ dim-x framehigh framew))
       (setq dim-y (- dim-y framehigh framew))
       (if (>= (+ pos-y dim-y darktab:title-dimension framew) (screen-height))
           (setq pos-y (- (screen-height) dim-y darktab:title-dimension framew)))
       (if (<= pos-y 0) (setq pos-y 0))
       (map-window-group
        (lambda (x)
          (window-put x 'title-position 'top)) (current-event-window))
       (call-window-hook 'window-state-change-hook (current-event-window) (list '(title-position)))
       (move-window-to (current-event-window) pos-x pos-y)
       (resize-window-to (current-event-window) dim-x dim-y))))
  (bind-keys
   tabbar-vertical-top-edge-keymap "Button3-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (unshade-window (current-event-window))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim-x (car (window-dimensions (current-event-window))))
       (setq dim-y (cdr (window-dimensions (current-event-window))))
       (setq dim (window-dimensions (current-event-window)))
       (setq fdim (window-frame-dimensions (current-event-window)))
       (when (not (eq current-title 'right))
         (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ (car dim) (cdr dim) darktab:title-dimension)) 3))
         (if (eq current-title 'left)
             (setq framehigh 0)
           (setq framehigh (+ (- darktab:title-dimension (* darktab:borders-dimension 2)) framew)))
         (setq dim-x (- (car (window-dimensions (current-event-window)))
                        framehigh))
         (setq dim-y (+ (cdr (window-dimensions (current-event-window)))
                        framehigh))
         (if (>= (+ pos-x dim-x) (screen-width))
             (setq pos-x (- (screen-width) dim-x darktab:title-dimension framew)))
         (map-window-group
          (lambda (x)
            (window-put x 'title-position 'right)) (current-event-window)))
       (when (eq current-title 'right)
         (setq framew (/ (- (+ (car fdim) (cdr fdim)) (+ dim-x dim-y darktab:title-dimension)) 3))
         (if (or (eq (window-get (current-event-window)
                                 'type) 'default)
                 (eq (window-get (current-event-window)
                                 'type) 'transient))
             (setq framehigh (- darktab:title-dimension (* darktab:borders-dimension 2)))
           (setq framehigh darktab:title-dimension))
         (setq dim-x (+ dim-x framehigh framew))
         (setq dim-y (- dim-y framehigh framew))
         (if (>= (+ pos-y dim-y darktab:title-dimension framew) (screen-height))
             (setq pos-y (- (screen-height) dim-y darktab:title-dimension framew)))
         (if (<= pos-y 0) (setq pos-y 0))
         (map-window-group
          (lambda (x)
            (window-put x 'title-position 'bottom)) (current-event-window)))
       (call-window-hook 'window-state-change-hook (current-event-window) (list '(title-position)))
       (move-window-to (current-event-window) pos-x pos-y)
       (resize-window-to (current-event-window) dim-x dim-y)))))

(defvar prev-button-keymap
  (bind-keys (make-keymap)
			 "Button3-Off" 'send-group-to-next-workspace
             "Button2-Click" 'popup-workspace-list
             "Button1-Off" 'send-group-to-previous-workspace))

(defvar next-button-keymap
  (bind-keys (make-keymap)
             "Button3-Off" 'send-group-to-previous-workspace
             "Button2-Click" 'popup-workspace-list
             "Button1-Off" 'send-group-to-next-workspace))

(def-frame-class frame-typ-button ()
  (bind-keys
   frame-typ-button-keymap "Button1-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim-x (car (window-dimensions (current-event-window))))
       (setq dim-y (cdr (window-dimensions (current-event-window))))
       (setq type (window-get (current-event-window) 'type))
       (if (eq type 'default)
           (setq typ 'transient)
         (setq typ 'default))
       (when (or (eq type 'shaped)
                 (eq type 'utility))
         (setq typ 'default)
         (setq dim-x (- dim-x (* darktab:borders-dimension 2)))         
         (setq dim-y (- dim-y darktab:borders-dimension))
         (when
             (not (or (eq current-title 'top)
                      (eq current-title 'bottom)))
           (setq dim-x (+ dim-x darktab:borders-dimension))
           (setq dim-y (- dim-y darktab:borders-dimension))))
       (when (eq type 'shaped-transient)
         (setq typ 'transient)
         (setq dim-x (- dim-x (* darktab:borders-dimension 2)))
         (setq dim-y (- dim-y darktab:borders-dimension))
         (when
             (not (or (eq current-title 'top)
                      (eq current-title 'bottom)))
           (setq dim-x (+ dim-x darktab:borders-dimension))
           (setq dim-y (- dim-y darktab:borders-dimension))))
       (set-window-type (current-event-window) typ)
       (move-resize-window-to (current-event-window) pos-x pos-y dim-x dim-y))))

  (bind-keys
   frame-typ-button-keymap "Button2-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim-x (car (window-dimensions (current-event-window))))
       (setq dim-y (cdr (window-dimensions (current-event-window))))
       (setq type (window-get (current-event-window) 'type))
       (when (or (eq type 'default)
                 (eq type 'transient))
         (setq typ 'shaped)
         (when (or (eq current-title 'top)
                   (eq current-title 'bottom))
           (setq dim-x (+ dim-x (* darktab:borders-dimension 2)))
           (setq dim-y (+ dim-y darktab:borders-dimension darktab:title-dimension)))
         (when (not (eq current-title 'top)
                    (eq current-title 'bottom))
           (setq dim-y (+ dim-y (* darktab:borders-dimension 2)))
           (setq dim-x (+ dim-x darktab:borders-dimension darktab:title-dimension))))
       (when (or (eq type 'shaped)
                 (eq type 'shaped-transient)
                 (eq type 'utility))
         (setq typ 'shaped)
         (if (or (eq current-title 'top)
                 (eq current-title 'bottom))
             (setq dim-y (+ dim-y darktab:title-dimension))
           (setq dim-x (+ dim-x darktab:title-dimension))))
       (set-window-type (current-event-window) 'unframed)
       (move-resize-window-to (current-event-window) pos-x pos-y dim-x dim-y))))
  
  (bind-keys
   frame-typ-button-keymap "Button3-Off"
   '(call-command
     (lambda ()
       (if (eq (window-get (current-event-window) 'title-position) nil)
           (case darktab:titlebar-place
                 ((top) (setq current-title 'top))
                 ((bottom) (setq current-title 'bottom))
                 ((left) (setq current-title 'left))
                 ((right) (setq current-title 'right)))
         (setq current-title (window-get (current-event-window) 'title-position)))
       (setq pos-x (car (window-position (current-event-window))))
       (setq pos-y (cdr (window-position (current-event-window))))
       (setq dim-x (car (window-dimensions (current-event-window))))
       (setq dim-y (cdr (window-dimensions (current-event-window))))
       (setq type (window-get (current-event-window) 'type))
       (if (or (eq type 'shaped)
               (eq type 'utility))
           (setq typ 'shaped-transient)
         (setq typ 'shaped))
       (when (eq type 'default)
         (setq typ 'shaped)
         (setq dim-x (+ dim-x (* darktab:borders-dimension 2)))
         (setq dim-y (+ dim-y darktab:borders-dimension))
         (when
             (not (or (eq current-title 'top)
                      (eq current-title 'bottom)))
           (setq dim-x (- dim-x darktab:borders-dimension))
           (setq dim-y (+ dim-y darktab:borders-dimension))))
       (when (eq type 'transient)
         (setq typ 'shaped-transient)
         (setq dim-x (+ dim-x (* darktab:borders-dimension 2)))
         (setq dim-y (+ dim-y darktab:borders-dimension))
         (when
             (not (or (eq current-title 'top)
                      (eq current-title 'bottom)))
           (setq dim-x (- dim-x darktab:borders-dimension))
           (setq dim-y (+ dim-y darktab:borders-dimension))))
       (set-window-type (current-event-window) typ)
       (move-resize-window-to (current-event-window) pos-x pos-y dim-x dim-y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; top images ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define top-frame-top-border-images 
  `((focused . ,(make-image "top-frame-top-border-f.png"))
	(inactive . ,(make-image "top-frame-top-border-i.png"))))

(define top-frame-top-left-corner-images 
  `((focused . ,(make-image "top-frame-top-left-corner-f.png"))
	(inactive . ,(make-image "top-frame-top-left-corner-i.png"))))

(define top-frame-top-left-corner-shaped-images 
  `((focused . ,(make-image "top-frame-top-left-corner-shaped-f.png"))
	(inactive . ,(make-image "top-frame-top-left-corner-shaped-i.png"))))

(define top-frame-tab-left-images 
  `((focused . ,(make-image "top-frame-tab-left-f.png"))
	(inactive . ,(make-image "top-frame-tab-left-i.png"))))

(define top-frame-tab-left-icon-images
  `((focused . ,(make-image "top-frame-tab-left-icon-f.png"))
	(inactive . ,(make-image "top-frame-tab-left-icon-i.png"))))

(define top-frame-tab-images 
  `((focused . ,(make-image "top-frame-tab-f.png"))
	(inactive . ,(make-image "top-frame-tab-i.png"))))

(define top-frame-tab-right-images 
  `((focused . ,(make-image "top-frame-tab-right-f.png"))
	(inactive . ,(make-image "top-frame-tab-right-i.png"))))

(define top-frame-title-images 
  `((focused . ,(make-image "top-frame-title-f.png"))
	(inactive . ,(make-image "top-frame-title-i.png"))))

(define top-frame-icon-title-images 
  (make-image "top-frame-icon-title-images-f.png"))

(define top-frame-top-right-corner-images 
  `((focused . ,(make-image "top-frame-top-right-corner-f.png"))
	(inactive . ,(make-image "top-frame-top-right-corner-i.png"))))

(define top-frame-top-right-corner-shaped-images 
  `((focused . ,(make-image "top-frame-top-right-corner-shaped-f.png"))
	(inactive . ,(make-image "top-frame-top-right-corner-shaped-i.png"))))

(define top-frame-menu-button-images 
  `((focused . ,(make-image "top-frame-menu-button-f.png"))
	(highlighted . ,(make-image "top-frame-menu-button-h.png"))
	(clicked . ,(make-image "top-frame-menu-button-c.png"))
	(inactive . ,(make-image "top-frame-menu-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-menu-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-menu-button-ic.png"))))

(define top-frame-shade-button-images 
  `((focused . ,(make-image "top-frame-shade-button-f.png"))
	(highlighted . ,(make-image "top-frame-shade-button-h.png"))
	(clicked . ,(make-image "top-frame-shade-button-c.png"))
	(inactive . ,(make-image "top-frame-shade-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-shade-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-shade-button-ic.png"))))

(define top-frame-unshade-button-images 
  `((focused . ,(make-image "top-frame-unshade-button-f.png"))
	(highlighted . ,(make-image "top-frame-unshade-button-h.png"))
	(clicked . ,(make-image "top-frame-unshade-button-c.png"))
	(inactive . ,(make-image "top-frame-unshade-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-unshade-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-unshade-button-ic.png"))))

(define top-frame-sticky-button-images 
  `((focused . ,(make-image "top-frame-sticky-button-f.png"))
	(highlighted . ,(make-image "top-frame-sticky-button-h.png"))
	(clicked . ,(make-image "top-frame-sticky-button-c.png"))
	(inactive . ,(make-image "top-frame-sticky-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-sticky-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-sticky-button-ic.png"))))

(define top-frame-unsticky-button-images 
  `((focused . ,(make-image "top-frame-unsticky-button-f.png"))
	(highlighted . ,(make-image "top-frame-unsticky-button-h.png"))
	(clicked . ,(make-image "top-frame-unsticky-button-c.png"))
	(inactive . ,(make-image "top-frame-unsticky-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-unsticky-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-unsticky-button-ic.png"))))

(define top-frame-iconify-button-images 
  `((focused . ,(make-image "top-frame-iconify-button-f.png"))
	(highlighted . ,(make-image "top-frame-iconify-button-h.png"))
	(clicked . ,(make-image "top-frame-iconify-button-c.png"))
	(inactive . ,(make-image "top-frame-iconify-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-iconify-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-iconify-button-ic.png"))))

(define top-frame-maximize-button-images 
  `((focused . ,(make-image "top-frame-maximize-button-f.png"))
	(highlighted . ,(make-image "top-frame-maximize-button-h.png"))
	(clicked . ,(make-image "top-frame-maximize-button-c.png"))
	(inactive . ,(make-image "top-frame-maximize-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-maximize-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-maximize-button-ic.png"))))

(define top-frame-unmaximize-button-images 
  `((focused . ,(make-image "top-frame-unmaximize-button-f.png"))
	(highlighted . ,(make-image "top-frame-unmaximize-button-h.png"))
	(clicked . ,(make-image "top-frame-unmaximize-button-c.png"))
	(inactive . ,(make-image "top-frame-unmaximize-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-unmaximize-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-unmaximize-button-ic.png"))))

(define top-frame-close-button-images 
  `((focused . ,(make-image "top-frame-close-button-f.png"))
	(highlighted . ,(make-image "top-frame-close-button-h.png"))
	(clicked . ,(make-image "top-frame-close-button-c.png"))
	(inactive . ,(make-image "top-frame-close-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-close-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-close-button-ic.png"))))

(define top-frame-lock-button-images 
  `((focused . ,(make-image "top-frame-lock-button-f.png"))
	(highlighted . ,(make-image "top-frame-lock-button-h.png"))
	(clicked . ,(make-image "top-frame-lock-button-c.png"))
	(inactive . ,(make-image "top-frame-lock-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-lock-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-lock-button-ic.png"))))

(define top-frame-unlock-button-images 
  `((focused . ,(make-image "top-frame-unlock-button-f.png"))
	(highlighted . ,(make-image "top-frame-unlock-button-h.png"))
	(clicked . ,(make-image "top-frame-unlock-button-c.png"))
	(inactive . ,(make-image "top-frame-unlock-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-unlock-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-unlock-button-ic.png"))))

(define top-frame-prev-button-images 
  `((focused . ,(make-image "top-frame-prev-button-f.png"))
	(highlighted . ,(make-image "top-frame-prev-button-h.png"))
	(clicked . ,(make-image "top-frame-prev-button-c.png"))
	(inactive . ,(make-image "top-frame-prev-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-prev-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-prev-button-ic.png"))))

(define top-frame-prev-last-button-images 
  `((focused . ,(make-image "top-frame-prev-last-button-f.png"))
	(highlighted . ,(make-image "top-frame-prev-last-button-h.png"))
	(clicked . ,(make-image "top-frame-prev-last-button-c.png"))
	(inactive . ,(make-image "top-frame-prev-last-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-prev-last-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-prev-last-button-ic.png"))))

(define top-frame-next-button-images 
  `((focused . ,(make-image "top-frame-next-button-f.png"))
	(highlighted . ,(make-image "top-frame-next-button-h.png"))
	(clicked . ,(make-image "top-frame-next-button-c.png"))
	(inactive . ,(make-image "top-frame-next-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-next-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-next-button-ic.png"))))

(define top-frame-next-last-button-images 
  `((focused . ,(make-image "top-frame-next-last-button-f.png"))
	(highlighted . ,(make-image "top-frame-next-last-button-h.png"))
	(clicked . ,(make-image "top-frame-next-last-button-c.png"))
	(inactive . ,(make-image "top-frame-next-last-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-next-last-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-next-last-button-ic.png"))))

(define top-frame-raise-lower-button-images 
  `((focused . ,(make-image "top-frame-raise-lower-button-f.png"))
	(highlighted . ,(make-image "top-frame-raise-lower-button-h.png"))
	(clicked . ,(make-image "top-frame-raise-lower-button-c.png"))
	(inactive . ,(make-image "top-frame-raise-lower-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-raise-lower-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-raise-lower-button-ic.png"))))

(define top-frame-ontop-button-images 
  `((focused . ,(make-image "top-frame-ontop-button-f.png"))
	(highlighted . ,(make-image "top-frame-ontop-button-h.png"))
	(clicked . ,(make-image "top-frame-ontop-button-c.png"))
	(inactive . ,(make-image "top-frame-ontop-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-ontop-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-ontop-button-ic.png"))))

(define top-frame-unontop-button-images 
  `((focused . ,(make-image "top-frame-unontop-button-f.png"))
	(highlighted . ,(make-image "top-frame-unontop-button-h.png"))
	(clicked . ,(make-image "top-frame-unontop-button-c.png"))
	(inactive . ,(make-image "top-frame-unontop-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-unontop-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-unontop-button-ic.png"))))

(define top-frame-move-resize-button-images 
  `((focused . ,(make-image "top-frame-move-resize-button-f.png"))
	(highlighted . ,(make-image "top-frame-move-resize-button-h.png"))
	(clicked . ,(make-image "top-frame-move-resize-button-c.png"))
	(inactive . ,(make-image "top-frame-move-resize-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-move-resize-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-move-resize-button-ic.png"))))

(define top-frame-rename-button-images 
  `((focused . ,(make-image "top-frame-rename-button-f.png"))
	(highlighted . ,(make-image "top-frame-rename-button-h.png"))
	(clicked . ,(make-image "top-frame-rename-button-c.png"))
	(inactive . ,(make-image "top-frame-rename-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-rename-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-rename-button-ic.png"))))

(define top-frame-frame-typ-button-images 
  `((focused . ,(make-image "top-frame-frame-typ-button-f.png"))
	(highlighted . ,(make-image "top-frame-frame-typ-button-h.png"))
	(clicked . ,(make-image "top-frame-frame-typ-button-c.png"))
	(inactive . ,(make-image "top-frame-frame-typ-button-i.png"))
	(inactive-highlighted . ,(make-image "top-frame-frame-typ-button-ih.png"))
	(inactive-clicked . ,(make-image "top-frame-frame-typ-button-ic.png"))))

(define top-frame-left-border-images 
  `((focused . ,(make-image "top-frame-left-border-f.png"))
	(inactive . ,(make-image "top-frame-left-border-i.png"))))

(define top-frame-bottom-left-corner-images 
  `((focused . ,(make-image "top-frame-bottom-left-corner-f.png"))
	(inactive . ,(make-image "top-frame-bottom-left-corner-i.png"))))

(define top-frame-bottom-border-images 
  `((focused . ,(make-image "top-frame-bottom-border-f.png"))
	(inactive . ,(make-image "top-frame-bottom-border-i.png"))))

(define top-frame-right-border-images 
  `((focused . ,(make-image "top-frame-right-border-f.png"))
	(inactive . ,(make-image "top-frame-right-border-i.png"))))

(define top-frame-bottom-right-corner-images 
  `((focused . ,(make-image "top-frame-bottom-right-corner-f.png"))
	(inactive . ,(make-image "top-frame-bottom-right-corner-i.png"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bottom images ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bottom-frame-top-border-images
  `((focused . ,(make-image "bottom-frame-top-border-f.png"))
    (inactive . ,(make-image "bottom-frame-top-border-i.png"))))

(define bottom-frame-top-left-corner-images
  `((focused . ,(make-image "bottom-frame-top-left-corner-f.png"))
    (inactive . ,(make-image "bottom-frame-top-left-corner-i.png"))))

(define bottom-frame-top-right-corner-images
  `((focused . ,(make-image "bottom-frame-top-right-corner-f.png"))
    (inactive . ,(make-image "bottom-frame-top-right-corner-i.png"))))

(define bottom-frame-left-border-images
  `((focused . ,(make-image "bottom-frame-left-border-f.png"))
    (inactive . ,(make-image "bottom-frame-left-border-i.png"))))

(define bottom-frame-right-border-images
  `((focused . ,(make-image "bottom-frame-right-border-f.png"))
    (inactive . ,(make-image "bottom-frame-right-border-i.png"))))

(define bottom-frame-bottom-border-cursor-images
  `((focused . ,(make-image "bottom-frame-bottom-border-cursor-f.png"))
    (inactive . ,(make-image "bottom-frame-bottom-border-cursor-f.png"))))

(define bottom-frame-bottom-left-corner-images
  `((focused . ,(make-image "bottom-frame-bottom-left-corner-f.png"))
    (inactive . ,(make-image "bottom-frame-bottom-left-corner-i.png"))))

(define bottom-frame-bottom-right-corner-images
  `((focused . ,(make-image "bottom-frame-bottom-right-corner-f.png"))
    (inactive . ,(make-image "bottom-frame-bottom-right-corner-i.png"))))

(define bottom-frame-title-images
  `((focused . ,(make-image "bottom-frame-title-f.png"))
    (inactive . ,(make-image "bottom-frame-title-i.png"))))

(define bottom-frame-bottom-border-images
  `((focused . ,(make-image "bottom-frame-bottom-border-f.png"))
    (inactive . ,(make-image "bottom-frame-bottom-border-i.png"))))

(define bottom-frame-tab-images
  `((focused . ,(make-image "bottom-frame-tab-f.png"))
    (inactive . ,(make-image "bottom-frame-tab-i.png"))))

(define bottom-frame-tab-left-icon-images
  `((focused . ,(make-image "bottom-frame-tab-left-icon-f.png"))
    (inactive . ,(make-image "bottom-frame-tab-left-icon-i.png"))))

(define bottom-frame-tab-right-images
  `((focused . ,(make-image "bottom-frame-tab-right-f.png"))
    (inactive . ,(make-image "bottom-frame-tab-right-i.png"))))

(define bottom-frame-menu-button-images
  `((focused . ,(make-image "bottom-frame-menu-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-menu-button-h.png"))
    (clicked . ,(make-image "bottom-frame-menu-button-c.png"))
    (inactive . ,(make-image "bottom-frame-menu-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-menu-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-menu-button-ic.png"))))

(define bottom-frame-shade-button-images
  `((focused . ,(make-image "bottom-frame-shade-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-shade-button-h.png"))
    (clicked . ,(make-image "bottom-frame-shade-button-c.png"))
    (inactive . ,(make-image "bottom-frame-shade-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-shade-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-shade-button-ic.png"))))

(define bottom-frame-unshade-button-images
  `((focused . ,(make-image "bottom-frame-unshade-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-unshade-button-h.png"))
    (clicked . ,(make-image "bottom-frame-unshade-button-c.png"))
    (inactive . ,(make-image "bottom-frame-unshade-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-unshade-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-unshade-button-ic.png"))))

(define bottom-frame-sticky-button-images
  `((focused . ,(make-image "bottom-frame-sticky-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-sticky-button-h.png"))
    (clicked . ,(make-image "bottom-frame-sticky-button-c.png"))
    (inactive . ,(make-image "bottom-frame-sticky-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-sticky-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-sticky-button-ic.png"))))

(define bottom-frame-unsticky-button-images
  `((focused . ,(make-image "bottom-frame-unsticky-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-unsticky-button-h.png"))
    (clicked . ,(make-image "bottom-frame-unsticky-button-c.png"))
    (inactive . ,(make-image "bottom-frame-unsticky-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-unsticky-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-unsticky-button-ic.png"))))

(define bottom-frame-iconify-button-images
  `((focused . ,(make-image "bottom-frame-iconify-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-iconify-button-h.png"))
    (clicked . ,(make-image "bottom-frame-iconify-button-c.png"))
    (inactive . ,(make-image "bottom-frame-iconify-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-iconify-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-iconify-button-ic.png"))))

(define bottom-frame-maximize-button-images
  `((focused . ,(make-image "bottom-frame-maximize-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-maximize-button-h.png"))
    (clicked . ,(make-image "bottom-frame-maximize-button-c.png"))
    (inactive . ,(make-image "bottom-frame-maximize-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-maximize-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-maximize-button-ic.png"))))

(define bottom-frame-unmaximize-button-images
  `((focused . ,(make-image "bottom-frame-unmaximize-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-unmaximize-button-h.png"))
    (clicked . ,(make-image "bottom-frame-unmaximize-button-c.png"))
    (inactive . ,(make-image "bottom-frame-unmaximize-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-unmaximize-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-unmaximize-button-ic.png"))))

(define bottom-frame-close-button-images
  `((focused . ,(make-image "bottom-frame-close-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-close-button-h.png"))
    (clicked . ,(make-image "bottom-frame-close-button-c.png"))
    (inactive . ,(make-image "bottom-frame-close-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-close-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-close-button-ic.png"))))

(define bottom-frame-lock-button-images
  `((focused . ,(make-image "bottom-frame-lock-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-lock-button-h.png"))
    (clicked . ,(make-image "bottom-frame-lock-button-c.png"))
    (inactive . ,(make-image "bottom-frame-lock-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-lock-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-lock-button-ic.png"))))

(define bottom-frame-unlock-button-images
  `((focused . ,(make-image "bottom-frame-unlock-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-unlock-button-h.png"))
    (clicked . ,(make-image "bottom-frame-unlock-button-c.png"))
    (inactive . ,(make-image "bottom-frame-unlock-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-unlock-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-unlock-button-ic.png"))))

(define bottom-frame-prev-button-images
  `((focused . ,(make-image "bottom-frame-prev-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-prev-button-h.png"))
    (clicked . ,(make-image "bottom-frame-prev-button-c.png"))
    (inactive . ,(make-image "bottom-frame-prev-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-prev-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-prev-button-ic.png"))))

(define bottom-frame-prev-last-button-images
  `((focused . ,(make-image "bottom-frame-prev-last-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-prev-last-button-h.png"))
    (clicked . ,(make-image "bottom-frame-prev-last-button-c.png"))
    (inactive . ,(make-image "bottom-frame-prev-last-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-prev-last-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-prev-last-button-ic.png"))))

(define bottom-frame-next-button-images
  `((focused . ,(make-image "bottom-frame-next-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-next-button-h.png"))
    (clicked . ,(make-image "bottom-frame-next-button-c.png"))
    (inactive . ,(make-image "bottom-frame-next-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-next-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-next-button-ic.png"))))

(define bottom-frame-next-last-button-images
  `((focused . ,(make-image "bottom-frame-next-last-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-next-last-button-h.png"))
    (clicked . ,(make-image "bottom-frame-next-last-button-c.png"))
    (inactive . ,(make-image "bottom-frame-next-last-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-next-last-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-next-last-button-ic.png"))))

(define bottom-frame-raise-lower-button-images
  `((focused . ,(make-image "bottom-frame-raise-lower-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-raise-lower-button-h.png"))
    (clicked . ,(make-image "bottom-frame-raise-lower-button-c.png"))
    (inactive . ,(make-image "bottom-frame-raise-lower-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-raise-lower-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-raise-lower-button-ic.png"))))

(define bottom-frame-ontop-button-images
  `((focused . ,(make-image "bottom-frame-ontop-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-ontop-button-h.png"))
    (clicked . ,(make-image "bottom-frame-ontop-button-c.png"))
    (inactive . ,(make-image "bottom-frame-ontop-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-ontop-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-ontop-button-ic.png"))))

(define bottom-frame-unontop-button-images
  `((focused . ,(make-image "bottom-frame-unontop-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-unontop-button-h.png"))
    (clicked . ,(make-image "bottom-frame-unontop-button-c.png"))
    (inactive . ,(make-image "bottom-frame-unontop-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-unontop-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-unontop-button-ic.png"))))

(define bottom-frame-move-resize-button-images
  `((focused . ,(make-image "bottom-frame-move-resize-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-move-resize-button-h.png"))
    (clicked . ,(make-image "bottom-frame-move-resize-button-c.png"))
    (inactive . ,(make-image "bottom-frame-move-resize-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-move-resize-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-move-resize-button-ic.png"))))

(define bottom-frame-rename-button-images
  `((focused . ,(make-image "bottom-frame-rename-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-rename-button-h.png"))
    (clicked . ,(make-image "bottom-frame-rename-button-c.png"))
    (inactive . ,(make-image "bottom-frame-rename-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-rename-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-rename-button-ic.png"))))

(define bottom-frame-frame-typ-button-images
  `((focused . ,(make-image "bottom-frame-frame-typ-button-f.png"))
    (highlighted . ,(make-image "bottom-frame-frame-typ-button-h.png"))
    (clicked . ,(make-image "bottom-frame-frame-typ-button-c.png"))
    (inactive . ,(make-image "bottom-frame-frame-typ-button-i.png"))
    (inactive-highlighted . ,(make-image "bottom-frame-frame-typ-button-ih.png"))
    (inactive-clicked . ,(make-image "bottom-frame-frame-typ-button-ic.png"))))

(define bottom-frame-bottom-left-corner-shaped-images
  `((focused . ,(make-image "bottom-frame-bottom-left-corner-shaped-f.png"))
    (inactive . ,(make-image "bottom-frame-bottom-left-corner-shaped-i.png"))))

(define bottom-frame-bottom-right-corner-shaped-images
  `((focused . ,(make-image "bottom-frame-bottom-right-corner-shaped-f.png"))
    (inactive . ,(make-image "bottom-frame-bottom-right-corner-shaped-i.png"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; left images ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define left-frame-left-border-images
  `((focused . ,(make-image "left-frame-left-border-f.png"))
    (inactive . ,(make-image "left-frame-left-border-i.png"))))

(define left-frame-tab-images
  `((focused . ,(make-image "left-frame-tab-f.png"))
    (inactive . ,(make-image "left-frame-tab-i.png"))))

(define left-frame-tab-bottom-icon-images
  `((focused . ,(make-image "left-frame-tab-bottom-icon-f.png"))
    (inactive . ,(make-image "left-frame-tab-bottom-icon-i.png"))))

(define left-frame-tab-top-images
  `((focused . ,(make-image "left-frame-tab-top-f.png"))
    (inactive . ,(make-image "left-frame-tab-top-i.png"))))

(define left-frame-bottom-left-corner-images
  `((focused . ,(make-image "left-frame-bottom-left-corner-f.png"))
    (inactive . ,(make-image "left-frame-bottom-left-corner-i.png"))))

(define left-frame-top-left-corner-images
  `((focused . ,(make-image "left-frame-top-left-corner-f.png"))
    (inactive . ,(make-image "left-frame-top-left-corner-i.png"))))

(define left-frame-title-images
  `((focused . ,(make-image "left-frame-title-f.png"))
    (inactive . ,(make-image "left-frame-title-i.png"))))

(define left-frame-bottom-border-images
  `((focused . ,(make-image "left-frame-bottom-border-f.png"))
    (inactive . ,(make-image "left-frame-bottom-border-i.png"))))

(define left-frame-bottom-right-corner-images
  `((focused . ,(make-image "left-frame-bottom-right-corner-f.png"))
    (inactive . ,(make-image "left-frame-bottom-right-corner-i.png"))))

(define left-frame-right-border-images
  `((focused . ,(make-image "left-frame-right-border-f.png"))
    (inactive . ,(make-image "left-frame-right-border-i.png"))))

(define left-frame-top-right-corner-images
  `((focused . ,(make-image "left-frame-top-right-corner-f.png"))
    (inactive . ,(make-image "left-frame-top-right-corner-i.png"))))

(define left-frame-top-border-images
  `((focused . ,(make-image "left-frame-top-border-f.png"))
    (inactive . ,(make-image "left-frame-top-border-i.png"))))

(define left-frame-menu-button-images 
  `((focused . ,(make-image "left-frame-menu-button-f.png"))
	(highlighted . ,(make-image "left-frame-menu-button-h.png"))
	(clicked . ,(make-image "left-frame-menu-button-c.png"))
	(inactive . ,(make-image "left-frame-menu-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-menu-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-menu-button-ic.png"))))

(define left-frame-shade-button-images 
  `((focused . ,(make-image "left-frame-shade-button-f.png"))
	(highlighted . ,(make-image "left-frame-shade-button-h.png"))
	(clicked . ,(make-image "left-frame-shade-button-c.png"))
	(inactive . ,(make-image "left-frame-shade-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-shade-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-shade-button-ic.png"))))

(define left-frame-unshade-button-images 
  `((focused . ,(make-image "left-frame-unshade-button-f.png"))
	(highlighted . ,(make-image "left-frame-unshade-button-h.png"))
	(clicked . ,(make-image "left-frame-unshade-button-c.png"))
	(inactive . ,(make-image "left-frame-unshade-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-unshade-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-unshade-button-ic.png"))))

(define left-frame-sticky-button-images 
  `((focused . ,(make-image "left-frame-sticky-button-f.png"))
	(highlighted . ,(make-image "left-frame-sticky-button-h.png"))
	(clicked . ,(make-image "left-frame-sticky-button-c.png"))
	(inactive . ,(make-image "left-frame-sticky-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-sticky-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-sticky-button-ic.png"))))

(define left-frame-unsticky-button-images 
  `((focused . ,(make-image "left-frame-unsticky-button-f.png"))
	(highlighted . ,(make-image "left-frame-unsticky-button-h.png"))
	(clicked . ,(make-image "left-frame-unsticky-button-c.png"))
	(inactive . ,(make-image "left-frame-unsticky-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-unsticky-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-unsticky-button-ic.png"))))

(define left-frame-iconify-button-images 
  `((focused . ,(make-image "left-frame-iconify-button-f.png"))
	(highlighted . ,(make-image "left-frame-iconify-button-h.png"))
	(clicked . ,(make-image "left-frame-iconify-button-c.png"))
	(inactive . ,(make-image "left-frame-iconify-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-iconify-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-iconify-button-ic.png"))))

(define left-frame-maximize-button-images 
  `((focused . ,(make-image "left-frame-maximize-button-f.png"))
	(highlighted . ,(make-image "left-frame-maximize-button-h.png"))
	(clicked . ,(make-image "left-frame-maximize-button-c.png"))
	(inactive . ,(make-image "left-frame-maximize-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-maximize-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-maximize-button-ic.png"))))

(define left-frame-unmaximize-button-images 
  `((focused . ,(make-image "left-frame-unmaximize-button-f.png"))
	(highlighted . ,(make-image "left-frame-unmaximize-button-h.png"))
	(clicked . ,(make-image "left-frame-unmaximize-button-c.png"))
	(inactive . ,(make-image "left-frame-unmaximize-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-unmaximize-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-unmaximize-button-ic.png"))))

(define left-frame-close-button-images 
  `((focused . ,(make-image "left-frame-close-button-f.png"))
	(highlighted . ,(make-image "left-frame-close-button-h.png"))
	(clicked . ,(make-image "left-frame-close-button-c.png"))
	(inactive . ,(make-image "left-frame-close-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-close-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-close-button-ic.png"))))

(define left-frame-lock-button-images 
  `((focused . ,(make-image "left-frame-lock-button-f.png"))
	(highlighted . ,(make-image "left-frame-lock-button-h.png"))
	(clicked . ,(make-image "left-frame-lock-button-c.png"))
	(inactive . ,(make-image "left-frame-lock-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-lock-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-lock-button-ic.png"))))

(define left-frame-unlock-button-images 
  `((focused . ,(make-image "left-frame-unlock-button-f.png"))
	(highlighted . ,(make-image "left-frame-unlock-button-h.png"))
	(clicked . ,(make-image "left-frame-unlock-button-c.png"))
	(inactive . ,(make-image "left-frame-unlock-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-unlock-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-unlock-button-ic.png"))))

(define left-frame-prev-button-images 
  `((focused . ,(make-image "left-frame-prev-button-f.png"))
	(highlighted . ,(make-image "left-frame-prev-button-h.png"))
	(clicked . ,(make-image "left-frame-prev-button-c.png"))
	(inactive . ,(make-image "left-frame-prev-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-prev-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-prev-button-ic.png"))))

(define left-frame-prev-last-button-images 
  `((focused . ,(make-image "left-frame-prev-last-button-f.png"))
	(highlighted . ,(make-image "left-frame-prev-last-button-h.png"))
	(clicked . ,(make-image "left-frame-prev-last-button-c.png"))
	(inactive . ,(make-image "left-frame-prev-last-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-prev-last-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-prev-last-button-ic.png"))))

(define left-frame-next-button-images 
  `((focused . ,(make-image "left-frame-next-button-f.png"))
	(highlighted . ,(make-image "left-frame-next-button-h.png"))
	(clicked . ,(make-image "left-frame-next-button-c.png"))
	(inactive . ,(make-image "left-frame-next-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-next-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-next-button-ic.png"))))

(define left-frame-next-last-button-images 
  `((focused . ,(make-image "left-frame-next-last-button-f.png"))
	(highlighted . ,(make-image "left-frame-next-last-button-h.png"))
	(clicked . ,(make-image "left-frame-next-last-button-c.png"))
	(inactive . ,(make-image "left-frame-next-last-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-next-last-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-next-last-button-ic.png"))))

(define left-frame-raise-lower-button-images 
  `((focused . ,(make-image "left-frame-raise-lower-button-f.png"))
	(highlighted . ,(make-image "left-frame-raise-lower-button-h.png"))
	(clicked . ,(make-image "left-frame-raise-lower-button-c.png"))
	(inactive . ,(make-image "left-frame-raise-lower-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-raise-lower-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-raise-lower-button-ic.png"))))

(define left-frame-ontop-button-images 
  `((focused . ,(make-image "left-frame-ontop-button-f.png"))
	(highlighted . ,(make-image "left-frame-ontop-button-h.png"))
	(clicked . ,(make-image "left-frame-ontop-button-c.png"))
	(inactive . ,(make-image "left-frame-ontop-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-ontop-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-ontop-button-ic.png"))))

(define left-frame-unontop-button-images 
  `((focused . ,(make-image "left-frame-unontop-button-f.png"))
	(highlighted . ,(make-image "left-frame-unontop-button-h.png"))
	(clicked . ,(make-image "left-frame-unontop-button-c.png"))
	(inactive . ,(make-image "left-frame-unontop-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-unontop-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-unontop-button-ic.png"))))

(define left-frame-move-resize-button-images 
  `((focused . ,(make-image "left-frame-move-resize-button-f.png"))
	(highlighted . ,(make-image "left-frame-move-resize-button-h.png"))
	(clicked . ,(make-image "left-frame-move-resize-button-c.png"))
	(inactive . ,(make-image "left-frame-move-resize-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-move-resize-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-move-resize-button-ic.png"))))

(define left-frame-rename-button-images 
  `((focused . ,(make-image "left-frame-rename-button-f.png"))
	(highlighted . ,(make-image "left-frame-rename-button-h.png"))
	(clicked . ,(make-image "left-frame-rename-button-c.png"))
	(inactive . ,(make-image "left-frame-rename-button-i.png"))
	(inactive-highlighted . ,(make-image "left-frame-rename-button-ih.png"))
	(inactive-clicked . ,(make-image "left-frame-rename-button-ic.png"))))

(define left-frame-frame-typ-button-images
  `((focused . ,(make-image "left-frame-frame-typ-button-f.png"))
    (highlighted . ,(make-image "left-frame-frame-typ-button-h.png"))
    (clicked . ,(make-image "left-frame-frame-typ-button-c.png"))
    (inactive . ,(make-image "left-frame-frame-typ-button-i.png"))
    (inactive-highlighted . ,(make-image "left-frame-frame-typ-button-ih.png"))
    (inactive-clicked . ,(make-image "left-frame-frame-typ-button-ic.png"))))

(define left-frame-bottom-left-corner-shaped-images
  `((focused . ,(make-image "left-frame-bottom-left-corner-shaped-f.png"))
    (inactive . ,(make-image "left-frame-bottom-left-corner-shaped-i.png"))))

(define left-frame-top-left-corner-shaped-images
  `((focused . ,(make-image "left-frame-top-left-corner-shaped-f.png"))
    (inactive . ,(make-image "left-frame-top-left-corner-shaped-i.png"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; right images ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define right-frame-right-border-images
  `((focused . ,(make-image "right-frame-right-border-f.png"))
    (inactive . ,(make-image "right-frame-right-border-i.png"))))

(define right-frame-tab-images
  `((focused . ,(make-image "right-frame-tab-f.png"))
    (inactive . ,(make-image "right-frame-tab-i.png"))))

(define right-frame-tab-bottom-icon-images
  `((focused . ,(make-image "right-frame-tab-bottom-icon-f.png"))
    (inactive . ,(make-image "right-frame-tab-bottom-icon-i.png"))))

(define right-frame-tab-top-images
  `((focused . ,(make-image "right-frame-tab-top-f.png"))
    (inactive . ,(make-image "right-frame-tab-top-i.png"))))

(define right-frame-bottom-right-corner-images
  `((focused . ,(make-image "right-frame-bottom-right-corner-f.png"))
    (inactive . ,(make-image "right-frame-bottom-right-corner-i.png"))))

(define right-frame-top-right-corner-images
  `((focused . ,(make-image "right-frame-top-right-corner-f.png"))
    (inactive . ,(make-image "right-frame-top-right-corner-i.png"))))

(define right-frame-title-images
  `((focused . ,(make-image "right-frame-title-f.png"))
    (inactive . ,(make-image "right-frame-title-i.png"))))

(define right-frame-top-border-images
  `((focused . ,(make-image "right-frame-top-border-f.png"))
    (inactive . ,(make-image "right-frame-top-border-i.png"))))

(define right-frame-bottom-left-corner-images
  `((focused . ,(make-image "right-frame-bottom-left-corner-f.png"))
    (inactive . ,(make-image "right-frame-bottom-left-corner-i.png"))))

(define right-frame-left-border-images
  `((focused . ,(make-image "right-frame-left-border-f.png"))
    (inactive . ,(make-image "right-frame-left-border-i.png"))))

(define right-frame-top-left-corner-images
  `((focused . ,(make-image "right-frame-top-left-corner-f.png"))
    (inactive . ,(make-image "right-frame-top-left-corner-i.png"))))

(define right-frame-bottom-border-images
  `((focused . ,(make-image "right-frame-bottom-border-f.png"))
    (inactive . ,(make-image "right-frame-bottom-border-i.png"))))

(define right-frame-menu-button-images 
  `((focused . ,(make-image "right-frame-menu-button-f.png"))
	(highlighted . ,(make-image "right-frame-menu-button-h.png"))
	(clicked . ,(make-image "right-frame-menu-button-c.png"))
	(inactive . ,(make-image "right-frame-menu-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-menu-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-menu-button-ic.png"))))

(define right-frame-shade-button-images 
  `((focused . ,(make-image "right-frame-shade-button-f.png"))
	(highlighted . ,(make-image "right-frame-shade-button-h.png"))
	(clicked . ,(make-image "right-frame-shade-button-c.png"))
	(inactive . ,(make-image "right-frame-shade-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-shade-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-shade-button-ic.png"))))

(define right-frame-unshade-button-images 
  `((focused . ,(make-image "right-frame-unshade-button-f.png"))
	(highlighted . ,(make-image "right-frame-unshade-button-h.png"))
	(clicked . ,(make-image "right-frame-unshade-button-c.png"))
	(inactive . ,(make-image "right-frame-unshade-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-unshade-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-unshade-button-ic.png"))))

(define right-frame-sticky-button-images 
  `((focused . ,(make-image "right-frame-sticky-button-f.png"))
	(highlighted . ,(make-image "right-frame-sticky-button-h.png"))
	(clicked . ,(make-image "right-frame-sticky-button-c.png"))
	(inactive . ,(make-image "right-frame-sticky-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-sticky-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-sticky-button-ic.png"))))

(define right-frame-unsticky-button-images 
  `((focused . ,(make-image "right-frame-unsticky-button-f.png"))
	(highlighted . ,(make-image "right-frame-unsticky-button-h.png"))
	(clicked . ,(make-image "right-frame-unsticky-button-c.png"))
	(inactive . ,(make-image "right-frame-unsticky-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-unsticky-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-unsticky-button-ic.png"))))

(define right-frame-iconify-button-images 
  `((focused . ,(make-image "right-frame-iconify-button-f.png"))
	(highlighted . ,(make-image "right-frame-iconify-button-h.png"))
	(clicked . ,(make-image "right-frame-iconify-button-c.png"))
	(inactive . ,(make-image "right-frame-iconify-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-iconify-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-iconify-button-ic.png"))))

(define right-frame-maximize-button-images 
  `((focused . ,(make-image "right-frame-maximize-button-f.png"))
	(highlighted . ,(make-image "right-frame-maximize-button-h.png"))
	(clicked . ,(make-image "right-frame-maximize-button-c.png"))
	(inactive . ,(make-image "right-frame-maximize-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-maximize-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-maximize-button-ic.png"))))

(define right-frame-unmaximize-button-images 
  `((focused . ,(make-image "right-frame-unmaximize-button-f.png"))
	(highlighted . ,(make-image "right-frame-unmaximize-button-h.png"))
	(clicked . ,(make-image "right-frame-unmaximize-button-c.png"))
	(inactive . ,(make-image "right-frame-unmaximize-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-unmaximize-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-unmaximize-button-ic.png"))))

(define right-frame-close-button-images 
  `((focused . ,(make-image "right-frame-close-button-f.png"))
	(highlighted . ,(make-image "right-frame-close-button-h.png"))
	(clicked . ,(make-image "right-frame-close-button-c.png"))
	(inactive . ,(make-image "right-frame-close-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-close-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-close-button-ic.png"))))

(define right-frame-lock-button-images 
  `((focused . ,(make-image "right-frame-lock-button-f.png"))
	(highlighted . ,(make-image "right-frame-lock-button-h.png"))
	(clicked . ,(make-image "right-frame-lock-button-c.png"))
	(inactive . ,(make-image "right-frame-lock-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-lock-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-lock-button-ic.png"))))

(define right-frame-unlock-button-images 
  `((focused . ,(make-image "right-frame-unlock-button-f.png"))
	(highlighted . ,(make-image "right-frame-unlock-button-h.png"))
	(clicked . ,(make-image "right-frame-unlock-button-c.png"))
	(inactive . ,(make-image "right-frame-unlock-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-unlock-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-unlock-button-ic.png"))))

(define right-frame-prev-button-images 
  `((focused . ,(make-image "right-frame-prev-button-f.png"))
	(highlighted . ,(make-image "right-frame-prev-button-h.png"))
	(clicked . ,(make-image "right-frame-prev-button-c.png"))
	(inactive . ,(make-image "right-frame-prev-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-prev-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-prev-button-ic.png"))))

(define right-frame-prev-last-button-images 
  `((focused . ,(make-image "right-frame-prev-last-button-f.png"))
	(highlighted . ,(make-image "right-frame-prev-last-button-h.png"))
	(clicked . ,(make-image "right-frame-prev-last-button-c.png"))
	(inactive . ,(make-image "right-frame-prev-last-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-prev-last-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-prev-last-button-ic.png"))))

(define right-frame-next-button-images 
  `((focused . ,(make-image "right-frame-next-button-f.png"))
	(highlighted . ,(make-image "right-frame-next-button-h.png"))
	(clicked . ,(make-image "right-frame-next-button-c.png"))
	(inactive . ,(make-image "right-frame-next-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-next-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-next-button-ic.png"))))

(define right-frame-next-last-button-images 
  `((focused . ,(make-image "right-frame-next-last-button-f.png"))
	(highlighted . ,(make-image "right-frame-next-last-button-h.png"))
	(clicked . ,(make-image "right-frame-next-last-button-c.png"))
	(inactive . ,(make-image "right-frame-next-last-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-next-last-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-next-last-button-ic.png"))))

(define right-frame-raise-lower-button-images 
  `((focused . ,(make-image "right-frame-raise-lower-button-f.png"))
	(highlighted . ,(make-image "right-frame-raise-lower-button-h.png"))
	(clicked . ,(make-image "right-frame-raise-lower-button-c.png"))
	(inactive . ,(make-image "right-frame-raise-lower-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-raise-lower-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-raise-lower-button-ic.png"))))

(define right-frame-ontop-button-images 
  `((focused . ,(make-image "right-frame-ontop-button-f.png"))
	(highlighted . ,(make-image "right-frame-ontop-button-h.png"))
	(clicked . ,(make-image "right-frame-ontop-button-c.png"))
	(inactive . ,(make-image "right-frame-ontop-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-ontop-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-ontop-button-ic.png"))))

(define right-frame-unontop-button-images 
  `((focused . ,(make-image "right-frame-unontop-button-f.png"))
	(highlighted . ,(make-image "right-frame-unontop-button-h.png"))
	(clicked . ,(make-image "right-frame-unontop-button-c.png"))
	(inactive . ,(make-image "right-frame-unontop-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-unontop-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-unontop-button-ic.png"))))

(define right-frame-move-resize-button-images 
  `((focused . ,(make-image "right-frame-move-resize-button-f.png"))
	(highlighted . ,(make-image "right-frame-move-resize-button-h.png"))
	(clicked . ,(make-image "right-frame-move-resize-button-c.png"))
	(inactive . ,(make-image "right-frame-move-resize-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-move-resize-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-move-resize-button-ic.png"))))

(define right-frame-rename-button-images 
  `((focused . ,(make-image "right-frame-rename-button-f.png"))
	(highlighted . ,(make-image "right-frame-rename-button-h.png"))
	(clicked . ,(make-image "right-frame-rename-button-c.png"))
	(inactive . ,(make-image "right-frame-rename-button-i.png"))
	(inactive-highlighted . ,(make-image "right-frame-rename-button-ih.png"))
	(inactive-clicked . ,(make-image "right-frame-rename-button-ic.png"))))

(define right-frame-frame-typ-button-images
  `((focused . ,(make-image "right-frame-frame-typ-button-f.png"))
    (highlighted . ,(make-image "right-frame-frame-typ-button-h.png"))
    (clicked . ,(make-image "right-frame-frame-typ-button-c.png"))
    (inactive . ,(make-image "right-frame-frame-typ-button-i.png"))
    (inactive-highlighted . ,(make-image "right-frame-frame-typ-button-ih.png"))
    (inactive-clicked . ,(make-image "right-frame-frame-typ-button-ic.png"))))

(define right-frame-bottom-right-corner-shaped-images
  `((focused . ,(make-image "right-frame-bottom-right-corner-shaped-f.png"))
    (inactive . ,(make-image "right-frame-bottom-right-corner-shaped-i.png"))))

(define right-frame-top-right-corner-shaped-images
  `((focused . ,(make-image "right-frame-top-right-corner-shaped-f.png"))
    (inactive . ,(make-image "right-frame-top-right-corner-shaped-i.png"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; buttons, colors settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define title-colors-images
  (lambda ()
    `((focused . ,darktab:focused-color)
      (highlighted . ,darktab:highlighted-color)
      (clicked . ,darktab:clicked-color)
      (inactive . ,darktab:inactive-color)
      (inactive-highlighted . ,darktab:inactive-highlighted-color)
      (inactive-clicked . ,darktab:inactive-clicked))))

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

;; window icons
;; ripped from Crux
;;
(define icon-table (make-weak-table eq-hash eq))

(define (window-icon w)
  (or (table-ref icon-table w)
      (let ((icon (window-icon-image w)))
        (if icon
            (let ((scaled (scale-image icon (- darktab:title-dimension 7) (- darktab:title-dimension 7))))
              (table-set icon-table w scaled)
              scaled)
          (scale-image top-frame-icon-title-images (- darktab:title-dimension 7) (- darktab:title-dimension 7))))))

(define frame-width
  (lambda (w)
    (if (or (eq (window-type w) 'default)
			(eq (window-type w) 'transient))
        darktab:borders-dimension
      0)))

(define frame-edge
  (lambda (w)
    (if (or (eq (window-type w) 'default)
            (eq (window-type w) 'transient))
        (- darktab:borders-dimension)
      0)))

(define title-hight (lambda (w) darktab:title-dimension)) 
(define title-hight-s (lambda (w) (- darktab:title-dimension 2)))
(define title-edge (lambda (w) (- darktab:title-dimension)))
(define title-edge-s (lambda (w) (- (- darktab:title-dimension 2))))
(define button-width-add 8)
(define top-frame-button-width (lambda (w) (+ darktab:title-dimension button-width-add)))
(define bottom-frame-button-width (lambda (w) (+ darktab:title-dimension button-width-add)))
(define left-frame-button-height (lambda (w) (+ darktab:title-dimension button-width-add)))
(define right-frame-button-height (lambda (w) (+ darktab:title-dimension button-width-add)))

(define-frame-class 'prev-button '((keymap . prev-button-keymap)))
(define-frame-class 'next-button '((keymap . next-button-keymap)))
(define-frame-class 'frame-typ-button '((keymap . frame-typ-button-keymap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; frames ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define top-frame-default-border-corner-group 
  `(((class . top-left-corner)
     (background . ,top-frame-top-left-corner-images)
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-hight)
     (width . ,frame-width))
	((class . top-right-corner)
     (background . ,top-frame-top-right-corner-images)
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-hight)
     (width . ,frame-width))
    ((class . title)
     (background . ,top-frame-title-images)
     (right-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-hight-s)
     (width . 2))))

(define bottom-frame-default-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,bottom-frame-bottom-left-corner-images)
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-hight)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,bottom-frame-bottom-right-corner-images)
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-hight)
     (width . ,frame-width))
    ((class . title)
     (background . ,bottom-frame-title-images)
     (right-edge . 0)
     (bottom-edge . ,title-edge)
     (height . ,title-hight-s)
     (width . 2))))

(define left-frame-default-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,left-frame-bottom-left-corner-images)
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))
    ((class . top-left-corner)
     (background . ,left-frame-top-left-corner-images)
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))))

(define right-frame-default-border-corner-group
  `(((class . bottom-right-corner)
     (background . ,right-frame-bottom-right-corner-images)
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))
    ((class . top-right-corner)
     (background . ,right-frame-top-right-corner-images)
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))))

(define top-frame-border-group
  `(((class . left-border)
     (background . ,top-frame-left-border-images)
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (top-edge . 0)
     (width . ,frame-width)
     (bottom-edge . 0))
	((class . bottom-left-corner)
     (background . ,top-frame-bottom-left-corner-images)
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-border)
     (background . ,top-frame-bottom-border-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . bottom-right-corner)
     (background . ,top-frame-bottom-right-corner-images)
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
	((class . right-border)
     (background . ,top-frame-right-border-images)
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))))

(define bottom-frame-border-group
  `(((class . left-border)
     (background . ,bottom-frame-left-border-images)
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (bottom-edge . 0)
     (width . ,frame-width)
     (top-edge . 0))
    ((class . top-left-corner)
     (background . ,bottom-frame-top-left-corner-images)
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-border)
     (background . ,bottom-frame-top-border-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . top-right-corner)
     (background . ,bottom-frame-top-right-corner-images)
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (top-edge . ,frame-edge))
    ((class . right-border)
     (background . ,bottom-frame-right-border-images)
     (cursor . sb_h_double_arrow)
     (bottom-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (top-edge . 0))))

(define left-frame-border-group
  `(((class . bottom-border)
     (background . ,left-frame-bottom-border-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-right-corner)
     (background . ,left-frame-bottom-right-corner-images)
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . right-border)
     (background . ,left-frame-right-border-images)
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-right-corner)
     (background . ,left-frame-top-right-corner-images)
     (top-edge . ,frame-edge)
     (right-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,left-frame-top-border-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define right-frame-border-group
  `(((class . bottom-border)
     (background . ,right-frame-top-border-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . bottom-left-corner)
     (background . ,right-frame-bottom-left-corner-images)
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width)
     (bottom-edge . ,frame-edge))
    ((class . left-border)
     (background . ,right-frame-left-border-images)
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (left-edge . ,frame-edge)
     (width . ,frame-width)
     (bottom-edge . 0))
    ((class . top-left-corner)
     (background . ,right-frame-top-left-corner-images)
     (top-edge . ,frame-edge)
     (left-edge . ,frame-edge)
     (height . ,frame-width)
     (width . ,frame-width))
    ((class . top-border)
     (background . ,right-frame-bottom-border-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (right-edge . 0)
     (height . ,frame-width)
     (top-edge . ,frame-edge))))

(define top-frame-title-group
  `(((class . top-border)
     (background . ,top-frame-top-border-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (top-edge . ,title-edge)
     (right-edge . 0)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- darktab:title-dimension 12)))
     (y-justify . center)
     (background . ,top-frame-tab-images)
     (foreground . ,title-colors-images)
     (top-edge . ,title-edge-s)
     (height . ,title-hight-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,top-frame-tab-left-icon-images)
     (cursor . hand2)
     (top-edge . ,title-edge-s)
     (height . ,title-hight-s)
     (width . ,title-hight-s)
     (y-justify . 2)
     (x-justify . 5))
    ((class . tabbar-horizontal-right-edge)
     (background . ,top-frame-tab-right-images)
     (width . 3)
     (height . ,title-hight-s)
     (top-edge . ,title-edge-s))))

(define bottom-frame-title-group
  `(((class . title)
     (background . ,bottom-frame-bottom-border-images)
     (left-edge . 0)
     (bottom-edge . -2)
     (right-edge . 0)
     (height . 2))
    ((class . tabbar-horizontal)
     (x-justify . ,(lambda (w) (- darktab:title-dimension 12)))
     (y-justify . center)
     (background . ,bottom-frame-tab-images)
     (foreground . ,title-colors-images)
     (bottom-edge . ,title-edge)
     (height . ,title-hight-s)
     (text . ,window-name))
    ((class . tabbar-horizontal-left-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,bottom-frame-tab-left-icon-images)
     (cursor . hand2)
     (bottom-edge . ,title-edge)
     (height . ,title-hight-s)
     (width . ,title-hight-s)
     (y-justify . 2)
     (x-justify . 5))
    ((class . tabbar-horizontal-right-edge)
     (background . ,bottom-frame-tab-right-images)
     (width . 3)
     (height . ,title-hight-s)
     (bottom-edge . ,title-edge))))

(define left-frame-title-group
  `(((class . left-border)
     (background . ,left-frame-left-border-images)
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (left-edge . ,title-edge)
     (bottom-edge . 0)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,left-frame-tab-top-images)
     (cursor . hand2)
     (height . ,title-hight-s)
     (width . ,title-hight-s)
     (left-edge . ,title-edge-s)
     (y-justify . 4)
     (x-justify . 2))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,left-frame-tab-images)
     (left-edge . ,title-edge-s)
     (width . ,title-hight-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,left-frame-tab-bottom-icon-images)
     (left-edge . ,title-edge-s)
     (height . ,title-hight-s)
     (width . ,title-hight-s)
     (y-justify . 1)
     (x-justify . 2))))

(define right-frame-title-group
  `(((class . right-border)
     (background . ,right-frame-right-border-images)
     (cursor . sb_h_double_arrow)
     (top-edge . 0)
     (right-edge . ,title-edge)
     (bottom-edge . 0)
     (width . 2))
    ((class . tabbar-vertical-top-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,right-frame-tab-top-images)
     (cursor . hand2)
     (height . ,title-hight-s)
     (width . ,title-hight-s)
     (right-edge . ,title-edge-s)
     (y-justify . 4)
     (x-justify . 4))
    ((class . tabbar-vertical)
     (x-justify . 12)
     (y-justify . center)
     (background . ,right-frame-tab-images)
     (right-edge . ,title-edge-s)
     (width . ,title-hight-s))
    ((class . tabbar-vertical-bottom-edge)
     (foreground . ,(lambda (w) (window-icon w)))
     (background . ,right-frame-tab-bottom-icon-images)
     (right-edge . ,title-edge-s)
     (height . ,title-hight-s)
     (width . ,title-hight-s)
     (y-justify . 1)
     (x-justify . 4))))

(define bottom-frame-title-cursor-images
  `(((class . bottom-border)
     (background . ,bottom-frame-bottom-border-cursor-images)
     (cursor . sb_v_double_arrow)
     (left-edge . 0)
     (bottom-edge . ,title-edge)
     (right-edge . 0)
     (height . 2))))

(define top-frame-close-button
  `((class . close-button)
    (background . ,top-frame-close-button-images)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-close-button
  `((class . close-button)
    (background . ,bottom-frame-close-button-images)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-close-button
  `((class . close-button)
    (background . ,left-frame-close-button-images)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-close-button
  `((class . close-button)
    (background . ,right-frame-close-button-images)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-menu-button
  `((class . menu-button)
    (background . ,top-frame-menu-button-images)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-menu-button
  `((class . menu-button)
    (background . ,bottom-frame-menu-button-images)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-menu-button
  `((class . menu-button)
    (background . ,left-frame-menu-button-images)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-menu-button
  `((class . menu-button)
    (background . ,right-frame-menu-button-images)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-iconify-button
  `((class . iconify-button)
    (background . ,top-frame-iconify-button-images)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-iconify-button
  `((class . iconify-button)
    (background . ,bottom-frame-iconify-button-images)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-iconify-button
  `((class . iconify-button)
    (background . ,left-frame-iconify-button-images)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-iconify-button
  `((class . iconify-button)
    (background . ,right-frame-iconify-button-images)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-maximize-button
  `((class . maximize-button)
    (background . ,top-frame-maximize-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-maximize-button
  `((class . maximize-button)
    (background . ,bottom-frame-maximize-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-maximize-button
  `((class . maximize-button)
    (background . ,left-frame-maximize-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-maximize-button
  `((class . maximize-button)
    (background . ,right-frame-maximize-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-shade-button
  `((class . shade-button)
    (background . ,top-frame-shade-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-shade-button
  `((class . shade-button)
    (background . ,bottom-frame-shade-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-shade-button
  `((class . shade-button)
    (background . ,left-frame-shade-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-shade-button
  `((class . shade-button)
    (background . ,right-frame-shade-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-sticky-button
  `((class . sticky-button)
    (background . ,top-frame-sticky-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-sticky-button
  `((class . sticky-button)
    (background . ,bottom-frame-sticky-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-sticky-button
  `((class . sticky-button)
    (background . ,left-frame-sticky-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-sticky-button
  `((class . sticky-button)
    (background . ,right-frame-sticky-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-space-button
  `((class . title)
    (background . ,top-frame-title-images)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-space-button
  `((class . title)
    (background . ,bottom-frame-title-images)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-space-button
  `((class . title)
    (background . ,left-frame-title-images)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-space-button
  `((class . title)
    (background . ,right-frame-title-images)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-prev-button
  `((class . prev-button)
    (background . ,top-frame-prev-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-prev-button
  `((class . prev-button)
    (background . ,bottom-frame-prev-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-prev-button
  `((class . prev-button)
    (background . ,left-frame-prev-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-prev-button
  `((class . prev-button)
    (background . ,right-frame-prev-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-next-button
  `((class . next-button)
    (background . ,top-frame-next-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-next-button
  `((class . next-button)
    (background . ,bottom-frame-next-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-next-button
  `((class . next-button)
    (background . ,left-frame-next-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-next-button
  `((class . next-button)
    (background . ,right-frame-next-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-lock-button
  `((class . lock-button)
    (background . ,top-frame-lock-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-lock-button
  `((class . lock-button)
    (background . ,bottom-frame-lock-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-lock-button
  `((class . lock-button)
    (background . ,left-frame-lock-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-lock-button
  `((class . lock-button)
    (background . ,right-frame-lock-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,top-frame-raise-lower-image-set)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,bottom-frame-raise-lower-image-set)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,left-frame-raise-lower-image-set)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-raise-lower-button
  `((class . raise-lower-button)
    (background . ,right-frame-raise-lower-image-set)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define  top-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,top-frame-move-resize-button-images)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define  bottom-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,bottom-frame-move-resize-button-images)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define  left-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,left-frame-move-resize-button-images)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define  right-frame-move-resize-button
  `((class . move-resize-button)
    (background . ,right-frame-move-resize-button-images)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-rename-button
  `((class . rename-button)
    (background . ,top-frame-rename-button-images)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-rename-button
  `((class . rename-button)
    (background . ,bottom-frame-rename-button-images)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-rename-button
  `((class . rename-button)
    (background . ,left-frame-rename-button-images)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-rename-button
  `((class . rename-button)
    (background . ,right-frame-rename-button-images)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-frame-typ-button
  `((class . frame-typ-button)
    (background . ,top-frame-frame-typ-button-images)
    (cursor . hand2)
    (top-edge . ,title-edge-s)
    (height . ,title-hight-s)
    (width . ,top-frame-button-width)))

(define bottom-frame-frame-typ-button
  `((class . frame-typ-button)
    (background . ,bottom-frame-frame-typ-button-images)
    (cursor . hand2)
    (bottom-edge . ,title-edge)
    (height . ,title-hight-s)
    (width . ,bottom-frame-button-width)))

(define left-frame-frame-typ-button
  `((class . frame-typ-button)
    (background . ,left-frame-frame-typ-button-images)
    (cursor . hand2)
    (left-edge . ,title-edge-s)
    (height . ,left-frame-button-height)
    (width . ,title-hight-s)))

(define right-frame-frame-typ-button
  `((class . frame-typ-button)
    (background . ,right-frame-frame-typ-button-images)
    (cursor . hand2)
    (right-edge . ,title-edge-s)
    (height . ,right-frame-button-height)
    (width . ,title-hight-s)))

(define top-frame-shaped-border-corner-group
  `(((class . top-left-corner)
     (background . ,top-frame-top-left-corner-shaped-images)
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (top-edge . ,title-edge)
     (height . ,title-hight)
     (width . ,frame-width))
	((class . top-right-corner)
     (background . ,top-frame-top-right-corner-shaped-images)
     (cursor . sb_h_double_arrow)
     (top-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-hight)
     (width . ,frame-width))
    ((class . title)
     (background . ,top-frame-title-images)
     (right-edge . 0)
     (top-edge . ,title-edge-s)
     (height . ,title-hight-s)
     (width . 2))))

(define bottom-frame-shaped-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,bottom-frame-bottom-left-corner-shaped-images)
     (cursor . sb_h_double_arrow)
     (left-edge . ,frame-edge)
     (bottom-edge . ,title-edge)
     (height . ,title-hight)
     (width . ,frame-width))
    ((class . bottom-right-corner)
     (background . ,bottom-frame-bottom-right-corner-shaped-images)
     (cursor . sb_h_double_arrow)
     (bottom-edge . ,title-edge)
     (right-edge . ,frame-edge)
     (height . ,title-hight)
     (width . ,frame-width))
    ((class . title)
     (background . ,bottom-frame-title-images)
     (right-edge . 0)
     (bottom-edge . ,title-edge)
     (height . ,title-hight-s)
     (width . 2))))

(define left-frame-shaped-border-corner-group
  `(((class . bottom-left-corner)
     (background . ,left-frame-bottom-left-corner-shaped-images)
     (bottom-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))
    ((class . top-left-corner)
     (background . ,left-frame-top-left-corner-shaped-images)
     (top-edge . ,frame-edge)
     (left-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))))

(define right-frame-shaped-border-corner-group
  `(((class . bottom-right-corner)
     (background . ,right-frame-bottom-right-corner-shaped-images)
     (bottom-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))
    ((class . top-right-corner)
     (background . ,right-frame-top-right-corner-shaped-images)
     (top-edge . ,frame-edge)
     (right-edge . ,title-edge)
     (height . ,frame-width)
     (width . ,title-hight))))

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
    (frame-typ . ,top-frame-frame-typ-button)
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
    (frame-typ . ,bottom-frame-frame-typ-button)
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
    (frame-typ . ,left-frame-frame-typ-button)
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
    (frame-typ . ,right-frame-frame-typ-button)
    (\(none\)   . ,nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; build themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
            (make-button-list nil darktab:top-left-buttons 'left-edge  0 (+ darktab:title-dimension button-width-add)))
           (top-frame-normal-buttons-right
            (make-button-list nil darktab:top-right-buttons 'right-edge 0 (+ darktab:title-dimension button-width-add)))
           (top-frame-transient-buttons-left
            (make-button-list t darktab:top-left-buttons 'left-edge  0 (+ darktab:title-dimension button-width-add)))
           (top-frame-transient-buttons-right
            (make-button-list t darktab:top-right-buttons 'right-edge 0 (+ darktab:title-dimension button-width-add)))

           (bottom-frame-normal-buttons-left 
            (make-button-list nil darktab:bottom-left-buttons 'left-edge  0 (+ darktab:title-dimension button-width-add)))
           (bottom-frame-normal-buttons-right
            (make-button-list nil darktab:bottom-right-buttons 'right-edge 0 (+ darktab:title-dimension button-width-add)))
           (bottom-frame-transient-buttons-left
            (make-button-list t darktab:bottom-left-buttons 'left-edge  0 (+ darktab:title-dimension button-width-add)))
           (bottom-frame-transient-buttons-right
            (make-button-list t darktab:bottom-right-buttons 'right-edge 0 (+ darktab:title-dimension button-width-add)))

           (left-frame-normal-buttons-left 
            (make-button-list nil darktab:left-bottom-buttons 'bottom-edge  0 (+ darktab:title-dimension button-width-add)))
           (left-frame-normal-buttons-right
            (make-button-list nil darktab:left-top-buttons 'top-edge 0 (+ darktab:title-dimension button-width-add)))
           (left-frame-transient-buttons-left
            (make-button-list t darktab:left-bottom-buttons 'bottom-edge  0 (+ darktab:title-dimension button-width-add)))
           (left-frame-transient-buttons-right
            (make-button-list t darktab:left-top-buttons 'top-edge 0 (+ darktab:title-dimension button-width-add)))

           (right-frame-normal-buttons-left 
            (make-button-list nil darktab:right-bottom-buttons 'bottom-edge  0 (+ darktab:title-dimension button-width-add)))
           (right-frame-normal-buttons-right
            (make-button-list nil darktab:right-top-buttons 'top-edge 0 (+ darktab:title-dimension button-width-add)))
           (right-frame-transient-buttons-left
            (make-button-list t darktab:right-bottom-buttons 'bottom-edge  0 (+ darktab:title-dimension button-width-add)))
           (right-frame-transient-buttons-right
            (make-button-list t darktab:right-top-buttons 'top-edge 0 (+ darktab:title-dimension button-width-add))))

      (require 'sawfish.wm.tabs.tab)
      (when (eq current-title 'top)
        (let ((top-left-d-w 11)
              (top-right-d-w 3)
              (top-left-m
               (if (numberp (cdr (car (car top-frame-normal-buttons-left))))
                   (+ (cdr (car (car top-frame-normal-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (top-rigth-m
               (if (numberp (cdr (car (car top-frame-normal-buttons-right))))
                   (+ (cdr (car (car top-frame-normal-buttons-right))) (+ darktab:title-dimension button-width-add)) 0))
              (top-left-m-t
               (if (numberp (cdr (car (car top-frame-transient-buttons-left))))
                   (+ (cdr (car (car top-frame-transient-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (top-right-m-t
               (if (numberp (cdr (car (car top-frame-transient-buttons-right))))
                   (+ (cdr (car (car top-frame-transient-buttons-right))) (+ darktab:title-dimension button-width-add)) 0)))
          (set-tab-adjustments #:theme-left-dec-width top-left-d-w #:theme-right-dec-width top-right-d-w #:theme-left-margin top-left-m
                               #:theme-right-margin top-rigth-m #:theme-left-margin-transient top-left-m-t
                               #:theme-right-margin-transient top-right-m-t))
        (setq normal-frame
              (append top-frame-title-group top-frame-normal-buttons-left top-frame-default-border-corner-group 
                      top-frame-border-group top-frame-normal-buttons-right))
        (setq shaped-frame
              (append top-frame-title-group top-frame-normal-buttons-left top-frame-shaped-border-corner-group 
                      top-frame-normal-buttons-right))
        (setq transient-frame
              (append top-frame-title-group top-frame-transient-buttons-left top-frame-default-border-corner-group 
                      top-frame-border-group top-frame-transient-buttons-right))
        (setq shaped-transient-frame
              (append top-frame-title-group top-frame-transient-buttons-left top-frame-shaped-border-corner-group 
                      top-frame-transient-buttons-right)))

      (when (eq current-title 'bottom)
        (let ((bottom-left-d-w 11)
              (bottom-right-d-w 3)
              (bottom-left-m
               (if (numberp (cdr (car (car bottom-frame-normal-buttons-left))))
                   (+ (cdr (car (car bottom-frame-normal-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (bottom-rigth-m
               (if (numberp (cdr (car (car bottom-frame-normal-buttons-right))))
                   (+ (cdr (car (car bottom-frame-normal-buttons-right))) (+ darktab:title-dimension button-width-add)) 0))
              (bottom-left-m-t
               (if (numberp (cdr (car (car bottom-frame-transient-buttons-left))))
                   (+ (cdr (car (car bottom-frame-transient-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (bottom-right-m-t
               (if (numberp (cdr (car (car bottom-frame-transient-buttons-right))))
                   (+ (cdr (car (car bottom-frame-transient-buttons-right))) (+ darktab:title-dimension button-width-add)) 0)))
          (set-tab-adjustments #:theme-left-dec-width bottom-left-d-w #:theme-right-dec-width bottom-right-d-w #:theme-left-margin bottom-left-m
                               #:theme-right-margin bottom-rigth-m #:theme-left-margin-transient bottom-left-m-t
                               #:theme-right-margin-transient bottom-right-m-t))
        (setq normal-frame
              (append bottom-frame-title-group bottom-frame-normal-buttons-left bottom-frame-default-border-corner-group 
                      bottom-frame-border-group bottom-frame-normal-buttons-right bottom-frame-title-cursor-images))
        (setq shaped-frame
              (append bottom-frame-title-group bottom-frame-normal-buttons-left bottom-frame-shaped-border-corner-group 
                      bottom-frame-normal-buttons-right bottom-frame-title-cursor-images))
        (setq transient-frame
              (append bottom-frame-title-group bottom-frame-transient-buttons-left bottom-frame-default-border-corner-group 
                      bottom-frame-border-group bottom-frame-transient-buttons-right bottom-frame-title-cursor-images))
        (setq shaped-transient-frame
              (append bottom-frame-title-group bottom-frame-transient-buttons-left bottom-frame-shaped-border-corner-group 
                      bottom-frame-transient-buttons-right bottom-frame-title-cursor-images)))
      
      (when (eq current-title 'left)
        (let ((left-left-d-w 11)
              (left-right-d-w (- darktab:title-dimension 2))
              (left-left-m
               (if (numberp (cdr (car (car left-frame-normal-buttons-left))))
                   (+ (cdr (car (car left-frame-normal-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (left-rigth-m
               (if (numberp (cdr (car (car left-frame-normal-buttons-right))))
                   (+ (cdr (car (car left-frame-normal-buttons-right))) (+ darktab:title-dimension button-width-add)) 0))
              (left-left-m-t
               (if (numberp (cdr (car (car left-frame-transient-buttons-left))))
                   (+ (cdr (car (car left-frame-transient-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (left-right-m-t
               (if (numberp (cdr (car (car left-frame-transient-buttons-right))))
                   (+ (cdr (car (car left-frame-transient-buttons-right))) (+ darktab:title-dimension button-width-add)) 0)))
          (set-tab-adjustments #:theme-left-dec-width left-left-d-w #:theme-right-dec-width left-right-d-w #:theme-left-margin left-left-m
                               #:theme-right-margin left-rigth-m #:theme-left-margin-transient left-left-m-t
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
              (right-right-d-w (- darktab:title-dimension 2))
              (right-left-m
               (if (numberp (cdr (car (car right-frame-normal-buttons-left))))
                   (+ (cdr (car (car right-frame-normal-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (right-rigth-m
               (if (numberp (cdr (car (car right-frame-normal-buttons-right))))
                   (+ (cdr (car (car right-frame-normal-buttons-right))) (+ darktab:title-dimension button-width-add)) 0))
              (right-left-m-t
               (if (numberp (cdr (car (car right-frame-transient-buttons-left))))
                   (+ (cdr (car (car right-frame-transient-buttons-left))) (+ darktab:title-dimension button-width-add)) 0))
              (right-right-m-t
               (if (numberp (cdr (car (car right-frame-transient-buttons-right))))
                   (+ (cdr (car (car right-frame-transient-buttons-right))) (+ darktab:title-dimension button-width-add)) 0)))
          (set-tab-adjustments #:theme-left-dec-width right-left-d-w #:theme-right-dec-width right-right-d-w #:theme-left-margin right-left-m
                               #:theme-right-margin right-rigth-m #:theme-left-margin-transient right-left-m-t
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
                         ((default)				normal-frame)
                         ((shaped)				shaped-frame)
                         ((transient)			transient-frame)
                         ((shaped-transient)	shaped-transient-frame)
                         ((utility)			    normal-frame)
                         ((shaded-utility)	    normal-frame)
                         ((unframed)			nil-frame))))

(define (current-title-w w)
  (if (eq (window-get w 'title-position) nil)
      (case darktab:titlebar-place
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

(define reframe-all-clean
  (lambda ()
    (setq icon-table (make-weak-table eq-hash eq))
    (reframe-all)))

(define (reframe-one w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (current-title-w w)
    (reframe-window w)))

(define (reframe-group w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (map-window-group
     (lambda (x)
       (reframe-one x)) w)))

;; create only frames when focus a window don't draw
;;
(add-hook 'focus-in-hook create-frames-only)
(add-hook 'add-window-hook create-frames-only)

(call-after-property-changed '(WM_HINTS WM_NAME _NET_WM_NAME _NET_WM_STATE _NET_WM_DESKTOP) reframe-one)

;; theme-title icon switch
;;
;;(call-after-state-changed '(title-position) reframe-one)
(call-after-state-changed '(title-position) reframe-group)

;; when the window is sent to another workspace
;; redraw it to update the buttons and titlebar if necessary
;;
(add-hook 'remove-from-workspace-hook
          (lambda (w)
            (reframe-window w)))

(custom-set-property 'darktab:focused-color ':after-set reframe-all)
(custom-set-property 'darktab:highlighted-color ':after-set reframe-all)
(custom-set-property 'darktab:clicked-color ':after-set reframe-all)
(custom-set-property 'darktab:inactive-color ':after-set reframe-all)
(custom-set-property 'darktab:inactive-highlighted-color ':after-set reframe-all)
(custom-set-property 'darktab:inactive-clicked ':after-set reframe-all)
(custom-set-property 'darktab:title-dimension ':after-set reframe-all-clean)
(custom-set-property 'darktab:borders-dimension ':after-set reframe-all)
(custom-set-property 'darktab:titlebar-place ':after-set reframe-all)
(custom-set-property 'darktab:top-left-buttons ':after-set reframe-all)
(custom-set-property 'darktab:top-right-buttons ':after-set reframe-all)
(custom-set-property 'darktab:bottom-left-buttons ':after-set reframe-all)
(custom-set-property 'darktab:bottom-right-buttons ':after-set reframe-all)
(custom-set-property 'darktab:left-top-buttons ':after-set reframe-all)
(custom-set-property 'darktab:left-bottom-buttons ':after-set reframe-all)
(custom-set-property 'darktab:right-top-buttons ':after-set reframe-all)
(custom-set-property 'darktab:right-bottom-buttons ':after-set reframe-all)
