;; Based on gradient and Step themes created by:
;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
;; Copyright (C) 1999 Michele Campeotto <micampe@f2s.com>

;; Copyright (C) 2009-2011 Sergey Kozhemyakin <serg.kozhemyakin@gmail.com>

(define theme-name 'gradient-tabbed)

(require 'gradient)

;; customize variables

(defgroup gradient-tabbed "Gradient Tabbed Theme"
  :group appearance)

(defcustom gradient-tabbed:gradient-type 'horizontal
  "Direction of gradient in frame style."
  :type symbol
  :options (horizontal vertical diagonal)
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:normal-from-color (get-color "#a8a8a8")
  "`From' color of inactive frames."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:normal-to-color (get-color "#a8a8a8")
  "`To' color of inactive frames."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:active-from-color (get-color "#64b4df")
  "`From' color of active frames."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:active-to-color (get-color "#000030")
  "`To' color of active frames."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:active-handle-color (get-color "#64b4df")
  "Color of handles for active frame."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:inactive-handle-color (get-color "#a8a8a8")
  "Color of handles for inactive frames."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:active-close-color (get-color "#64b4df")
  "Color of close button for active frame."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:inactive-close-color (get-color "#a8a8a8")
  "Color of close button for inactive frames."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:active-menu-color (get-color "#64b4df")
  "Color of menu button for active frame."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:inactive-menu-color (get-color "#a8a8a8")
  "Color of menu button for inactive frames."
  :type color
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

(defcustom gradient-tabbed:save-memory t
  "Use less memory when creating gradients, possibly affecting quality."
  :type boolean
  :group (appearance gradient-tabbed)
  :after-set after-setting-frame-option)

;; helpers definitions

(define text-colors (list "grey50" "white"))

(define render-bg
  (lambda (img state)
    (apply (cond ((eq gradient-tabbed:gradient-type 'diagonal)
                  draw-diagonal-gradient)
                 ((eq gradient-tabbed:gradient-type 'horizontal)
                  draw-horizontal-gradient)
                 ((eq gradient-tabbed:gradient-type 'vertical)
                  draw-vertical-gradient))
           img (if state
                   (list gradient-tabbed:active-from-color
                         gradient-tabbed:active-to-color)
                 (list gradient-tabbed:normal-from-color
                       gradient-tabbed:normal-to-color)))
    (when (> (cdr (image-dimensions img)) 4)
      (bevel-image img 1 (not (eq state 'clicked))))
    (set-image-border img 1 1 1 1)))

(define scale (lambda () (if gradient-tabbed:save-memory 2 1)))
(define border-color (get-color "black"))

(define render-menu-button
  (lambda (img state)
    (clear-image
     img
     (if state
         gradient-tabbed:active-menu-color
       gradient-tabbed:inactive-menu-color))
    (when (> (cdr (image-dimensions img)) 4)
      (bevel-image img 1 (not (eq state 'clicked))))
    (set-image-border img 1 1 1 1)))

(define render-close-button
  (lambda (img state)
    (clear-image
     img
     (if state
         gradient-tabbed:active-close-color
       gradient-tabbed:inactive-close-color))
    (when (> (cdr (image-dimensions img)) 4)
      (bevel-image img 1 (not (eq state 'clicked))))
    (set-image-border img 1 1 1 1)))

(define menu-button
  (list
   (make-image "minimize-button.png")
   (make-image "minimize-button.png")
   nil
   (make-image "minimize-pressed.png")))

(define close-button
  (list
   (make-image "close-button.png")
   (make-image "close-button.png")
   nil
   (make-image "close-pressed.png")))

(define render-resizebar
  (lambda (img state)
    (clear-image
     img
     (if state
         gradient-tabbed:active-from-color
       gradient-tabbed:normal-from-color))
    (when (> (cdr (image-dimensions img)) 4)
      (bevel-image img 1 (not (eq state 'clicked))))
    (set-image-border img 1 1 1 1)))

(define render-resizebar-handle
  (lambda (img state)
    (clear-image
     img
     (if state
         gradient-tabbed:active-handle-color
       gradient-tabbed:inactive-handle-color))
    (when (> (cdr (image-dimensions img)) 4)
      (bevel-image img 1 (not (eq state 'clicked))))
    (set-image-border img 1 1 1 1)))

(define menu
  `((renderer . ,render-menu-button)
    (foreground . ,menu-button)
    (left-edge . 0)
    (width . 21)
    (top-edge . -21)
    (height . 21)
    (class . menu-button)
    (removable . nil)))

(define close
  `((renderer . ,render-close-button)
    (foreground . ,close-button)
    (right-edge . 0)
    (width . 21)
    (top-edge . -21)
    (height . 21)
    (class . close-button)
    (removable . nil)))

(define tab
  `((renderer . ,render-bg)
    (foreground . ,text-colors)
    (top-edge . -21)
    (height . 21)
    (text . ,window-name)
    (x-justify . 10)
    (y-justify . center)
    (class . tabbar-horizontal)))

(define transient-titlebar
  `((renderer . ,render-bg)
    (foreground . ,text-colors)
    (x-justify . 10)
    (y-justify . center)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -21)
    (height . 21)
    (text . ,window-name)
    (class . title)))

(define resizebar
  `((renderer . ,render-resizebar)
    (left-edge . 29)
    (right-edge . 29)
    (bottom-edge . -6)
    (height . 6)
    (class . bottom-border)))

(define full-resizebar-top
  `((renderer . ,render-resizebar)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -6)
    (height . 6)
    (class . top-border)))

(define full-resizebar-bottom
  `((renderer . ,render-resizebar)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -6)
    (height . 6)
    (class . bottom-border)))

(define resizebar-right
  `((renderer . ,render-resizebar-handle)
    (right-edge . 0)
    (width . 29)
    (bottom-edge . -6)
    (height . 6)
    (class . bottom-right-corner)))

(define resizebar-left
  `((renderer . ,render-resizebar-handle)
    (left-edge . 0)
    (width . 29)
    (bottom-edge . -6)
    (height . 6)
    (class . bottom-left-corner)))

(define left-border
  `((background . ,border-color)
    (left-edge . -1)
    (width . 1)
    (top-edge . -21)
    (bottom-edge . -6)
    (class . left-border)))

(define right-border
  `((background . ,border-color)
    (right-edge . -1)
    (width . 1)
    (top-edge . -21)
    (bottom-edge . -6)
    (class . right-border)))

(define left-transient-border
  `((background . ,border-color)
    (left-edge . -1)
    (width . 1)
    (top-edge . -21)
    (bottom-edge . -1)
    (class . left-border)))

(define right-transient-border
  `((background . ,border-color)
    (right-edge . -1)
    (width . 1)
    (top-edge . -21)
    (bottom-edge . -1)
    (class . right-border)))

(define bottom-border
  `((background . ,border-color)
    (left-edge . -1)
    (right-edge . -1)
    (bottom-edge . -1)
    (height . 1)
    (class . bottom-border)))

(define top-border
  `((background . ,border-color)
    (left-edge . -1)
    (right-edge . -1)
    (top-edge . -22)
    (height . 1)
    (class . top-border)))

;; frames definitions

(define frame
  `(,menu
    ,close
    ,tab
    ,resizebar
    ,resizebar-left
    ,resizebar-right
    ,left-border
    ,right-border
    ,top-border))

(define shaped-frame
  `(,menu
    ,close
    ,tab
    ,left-transient-border
    ,right-transient-border
    ,bottom-border
    ,top-border))

(define transient-frame
  `(,tab
    ,left-transient-border
    ,right-transient-border
    ,bottom-border
    ,top-border))

(define shaped-transient-frame
  `(,tab))

;; build frames

(define create-frames
  (lambda ()
    (let ((gradient-tabbed-left-d-w 0)
          (gradient-tabbed-right-d-w 0)
          (gradient-tabbed-left-m 21)
          (gradient-tabbed-rigth-m 21)
          (gradient-tabbed-left-m-t 0)
          (gradient-tabbed-right-m-t 0))
      (require 'sawfish.wm.tabs.tab)
      (set-tab-adjustments #:theme-left-dec-width gradient-tabbed-left-d-w
                           #:theme-right-dec-width gradient-tabbed-right-d-w
                           #:theme-left-margin gradient-tabbed-left-m
                           #:theme-right-margin gradient-tabbed-rigth-m
                           #:theme-left-margin-transient gradient-tabbed-left-m-t
                           #:theme-right-margin-transient gradient-tabbed-right-m-t))))

(define (frame-style-name w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (set-tab-theme-name #:frame-style-supported-tabs theme-name)))

(define (frame-style-tabbars w)
  (when (eq (window-get w 'current-frame-style) theme-name)
    (set-tab-theme-tabbars #:frame-style-supported-tabbars (list theme-name styletab-c:titlebar-place))))

(define (get-frame w type)
  (create-frames)
  (case type
        ((default) frame)
        ((transient) transient-frame)
        ((shaped) shaped-frame)
        ((shaped-transient) shaped-transient-frame)))

(create-frames)

(add-frame-style theme-name get-frame)
(call-after-state-changed '(tab-theme-name) frame-style-name)
(call-after-state-changed '(tab-theme-tabbars) frame-style-tabbars)
