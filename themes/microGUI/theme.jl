; microGUI/theme.jl

;; Copyright (C) 1999 Ryan Lovett <ryan@ocf.berkeley.edu>

;; This theme was based on the QNX Photon microGUI found at www.qnx.com
;; (screenshots at http://www.qnx.com/amiga/delivering.html). Many image
;; components were taken from the minEguE enlightenment theme by Ben
;; FrantzDale (frantzdb@admin.arhs.net). The theme and the rest of the images
;; were put together by Ryan (ryan@ocf.berkeley.edu).


;; images

;; 2x6
(defvar microGUI:bottom-images (make-image "bottom.png"))

;; 6x27
(defvar microGUI:top-left-images (list (make-image "top_left_inactive.png")
				       (make-image "top_left.png")))

;; 19x19
(defvar microGUI:top-blue-images (list (make-image "top_blue_inactive.png")
				       (make-image "top_blue.png")))

;; 12x19
(defvar microGUI:top-curves-images (list (make-image "top_curves_inactive.png")
					 (make-image "top_curves.png")))

;; 19x19
(defvar microGUI:top-grey-images (make-image "top_grey.png"))

;; 7x19
(defvar microGUI:top-right-images (make-image "top_right.png"))

;; 25x25
(defvar microGUI:bottom-left-images (make-image "bl.png"))

;; 25x25
(defvar microGUI:bottom-right-images (make-image "br.png"))

(defvar microGUI:top-left-trans-images (make-image "tl.png"))

(defvar microGUI:top-right-trans-images
  (flip-image-diagonally (copy-image microGUI:bottom-left-images)))

;; 17x16
(defvar microGUI:close-images (list (make-image "close_normal.png")
				    (make-image "close_active.png") nil
				    (make-image "close_clicked.png")))

;; 17x16
(defvar microGUI:maximize-images (list (make-image "maximize_normal.png")
				       (make-image "maximize_active.png") nil
				       (make-image "maximize_clicked.png")))

;; 17x16
(defvar microGUI:iconify-images (list (make-image "minimize_normal.png")
				      (make-image "minimize_active.png") nil
				      (make-image "minimize_clicked.png")))

;; 17x16
(defvar microGUI:menu-images (list (make-image "menu_normal.png")
				   (make-image "menu_active.png") nil
				   (make-image "menu_clicked.png")))

;; 6x19
(defvar microGUI:left-images (make-image "left.png"))

;; 6x19
(defvar microGUI:right-images (make-image "right.png"))

(defvar microGUI:top-images
  (flip-image-diagonally (copy-image microGUI:left-images)))


;; frame layout

(defvar microGUI:frame
 `(
   ;; top-left corner
   ((background . ,microGUI:top-left-images)
    (left-edge . -6)
    (top-edge . -19)
    (class . top-left-corner))

   ;; top blue
   ((background . ,microGUI:top-blue-images)
    (foreground . "black")
    (text . window-name)
    (x-justify . 23)
    (y-justify . center)
    (top-edge . -19)
    (left-edge . 0)
    (width . (lambda (w) (+ (microGUI:title-width w) 32)))
    (class . title))

   ;; menu button
   ((background . ,microGUI:menu-images)
    (top-edge . -17)
    (left-edge . -1)
    (class . menu-button))

   ;; top curves
   ((background . ,microGUI:top-curves-images)
    (left-edge . (lambda (w) (+ (microGUI:title-width w) 32)))
    (top-edge . -19)
    (class . title))

   ;; top grey
   ((background . ,microGUI:top-grey-images)
    (top-edge . -19)
    (left-edge . (lambda (w) (+ (microGUI:title-width w) 44)))
    (right-edge . 1)
    (class . title))

   ;; left border
   ((background . ,microGUI:left-images)
    (left-edge . -6)
    (top-edge . 8)
    (bottom-edge . 19)
    (class . left-border))

   ;; top-right corner
   ((background . ,microGUI:top-right-images)
    (right-edge . -6)
    (top-edge . -19)
    (class . top-right-corner))

   ;; right border
   ((background . ,microGUI:right-images)
    (right-edge . -6)
    (top-edge . 0)
    (bottom-edge . 0)
    (class . right-border))

   ;; bottom border
   ((background . ,microGUI:bottom-images)
    (left-edge . 17)
    (right-edge . 19)
    (bottom-edge . -6)
    (class . bottom-border))

   ;; bottom-left corner
   ((background . ,microGUI:bottom-left-images)
    (left-edge . -6)
    (bottom-edge . -6)
    (class . bottom-left-corner))

   ;; bottom-right corner
   ((background . ,microGUI:bottom-right-images)
    (right-edge . -6)
    (bottom-edge . -6)
    (class . bottom-right-corner))

   ;; iconify button
   ((background . ,microGUI:iconify-images)
    (right-edge . 35)
    (top-edge . -17)
    (class . iconify-button))

   ;; maximize button
   ((background . ,microGUI:maximize-images)
    (right-edge . 19)
    (top-edge . -17)
    (class . maximize-button))

   ;; delete button
   ((background . ,microGUI:close-images)
    (right-edge . 1)
    (top-edge . -17)
    (class . close-button))
))

(defvar microGUI:shaped-frame
 `(
   ;; top-left corner
   ((background . ,microGUI:top-left-images)
    (left-edge . -6)
    (top-edge . -19)
    (height . 19)
    (class . top-left-corner))

   ;; top blue
   ((background . ,microGUI:top-blue-images)
    (foreground . "black")
    (text . window-name)
    (x-justify . 23)
    (y-justify . center)
    (top-edge . -19)
    (left-edge . 0)
    (width . (lambda (w) (+ (microGUI:title-width w) 32)))
    (class . title))

   ;; menu button
   ((background . ,microGUI:menu-images)
    (top-edge . -17)
    (left-edge . -1)
    (class . menu-button))

   ;; top curves
   ((background . ,microGUI:top-curves-images)
    (left-edge . (lambda (w) (+ (microGUI:title-width w) 32)))
    (top-edge . -19)
    (class . title))

   ;; top grey
   ((background . ,microGUI:top-grey-images)
    (top-edge . -19)
    (left-edge . (lambda (w) (+ (microGUI:title-width w) 44)))
    (right-edge . 1)
    (class . title))

   ;; top-right corner
   ((background . ,microGUI:top-right-images)
    (right-edge . -6)
    (top-edge . -19)
    (height . 19)
    (class . top-right-corner))

   ;; iconify button
   ((background . ,microGUI:iconify-images)
    (right-edge . 35)
    (top-edge . -17)
    (class . iconify-button))

   ;; maximize button
   ((background . ,microGUI:maximize-images)
    (right-edge . 19)
    (top-edge . -17)
    (class . maximize-button))

   ;; delete button
   ((background . ,microGUI:close-images)
    (right-edge . 1)
    (top-edge . -17)
    (class . close-button))
))

(defvar microGUI:transient-frame
  `(;;top-left corner
    ((background . ,microGUI:top-left-trans-images)
     (left-edge . -6)
     (top-edge . -6)
     (class . top-left-corner))

    ;;top-right corner
    ((background . ,microGUI:top-right-trans-images)
     (right-edge . -6)
     (top-edge . -6)
     (class . top-right-corner))

    ;;title border
    ((background . ,microGUI:top-images)
     (left-edge . 17)
     (right-edge . 19)
     (top-edge . -6)
     (class . title))

   ;; left border
   ((background . ,microGUI:left-images)
    (left-edge . -6)
    (top-edge . 19)
    (bottom-edge . 19)
    (class . left-border))

   ;; right border
   ((background . ,microGUI:right-images)
    (right-edge . -6)
    (top-edge . 0)
    (bottom-edge . 0)
    (class . right-border))

   ;; bottom border
   ((background . ,microGUI:bottom-images)
    (left-edge . 17)
    (right-edge . 19)
    (bottom-edge . -6)
    (class . bottom-border))

   ;; bottom-left corner
   ((background . ,microGUI:bottom-left-images)
    (left-edge . -6)
    (bottom-edge . -6)
    (class . bottom-left-corner))

   ;; bottom-right corner
   ((background . ,microGUI:bottom-right-images)
    (right-edge . -6)
    (bottom-edge . -6)
    (class . bottom-right-corner))
))


(defun microGUI:frame-style (w type)
  (cond ((eq type 'shaped)
	 'microGUI:shaped-frame)
	((eq type 'transient)
	 'microGUI:transient-frame)
	((eq type 'shaped-transient)
	 'nil-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'microGUI:frame)))

;; Update window title pixel length
(defun microGUI:update-text-width (w atom type)
  (if (eq atom 'WM_NAME)
      (rebuild-frame w)))

(defun microGUI:title-width (w)
  (text-width (window-name w)))

(defun microGUI:init ()
  (add-hook 'property-notify-hook 'microGUI:update-text-width))

(add-frame-style 'microGUI 'microGUI:frame-style)


;; initialization

(unless batch-mode
  (microGUI:init))
