;; absolute-e.jl
;; $Id$

;; The images and design of this theme are from Hallvar Helleseth's
;; Absolute E theme for Enlightenment

;; from the ABOUT file:

;;    Thanks for using Absolute E, the theme that thinks diffrently.
;;    This theme is created by Hallvar Helleseth using nothing but The
;;    Gimp.  Everything is originaly made by me - Propably the only
;;    Etheme that can say that ;) Though alot of people have helped me
;;    (too many to list here/can't remeber all of them either...) But I
;;    give them a BIG thank you!

;;  There is a GTK and WMaker version of this theme available at
;;  themes.org

;;  This theme is going to be following the cvs version of E and not
;;  the stable version (DR0.15.5)

;;  Reach me at hallvar@ii.uib.no (www.ii.uib.no/~hallvar)

;;  Date: July 19 1999.

(provide 'absolute-e)

(let
    ((image-load-path (cons (expand-file-name "absolute-e" image-directory)
			    image-load-path)))
  ;; 100x16
  (defvar absolute-e-bar-images
    (mapcar #'(lambda (i)
		(when i
		  (set-image-border i 4 4 4 4))
		i)
	    (list (make-image "bar_normal.png")
		  (make-image "bar_normal_active.png")
		  nil
		  (make-image "bar_clicked_active.png")))))

(defun absolute-e-title-width (w)
  (let
      ((w-width (car (window-dimensions w))))
    (- (min (max (/ w-width 2) 100) w-width) 16)))

(defvar absolute-e-frame (make-frame "absolute-e"))

(set-frame-generator absolute-e-frame
 `(;; iconify button
   ((background . ,absolute-e-bar-images)
    (left-edge . 0)
    (width . 16)
    (top-edge . -16)
    (height . 16)
    (keymap . iconify-button-keymap))
   ;; title bar
   ((background . ,absolute-e-bar-images)
    (foreground . "white")
    (text . window-name)
    (x-justify . center)
    (y-justify . center)
    (left-edge . 16)
    (width . absolute-e-title-width)
    (top-edge . -16)
    (keymap . title-keymap)
    (cursor . hand2))
   ;; rhs bit of title
   ((background . ,absolute-e-bar-images)
    (left-edge . (lambda (w)
		   (+ (absolute-e-title-width w) 16)))
    (right-edge . 0)
    (top-edge . -4)
    (height . 4)
    (keymap . title-keymap)
    (cursor . hand2))
   ;; left frame
   ((background . ,absolute-e-bar-images)
    (right-edge . -4)
    (width . 4)
    (top-edge . 0)
    (bottom-edge . 0)
    (keymap . title-keymap)
    (cursor . hand2))
   ;; right frame
   ((background . ,absolute-e-bar-images)
    (left-edge . -4)
    (width . 4)
    (top-edge . 0)
    (bottom-edge . 0)
    (keymap . title-keymap)
    (cursor . hand2))
   ;; bottom frame
   ((background . ,absolute-e-bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (height . 4)
    (keymap . title-keymap)
    (cursor . hand2))
   ;; top-left corner
   ((background . ,absolute-e-bar-images)
    (left-edge . -4)
    (width . 4)
    (top-edge . -4)
    (height . 4))
   ;; top-right corner
   ((background . ,absolute-e-bar-images)
    (right-edge . -4)
    (width . 4)
    (top-edge . -4)
    (height . 4))
   ;; bottom-left corner
   ((background . ,absolute-e-bar-images)
    (left-edge . -4)
    (width . 4)
    (bottom-edge . -4)
    (height . 4))
   ;; bottom-right corner
   ((background . ,absolute-e-bar-images)
    (right-edge . -4)
    (width . 4)
    (bottom-edge . -4)
    (height . 4))))

(defvar absolute-e-transient-frame (make-frame "absolute-e-transient"))

(set-frame-generator absolute-e-transient-frame
 `(((background . ,absolute-e-bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -4)
    (height . 4)
    (keymap . title-keymap)
    (cursor . hand2))
   ((background . ,absolute-e-bar-images)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (height . 4)
    (keymap . title-keymap)
    (cursor . hand2))
   ((background . ,absolute-e-bar-images)
    (left-edge . -4)
    (width . 4)
    (top-edge . 0)
    (bottom-edge . 0)
    (keymap . title-keymap)
    (cursor . hand2))
   ((background . ,absolute-e-bar-images)
    (right-edge . -4)
    (width . 4)
    (top-edge . 0)
    (bottom-edge . 0)
    (keymap . title-keymap)
    (cursor . hand2))
   ;; top-right corner
   ((background . ,absolute-e-bar-images)
    (right-edge . -4)
    (width . 4)
    (top-edge . -4)
    (height . 4))
   ;; top-left corner
   ((background . ,absolute-e-bar-images)
    (left-edge . -4)
    (width . 4)
    (top-edge . -4)
    (height . 4))
   ;; bottom-left corner
   ((background . ,absolute-e-bar-images)
    (left-edge . -4)
    (width . 4)
    (bottom-edge . -4)
    (height . 4))
   ;; bottom-right corner
   ((background . ,absolute-e-bar-images)
    (right-edge . -4)
    (width . 4)
    (bottom-edge . -4)
    (height . 4))))

(setq default-frame absolute-e-frame)
(setq transient-frame absolute-e-transient-frame)
