;; simple.jl
;; $Id$

(defvar simple-frame (make-frame "simple"))
(frame-put simple-frame 'unshaped t)

(defvar simple-frame-colors '("lightsteelblue4" "lightsteelblue2"))

(let
    ((image-load-path (cons (expand-file-name "misc" image-directory)
			    image-load-path)))
  ;; 15x15
  (defvar simple-minimize (make-image "as_min.png"))
  (defvar simple-minimize-clicked (make-image "as_min-b.png"))
  (defvar simple-close (make-image "as_close.png"))
  (defvar simple-close-clicked (make-image "as_close-b.png")))

(set-frame-generator simple-frame
 `(;; title bar
   ((background . ,simple-frame-colors)
    (foreground . "black")
    (text . window-name)
    (x-justify . 30)
    (y-justify . center)
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -21)
    (height . 21)
    (keymap . title-keymap))
   ;; title frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (top-edge . -22)
    (height . 1))
   ;; left frame
   ((background . "black")
    (left-edge . -1)
    (width . 1)
    (top-edge . -22)
    (bottom-edge . -5))
   ;; right frame
   ((background . "black")
    (right-edge . -1)
    (width . 1)
    (top-edge . -22)
    (bottom-edge . -5))
   ;; bottom bar
   ((background . ,simple-frame-colors)
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -4)
    (height . 4)
    (keymap . title-keymap))
   ;; bottom frame
   ((background . "black")
    (left-edge . 0)
    (right-edge . 0)
    (bottom-edge . -5)
    (height . 1))
   ;; minimize button
   ((background . ,(list simple-minimize simple-minimize
			 simple-minimize-clicked simple-minimize-clicked))
    (left-edge . 5)
    (top-edge . -18)
    (keymap . iconify-button-keymap))
   ;; close button
   ((background . ,(list simple-close simple-close
			 simple-close-clicked simple-close-clicked))
    (right-edge . 5)
    (top-edge . -18)
    (keymap . close-button-keymap))))

(setq default-frame simple-frame)
