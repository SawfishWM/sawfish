;; move-resize.jl -- interactive moving and resizing of windows
;; $Id$

(provide 'move-resize)

;; todo:
;;  * obey the size hints (min/max and increments)
;;  * resize has truly bizarre behaviour

(defvar move-outline-mode 'opaque)
(defvar resize-outline-mode 'opaque)

(defvar move-resize-map (bind-keys (make-sparse-keymap)
			  "Any-PointerUp" 'move-resize-finished
			  "Any-PointerMove" 'move-resize-motion))

(defvar move-resize-raise-window nil)

;; specials
(defvar move-resize-window nil)
(defvar move-resize-function nil)
(defvar move-resize-x nil)
(defvar move-resize-y nil)
(defvar move-resize-width nil)
(defvar move-resize-height nil)
(defvar move-resize-old-x nil)
(defvar move-resize-old-y nil)
(defvar move-resize-old-width nil)
(defvar move-resize-old-height nil)
(defvar move-resize-old-ptr-x nil)
(defvar move-resize-old-ptr-y nil)
(defvar move-resize-mode nil)
(defvar move-resize-hints nil)
(defvar move-resize-frame nil)

(defun move-resize-window (w function)
  (when move-resize-raise-window
    (raise-window w))
  (let*
      ((override-keymap move-resize-map)
       (move-resize-window w)
       (move-resize-function function)
       (move-resize-old-x (car (window-position w)))
       (move-resize-old-y (cdr (window-position w)))
       (move-resize-old-width (car (window-dimensions w)))
       (move-resize-old-height (cdr (window-dimensions w)))
       (move-resize-x move-resize-old-x)
       (move-resize-y move-resize-old-y)
       (move-resize-width move-resize-old-width)
       (move-resize-height move-resize-old-height)
       (move-resize-old-ptr-x (car (query-pointer)))
       (move-resize-old-ptr-y (cdr (query-pointer)))
       (move-resize-hints (window-size-hints w))
       (move-resize-frame (cons (- (car (window-frame-dimensions w))
				   move-resize-old-width)
				(- (cdr (window-frame-dimensions w))
				   move-resize-old-height)))
       (move-resize-mode (if (eq function 'move)
			     move-outline-mode
			   resize-outline-mode)))
    (unless (eq move-resize-mode 'opaque)
      ;; prevent any other programs drawing on the display
      (grab-server))
    (unwind-protect
	(progn
	  ;; ensure that we catch _all_ mouse events
	  (grab-pointer w)
	  (unwind-protect
	      (progn
		(unless (eq move-resize-mode 'opaque)
		  (draw-window-outline move-resize-mode
				       move-resize-x move-resize-y
				       (+ move-resize-width
					  (car move-resize-frame))
				       (+ move-resize-height
					  (cdr move-resize-frame))))
		(catch 'move-resize-done
		  (recursive-edit))))
	  (ungrab-pointer)))
    (unless (eq move-resize-mode 'opaque)
      (ungrab-server))))

(defsubst move-resize-roundup (x inc base)
  (+ base (max 0 (* (1+ (/ (1- (- x base)) inc)) inc))))

(defun move-resize-motion ()
  (interactive)
  (let
      ((ptr-x (car (query-pointer)))
       (ptr-y (cdr (query-pointer))))
    (unless (eq move-resize-mode 'opaque)
      (erase-window-outline (if (eq move-resize-function 'move)
				move-outline-mode
			      resize-outline-mode)
			    move-resize-x move-resize-y
			    (+ move-resize-width (car move-resize-frame))
			    (+ move-resize-height (cdr move-resize-frame))))
    (cond ((eq move-resize-function 'move)
	   (setq move-resize-x (+ move-resize-old-x
				  (- ptr-x move-resize-old-ptr-x)))
	   (setq move-resize-y (+ move-resize-old-y
				  (- ptr-y move-resize-old-ptr-y))))
	  ((eq move-resize-function 'resize)
	   (setq move-resize-width
		 (move-resize-roundup
		  (+ move-resize-old-width (- ptr-x move-resize-old-ptr-x))
		  (or (cdr (assq 'width-inc move-resize-hints)) 1)
		  (or (cdr (or (assq 'base-width move-resize-hints)
			       (assq 'min-width move-resize-hints))) 1)))
	   (setq move-resize-height
		 (move-resize-roundup
		  (+ move-resize-old-height (- ptr-y move-resize-old-ptr-y))
		  (or (cdr (assq 'height-inc move-resize-hints)) 1)
		  (or (cdr (or (assq 'base-height move-resize-hints)
			       (assq 'min-height move-resize-hints))) 1)))))
    (if (eq move-resize-mode 'opaque)
	(move-resize-apply)
      (draw-window-outline (if (eq move-resize-function 'move)
			       move-outline-mode
			     resize-outline-mode)
			   move-resize-x move-resize-y
			   (+ move-resize-width (car move-resize-frame))
			   (+ move-resize-height (cdr move-resize-frame))))))

(defun move-resize-finished ()
  (interactive)
  (unless (eq move-resize-mode 'opaque)
    (erase-window-outline (if (eq move-resize-function 'move)
			      move-outline-mode
			    resize-outline-mode)
			  move-resize-x move-resize-y
			  (+ move-resize-width (car move-resize-frame))
			  (+ move-resize-height (cdr move-resize-frame))))
  (move-resize-apply)
  (throw 'move-resize-done t))

(defun move-resize-apply ()
  (cond ((eq move-resize-function 'move)
	 (move-window-to move-resize-window move-resize-x move-resize-y))
	((eq move-resize-function 'resize)
	 (resize-window-to move-resize-window
			   move-resize-width move-resize-height))))


;; Entry points

;;;###autoload
(defun move-window-interactively (w)
  (interactive "f")
  (move-resize-window w 'move))

;;;###autoload
(defun resize-window-interactively (w)
  (interactive "f")
  (move-resize-window w 'resize))

;;;###autoload
(defun move-selected-window ()
  (interactive)
  (let
      ((w (select-window)))
    (if w
	(move-window-interactively w)
      (beep))))

;;;###autoload
(defun resize-selected-window ()
  (interactive)
  (let
      ((w (select-window)))
    (if w
	(resize-window-interactively w)
      (beep))))
