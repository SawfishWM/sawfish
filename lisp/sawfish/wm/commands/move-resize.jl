;; move-resize.jl -- interactive moving and resizing of windows
;; $Id$

(provide 'move-resize)

;; todo:
;;  * obey the size hints (min/max and increments)
;;  * resize has truly bizarre behaviour
;;  * opaque modes

(defvar move-outline-mode nil)
(defvar resize-outline-mode nil)

(defvar move-resize-map (bind-keys (make-sparse-keymap)
			  "Any-PointerUp" 'move-resize-finished
			  "Any-PointerMove" 'move-resize-motion))

;; specials
(defvar move-resize-window nil)
(defvar move-resize-function nil)
(defvar move-resize-last-x nil)
(defvar move-resize-last-y nil)
(defvar move-resize-last-width nil)
(defvar move-resize-last-height nil)
(defvar move-resize-last-ptr-x nil)
(defvar move-resize-last-ptr-y nil)

(defun move-resize-window (w function)
  ;; prevent any other programs drawing on the display
  (grab-server)
  (unwind-protect
      (progn
	;; ensure that we catch _all_ mouse events
	(grab-pointer w)
	(unwind-protect
	    (progn
	      (let
		  ((override-keymap move-resize-map)
		   (move-resize-window w)
		   (move-resize-function function)
		   (move-resize-last-x (car (window-position w)))
		   (move-resize-last-y (cdr (window-position w)))
		   (move-resize-last-width (car (window-frame-dimensions w)))
		   (move-resize-last-height (cdr (window-frame-dimensions w)))
		   (move-resize-last-ptr-x (car (query-pointer)))
		   (move-resize-last-ptr-y (cdr (query-pointer))))
		(draw-window-outline (if (eq function 'move)
					 move-outline-mode
				       resize-outline-mode)
				     move-resize-last-x
				     move-resize-last-y
				     move-resize-last-width
				     move-resize-last-height)
		(catch 'move-resize-done
		  (recursive-edit))))
	  (ungrab-pointer)))
    (ungrab-server)))

(defun move-resize-motion ()
  (interactive)
  (let
      ((ptr-x (car (query-pointer)))
       (ptr-y (cdr (query-pointer))))
    (when (or (/= ptr-x move-resize-last-ptr-x)
	      (/= ptr-y move-resize-last-ptr-y))
      (erase-window-outline (if (eq move-resize-function 'move)
				move-outline-mode
			      resize-outline-mode)
			    move-resize-last-x
			    move-resize-last-y
			    move-resize-last-width
			    move-resize-last-height)
      (cond ((eq move-resize-function 'move)
	     (setq move-resize-last-x (+ move-resize-last-x
					 (- ptr-x move-resize-last-ptr-x)))
	     (setq move-resize-last-y (+ move-resize-last-y
					 (- ptr-y move-resize-last-ptr-y))))
	    ((eq move-resize-function 'resize)
	     (setq move-resize-last-width
		   (max 0 (+ move-resize-last-width
			     (- ptr-x move-resize-last-ptr-x))))
	     (setq move-resize-last-height
		   (max 0 (+ move-resize-last-height
			     (- ptr-y move-resize-last-ptr-y))))))
      (setq move-resize-last-ptr-x ptr-x)
      (setq move-resize-last-ptr-y ptr-y)
      (draw-window-outline (if (eq move-resize-function 'move)
			       move-outline-mode
			     resize-outline-mode)
			   move-resize-last-x
			   move-resize-last-y
			   move-resize-last-width
			   move-resize-last-height))))

(defun move-resize-finished ()
  (interactive)
  (erase-window-outline (if (eq move-resize-function 'move)
			    move-outline-mode
			  resize-outline-mode)
			move-resize-last-x
			move-resize-last-y
			move-resize-last-width
			move-resize-last-height)
  (cond ((eq move-resize-function 'move)
	 (move-window-to move-resize-window
			 move-resize-last-x move-resize-last-y))
	((eq move-resize-function 'resize)
	 (resize-window-to move-resize-window
			   move-resize-last-width move-resize-last-height)))
  (throw 'move-resize-done t))


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
