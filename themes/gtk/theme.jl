;; gtk/theme.jl
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

(defvar gtk-style-program
  (expand-file-name "gtk-style" sawmill-exec-directory))

(defvar gtk-style nil)

(defvar gtk-background nil)
(defvar gtk-background-images nil)	;for bg colors
(defvar gtk-font nil)
(defvar gtk-foreground nil)

(defvar gtk-dummy-window nil)

;; 15x15
(defvar gtk-minimize (list (make-image "as_min.png")
			   nil nil (make-image "as_min-b.png")))
(defvar gtk-close (list (make-image "as_close.png")
			nil nil (make-image "as_close-b.png")))

;; read the current default style
(defun gtk-get-style ()
  (let*
      ((output (make-string-output-stream))
       (process (make-process output)))
    (set-process-error-stream process nil)
    (unless (zerop (call-process process nil gtk-style-program))
      (error "Can't start gtk-style-program"))
    (setq output (make-string-input-stream (get-output-stream-string output)))
    (setq gtk-style nil)
    (condition-case nil
	(while t
	  (setq gtk-style (cons (read output) gtk-style)))
      (end-of-stream
       (setq gtk-style (nreverse gtk-style))))))

(defun gtk-fix-image-name (str)
  (while (string-match "//" str)
    (setq str (concat (substring str 0 (match-start))
		      ?/ (substring str (match-end)))))
  str)

;; act on settings in gtk-style alist
(defun gtk-apply-style ()
  (let
      (tem i)
    (setq gtk-background-images nil)
    (when (setq tem (cdr (assq 'font gtk-style)))
      (setq gtk-font (get-font tem)))
    (when (setq tem (cdr (assq 'fg gtk-style)))
      (setq gtk-foreground (list (cdr (assq 'normal tem))
				 (cdr (assq 'prelight tem))
				 (cdr (assq 'active tem))
				 (cdr (assq 'selected tem)))))
    (cond ((setq tem (cdr (assq 'bg-pixmap gtk-style)))
	   (setq gtk-background (list (cdr (assq 'normal tem))
				      (cdr (assq 'prelight tem))
				      (cdr (assq 'active tem))
				      (cdr (assq 'selected tem))))
	   (setq gtk-background
		 (mapcar #'(lambda (x)
			     (when x
			       (setq x (make-image (gtk-fix-image-name x)))
			       (image-put x 'tiled t)
			       x))
			 gtk-background)))
	  ((setq tem (cdr (assq 'bg gtk-style)))
	   (setq gtk-background (list (cdr (assq 'normal tem))
				      (cdr (assq 'prelight tem))
				      (cdr (assq 'active tem))
				      (cdr (assq 'selected tem))))
	   (setq gtk-background (mapcar #'(lambda (x)
					    (and x (get-color x)))
					gtk-background))
	   (setq i -1)
	   (setq gtk-background-images
		 (mapcar #'(lambda (x)
			     (setq i (1+ i))
			     (and (colorp x)
				  (progn
				    (setq x (make-sized-image 16 16 x))
				    (bevel-image x 1 (/= i 3))
				    (set-image-border x 1 1 1 1)
				    x)))
			 gtk-background))))))

(defun gtk-rebuild-frames ()
  (mapc #'(lambda (w)
	    (when (eq (window-get w 'current-frame-style) 'gtk)
	      (set-window-frame-style w 'gtk)))
	(managed-windows)))

(defun gtk-reload-style ()
  (interactive)
  (gtk-get-style)
  (gtk-apply-style)
  (gtk-construct-frame-defs)
  (gtk-rebuild-frames))

;; recognize when the GTK theme has been switched
(defun gtk-handle-client-msg (w type data)
  (when (and (eq w gtk-dummy-window) (eq type '_GTK_READ_RCFILES))
    (gtk-reload-style)
    (when (and (featurep 'menus) (not menu-active))
      (menu-stop-process t))
    t))

(defun gtk-init ()
  (setq gtk-dummy-window (create-window 'root -100 -100 10 10))
  (set-x-property gtk-dummy-window 'WM_STATE (vector 0) 'WM_STATE 32)
  (add-hook 'client-message-hook 'gtk-handle-client-msg)
  (add-hook 'before-exit-hook 'gtk-quit)
  (gtk-reload-style))

(defun gtk-quit ()
  (destroy-window gtk-dummy-window))

;; for pixmap frames; this is going to use horrendous amounts of memory,
;; but what other options are there..?
(defun gtk-render-bg (img state)
  (let
      ((bg (cond ((eq state nil)
		  (nth 0 gtk-background))
		 ((eq state 'focused)
		  (nth 1 gtk-background))
		 ((eq state 'highlighted)
		  (nth 2 gtk-background))
		 (t
		  (nth 3 gtk-background)))))
    (tile-image img bg)
    (bevel-image img 1 (not (eq state 'clicked)))))

(defun gtk-foreground ()
  gtk-foreground)

(defun gtk-background ()
  (if gtk-background-images
      (cons 'background gtk-background-images)
    (cons 'renderer 'gtk-render-bg)))


;; frame defs

(defvar gtk-frame nil)
(put 'gtk-frame 'unshaped t)

(defvar gtk-shaped-frame nil)
(put 'gtk-shaped-frame 'unshaped t)

(defvar gtk-transient-frame nil)
(put 'gtk-transient-frame 'unshaped t)

(defvar gtk-shaped-transient-frame nil)
(put 'gtk-transient-shaped-frame 'unshaped t)

(defun gtk-construct-frame-defs ()
  (setq gtk-frame
	`(;; title bar
	 (,(gtk-background)
	  (foreground . gtk-foreground)
	  (text . window-name)
	  (x-justify . 30)
	  (y-justify . center)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -21)
	  (height . 21)
	  (class . title))
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
	 (,(gtk-background)
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -4)
	  (height . 4)
	  (class . bottom-border))
	 ;; bottom frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -5)
	  (height . 1))
	 ;; minimize button
	 ((background . ,gtk-minimize)
	  (left-edge . 4)
	  (top-edge . -18)
	  (class . iconify-button)
	  (removable . t))
	 ;; close button
	 ((background . ,gtk-close)
	  (right-edge . 4)
	  (top-edge . -18)
	  (class . close-button)
	  (removable . t))))

  (setq gtk-shaped-frame
	`(;; title bar
	 (,(gtk-background)
	  (foreground . gtk-foreground)
	  (text . window-name)
	  (x-justify . 30)
	  (y-justify . center)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -22)
	  (height . 21)
	  (class . title))
	 ;; title frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -23)
	  (height . 1))
	 ;; left frame
	 ((background . "black")
	  (left-edge . -1)
	  (width . 1)
	  (top-edge . -23)
	  (height . 23))
	 ;; right frame
	 ((background . "black")
	  (right-edge . -1)
	  (width . 1)
	  (top-edge . -23)
	  (height . 23))
	 ;; bottom frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -1)
	  (height . 1))
	 ;; minimize button
	 ((background . ,gtk-minimize)
	  (left-edge . 5)
	  (top-edge . -19)
	  (class . iconify-button)
	  (removable . t))
	 ;; close button
	 ((background . ,gtk-close)
	  (right-edge . 5)
	  (top-edge . -19)
	  (class . close-button)
	  (removable . t))))

  (setq gtk-transient-frame
	`(;; title bar
	 (,(gtk-background)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -4)
	  (height . 4)
	  (class . title))
	 ;; title frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -5)
	  (height . 1))
	 ;; left frame
	 ((background . "black")
	  (left-edge . -1)
	  (width . 1)
	  (top-edge . -5)
	  (bottom-edge . -5))
	 ;; right frame
	 ((background . "black")
	  (right-edge . -1)
	  (width . 1)
	  (top-edge . -5)
	  (bottom-edge . -5))
	 ;; bottom bar
	 (,(gtk-background)
	  (render-scale . 2)
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -4)
	  (height . 4)
	  (class . bottom-border))
	 ;; bottom frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (bottom-edge . -5)
	  (height . 1))))

  (setq gtk-shaped-transient-frame
	`(;; title bar
	 (,(gtk-background)
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -5)
	  (height . 4)
	  (class . title))
	 ;; title frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -6)
	  (height . 1))
	 ;; left frame
	 ((background . "black")
	  (left-edge . -1)
	  (width . 1)
	  (top-edge . -6)
	  (height . 6))
	 ;; right frame
	 ((background . "black")
	  (right-edge . -1)
	  (width . 1)
	  (top-edge . -6)
	  (height . 6))
	 ;; bottom frame
	 ((background . "black")
	  (left-edge . 0)
	  (right-edge . 0)
	  (top-edge . -1)
	  (height . 1)))))

(defun gtk-frame-style (w type)
  (cond ((eq type 'shaped)
	 'gtk-shaped-frame)
	((eq type 'transient)
	 'gtk-transient-frame)
	((eq type 'shaped-transient)
	 'gtk-shaped-transient-frame)
	((eq type 'unframed)
	 'nil-frame)
	(t
	 'gtk-frame)))

(add-frame-style 'gtk 'gtk-frame-style)


;; initialisation

(unless batch-mode
  (gtk-init))
