;; gtkrc.jl -- code to read current gtkrc settings
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

(provide 'gtkrc)

(defvar gtkrc-style-program
  (expand-file-name "gtk-style" sawmill-exec-directory))

(defvar gtkrc-style nil)

(defvar gtkrc-background nil)
(defvar gtkrc-background-colors nil)
(defvar gtkrc-background-images nil)	;for bg colors
(defvar gtkrc-font nil)
(defvar gtkrc-foreground nil)

(defvar gtkrc-dummy-window nil)

(defvar gtkrc-changed-hook nil)

;; read the current default style
(defun gtkrc-get-style ()
  (let*
      ((output (make-string-output-stream))
       (process (make-process output)))
    (set-process-error-stream process nil)
    (unless (zerop (call-process process nil gtkrc-style-program))
      (error "Can't start gtkrc-style-program"))
    (setq output (make-string-input-stream (get-output-stream-string output)))
    (setq gtkrc-style nil)
    (condition-case nil
	(while t
	  (setq gtkrc-style (cons (read output) gtkrc-style)))
      (end-of-stream
       (setq gtkrc-style (nreverse gtkrc-style))))))

(defun gtkrc-fix-image-name (str)
  (while (string-match "//" str)
    (setq str (concat (substring str 0 (match-start))
		      ?/ (substring str (match-end)))))
  str)

;; act on settings in gtkrc-style alist
(defun gtkrc-apply-style ()
  (let
      (tem i)
    (setq gtkrc-background-images nil)
    (setq gtkrc-background-colors nil)
    (setq gtkrc-foreground nil)
    (setq gtkrc-font nil)
    (when (setq tem (cdr (assq 'font gtkrc-style)))
      (setq gtkrc-font (condition-case nil
			   (get-font tem)
			 (error
			  default-font))))
    (when (setq tem (cdr (assq 'fg gtkrc-style)))
      (setq gtkrc-foreground (list (cdr (assq 'normal tem))
				   (cdr (assq 'prelight tem))
				   (cdr (assq 'active tem))
				   (cdr (assq 'selected tem)))))
    (when (setq tem (cdr (assq 'bg gtkrc-style)))
      (setq gtkrc-background-colors (list (cdr (assq 'normal tem))
					  (cdr (assq 'prelight tem))
					  (cdr (assq 'active tem))
					  (cdr (assq 'selected tem))))
      (setq gtkrc-background-colors (mapcar #'(lambda (x)
						(and x (get-color x)))
					    gtkrc-background-colors)))
    (cond ((setq tem (cdr (assq 'bg-pixmap gtkrc-style)))
	   (setq gtkrc-background (list (cdr (assq 'normal tem))
					(cdr (assq 'prelight tem))
					(cdr (assq 'active tem))
					(cdr (assq 'selected tem))))
	   (setq gtkrc-background
		 (mapcar #'(lambda (x)
			     (when x
			       (setq x (make-image (gtkrc-fix-image-name x)))
			       (image-put x 'tiled t)
			       x))
			 gtkrc-background)))
	  (gtkrc-background-colors
	   (setq gtkrc-background gtkrc-background)
	   (setq i -1)
	   (setq gtkrc-background-images
		 (mapcar #'(lambda (x)
			     (setq i (1+ i))
			     (and (colorp x)
				  (progn
				    (setq x (make-sized-image 16 16 x))
				    (bevel-image x 1 (/= i 3))
				    (set-image-border x 1 1 1 1)
				    x)))
			 gtkrc-background))))))

(defun gtkrc-reload-style ()
  "Reload the gtkrc settings."
  (interactive)
  (gtkrc-get-style)
  (gtkrc-apply-style)
  (call-hook 'gtkrc-changed-hook))

;; recognize when the GTK theme has been switched
(defun gtkrc-handle-client-msg (w type data)
  (when (and (eq w gtkrc-dummy-window) (eq type '_GTK_READ_RCFILES))
    (gtkrc-reload-style)
    (when (and (featurep 'menus) (not menu-active))
      (menu-stop-process t))
    t))

;; for pixmap frames; this is going to use horrendous amounts of memory,
;; but what other options are there..?
(defun gtkrc-render-bg (img state)
  (let
      ((bg (cond ((eq state nil)
		  (nth 0 gtkrc-background))
		 ((eq state 'focused)
		  (nth 1 gtkrc-background))
		 ((eq state 'highlighted)
		  (nth 2 gtkrc-background))
		 (t
		  (nth 3 gtkrc-background)))))
    (tile-image img bg)
    (bevel-image img 1 (not (eq state 'clicked)))))

(defun gtkrc-foreground ()
  gtkrc-foreground)

(defun gtkrc-background ()
  (if gtkrc-background-images
      (cons 'background gtkrc-background-images)
    (cons 'renderer 'gtkrc-render-bg)))


;; init

(defun gtkrc-quit ()
  (destroy-window gtkrc-dummy-window))

(unless batch-mode
  (setq gtkrc-dummy-window (create-window 'root -100 -100 10 10))
  (set-x-property gtkrc-dummy-window 'WM_STATE (vector 0) 'WM_STATE 32)
  (add-hook 'client-message-hook 'gtkrc-handle-client-msg)
  (add-hook 'before-exit-hook 'gtkrc-quit)
  (gtkrc-reload-style))
