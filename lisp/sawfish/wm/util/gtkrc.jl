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
(defvar gtkrc-background-pixmaps nil)
(defvar gtkrc-base nil)
(defvar gtkrc-foreground nil)
(defvar gtkrc-font nil)

(defvar gtkrc-loaded-pixmaps nil)

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
    (setq gtkrc-background nil)
    (setq gtkrc-background-pixmaps nil)
    (setq gtkrc-base nil)
    (setq gtkrc-foreground nil)
    (setq gtkrc-font nil)
    (setq gtkrc-loaded-pixmaps nil)
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
      (setq gtkrc-background (list (cdr (assq 'normal tem))
				   (cdr (assq 'prelight tem))
				   (cdr (assq 'active tem))
				   (cdr (assq 'selected tem)))))
    (when (setq tem (cdr (assq 'base gtkrc-style)))
      (setq gtkrc-base (list (cdr (assq 'normal tem))
			     (cdr (assq 'prelight tem))
			     (cdr (assq 'active tem))
			     (cdr (assq 'selected tem)))))
    (when (setq tem (cdr (assq 'bg-pixmap gtkrc-style)))
      (setq gtkrc-background-pixmaps (list (cdr (assq 'normal tem))
					   (cdr (assq 'prelight tem))
					   (cdr (assq 'active tem))
					   (cdr (assq 'selected tem)))))
    (mapc #'(lambda (var)
	      (when (symbol-value var)
		(set var (mapcar #'(lambda (x)
				     (and x (get-color x)))
				 (symbol-value var)))))
	  '(gtkrc-background gtkrc-base gtkrc-foreground))))

;; if a theme want's to use the pixmaps, it must call this function first
(defun gtkrc-load-pixmaps ()
  (when (and gtkrc-background-pixmaps (not gtkrc-loaded-pixmaps))
    (setq gtkrc-background-pixmaps
	  (mapcar #'(lambda (x)
		      (when x
			(setq x (make-image (gtkrc-fix-image-name x)))
			(image-put x 'tiled t)
			x))
		  gtkrc-background-pixmaps))
    (setq gtkrc-loaded-pixmaps t)))

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


;; for restricted themes

(let
    (funs)

  (defun gtkrc-call-after-changed (fun)
    (or (closurep fun) (error "Non-closure to gtkrc-call-after-changed"))
    (setq funs (cons fun funs)))

  (add-hook 'gtkrc-changed-hook #'(lambda () (mapc 'funcall funs))))


;; init

(defun gtkrc-quit ()
  (destroy-window gtkrc-dummy-window))

(unless batch-mode
  (setq gtkrc-dummy-window (create-window 'root -100 -100 10 10))
  (set-x-property gtkrc-dummy-window 'WM_STATE (vector 0) 'WM_STATE 32)
  (add-hook 'client-message-hook 'gtkrc-handle-client-msg)
  (add-hook 'before-exit-hook 'gtkrc-quit)
  (mapc 'gaol-add-function
	'(gtkrc-load-pixmaps gtkrc-reload-style gtkrc-call-after-changed))
  (mapc 'gaol-add-special '(gtkrc-background gtkrc-background-pixmaps
			    gtkrc-base gtkrc-foreground gtkrc-font))
  (gtkrc-reload-style))
