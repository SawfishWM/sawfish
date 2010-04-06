;; gtkrc.jl -- code to read current gtkrc settings

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.util.gtkrc

    (export gtkrc-style
	    gtkrc-background
	    gtkrc-background-pixmaps
	    gtkrc-base
	    gtkrc-light
	    gtkrc-dark
	    gtkrc-mid
	    gtkrc-foreground
	    gtkrc-font
	    gtkrc-load-pixmaps
	    gtkrc-reload-style
	    gtkrc-call-after-changed)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.files
	  rep.io.processes
	  sawfish.wm.colors
	  sawfish.wm.fonts
	  sawfish.wm.images
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.gaol)

  (define-structure-alias gtkrc sawfish.wm.util.gtkrc)

  (defvar gtkrc-style-program
    (expand-file-name "gtk-style" sawfish-exec-directory))

  (define gtkrc-style nil)

  (define gtkrc-background nil)
  (define gtkrc-background-pixmaps nil)
  (define gtkrc-base nil)
  (define gtkrc-light nil)
  (define gtkrc-dark nil)
  (define gtkrc-mid nil)
  (define gtkrc-foreground nil)
  (define gtkrc-font nil)

  (define gtkrc-loaded-pixmaps nil)

  (define gtkrc-dummy-window nil)

  (define gtkrc-changed-hook nil)

  ;; read the current default style
  (define (gtkrc-get-style)
    (let* ((output (make-string-output-stream))
	   (process (make-process output)))
      (set-process-error-stream process nil)
      ;; XXX the gtk-style program can't connect to the X server
      ;; XXX if it's currently grabbed, hence this kludge..
      (call-with-server-ungrabbed
       (lambda ()
	 (unless (zerop (call-process process nil gtkrc-style-program))
	   (error "Can't start gtkrc-style-program: `%s'."
		  gtkrc-style-program))))
      (setq output (make-string-input-stream
		    (get-output-stream-string output)))
      (setq gtkrc-style nil)
      (condition-case nil
	  (while t
	    (setq gtkrc-style (cons (read output) gtkrc-style)))
	(end-of-stream
	 (setq gtkrc-style (nreverse gtkrc-style))))))

  (define (gtkrc-fix-image-name str)
    (while (string-match "//" str)
      (setq str (concat (substring str 0 (match-start))
			?/ (substring str (match-end)))))
    str)

  ;; act on settings in gtkrc-style alist
  (define (gtkrc-apply-style)
    (let (tem)
      (setq gtkrc-background nil)
      (setq gtkrc-background-pixmaps nil)
      (setq gtkrc-base nil)
      (setq gtkrc-light nil)
      (setq gtkrc-dark nil)
      (setq gtkrc-mid nil)
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
      (when (setq tem (cdr (assq 'light gtkrc-style)))
	(setq gtkrc-light (list (cdr (assq 'normal tem))
				(cdr (assq 'prelight tem))
				(cdr (assq 'active tem))
				(cdr (assq 'selected tem)))))
      (when (setq tem (cdr (assq 'dark gtkrc-style)))
	(setq gtkrc-dark (list (cdr (assq 'normal tem))
			       (cdr (assq 'prelight tem))
			       (cdr (assq 'active tem))
			       (cdr (assq 'selected tem)))))
      (when (setq tem (cdr (assq 'mid gtkrc-style)))
	(setq gtkrc-mid (list (cdr (assq 'normal tem))
			      (cdr (assq 'prelight tem))
			      (cdr (assq 'active tem))
			      (cdr (assq 'selected tem)))))
      (when (setq tem (cdr (assq 'bg-pixmap gtkrc-style)))
	(setq gtkrc-background-pixmaps (list (cdr (assq 'normal tem))
					     (cdr (assq 'prelight tem))
					     (cdr (assq 'active tem))
					     (cdr (assq 'selected tem)))))
      (mapc (lambda (var)
	      (when (symbol-value var)
		(set var (mapcar (lambda (x)
				   (and x (get-color x)))
				 (symbol-value var)))))
	    '(gtkrc-background gtkrc-base gtkrc-foreground))))

  ;; if a theme want's to use the pixmaps, it must call this function first
  (define (gtkrc-load-pixmaps)
    (when (and gtkrc-background-pixmaps (not gtkrc-loaded-pixmaps))
      (setq gtkrc-background-pixmaps
	    (mapcar (lambda (x)
		      (when x
			(setq x (make-image (gtkrc-fix-image-name x)))
			(image-put x 'tiled t)
			x))
		    gtkrc-background-pixmaps))
      (setq gtkrc-loaded-pixmaps t)))

  (define (gtkrc-reload-style)
    "Reload the gtkrc settings."
    (gtkrc-get-style)
    (gtkrc-apply-style)
    (reload-gaol)
    (call-hook 'gtkrc-changed-hook))

  (define-command 'gtkrc-reload-style gtkrc-reload-style)

  ;; recognize when the GTK theme has been switched
  (define (gtkrc-handle-client-msg w type data)
    (declare (unused data))
    (when (and (eq w gtkrc-dummy-window) (eq type '_GTK_READ_RCFILES))
      (gtkrc-reload-style)
      ;; XXX make conditional
      (require 'sawfish.wm.menus)
      (menu-stop-process t)
      t))

;;; for restricted themes

  (define gtkrc-funs '())

  (define (gtkrc-call-after-changed fun)
    (setq gtkrc-funs (cons fun gtkrc-funs)))

  (add-hook 'gtkrc-changed-hook (lambda () (mapc funcall gtkrc-funs)))

  (define (reload-gaol)
    (gaol-define 'gtkrc-background gtkrc-background)
    (gaol-define 'gtkrc-background-pixmaps gtkrc-background-pixmaps)
    (gaol-define 'gtkrc-base gtkrc-base)
    (gaol-define 'gtkrc-light gtkrc-light)
    (gaol-define 'gtkrc-dark gtkrc-dark)
    (gaol-define 'gtkrc-mid gtkrc-mid)
    (gaol-define 'gtkrc-foreground gtkrc-foreground)
    (gaol-define 'gtkrc-font gtkrc-font))

;;; init

  (define (gtkrc-quit)
    (destroy-window gtkrc-dummy-window))

  (unless batch-mode
    (setq gtkrc-dummy-window (create-window 'root -100 -100 10 10))
    (set-x-property gtkrc-dummy-window 'WM_STATE (vector 0) 'WM_STATE 32)
    (add-hook 'client-message-hook gtkrc-handle-client-msg)
    (add-hook 'before-exit-hook gtkrc-quit)
    (gtkrc-reload-style))

  (gaol-define 'gtkrc-load-pixmaps gtkrc-load-pixmaps)
  (gaol-define 'gtkrc-reload-style gtkrc-reload-style)
  (gaol-define 'gtkrc-call-after-changed gtkrc-call-after-changed)
  (reload-gaol))
