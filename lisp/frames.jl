;; frames.jl -- handle window framing
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

(provide 'frames)

(put 'frame-style 'custom-set 'custom-set-frame-style)
(put 'frame-style 'custom-widget 'custom-make-frame-style-widget)

(defcustom default-frame-style nil
  "Frame style for otherwise unspecified windows."
  :type frame-style
  :group appearance)

(defcustom always-update-frames t
  "Update all existing window frames when frame style is changed."
  :type boolean
  :group appearance)

(defcustom decorate-transients nil
  "Decorate transient windows in the same way as non-transient windows."
  :type boolean
  :group appearance
  :after-set (lambda ()
	       (when always-update-frames
		 (reframe-all-windows))))

(defvar user-theme-directory "~/.sawmill/themes"
  "Directory containing user-local themes.")

(defvar system-theme-directory (expand-file-name
				"../themes" sawmill-lisp-lib-directory)
  "Directory containing system themes.")

(defvar theme-load-path (list user-theme-directory system-theme-directory)
  "List of directories from which themes may be loaded.")

(defvar frame-styles nil
  "List of (NAME . FUNCTION) defining all loaded frame styles.")

;; used when decorate-transients is non-nil, map transient window
;; types to type to pass to frame style function
(defvar transient-normal-frame-alist '((transient . default)
				       (shaped-transient . shaped)))


;; managing frame styles

(defun add-frame-style (name function)
  (let
      ((cell (assq name frame-styles)))
    (if cell
	(rplacd cell function)
      (setq frame-styles (cons (cons name function) frame-styles)))
    (unless default-frame-style
      (setq default-frame-style name))))

(defun set-frame-style (name)
  (unless (assq name frame-styles)
    (load-frame-style name)
    (or (assq name frame-styles) (error "No such frame style: %s" name)))
  (setq default-frame-style name)
  (when always-update-frames
    (reframe-all-windows)))

(defun set-frame-for-window (w &optional override type)
  (when (or override (not (window-frame w)))
    (let*
	((style (or (window-get w 'frame-style) default-frame-style))
	 fun tem)
      (unless type
	(setq type (window-type w))
	(when (and decorate-transients
		   (setq tem (cdr (assq type transient-normal-frame-alist))))
	  (setq type tem)))
      (setq fun (cdr (assq style frame-styles)))
      (set-window-frame w (or (funcall fun w type) default-frame)))))

(add-hook 'add-window-hook 'set-frame-for-window t)

(defun reframe-all-windows ()
  (save-stacking-order
    (mapc #'(lambda (w)
	      (when (and (windowp w) (not (window-get w 'ignored)))
		(set-frame-for-window w t (window-get w 'type))))
	  (managed-windows))))


;; kludge different window decors by modifying the assumed window type

;; The different window types are:

;;	default			title bar and border
;;	transient		border only
;;	shaped			title-bar only
;;	shaped-transient	border-like title-bar only
;;	unframed		no frame at all

(defun window-type (w)
  (or (window-get w 'type)
      (if (window-transient-p w)
	  (if (window-shaped-p w)
	      'shaped-transient
	    'transient)
	(if (window-shaped-p w)
	    'shaped
	  'default))))

(defun window-type-remove-title (type)
  (cond ((eq type 'default)
	 'transient)
	((eq type 'shaped)
	 'unframed)
	(t
	 type)))

(defun window-type-remove-border (type)
  (cond ((eq type 'default)
	 'shaped)
	((eq type 'transient)
	 'unframed)
	(t
	 type)))

(defun window-type-add-title (type)
  (cond ((eq type 'transient)
	 'default)
	((eq type 'unframed)
	 'shaped)
	(t
	 type)))

(defun window-type-add-border (type)
  (cond ((eq type 'shaped)
	 'default)
	((eq type 'unframed)
	 'transient)
	(t
	 type)))

;; create some commands for setting the window type
(mapc #'(lambda (type)
	  (fset (intern (concat "set-frame:" (symbol-name type)))
		`(lambda (w)
		   (interactive "W")
		   (set-frame-for-window w t ',type))))
      '(default transient shaped shaped-transient unframed))


;; custom support

(defun custom-set-frame-style (symbol value &rest args)
  (if (eq symbol 'default-frame-style)
      (set-frame-style value)
    (apply 'custom-set-variable symbol value args)))

(defun custom-make-frame-style-widget (symbol value doc)
  (let
      ((styles (find-all-frame-styles)))
    (setq styles (sort styles #'(lambda (x y)
				  (< (symbol-name x) (symbol-name y)))))
    `(hbox (set ,styles
		:variable ,symbol
		:value ,value)
	   (label ,doc))))


;; loading ``themes'' (currently just frame styles)

(defun frame-style-directory-p (dir)
  (or (file-exists-p (expand-file-name "theme.jl" dir))
      (file-exists-p (expand-file-name "theme.jlc" dir))))

(defun load-frame-style (name)
  (catch 'out
    (mapc #'(lambda (dir)
	      (let
		  ((t-dir  (expand-file-name (symbol-name name) dir)))
		;; XXX allow .tar and .tar.gz files..?
		(when (and (file-directory-p t-dir)
			   (frame-style-directory-p t-dir))
		  (let
		      ((image-load-path (cons t-dir image-load-path)))
		    (load (expand-file-name "theme" t-dir) nil t)
		    (throw 'out t)))))
	  theme-load-path)
    nil))

(defun find-all-frame-styles ()
  (let
      (list)
    (mapc #'(lambda (dir)
	      (when (file-directory-p dir)
		(mapc #'(lambda (t-dir)
			  (when (and (frame-style-directory-p
				      (expand-file-name t-dir dir))
				     (not (member t-dir list)))
			    (setq list (cons t-dir list))))
		      (directory-files dir))))
	  theme-load-path)
    (mapcar 'intern list)))
