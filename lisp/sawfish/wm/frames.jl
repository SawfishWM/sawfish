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

(defvar frame-style-directory
  (expand-file-name "themes" sawmill-lisp-lib-directory))
(setq load-path (nconc load-path (list frame-style-directory)))

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
    (require name)
    (or (assq name frame-styles) (error "No such frame style: %s" name)))
  (setq default-frame-style name)
  (when always-update-frames
    (reframe-all-windows)))

(defun set-frame-for-window (w &optional override)
  (when (or override (not (window-frame w)))
    (let*
	((style (or (window-get w 'frame-style) default-frame-style))
	 (type (window-type w))
	 fun tem)
      (when (and decorate-transients
		 (setq tem (cdr (assq type transient-normal-frame-alist))))
	(setq type tem))
      (setq fun (cdr (assq style frame-styles)))
      (set-window-frame w (or (funcall fun w type)
			      default-frame)))))

(add-hook 'add-window-hook 'set-frame-for-window t)

(defun reframe-all-windows ()
  (mapc #'(lambda (w)
	    (when (and (windowp w) (not (window-get w 'ignored)))
	      (set-frame-for-window w t)))
	(managed-windows)))


;; kludge different window decors by modifying the assumed window type

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
	 'shaped-transient)
	(t
	 type)))

(defun window-type-remove-border (type)
  (cond ((eq type 'default)
	 'shaped)
	((eq type 'transient)
	 'shaped-transient)
	(t
	 type)))

(defun window-type-add-title (type)
  (cond ((eq type 'transient)
	 'default)
	((eq type 'shaped-transient)
	 'shaped)
	(t
	 type)))

(defun window-type-add-border (type)
  (cond ((eq type 'shaped)
	 'default)
	((eq type 'shaped-transient)
	 'transient)
	(t
	 type)))


;; custom support

(defun custom-set-frame-style (symbol value &rest args)
  (if (eq symbol 'default-frame-style)
      (set-frame-style value)
    (apply 'custom-set-variable symbol value args)))

(defun custom-make-frame-style-widget (symbol value doc)
  (let
      ((styles (mapcar 'car frame-styles)))
    (mapc #'(lambda (f)
	      (when (string-match "\\.jlc?$" f)
		(let
		    ((name (intern (substring f 0 (match-start)))))
		  (unless (memq name styles)
		    (setq styles (cons name styles))))))
	  (directory-files frame-style-directory))
    (setq styles (sort styles #'(lambda (x y)
				  (< (symbol-name x) (symbol-name y)))))
    `(hbox (set ,styles
		:variable ,symbol
		:value ,value)
	   (label ,doc))))
