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

;; Commentary:

;; This sets the following window properties:

;;	frame-style		If set, the _user-chosen_ frame style
;;	current-frame-style	The current frame style
;;	type			The notional window type, defining
;;				 which parts of the frame are included

;; The different window types are:

;;	default			title bar and border
;;	transient		border only
;;	shaped			title-bar only
;;	shaped-transient	border-like title-bar only
;;	unframed		no frame at all

(defvar frame-part-classes
  '((title . ((cursor . hand2)
	      (keymap . title-keymap)))
    (menu-button . ((keymap . menu-button-keymap)))
    (close-button . ((keymap . close-button-keymap)
		     (cursor . dotbox)))
    (iconify-button . ((keymap . iconify-button-keymap)
		       (cursor . sb_down_arrow)))
    (maximize-button . ((keymap . maximize-button-keymap)
			(cursor . sb_v_double_arrow)))
    (top-border . ((cursor . top_side)
		   (keymap . border-keymap)))
    (left-border . ((cursor . left_side)
		    (keymap . border-keymap)))
    (right-border . ((cursor . right_side)
		     (keymap . border-keymap)))
    (bottom-border . ((cursor . bottom_side)
		      (keymap . border-keymap)))
    (top-left-corner . ((cursor . top_left_corner)
			(keymap . border-keymap)))
    (top-right-corner . ((cursor . top_right_corner)
			 (keymap . border-keymap)))
    (bottom-left-corner . ((cursor . bottom_left_corner)
			   (keymap . border-keymap)))
    (bottom-right-corner . ((cursor . bottom_right_corner)
			    (keymap . border-keymap))))
  "Alist of (CLASS . ALIST) associating classes of frame parts with state
they inherit.")

(defvar override-frame-part-classes nil
  "Alist of (CLASS . ALIST) associating classes of frame parts with state
that overrides settings set elsewhere.")

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
  :after-set after-setting-frame-option)

;; from frames.c
(defcustom highlight-when-unfocused nil
  "Highlight frame pieces even when the window is unfocused."
  :type boolean
  :group appearance)

(defvar user-theme-directory "~/.sawmill/themes"
  "Directory containing user-local themes.")

(defvar system-theme-directory (expand-file-name
				"../themes" sawmill-lisp-lib-directory)
  "Directory containing system themes.")

(defvar theme-load-path (list user-theme-directory system-theme-directory)
  "List of directories from which themes may be loaded.")

(defvar frame-styles nil
  "List of (NAME . FUNCTION) defining all loaded frame styles.")

(defvar auto-frame-style-alist nil
  "List of (REGEXP . STYLE) associating window names with frame styles.")

(defvar auto-window-type-alist nil
  "List of (REGEXP . TYPE) associating window names with window types.")

;; used when decorate-transients is non-nil, map transient window
;; types to type to pass to frame style function
(defvar transient-normal-frame-alist '((transient . default)
				       (shaped-transient . shaped)))

;; list of (REGEXP DIR-EXPAND NAME-EXPAND)
(defvar theme-suffix-regexps
  '(("^(.*)\\.tar(\\.gz|\\.Z|\\.bz2)$" "\\0#tar" "\\1")))

(defvar theme-suffixes '("" ".tar" ".tar.gz" ".tar.Z" ".tar.bz2"))


;; managing frame styles

(defun add-frame-style (name function)
  (let
      ((cell (assq name frame-styles)))
    (if cell
	(rplacd cell function)
      (setq frame-styles (cons (cons name function) frame-styles)))
    (unless default-frame-style
      (setq default-frame-style name))))

(defun check-frame-availability (name)
  (unless (assq name frame-styles)
    (load-frame-style name)
    (or (assq name frame-styles) (error "No such frame style: %s" name))))

(defun set-frame-style (name)
  (check-frame-availability name)
  (setq default-frame-style name)
  (when always-update-frames
    (reframe-all-windows)))

(defun set-window-frame-style (w style &optional type from-user)
  (let
      (tem)
    (check-frame-availability style)
    (if type
	(window-put w 'type type)
      (setq type (window-type w)))
    (window-put w 'current-frame-style style)
    (when from-user
      (window-put w 'frame-style style))      
    (setq fun (cdr (assq style frame-styles)))
    (set-window-frame w (or (funcall fun w type) default-frame))))

(defun set-frame-for-window (w &optional override type)
  (when (or override (not (window-frame w)))
    (let*
	((style (window-get w 'frame-style))
	 fun tem)
      (unless style
	(setq style (cdr (assoc-regexp
			  (window-name w) auto-frame-style-alist)))
	(if style
	    (progn
	      (unless (assq style frame-styles)
		(load-frame-style style))
	      (window-put w 'frame-style style))
	  (setq style default-frame-style)))
      (set-window-frame-style w style type))))

(add-hook 'add-window-hook 'set-frame-for-window t)

(defun reframe-all-windows ()
  (mapc #'(lambda (w)
	    (when (and (windowp w) (not (window-get w 'ignored)))
	      (set-frame-for-window w t (window-get w 'type))))
	(managed-windows)))


;; kludge different window decors by modifying the assumed window type

(defun window-type (w)
  (or (window-get w 'type)
      (cdr (assoc-regexp (window-name w) auto-window-type-alist))
      (let
	  ((type (if (window-transient-p w)
		     (if (window-shaped-p w)
			 'shaped-transient
		       'transient)
		   (if (window-shaped-p w)
		       'shaped
		     'default)))
	   tem)
	(when (and decorate-transients (window-transient-p w)
		   (setq tem (cdr (assq type transient-normal-frame-alist))))
	  (setq type tem))
	type)))

(defun window-type-remove-title (type)
  (cond ((eq type 'default)
	 'transient)
	((memq type '(shaped shaped-transient))
	 'unframed)
	(t
	 type)))

(defun window-type-remove-border (type)
  (cond ((eq type 'default)
	 'shaped)
	((memq type '(transient shaped-transient))
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
		   (interactive "%W")
		   (set-frame-for-window w t ',type))))
      '(default transient shaped shaped-transient unframed))


;; custom support

(defun custom-set-frame-style (symbol value &rest args)
  (if (eq symbol 'default-frame-style)
      (set-frame-style value)
    (apply 'custom-set-variable symbol value args)))

(defun custom-make-frame-style-widget (symbol value doc)
  (let
      ((styles (find-all-frame-styles t)))
    `(frame-style ,styles
		  :variable ,symbol
		  :value ,value
		  :doc ,doc
		  :theme-path ,theme-load-path)))

(defun after-setting-frame-option ()
  (when always-update-frames
    (reframe-all-windows)))


;; loading ``themes'' (currently just frame styles)

(defun frame-style-directory (dir &optional get-name)
  (if (and (file-directory-p dir)
	   (or (file-exists-p (expand-file-name "theme.jl" dir))
	       (file-exists-p (expand-file-name "theme.jlc" dir))))
      (if get-name
	  (file-name-nondirectory dir)
	dir)
    ;; try the list of suffixes
    (catch 'out
      (mapc #'(lambda (cell)
		(when (string-match (car cell) dir)
		  (if get-name
		      (throw 'out (file-name-nondirectory
				   (expand-last-match (nth 2 cell))))
		    (throw 'out (expand-last-match (nth 1 cell))))))
	    theme-suffix-regexps)
      nil)))

(defun load-frame-style (name)
  (catch 'out
    (mapc #'(lambda (dir)
	      (mapc #'(lambda (suf)
			(let*
			    ((t-dir (expand-file-name
				     (concat (symbol-name name) suf) dir))
			     tem)
			  (when (file-exists-p t-dir)
			    (setq tem (frame-style-directory t-dir))
			    (when tem
			      (let
				  ((image-load-path
				    (cons tem image-load-path)))
				(load (expand-file-name "theme" tem) nil t)
				(throw 'out t))))))
		    theme-suffixes))
	  theme-load-path)
    nil))

(defun find-all-frame-styles (&optional sorted)
  (let
      (list tem)
    (mapc #'(lambda (dir)
	      (when (file-directory-p dir)
		(mapc #'(lambda (t-dir)
			  (when (setq tem (frame-style-directory
					   (expand-file-name t-dir dir) t))
			    (setq list (cons tem list))))
		      (directory-files dir))))
	  theme-load-path)
    (when sorted
      (setq list (sort list)))
    (mapcar 'intern list)))

(defun frame-style-menu ()
  (let
      ((styles (find-all-frame-styles t)))
    (nconc (mapcar #'(lambda (s)
		       (list (symbol-name s)
			     `(lambda ()
				(set-window-frame-style
				 (current-event-window) ',s nil t))))
		   styles)
	   `(() ("Default" (lambda ()
			     (let
				 ((w (current-event-window)))
			       (window-put w 'frame-style nil)
			       (set-frame-for-window w t))))))))


;; removing frame parts

(defun remove-frame-class (w class)
  (window-put w 'removed-classes
	      (cons class (delq class (window-get w 'removed-classes))))
  (when (window-framed-p w)
    (rebuild-frame w)))

(defun add-frame-class (w class)
  (window-put w 'removed-classes (delq class (window-get w 'removed-classes)))
  (when (window-framed-p w)
    (rebuild-frame w)))

(defun set-frame-part-value (class key value &optional override)
  (let*
      ((var (if override 'override-frame-part-classes 'frame-part-classes))
       (elt (assq class (symbol-value var)))
       tem)
    (if elt
	(if (setq tem (assq key (cdr elt)))
	    (rplacd tem value)
	  (rplacd elt (cons (cons key value) (cdr elt))))
      (set var (cons (cons class (list (cons key value)))
		     (symbol-value var))))))


;; initialisation

(sm-add-saved-properties 'type 'ignored 'frame-style)
