;; custom.jl -- Emacs-like ``customizing'' (but more simple)
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

(provide 'custom)

;; list associating groups with the list of variables in that group
(defvar custom-groups nil)

(defvar custom-required nil
  "List of features to load before running customize.")

(defvar custom-user-file "~/.sawmill/custom"
  "File used to store user's configuration settings.")

;; (defcustom VARIABLE VALUE DOC &rest CUSTOM-KEYS)

;; where CUSTOM-KEYS is a plist containing any of the following:

;;	:group GROUP
;;	:require FEATURE
;;	:type TYPE
;;	:allow-nil t
;;	:set FUNCTION
;;	:get FUNCTION
;;	:before-set FUNCTION
;;	:after-set FUNCTION
;;	:range (MIN . MAX)		for number type

;; TYPE may be `boolean', `number', `string', `(set SYMBOLS..)',
;; `file-name', `program-name', `font', `color'

;; Each defcustom'd variable may have several special properties

;;	custom-set (FUNCTION SYMBOL VALUE)
;;	custom-get (FUNCTION SYMBOL)
;;	custom-widget (FUNCTION SYMBOL VALUE DOC)

;; these functions are used while constructing and responding to the
;; customisation dialog. If not set in the symbol itself they may be
;; inherited from the plist of the type of the variable.

;; custom-set and custom-get may be used to translate data types to
;; the string representation required by some widget types. custom-widget
;; may construct the widget definition passed to the ui backend

(defmacro defcustom (symbol value doc &rest keys)
  (list 'defvar
	symbol
	(list 'custom-declare-variable
	      (list 'quote symbol) value (list 'quote keys))
	doc))

(defmacro defgroup (symbol doc &rest keys)
  (list 'custom-declare-group (list 'quote symbol) doc (list 'quote keys)))

(defun custom-declare-variable (symbol value keys)
  (let
      (tem type)
    (while keys
      (setq tem (car keys))
      (setq keys (cdr keys))
      (cond ((eq tem ':group)
	     (put symbol 'custom-group (car keys))
	     (custom-add-to-group symbol (car keys)))
	    ((eq tem ':require)
	     (put symbol 'custom-require (car keys)))
	    ((eq tem ':type)
	     (setq type (car keys))
	     (put symbol 'custom-type type))
	    ((eq tem ':allow-nil)
	     (put symbol 'custom-allow-nil (car keys)))
	    ((eq tem ':set)
	     (put symbol 'custom-set (car keys)))
	    ((eq tem ':get)
	     (put symbol 'custom-get (car keys)))
	    ((eq tem ':widget)
	     (put symbol 'custom-widget (car keys)))
	    ((eq tem ':after-set)
	     (put symbol 'custom-after-set (car keys)))
	    ((eq tem ':before-set)
	     (put symbol 'custom-before-set (car keys)))
	    ((eq tem ':range)
	     (put symbol 'custom-range (car keys))))
      (setq keys (cdr keys)))
    (when (symbolp type)
      (when (and (not (get symbol 'custom-get))
		 (get type 'custom-get))
	(put symbol 'custom-get (get type 'custom-get)))
      (when (and (not (get symbol 'custom-set))
		 (get type 'custom-set))
	(put symbol 'custom-set (get type 'custom-set)))
      (when (and (not (get symbol 'custom-widget))
		 (get type 'custom-widget))
	(put symbol 'custom-widget (get type 'custom-widget))))
    value))

(defun custom-declare-group (group &optional doc keys)
  (unless (assq group custom-groups)
    (setq custom-groups (nconc custom-groups (list (list group)))))
  (when doc
    (put group 'custom-group-doc doc))
  (let
      (tem)
    (while keys
      (setq tem (car keys))
      (setq keys (cdr keys))
      (cond ((eq tem ':widget)
	     (put group 'custom-group-widget (car keys))))
      (setq keys (cdr keys)))
    ;; declare a command to customize this group
    (fset (intern (concat "customize:" (symbol-name group)))
	  `(lambda ()
	     (interactive)
	     (customize ',group)))))

(defun custom-add-to-group (symbol group)
  (let
      ((group-list (assq group custom-groups)))
    (unless group-list
      (custom-declare-group group)
      (setq group-list (assq group custom-groups)))
    (rplacd group-list (nconc (delq symbol (cdr group-list)) (list symbol)))))

(defun custom-set-variable (symbol value &optional require)
  (when require
    (require require))
  (when (get symbol 'custom-before-set)
    (funcall (get symbol 'custom-before-set) symbol))
  (set symbol value)
  (when (get symbol 'custom-after-set)
    (funcall (get symbol 'custom-after-set) symbol)))

(defun custom-menu ()
  (list*
   '("All settings" customize)
   '()
   (mapcar #'(lambda (group-list)
	       (list (or (get (car group-list) 'custom-group-doc)
			 (symbol-name (car group-list)))
		     `(lambda ()
			(customize ',(car group-list))))) custom-groups)))


;; support for font and color primitive types

(put 'font 'custom-set 'custom-set-font)
(put 'font 'custom-get 'custom-get-font)

(defun custom-set-font (symbol value &rest args)
  (apply 'custom-set-variable symbol (if (stringp value)
					 (get-font value)
				       value) args))

(defun custom-get-font (symbol)
  (let
      ((value (symbol-value symbol)))
    (if (fontp value)
	(font-name value)
      value)))

(put 'color 'custom-set 'custom-set-color)
(put 'color 'custom-get 'custom-get-color)

(defun custom-set-color (symbol value &rest args)
  (apply 'custom-set-variable symbol (if (stringp value)
					 (get-color value)
				       value) args))

(defun custom-get-color (symbol)
  (let
      ((value (symbol-value symbol)))
    (if (colorp value)
	(color-name value)
      value)))


;; default groups

(defgroup focus "Focus")
(defgroup move "Move/Resize")
(defgroup placement "Placement")
(defgroup appearance "Appearance")
(defgroup workspace "Workspaces")
(defgroup viewport "Viewports")
(defgroup bindings "Bindings")
(defgroup iconify "Iconifying")
(defgroup maximize "Maximizing")
(defgroup misc "Miscellaneous")


;; loading user's customisations

(defun custom-load-user-file ()
  (when (file-exists-p custom-user-file)
    (load custom-user-file t t t)))
