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

(defvar custom-unquoted-keys
  '(:group :require :type :options :allow-nil :range))

(defvar custom-option-alist '((:group . custom-group)
			      (:require . custom-require)
			      (:type . custom-type)
			      (:options . custom-options)
			      (:allow-nil . custom-allow-nil)
			      (:set . custom-set)
			      (:get . custom-get)
			      (:widget . custom-widget)
			      (:after-set . custom-after-set)
			      (:before-set . custom-before-set)
			      (:range . custom-range)))

(defvar custom-group-option-alist '((:widget . custom-group-widget)))

;; alist of (CLOSURE . SYMBOL) mapping custom-set functions to
;; their names
(defvar custom-set-alist nil)

;; (defcustom VARIABLE VALUE DOC &rest CUSTOM-KEYS)

;; where CUSTOM-KEYS is a plist containing any of the following:

;;	:group GROUP
;;	:require FEATURE
;;	:type TYPE
;;	:options OPTIONS
;;	:allow-nil t
;;	:set FUNCTION
;;	:get FUNCTION
;;	:before-set FUNCTION
;;	:after-set FUNCTION
;;	:range (MIN . MAX)		for number type

;; TYPE may be `boolean', `number', `string', `symbol',
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
	      (list 'quote symbol) value (custom-quote-keys keys))
	doc))

(defmacro defgroup (symbol doc &rest keys)
  (list 'custom-declare-group (list 'quote symbol)
	doc (custom-quote-keys keys)))

(defun custom-declare-variable (symbol value keys)
  (let
      (type prop)
    (while keys
      (setq prop (cdr (assq (car keys) custom-option-alist)))
      (setq keys (cdr keys))
      (when prop
	(put symbol prop (car keys)))
      (setq keys (cdr keys)))
    (custom-add-to-group symbol (or (get symbol 'custom-group)
				    (error "No :group attribute: %s" symbol)))
    (setq type (get symbol 'custom-type))
    (when (eq (car type) 'set)
      ;; backwards compatibility
      (put symbol 'custom-options (cdr type))
      (setq type 'symbol))
    (when (and type (symbolp type))
      (when (and (not (get symbol 'custom-get)) (get type 'custom-get))
	(put symbol 'custom-get (get type 'custom-get)))
      (when (and (not (get symbol 'custom-set)) (get type 'custom-set))
	(put symbol 'custom-set (get type 'custom-set)))
      (when (and (not (get symbol 'custom-widget)) (get type 'custom-widget))
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
      (setq tem (cdr (assq (car keys) custom-group-option-alist)))
      (setq keys (cdr keys))
      (when tem
	(put group tem (car keys)))
      (setq keys (cdr keys)))
    ;; declare a command to customize this group
    (set (intern (concat "customize:" (symbol-name group)))
	 (make-closure `(lambda ()
			  (interactive)
			  (customize ',group))))))

(defun custom-quote-keys (keys)
  (let
      ((out nil))
    (while (and keys (cdr keys))
      (if (memq (car keys) custom-unquoted-keys)
	  (setq out (cons (list 'quote (nth 1 keys))
			  (cons (list 'quote (car keys)) out)))
	(setq out (cons (nth 1 keys) (cons (list 'quote (car keys)) out))))
      (setq keys (nthcdr 2 keys)))
    (cons 'list (nreverse out))))

(defmacro custom-set-property (sym prop value)
  `(let
       ((__prop__ (cdr (assq ,prop custom-option-alist))))
     (when __prop__
       (put ,sym __prop__ ,value))))

(defmacro custom-set-group-property (group prop value)
  `(let
       ((__prop__ (cdr (assq ,prop custom-group-option-alist))))
     (when __prop__
       (put ,group __prop__ ,value))))

(defmacro custom-add-option (sym option)
  `(put ,sym 'custom-options
	(nconc (get ,sym 'custom-options) (list ,option))))

(defmacro custom-get-options (sym)
  `(get ,sym 'custom-options))

(defun custom-add-to-group (symbol group)
  (let
      ((group-list (assq group custom-groups)))
    (unless group-list
      (custom-declare-group group)
      (setq group-list (assq group custom-groups)))
    (rplacd group-list (nconc (delq symbol (cdr group-list)) (list symbol)))))

(defun custom-set-variable (symbol value &optional req)
  (when req
    (require req))
  (when (get symbol 'custom-before-set)
    (funcall (get symbol 'custom-before-set) symbol))
  (set symbol value)
  (when (get symbol 'custom-after-set)
    (funcall (get symbol 'custom-after-set) symbol)))

(defun custom-menu ()
  (list*
   '("All settings" customize)
   '()
   (mapcar (lambda (group-list)
	     (list (_ (or (get (car group-list) 'custom-group-doc)
			  (symbol-name (car group-list))))
		   `(customize ',(car group-list)))) custom-groups)))


;; support for font and color primitive types

(defun custom-set-font (symbol value &rest args)
  (apply custom-set-variable symbol (if (stringp value)
					(get-font value)
				      value) args))

(defun custom-get-font (symbol)
  (let
      ((value (symbol-value symbol)))
    (if (fontp value)
	(font-name value)
      value)))

(put 'font 'custom-get custom-get-font)
(put 'font 'custom-set custom-set-font)
(setq custom-set-alist (cons (cons custom-set-font 'custom-set-font)
			     custom-set-alist))

(defun custom-set-color (symbol value &rest args)
  (apply custom-set-variable symbol (if (stringp value)
					(get-color value)
				      value) args))

(defun custom-get-color (symbol)
  (let
      ((value (symbol-value symbol)))
    (if (colorp value)
	(color-name value)
      value)))

(put 'color 'custom-get custom-get-color)
(put 'color 'custom-set custom-set-color)
(setq custom-set-alist (cons (cons custom-set-color 'custom-set-color)
			     custom-set-alist))


;; default groups

(defgroup focus "Focus")
(defgroup move "Move/Resize")
(defgroup placement "Placement")
(defgroup appearance "Appearance")
(defgroup workspace "Workspaces")
(defgroup bindings "Bindings")
(defgroup iconify "Iconifying")
(defgroup maximize "Maximizing")
(defgroup misc "Miscellaneous")


;; loading user's customisations

(defun custom-load-user-file ()
  (when (file-exists-p custom-user-file)
    (load custom-user-file t t t)))
