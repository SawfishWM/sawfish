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
(defvar custom-groups (list 'root "Sawmill"))

(defvar custom-required nil
  "List of features to load before running customize.")

(defvar custom-user-file "~/.sawmill/custom"
  "File used to store user's configuration settings.")

(defvar custom-default-file "custom-defaults"
  "Lisp library storing default customization settings.")

(defvar custom-quoted-keys
  '(:group :require :type :options :allow-nil :range)
  "defcustom keys whose values are quoted by the macro expansion.")

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

(defvar custom-set-alist nil
  "Alist of (CLOSURE . SYMBOL) mapping custom-set functions to their names.")

(defvar custom-menu-includes-all-settings t
  "When non-nil, the custom menu includes the `All settings' item.")

(defmacro defcustom (symbol value doc &rest keys)
  "Define a new customization variable SYMBOL which initially has value
VALUE (unless SYMBOL is already bound, in which case its value is not
altered), and documentations string DOC.

KEYS is a property-list containing any of the following:

	:group GROUP
	:require FEATURE
	:type TYPE
	:options OPTIONS
	:range (MIN . MAX)		for `number' type
	:allow-nil t
	:set FUNCTION
	:get FUNCTION
	:before-set FUNCTION
	:after-set FUNCTION
	:widget FUNCTION

TYPE may be `boolean', `number', `string', `symbol', `file-name',
`program-name', `font', `color'.

Note that the values of the `:group', `:require', `:type', `:options',
`:allow-nil' and `:range' keys are not evaluated. All other key values
are evaluated.

Each defcustom'd symbol may have several special properties

	custom-set (FUNCTION SYMBOL VALUE)
	custom-get (FUNCTION SYMBOL)
	custom-widget (FUNCTION SYMBOL VALUE DOC)

These functions are used while constructing and responding to the
customisation dialog. If not set in the symbol itself they may be
inherited from the plist of the type of the variable.

custom-set and custom-get may be used to translate data types to the
string representation required by some widget types. custom-widget may
construct the widget definition passed to the ui backend."
  `(defvar ,symbol (custom-declare-variable
		    ',symbol ,value ,(custom-quote-keys keys))
	   ,doc))

(defmacro defgroup (symbol doc &rest keys)
  "Declare a new custom group called SYMBOL, with English name DOC. The
property list KEYS may contain the following key-value items:

	:group PARENT-GROUP
	:widget WIDGET-FUNCTION

Note that the value of the `:group' key is not evaluated."
  `(custom-declare-group ',symbol ,doc ,(custom-quote-keys keys)))

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
      (setq type 'symbol)
      (put symbol 'custom-type type))
    (when (and type (symbolp type))
      (when (and (not (get symbol 'custom-get)) (get type 'custom-get))
	(put symbol 'custom-get (get type 'custom-get)))
      (when (and (not (get symbol 'custom-set)) (get type 'custom-set))
	(put symbol 'custom-set (get type 'custom-set)))
      (when (and (not (get symbol 'custom-widget)) (get type 'custom-widget))
	(put symbol 'custom-widget (get type 'custom-widget))))
    value))

(defun custom-declare-group (group &optional doc keys)
  (let
      (container tem)
    (while keys
      (if (eq (car keys) ':group)
	  (setq container (nth 1 keys))
	(setq tem (cdr (assq (car keys) custom-group-option-alist)))
	(when tem
	  (put group tem (cadr keys))))
      (setq keys (cddr keys)))
    (custom-add-to-group (list group doc) container)
    (unless container
      ;; declare a command to customize this group
      (define-value (intern (concat "customize:" (symbol-name group)))
		    (lambda ()
		      (interactive)
		      (customize group))))))

(defun custom-quote-keys (keys)
  (let
      ((out nil))
    (while (and keys (cdr keys))
      (if (memq (car keys) custom-quoted-keys)
	  (setq out (cons (list 'quote (nth 1 keys))
			  (cons (list 'quote (car keys)) out)))
	(setq out (cons (nth 1 keys) (cons (list 'quote (car keys)) out))))
      (setq keys (nthcdr 2 keys)))
    (cons 'list (nreverse out))))

(defmacro custom-set-property (sym prop value)
  "Set the custom key PROP for defcustom'd symbol SYM to value."
  (let ((tem (gensym)))
    `(let
	 ((,tem (cdr (assq ,prop custom-option-alist))))
       (when ,tem
	 (put ,sym ,tem ,value)))))

(defmacro custom-set-group-property (group prop value)
  "Set the custom key PROP for defgroup'd symbol SYM to value."
  (let ((tem (gensym)))
    `(let
	 ((,tem (cdr (assq ,prop custom-group-option-alist))))
       (when ,tem
	 (put ,group ,tem ,value)))))

(defmacro custom-add-option (sym option)
  "Assuming that defcustom'd symbol SYM is of type `symbol', add the
symbol OPTION to the list of choices."
  `(put ,sym 'custom-options
	(nconc (get ,sym 'custom-options) (list ,option))))

(defmacro custom-get-options (sym)
  "Assuming that defcustom'd symbol SYM is of type `symbol', return the
of choices."
  `(get ,sym 'custom-options))

(defun custom-find-group (full-group)
  (when (and (symbolp full-group) (not (null full-group)))
    (setq full-group (list full-group)))
  (letrec
      ((iterate
	(lambda (group parent)
	  (if (null group)
	      parent
	    (iterate (cdr group)
		     (or (assq (car group) (cddr parent))
			 (error "No such group: %S" full-group)))))))
    (iterate full-group custom-groups)))

(defun custom-add-to-group (cell full-group &optional tail)
  (require 'sort)
  (when (and (symbolp full-group) (not (null full-group)))
    (setq full-group (list full-group)))
  (letrec
      ((inner-fun
	(lambda (group parent)
	  (if (null group)
	      (unless (or (memq cell (cddr parent))
			  (assq (car cell) (cddr parent)))
		;; reached the bottom most group
		(rplacd (cdr parent) (nconc (cddr parent) (list cell))))
	    ;; keep on recursing
	    (inner-fun (cdr group)
		       (or (assq (car group) (cddr parent))
			   (error "Unknown group %s" full-group)))
	    (unless (cdr group)
	      (rplacd (cdr custom-groups)
		      (nconc (sort (filter consp (cddr custom-groups))
				   (lambda (x y)
				     (string-lessp (cadr x) (cadr y))))
			     (filter atom (cddr custom-groups)))))))))
    (inner-fun full-group custom-groups)))

(defun custom-set-variable (symbol value &optional req)
  (when (and req value)
    (require req))
  (when (get symbol 'custom-before-set)
    (funcall (get symbol 'custom-before-set) symbol))
  (set symbol value)
  (when (get symbol 'custom-after-set)
    (funcall (get symbol 'custom-after-set) symbol)))

(defun custom-menu ()
  (letrec
      ((iterator
	(lambda (tree group)
	  (let
	      ((subtrees (filter consp (cddr tree))))
	    (if (null subtrees)
		(list (_ (cadr tree)) `(customize ',group))
	      (list* (_ (cadr tree))
		     (list (_ "Main") `(customize ',group))
		     (mapcar (lambda (sub)
			       (iterator
				sub (append group (list (car sub)))))
			     subtrees)))))))
    `(,@(and custom-menu-includes-all-settings
	     (list (list (_ "All settings") 'customize) nil))
      ,@(mapcar (lambda (sub)
		  (list (_ (cadr sub)) `(customize ',(car sub))))
		(filter consp (cddr custom-groups)))
      ,@(and (frame-style-editable-p default-frame-style)
	     (list nil `(,(_"Edit theme...") edit-frame-style))))))

(defun custom-add-required (feature)
  "Add the symbol FEATURE to the list of modules required before
running the configuration tool."
  (unless (memq feature custom-required)
    (setq custom-required (cons feature custom-required))))


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
(defgroup advanced "Advanced" :group focus)
(defgroup move "Move/Resize")
(defgroup advanced "Advanced" :group move)
(defgroup placement "Placement")
(defgroup appearance "Appearance")
(defgroup advanced "Advanced" :group appearance)
(defgroup workspace "Workspaces")
(defgroup advanced "Advanced" :group workspace)
(defgroup bindings "Bindings")
(defgroup min-max "Minimizing/Maximizing")
(defgroup iconify "Minimizing" :group min-max)
(defgroup maximize "Maximizing" :group min-max)
(defgroup misc "Miscellaneous")


;; loading user's customisations

(defun custom-load-user-file ()
  "Load the user's customization file, or the custom-default-file."
  (cond ((file-exists-p custom-user-file)
	 (load custom-user-file t t t))
	(custom-default-file
	 (load custom-default-file t))))
