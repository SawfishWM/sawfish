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

(require 'tables)
(provide 'custom)

;; list associating groups with the list of variables in that group
(define custom-groups (list 'root "Sawfish"))

(defvar custom-user-file "~/.sawfish/custom"
  "File used to store user's configuration settings.")

(defvar custom-default-file "custom-defaults"
  "Lisp library storing default customization settings.")

(define custom-quoted-keys
  '(:group :require :type :options :range :depends :user-level :layout)
  "defcustom keys whose values are quoted by the macro expansion.")

(define custom-option-alist '((:group . custom-group)
			      (:require . custom-require)
			      (:type . custom-type)
			      (:type* . custom-type)
			      (:options . custom-options)
			      (:depends . custom-depends)
			      (:user-level . custom-user-level)
			      (:set . custom-set)
			      (:get . custom-get)
			      (:widget . custom-widget)
			      (:after-set . custom-after-set)
			      (:before-set . custom-before-set)
			      (:range . custom-range)))

(define custom-group-option-alist '((:layout . custom-group-layout)
				    (:require . custom-group-require)))

;; hash group names (lists of symbols) to alist of options 
(define custom-group-table (make-table equal-hash equal))

(defvar custom-set-alist nil
  "Alist of (CLOSURE . SYMBOL) mapping custom-set functions to their names.")

(defvar custom-menu-includes-all-settings t
  "When non-nil, the custom menu includes the `All settings' item.")


;; defining custom variables and groups

(defmacro defcustom (symbol value doc &rest keys)
  "Define a new customization variable SYMBOL which initially has value
VALUE (unless SYMBOL is already bound, in which case its value is not
altered), and documentations string DOC.

KEYS is a property-list containing any of the following:

	:group GROUP
	:require FEATURE
	:type TYPE
	:options OPTIONS
	:depends SYMBOL
	:user-level LEVEL		novice, intermediate, expert
	:range (MIN . MAX)		for `number' type
	:set FUNCTION
	:get FUNCTION
	:before-set FUNCTION
	:after-set FUNCTION
	:widget FUNCTION

TYPE may be `boolean', `number', `string', `symbol', `file-name',
`program-name', `font', `color'.

Note that the values of the `:group', `:require', `:type', `:options',
`:depends', `:user-level' and `:range' keys are not evaluated. All
other key values are evaluated.

Each defcustom'd symbol may have several special properties

	custom-set (FUNCTION SYMBOL VALUE)
	custom-get (FUNCTION SYMBOL)
	custom-widget (FUNCTION SYMBOL)

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
	:layout LAYOUT-TYPE

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
    (custom-add-to-group symbol (get symbol 'custom-group))
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
      (case (car keys)
	((:group)
	 (setq container (nth 1 keys))
	 (unless (listp container)
	   (setq container (list container))))
	((:require)
	 (custom-group-requires (append container (list group)) (cadr keys)))
	(t
	 (custom-set-group-property (append container (list group))
				    (car keys) (cadr keys))))
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


;; general management

(defun define-custom-setter (name def)
  (setq custom-set-alist (cons (cons def name) custom-set-alist)))

(defun define-command-args (name spec)
  (put name 'custom-command-args spec))

(defmacro custom-set-property (sym prop value)
  "Set the custom key PROP for defcustom'd symbol SYM to value."
  (let ((tem (gensym)))
    `(let
	 ((,tem (cdr (assq ,prop custom-option-alist))))
       (when ,tem
	 (put ,sym ,tem ,value)))))

(defun custom-set-group-property (group prop value)
  "Set the custom key PROP for defgroup'd symbol SYM to value."
  (unless (listp group)
    (setq group (list group)))
  (let* ((alist (table-ref custom-group-table group))
	 (cell (and alist (assq prop alist))))
    (if cell
	(rplacd cell value)
      (setq alist (cons (cons prop value) alist))
      (table-set custom-group-table group alist))))

(defun custom-get-group-property (group prop)
  (unless (listp group)
    (setq group (list group)))
  (let ((alist (table-ref custom-group-table group)))
    (cdr (assq prop alist))))

(defun custom-group-requires (group feature)
  (unless (listp group)
    (setq group (list group)))
  (custom-set-group-property
   group ':require
   (cons feature (delq feature
		       (custom-get-group-property ':require group)))))

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


;; setting values

(defun custom-set (setter symbol)
  (when (get symbol 'custom-before-set)
    ((get symbol 'custom-before-set) symbol))
  (setter)
  (put symbol 'custom-user-value t)
  (when (get symbol 'custom-after-set)
    ((get symbol 'custom-after-set) symbol)))
 
(defun custom-set-variable (symbol value &optional req)
  (when (and req value)
    (require req))
  (custom-set (lambda ()
		(make-variable-special symbol)
		(set symbol value))
	      symbol))

(defun custom-set-typed-variable (symbol value type &optional req)
  (when (and req value)
    (require req))
  (custom-set (lambda ()
		(make-variable-special symbol)
		(set symbol (custom-deserialize value type)))
	      symbol))

(defun variable-customized-p (symbol)
  "Returns `t' if the variable named SYMBOL has been customized by the user."
  (get symbol 'custom-user-value))

(defun variable-type (symbol)
  "Returns the customizable type of the variable named SYMBOL."
  (get symbol 'custom-type))


;; serializing unreadable types

;; property name (symbol) to find type converters on during custom-convert
(define custom-converter-property (make-fluid))

;; convert VALUE of TYPE to or from a printable object
(define (custom-convert value type)
  (case (or (car type) type)
    ((optional)
     (custom-convert value (cadr type)))

    ((labelled)
     (custom-convert value (caddr type)))

    ((quoted)
     `(quote ,(custom-convert (cadr value) (cadr type))))

    ((pair)
     (cons (custom-convert (car value) (cadr type))
	   (custom-convert (cdr value) (caddr type))))

    ((list)
     (mapcar (lambda (x) (custom-convert x (cadr type))) value))

    ((alist)
     (let ((k-type (or (car (cadr type)) (cadr type)))
	   (v-type (or (car (caddr type)) (caddr type))))
       (mapcar (lambda (x)
		 (cons (custom-convert (car x) k-type)
		       (custom-convert (cdr x) v-type))) value)))

    ((and v-and h-and)
     (do ((values value (cdr values))
	  (types (cdr type) (cdr types))
	  (out '() (cons (custom-convert (car values) (car types)) out)))
	 ((or (null types) (null values))
	  (nreverse out))))

    ;; XXX handle `or' (needs type predicates)

    (t (let ((converter (get (or (car type) type)
			     (fluid custom-converter-property))))
	 (if converter
	     (converter value type)
	   value)))))

(defun custom-serialize (value type)
  "Convert VALUE of TYPE to a printable value."
  (let-fluids ((custom-converter-property 'custom-serializer))
    (custom-convert value type)))

(defun custom-deserialize (value type)
  "Convert VALUE of TYPE back from a printable value."
  (let-fluids ((custom-converter-property 'custom-deserializer))
    (custom-convert value type)))

(defun define-custom-serializer (type fun)
  (put type 'custom-serializer fun))

(defun define-custom-deserializer (type fun)
  (put type 'custom-deserializer fun))


;; customize menu

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


;; support for font and color primitive types

(define-custom-serializer 'font (lambda (value)
				  (if (fontp value)
				      (font-name value)
				    value)))

(define-custom-deserializer 'font (lambda (value)
				    (if (stringp value)
					(get-font value)
				      value)))

(define-custom-serializer 'color (lambda (value)
				   (if (colorp value)
				       (color-name value)
				     value)))

(define-custom-deserializer 'color (lambda (value)
				     (if (stringp value)
					 (get-color value)
				       value)))


;; default groups

(defgroup focus "Focus" :require auto-raise)
(defgroup move "Move/Resize" :require move-resize)
(defgroup placement "Placement")
(defgroup appearance "Appearance")
(defgroup workspace "Workspaces")
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
