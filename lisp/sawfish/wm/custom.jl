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

(define-structure sawfish.wm.custom

    (export custom-groups
	    custom-option-alist
	    custom-group-option-alist
	    defcustom
	    defgroup
	    custom-declare-variable
	    custom-declare-group
	    custom-quote-keys
	    define-custom-setter
	    custom-set-property
	    custom-set-group-property
	    custom-get-group-property
	    custom-add-option
	    custom-get-options
	    custom-find-group
	    custom-set
	    custom-set-variable
	    custom-set-typed-variable
	    variable-customized-p
	    variable-type
	    make-custom-form
	    custom-eval
	    custom-serialize
	    custom-deserialize
	    define-custom-serializer
	    define-custom-deserializer
	    custom-load-user-file)

    (open rep
	  rep.data.tables
	  rep.structures
	  sawfish.wm.commands
	  sawfish.wm.gaol
	  sawfish.wm.colors
	  sawfish.wm.fonts)

  ;; list associating groups with the list of variables in that group
  (define custom-groups (list 'root "Sawfish"))

  (defvar custom-user-file "~/.sawfish/custom"
    "File used to store user's configuration settings.")

  (defvar custom-default-file "sawfish/wm/custom-defaults"
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


;;; defining custom variables and groups

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

  (define (custom-declare-variable symbol value keys)
    (let (type prop)
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

  (define (custom-declare-group group &optional doc keys)
    (let (container tem)
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
	(define-command (intern (concat "customize:" (symbol-name group)))
			(lambda ()
			  (require 'sawfish.wm.customize)
			  (customize group))))))

  (define (custom-quote-keys keys)
    (let ((out '()))
      (while (and keys (cdr keys))
	(if (memq (car keys) custom-quoted-keys)
	    (setq out (cons (list 'quote (nth 1 keys))
			    (cons (list 'quote (car keys)) out)))
	  (setq out (cons (nth 1 keys) (cons (list 'quote (car keys)) out))))
	(setq keys (nthcdr 2 keys)))
      (cons 'list (nreverse out))))


;;; general management

  ;; XXX this is lame -- it should build a dedicated environment
  ;; XXX containing only the custom-set-foo functions (and quote), then
  ;; XXX run in that.
  (define (define-custom-setter name def)
    (structure-define (current-structure) name def))
  (define (custom-setter name)
    (and (structure-bound-p (current-structure) name)
	 (%structure-ref (current-structure) name)))
      
  (defmacro custom-set-property (sym prop value)
    "Set the custom key PROP for defcustom'd symbol SYM to value."
    (let ((tem (gensym)))
      `(let ((,tem (cdr (assq ,prop custom-option-alist))))
	 (when ,tem
	   (put ,sym ,tem ,value)))))

  (define (custom-set-group-property group prop value)
    "Set the custom key PROP for defgroup'd symbol SYM to value."
    (unless (listp group)
      (setq group (list group)))
    (let* ((alist (table-ref custom-group-table group))
	   (cell (and alist (assq prop alist))))
      (if cell
	  (rplacd cell value)
	(setq alist (cons (cons prop value) alist))
	(table-set custom-group-table group alist))))

  (define (custom-get-group-property group prop)
    (unless (listp group)
      (setq group (list group)))
    (let ((alist (table-ref custom-group-table group)))
      (cdr (assq prop alist))))

  (define (custom-group-requires group feature)
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

  (define (custom-find-group full-group)
    (when (and (symbolp full-group) (not (null full-group)))
      (setq full-group (list full-group)))
    (let loop ((group full-group)
	       (parent custom-groups))
      (if (null group)
	  parent
	(loop (cdr group)
	      (or (assq (car group) (cddr parent))
		  (error "No such group: %S" full-group))))))

  (define (custom-add-to-group cell full-group &optional tail)
    (when (and (symbolp full-group) (not (null full-group)))
      (setq full-group (list full-group)))
    (let loop ((group full-group)
	       (parent custom-groups))
      (if (null group)
	  (unless (or (memq cell (cddr parent))
		      (assq (car cell) (cddr parent)))
	    ;; reached the bottom most group
	    (rplacd (cdr parent) (nconc (cddr parent) (list cell))))
	;; keep on recursing
	(loop (cdr group)
	      (or (assq (car group) (cddr parent))
		  (error "Unknown group %s" full-group)))
	(unless (cdr group)
	  (rplacd (cdr custom-groups)
		  (nconc (sort (filter consp (cddr custom-groups))
			       (lambda (x y)
				 (string-lessp (cadr x) (cadr y))))
			 (filter atom (cddr custom-groups))))))))


;;; setting values

  (define (custom-set setter symbol)
    (when (get symbol 'custom-before-set)
      ((get symbol 'custom-before-set) symbol))
    (make-variable-special symbol)
    (setter)
    (put symbol 'custom-user-value t)
    (when (get symbol 'custom-after-set)
      ((get symbol 'custom-after-set) symbol)))
 
  (define (custom-set-variable symbol value &optional req)
    ;; XXX kludge for old custom files..
    (when (eq value 'nil) (setq value nil))
    (when (and req value)
      (require req))
    (custom-set (lambda ()
		  (make-variable-special symbol)
		  (set symbol value)) symbol))

  (define (custom-set-typed-variable symbol value type &optional req)
    ;; XXX kludge for old custom files..
    (when (eq value 'nil) (setq value nil))
    (when (and req value)
      (require req))
    (custom-set (lambda ()
		  (make-variable-special symbol)
		  (set symbol (custom-deserialize value type)))
		symbol))

  (define (variable-customized-p symbol)
    "Returns `t' if the variable named SYMBOL has been customized by the user."
    (get symbol 'custom-user-value))

  (define (variable-type symbol)
    "Returns the customizable type of the variable named SYMBOL."
    (get symbol 'custom-type))

  (define (make-custom-form symbol value)
    (let ((fun (or (get symbol 'custom-set) 'custom-set-typed-variable)))
      `(,fun ',symbol ',value
        ,@(and (eq fun 'custom-set-typed-variable)
	       (list (list 'quote (variable-type symbol))))
        ,@(and (get symbol 'custom-require)
	       (list (list 'quote (get symbol 'custom-require)))))))

  (define (custom-eval form) (eval form))


;;; serializing unreadable types

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

  (define (custom-serialize value type)
    "Convert VALUE of TYPE to a printable value."
    (let-fluids ((custom-converter-property 'custom-serializer))
      (custom-convert value type)))

  (define (custom-deserialize value type)
    "Convert VALUE of TYPE back from a printable value."
    (let-fluids ((custom-converter-property 'custom-deserializer))
      (custom-convert value type)))

  (define (define-custom-serializer type fun)
    (put type 'custom-serializer fun))

  (define (define-custom-deserializer type fun)
    (put type 'custom-deserializer fun))


;;; support for font and color primitive types

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


;;; default groups

  (defgroup focus "Focus" :require sawfish.wm.ext.auto-raise)
  (defgroup move "Move/Resize" :require sawfish.wm.commands.move-resize)
  (defgroup placement "Placement")
  (defgroup appearance "Appearance")
  (defgroup workspace "Workspaces")
  (defgroup bindings "Bindings")
  (defgroup min-max "Minimizing/Maximizing")
  (defgroup iconify "Minimizing" :group min-max)
  (defgroup maximize "Maximizing" :group min-max)
  (defgroup misc "Miscellaneous")


;;; loading user's customisations

  (define (custom-load-user-file)
    "Load the user's customization file, or the custom-default-file."
    (cond ((file-exists-p custom-user-file)
	   (load custom-user-file t t t))
	  (custom-default-file
	   (load custom-default-file t))))


;;; gaol init

  (gaol-add defcustom defgroup custom-declare-variable custom-declare-group
	    custom-quote-keys custom-set-property custom-set-group-property
	    custom-option-alist custom-group-option-alist))
