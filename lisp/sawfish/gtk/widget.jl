;; nokogiri-widget.jl -- high-level widget encapsulation
;;
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.gtk.widget

    (export add-widget-prefix
	    define-widget-type
	    widget-type-constructor
	    widget-accepts-doc-string
	    widget-accepts-doc-string-p
	    make-widget
	    widget-ref
	    widget-set
	    widget-clear
	    widget-gtk-widget
	    widget-valid-p
	    call-callback
	    make-signal-callback
	    tooltip-split
	    tooltip-set
	    set-widget-enabled
	    enable-widget
	    disable-widget
	    box-spacing
	    box-border
	    button-box-spacing)

    ((open rep
	   gui.gtk-2.gtk
	   rep.system
	   rep.util.utf8
	   rep.regexp)
     (access rep.structures))

  (defconst box-spacing 4)
  (defconst box-border 5)
  (defconst button-box-spacing 8)

  ;; list of possible module prefixes when auto-loading widgets
  (define widget-prefixes '(sawfish.gtk.widgets))

  ;; predefined widget types are:

  ;;	(choice SYMBOLS)
  ;;	(symbol OPTIONS...)
  ;;	(string)
  ;;	(number [MIN [MAX [INITIAL-VALUE]]]) ;; integer only
  ;;	  The default of minimum is 0, max 65536,
  ;;	  and initial value is the same as the min.
  ;;	(boolean [LABEL])
  ;;	(color)
  ;;	(font)
  ;;	(or ITEMS...)
  ;;	(and ITEMS...)
  ;;	(v-and ITEMS...)
  ;;	(h-and ITEMS...)
  ;;	(labelled LABEL ITEM)
  ;;	(optional ITEM)

  ;; items without arguments may be specified by name, i.e. `string'
  ;; instead of `(string)'

  (define (add-widget-prefix arg)
    (setq widget-prefixes (cons arg (delq arg widget-prefixes))))

  (define (define-widget-type name constructor)
    (put name 'nokogiri-widget-constructor constructor))

  (define (widget-type-constructor name)
    (or (get name 'nokogiri-widget-constructor)
	;; try to dynamically load the widget constructor..
	(catch 'out
	  (mapc (lambda (prefix)
		  (let ((module-name (intern (concat (symbol-name prefix)
						     #\. (symbol-name name)))))
		    (condition-case nil
			(progn
			  (rep.structures#intern-structure module-name)
			  (throw 'out (get name 'nokogiri-widget-constructor)))
		      (error nil))))
		widget-prefixes)
	  (widget-type-constructor 'unknown))))

  (define (widget-accepts-doc-string name)
    (put name 'nokogiri-widget-accepts-doc-string t))

  (define (widget-accepts-doc-string-p name)
    (widget-type-constructor name)
    (get name 'nokogiri-widget-accepts-doc-string))

  ;; stack `and' items horizontally by default
  (define and-direction (make-fluid 'horizontal))

  (define callback-enabled (make-fluid t))

;;; High level widget management

  ;; each widget is a function taking a single argument, the operation to
  ;; perform on the item. Operations include:

  ;;	(ref) => VALUE
  ;; 	(set VALUE)
  ;;	(clear)
  ;;	gtk-widget => GTK-WIDGET
  ;;	(validp ARG) => BOOL

  ;; functional operations return the function to perform the operation

  ;; create a new item of type defined by CELL, either a list (TYPE ARGS...)
  ;; or a single symbol TYPE. CALLBACK is a function to be called whenever
  ;; the item's value changes
  (define (make-widget cell #!optional callback doc-string)
    (let*
	((type (or (car cell) cell))
	 (maker (or (widget-type-constructor type)
		    (widget-type-constructor 'unknown))))
      (if maker
	  (if (and doc-string (widget-accepts-doc-string-p type))
	      (let* ((split (tooltip-split doc-string))
		     (widget (apply maker callback (car split) (cdr cell))))
		(when (cdr split)
		  (tooltip-set (widget-gtk-widget widget) (cdr split)))
		widget)
	    (apply maker callback (cdr cell)))
	(error "No widget of type %s" type))))

  (define (widget-ref item) ((item 'ref)))

  (define (widget-set item value)
    (let-fluids ((callback-enabled nil))
      ((item 'set) value)))

  (define (widget-clear item)
    (let-fluids ((callback-enabled nil))
      ((item 'clear))))

  (define (widget-gtk-widget item) (item 'gtk-widget))

  (define (widget-valid-p item value) ((item 'validp) value))

  (define (set-widget-enabled item enabled)
    (gtk-widget-set-sensitive (widget-gtk-widget item) enabled))

  (define (enable-widget item) (set-widget-enabled item t))
  (define (disable-widget item) (set-widget-enabled item nil))

  (define (call-callback fun)
    (when (and fun (fluid callback-enabled))
      (fun)))

  (define (make-signal-callback fun) (lambda () (call-callback fun)))

;;; tooltip support

  (define tooltips)

  ;; returns (LABEL-STRING . TOOLTIP-STRING-OR-NIL)
  (define (tooltip-split doc)
    (setq doc (_ doc))
    (if (string-match "\n\n\\s*" doc)
	(cons (substring doc 0 (match-start))
	      (substring doc (match-end)))
      (cons doc nil)))

  (define (tooltip-set widget tip-string #!optional (key "Foo"))
    (unless tooltips
      (setq tooltips (gtk-tooltips-new)))
    (gtk-tooltips-set-tip tooltips widget tip-string key))

;;; Predefined widget constructors

  (define (make-choice-item changed-callback . options)
    (let ((omenu (gtk-option-menu-new))
	  (menu (gtk-menu-new))
	  (value (or (caar options) (car options))))
      (let loop ((rest options)
		 (last nil))
	(when rest
	  (let ((button (gtk-radio-menu-item-new-with-label-from-widget
			 last (_ (or (cadar rest)
				     (symbol-name (car rest)))))))
	    (gtk-menu-shell-append menu button)
	    (gtk-widget-show button)
	    (g-signal-connect button "toggled"
			      (lambda (w)
				(when (gtk-check-menu-item-active w)
				  (setq value (or (caar rest) (car rest)))
				  (call-callback changed-callback))))
	    (loop (cdr rest) button))))
      (gtk-option-menu-set-menu omenu menu)
      (gtk-widget-show-all omenu)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (setq value x)
		   (let ((idx (option-index options x)))
		     (gtk-option-menu-set-history omenu (or idx 0))
		     (do ((i 0 (1+ i))
			  (rest (gtk-container-get-children menu) (cdr rest)))
			 ((null rest))
		       (gtk-check-menu-item-set-active (car rest) (= i idx))))))
	  ((clear) nop)
	  ((ref) (lambda () value))
	  ((gtk-widget) omenu)
	  ((validp) (lambda (x) (option-index options x)))))))

  (define-widget-type 'choice make-choice-item)

  (define (make-symbol-item changed-callback #!rest options)
    (let ((widget (gtk-combo-new)))
      (when options
	(gtk-combo-set-popdown-strings
	 widget (cons "" (mapcar symbol-name options))))
      (when changed-callback
	(g-signal-connect
	 (gtk-combo-entry widget)
	 "changed" (make-signal-callback changed-callback)))
      (gtk-widget-show widget)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   ;; Can't i18n'ize these strings..
		   (gtk-entry-set-text (gtk-combo-entry widget)
				       (if x (symbol-name x) ""))))
	  ((clear) (lambda ()
		     (gtk-entry-set-text (gtk-combo-entry widget) "")))
	  ((ref) (lambda ()
		   (string->symbol
		    (gtk-entry-get-text (gtk-combo-entry widget)))))
	  ((gtk-widget) widget)
	  ((validp) symbolp)))))

  (define-widget-type 'symbol make-symbol-item)

  (define (make-string-item changed-callback)
    (let ((widget (gtk-entry-new)))
      (when changed-callback
	(g-signal-connect
	 widget "changed" (make-signal-callback changed-callback)))
      (gtk-widget-show widget)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (gtk-entry-set-text widget (or x ""))))
	  ((clear) (lambda ()
		     (gtk-entry-set-text widget "")))
	  ((ref) (lambda () (gtk-entry-get-text widget)))
	  ((gtk-widget) widget)
	  ((validp) stringp)))))

  (define-widget-type 'string make-string-item)

  (define (make-number-item changed-callback
                            #!optional minimum maximum initial-value)
    ;; XXX backwards compat..
    (when (eq minimum 'nil) (setq minimum nil))
    (when (eq maximum 'nil) (setq maximum nil))
    (let ((widget (gtk-spin-button-new-with-range (or minimum 0)
						  (or maximum 65535) 1)))
      (when initial-value
        (gtk-spin-button-set-value widget initial-value))
      (when changed-callback
	(g-signal-connect
	 widget "value-changed" (make-signal-callback changed-callback)))
      (gtk-widget-show widget)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (when (numberp x)
		     (gtk-spin-button-set-value widget x))))
	  ((clear) nop)
	  ((ref) (lambda ()
		   (let ((value (gtk-spin-button-get-value widget)))
		     (if (integerp value)
			 (inexact->exact value)
		       value))))
	  ((gtk-widget) widget)
	  ((validp) numberp)))))

  (define-widget-type 'number make-number-item)

  (define (make-boolean-item changed-callback #!optional label)
    (let ((widget (if label
		      (gtk-check-button-new-with-label label)
		    (gtk-check-button-new))))
      (when label
	(gtk-label-set-justify (car (gtk-container-get-children widget)) 'left))
      (when changed-callback
	(g-signal-connect
	 widget "toggled" (make-signal-callback changed-callback)))
      (gtk-widget-show widget)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (gtk-toggle-button-set-active widget x)))
	  ((clear) (lambda ()
		     (gtk-toggle-button-set-active widget nil)))
	  ((ref) (lambda ()
		   (gtk-toggle-button-get-active widget)))
	  ((gtk-widget) widget)
	  ((validp) (lambda () t))))))

  (define-widget-type 'boolean make-boolean-item)
  (widget-accepts-doc-string 'boolean)

;;; ``Meta'' widgets

  (define (make-or-item changed-callback #!rest items)
    (setq items (mapcar (lambda (x)
			  (make-widget x changed-callback)) items))
    (let* ((box (gtk-vbox-new nil box-spacing))
	   (hboxes (mapcar (lambda ()
			     (gtk-hbox-new nil box-spacing)) items))
	   (checks (mapcar (lambda ()
			     (gtk-check-button-new)) items))
	   (enabled-item nil)
	   (refresh-item
	    (lambda ()
	      (let ((i 0))
		(mapc (lambda (x)
			(set-widget-enabled x (eq enabled-item x))
			(gtk-toggle-button-set-active
			 (nth i checks) (eq enabled-item x))
			(setq i (1+ i))) items))
	      (call-callback changed-callback)))
	   (toggle-fun
	    (lambda (index)
	      (when (or (eq (nth index items) enabled-item)
			(gtk-toggle-button-get-active (nth index checks)))
		(setq enabled-item (and (gtk-toggle-button-get-active
					 (nth index checks))
					(nth index items))))
	      (refresh-item))))
      (do ((i 0 (1+ i)))
	  ((= i (length items)))
	(g-signal-connect (nth i checks) "toggled" (lambda ()
                                                     (toggle-fun i)))
	(gtk-box-pack-start box (nth i hboxes))
	(gtk-box-pack-start (nth i hboxes) (nth i checks))
	(gtk-box-pack-start (nth i hboxes) (widget-gtk-widget (nth i items))))
      (refresh-item)
      (gtk-widget-show-all box)
      (lambda (op)
	(case op
	  ((set)
	   (lambda (x)
	     (if (null x)
		 (setq enabled-item nil)
	       (if (and enabled-item (widget-valid-p enabled-item x))
		   ;; set the enabled value if possible
		   (widget-set enabled-item x)
		 ;; look for a matching type
		 (catch 'done
		   (do ((i 0 (1+ i)))
		       ((= i (length items)))
		     (when (widget-valid-p (nth i items) x)
		       (setq enabled-item (nth i items))
		       (widget-set enabled-item x)
		       (throw 'done t)))
		   (message (format nil (_ "No matching item for %S") x)))))
	     (refresh-item)))
	  ((clear) (lambda ()
		     (setq enabled-item nil)
		     (mapc (lambda (item)
			     (widget-clear item)) items)
		     (refresh-item)))
	  ((ref) (lambda ()
		   (and enabled-item (widget-ref enabled-item))))
	  ((gtk-widget) box)
	  ((validp) (lambda (x)
		      (catch 'out
			(do ((i 0 (1+ i)))
			    ((= i (length items)))
			  (when (widget-valid-p (nth i items) x)
			    (throw 'out t))))))))))

  (define-widget-type 'or make-or-item)

  (defun make-and-item (changed-callback #!rest items)
    (setq items (mapcar (lambda (x)
			  (make-widget x changed-callback)) items))
    (let
	((box ((if (eq (fluid and-direction) 'horizontal)
		   gtk-hbox-new
		 gtk-vbox-new) nil box-spacing)))
      (do ((i 0 (1+ i)))
	  ((= i (length items)))
	(gtk-box-pack-start box (widget-gtk-widget (nth i items)) t t))
      (gtk-widget-show-all box)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (do ((i 0 (1+ i)))
		       ((= i (length items)))
		     (widget-set (nth i items) (nth i x)))))
	  ((clear) (lambda () (mapc widget-clear items)))
	  ((ref) (lambda () (mapcar widget-ref items)))
	  ((gtk-widget) box)
	  ((validp) (lambda (x)
		      (cond ((null x))
			    ((or (not (listp x))
				 (/= (length x) (length items)))
			     nil)
			    (t
			     (catch 'out
			       (do ((i 0 (1+ i)))
				   ((= i (length items)))
				 (unless (widget-valid-p
					  (nth i items) (nth i x))
				   (throw 'out nil)))
			       t)))))))))

  (define-widget-type 'and make-and-item)

  (define-widget-type 'h-and (lambda (#!rest args)
			       (let-fluids ((and-direction 'horizontal))
				 (apply make-and-item args))))

  (define-widget-type 'v-and (lambda (#!rest args)
			       (let-fluids ((and-direction 'vertical))
				 (apply make-and-item args))))

  (define (make-labelled-item changed-callback label item)
    (let ((box (gtk-hbox-new nil box-spacing)))
      (setq item (make-widget item changed-callback))
      ;; XXX i18n the label string?
      (gtk-box-pack-start box (gtk-label-new (_ label)))
      (gtk-container-add box (widget-gtk-widget item))
      (gtk-widget-show-all box)
      (lambda (op)
	(if (eq op 'gtk-widget)
	    box
	  (item op)))))

  (define-widget-type 'labelled make-labelled-item)

  (define (make-optional-item changed-callback item)
    (let ((box (gtk-hbox-new nil box-spacing))
	  (check (gtk-check-button-new)))
      (setq item (make-widget item changed-callback))
      (gtk-box-pack-start box check)
      (gtk-box-pack-start box (widget-gtk-widget item))
      (g-signal-connect
       check "toggled"
       (lambda ()
	 (set-widget-enabled item (gtk-toggle-button-get-active check))
	 (call-callback changed-callback)))
      (gtk-toggle-button-set-active check nil)
      (disable-widget item)
      (gtk-widget-show-all box)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (when x
		     (widget-set item x))
		   (set-widget-enabled item x)
		   (gtk-toggle-button-set-active check x)))
	  ((clear) (lambda ()
		     (widget-clear item)
		     (disable-widget item)
		     (gtk-toggle-button-set-active check nil)))
	  ((ref) (lambda ()
		   (and (gtk-toggle-button-get-active check)
			(widget-ref item))))
	  ((gtk-widget) box)
	  ((validp) (lambda (x)
		      (or (null x) (widget-valid-p item x))))))))

  (define-widget-type 'optional make-optional-item)

  (define (make-quoted-item changed item)
    (let ((widget (make-widget item changed)))
      (lambda (op)
	(case op
	  ((ref) (lambda ()
		   (list 'quote (widget-ref widget))))
	  ((set) (lambda (x)
		   (widget-set widget (cadr x))))
	  ((validp) (lambda (x)
		      (and (eq (car x) 'quote)
			   (widget-valid-p widget (cadr x)))))
	  (t (widget op))))))

  (define-widget-type 'quoted make-quoted-item)

;;; widget used for unknown widget types

  (define (make-unknown-item changed-callback)
    (declare (unused changed-callback))
    (let ((label (gtk-label-new (format nil (_ "** unknown widget **  "))))
	  value)
      (gtk-widget-show label)
      (lambda (op)
	(case op
	  ((set) (lambda (x) (setq value x)))
	  ((clear) (lambda () (setq value nil)))
	  ((ref) (lambda () value))
	  ((gtk-widget) label)
	  ((validp) (lambda (x) (declare (unused x)) t))))))

  (define-widget-type 'unknown make-unknown-item)

;;; utility functions

  (define string->symbol intern)

  (define (option-index lst x)
    (let loop ((i 0) (rest lst))
      (cond ((null rest) nil)
	    ((eq (or (caar rest) (car rest)) x) i)
	    (t (loop (1+ i) (cdr rest)))))))
