;; nokogiri.jl -- interface to sawfish-config
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

(define-structure sawfish.wm.util.nokogiri

    (export nokogiri-report-slot
	    nokogiri-report-slots
	    nokogiri-report-group
	    nokogiri-apply-changes
	    nokogiri-report-commands
	    nokogiri-grab-key
	    nokogiri-grab-match-window-property)

    (open rep
	  sawfish.wm.custom
	  sawfish.wm.events
	  sawfish.wm.customize
	  sawfish.wm.commands
	  sawfish.wm.util.keymap
	  sawfish.wm.ext.match-window)

  (defvar customize-show-symbols nil
    "Show variable names of each customization option.")

  (defvar customize-command-classes '(default)
    "In configurator \"binding\" section, also include commands of
these classes. Possible keys are default, advanced, viewport, and
deprecated.")

;;; interfaces

  (define (nokogiri-report-slot symbol)
    (require 'rep.lang.doc)
    (let* ((type (variable-type symbol))
	   (dep (get symbol 'custom-depends))
	   (doc (documentation symbol))
	   (value (if (get symbol 'custom-get)
		      ((get symbol 'custom-get) symbol)
		    (custom-serialize (symbol-value symbol) type)))
	   (widget-flags (get symbol 'custom-widget-flags)))
      (when (stringp doc)
	(setq doc (_ doc))
	(when customize-show-symbols
	  (setq doc (format nil "%s\n[%s]" doc (symbol-name symbol)))))

      (when (and (symbolp type) (get type 'custom-widget))
	(setq type ((get type 'custom-widget) type)))

      ;; backwards compatibility
      (case type
	((symbol)
	 (setq type `(choice ,@(custom-get-options symbol))))
	((number)
	 (when (get symbol 'custom-range)
	   (let ((range (get symbol 'custom-range)))
	     (setq type `(number ,(car range) ,(cdr range))))))
	((file-name)
	 (setq type 'file))
	((program-name)
	 (setq type 'program)))

      (nconc (list #:name symbol
		   #:type type
		   #:value value)
	     (and dep (list #:depends dep))
	     (and doc (list #:doc doc))
	     (and widget-flags (list #:widget-flags widget-flags)))))

  (define (nokogiri-report-slots names)
    (mapcar nokogiri-report-slot names))

  (define (nokogiri-report-group group)
    (let ((group (custom-find-group (cdr group)))
	  (layout (custom-get-group-property (cdr group) ':layout))
	  (requires (custom-get-group-property (cdr group) ':require)))
      (mapc require requires)
      (nconc (list (car group) (cadr group)
		   (mapcar (lambda (x)
			     (if (atom x)
				 x
			       (list (car x) (cadr x)))) (cddr group)))
	     (and layout (list #:layout layout)))))

  (define (nokogiri-apply-changes changes)
    (mapc (lambda (cell)
	    (customize-set (car cell)
			   (custom-deserialize (cdr cell)
					       (variable-type (car cell)))))
	  changes))

  (define (nokogiri-report-commands)
    (mapcar (lambda (sym)
	      (let ((params (command-type sym)))
		(if params
		    (list sym #:type params)
		  sym)))
	    (sort (apropos "" (lambda (x)
				(and (commandp x)
				     (memq (command-class x)
					   customize-command-classes)))))))

  (define (nokogiri-grab-key) (event-name (read-event)))

  (define (nokogiri-grab-match-window-property name)
    (match-window-grab-x-property name))

;;; ignore these commands in customizer

  (mapc (lambda (x) (put x 'deprecated-command t))
	'(apropos-function apropos-variable beep call-command
                           copy-file delete-directory delete-file describe-key
                           describe-symbol focus-click garbage-collect load
                           make-directory nop rename-file set step system
                           trace untrace recursive-edit)))
