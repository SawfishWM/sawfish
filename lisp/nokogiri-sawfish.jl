#| nokogiri-sawfish.jl -- code to load into window manager

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#


;; meta customizations

(defcustom nokogiri-user-level 'intermediate
  "Show options suitable for \\w users."
  :type (choice novice intermediate expert)
  :user-level novice
  :group ())

(defcustom nokogiri-buttons 'ok
  "Buttons shown in configurator: \\w"
  :type (choice ok revert/cancel/ok apply/revert/cancel/ok)
  :user-level expert
  :group ())

(defcustom customize-show-symbols nil
  "Show variable names of each customization option."
  :group misc
  :user-level expert
  :type boolean)


;; interfaces

(define (nokogiri-report-slot symbol)
  (require 'lisp-doc)
  (let ((type (or (get symbol 'custom-type) 'boolean))
	(dep (get symbol 'custom-depends))
	(doc (documentation symbol))
	(value ((or (get symbol 'custom-get) symbol-value) symbol))
	(allow-nil (get symbol 'custom-allow-nil))
	(user-level (get symbol 'custom-user-level)))
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

    (nconc (list ':name symbol
		 ':spec type
		 ':value value)
	   (and dep (list ':depends dep))
	   (and doc (list ':doc doc))
	   (and user-level (list ':user-level user-level)))))

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
	   (and layout (list ':layout layout)))))

(define (nokogiri-apply-changes changes)
  (mapc (lambda (cell)
	  (customize-set (car cell) (cdr cell))) changes))
