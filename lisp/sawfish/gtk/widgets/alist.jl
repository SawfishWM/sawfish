#| nokogiri-widgets/alist.jl -- alist widget

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

(define-structure nokogiri-widgets/alist ()

    (open rep
	  gtk
	  nokogiri-widget
	  nokogiri-widget-dialog)

  ;; (alist (KEY-SPEC "KEY-TITLE") (VALUE-SPEC "VALUE-TITLE"))

  (define (make-alist-item changed-callback key value)

    (let* ((spec `(pair ,(or (car key) key)
			,(or (car value) value)))
	   (title `(,(or (cadr key) (_ "Key"))
		    ,(or (cadr value) (_ "Value"))))
	   (type (lambda (op)
		   (case op
		     ((print) (lambda (x) (list (prin1-to-string (car x))
						(prin1-to-string (cdr x)))))
		     ((dialog) (lambda (title callback &optional value)
				 (widget-dialog title spec callback value)))
		     ((validp) ((make-widget spec) 'validp))))))

      (make-widget `(list ,type ,title) changed-callback)))

  (define-widget-type 'alist make-alist-item))
