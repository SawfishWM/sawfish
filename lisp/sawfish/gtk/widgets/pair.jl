#| nokogiri-widgets/pair.jl -- cons-cell widget

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

(define-structure nokogiri-widgets/pair ()

    (open rep
	  gtk
	  nokogiri-widget)

  ;; (pair CAR-SPEC CDR-SPEC)

  (define (make-pair-item changed-callback left right
			  &optional use-vbox reversed)
    (let ((hbox ((if use-vbox gtk-vbox-new gtk-hbox-new) nil box-spacing))
	  (left-widget (make-widget left changed-callback))
	  (right-widget (make-widget right changed-callback)))

      (unless reversed
	(gtk-container-add hbox (widget-gtk-widget left-widget)))
      (gtk-container-add hbox (widget-gtk-widget right-widget))
      (when reversed
	(gtk-container-add hbox (widget-gtk-widget left-widget)))
      (gtk-widget-show hbox)

      (lambda (op)
	(case op
	  ((gtk-widget) hbox)
	  ((set) (lambda (x)
		   (widget-set left-widget (car x))
		   (widget-set right-widget (cdr x))))
	  ((clear) (lambda ()
		     (widget-clear left-widget)
		     (widget-clear right-widget)))
	  ((ref) (lambda ()
		   (cons (widget-ref left-widget)
			 (widget-ref right-widget))))
	  ((validp) (lambda (x)
		      (and (widget-valid-p left-widget (car x))
			   (widget-valid-p right-widget (cdr x)))))))))

  (define-widget-type 'pair make-pair-item))
