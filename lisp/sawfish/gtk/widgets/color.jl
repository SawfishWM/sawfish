#| nokogiri-widgets/color.jl -- color selector widget

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

(define-structure nokogiri-widgets/color ()

    (open rep
	  gtk
	  nokogiri-widget
	  nokogiri-color-preview)

  (defconst default-color "#000000")

  (define (make-color-item changed-callback)
    (let* ((value default-color)
	   (button (button-new-with-color value)))
      (gtk-signal-connect
       button "clicked"
       (lambda ()
	 (let ((colorsel (gtk-color-selection-dialog-new (_ "Select color"))))
	   (gtk-color-selection-set-color-interp
	    (gtk-color-selection-dialog-colorsel colorsel)
	    (gdk-color-parse-interp value))
	   (gtk-signal-connect
	    (gtk-color-selection-dialog-ok-button colorsel) "clicked"
	    (lambda ()
	      (let ((color (gtk-color-selection-get-color-interp
			    (gtk-color-selection-dialog-colorsel colorsel))))
		(setq value (format nil "#%04x%04x%04x"
				    (gdk-color-red color)
				    (gdk-color-green color)
				    (gdk-color-blue color)))
		(set-button-color button value)
		(call-callback changed-callback)
		(gtk-widget-destroy colorsel))))
	   (gtk-signal-connect
	    (gtk-color-selection-dialog-cancel-button colorsel) "clicked"
	    (lambda () (gtk-widget-destroy colorsel)))
	   (gtk-signal-connect colorsel "delete_event"
			       (lambda () (gtk-widget-destroy colorsel)))
	   (gtk-widget-hide (gtk-color-selection-dialog-help-button colorsel))
	   (gtk-widget-show colorsel)
	   (gtk-grab-add colorsel))))
      (gtk-widget-show button)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (setq value x)
		   (set-button-color button value)))
	  ((clear) (lambda ()
		     (setq value default-color)
		     (set-button-color button nil)))
	  ((ref) (lambda () value))
	  ((gtk-widget) button)
	  ((validp) (lambda (x)
		      (and (stringp x) (string-match "^#" x))))))))

  (define-widget-type 'color make-color-item))
