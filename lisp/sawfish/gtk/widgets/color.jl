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

(define-structure sawfish.gtk.widgets.color ()

    (open rep
	  gui.gtk-2.gtk
	  rep.regexp
	  sawfish.gtk.widget)

  (defconst default-color "#000000")

  (define (make-color-item changed-callback)
    (let* ((value default-color)
	   (button (gtk-color-button-new-with-color (gdk-color-parse-interp value))))
      (g-signal-connect button "color-set"
        (lambda ()
	  (let ((color (gtk-color-button-get-color-interp button)))
	    (setq value color)
	    (call-callback changed-callback))))
      (gtk-widget-show button)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (setq value x)
		   (gtk-color-button-set-color button (gdk-color-parse-interp value))))
	  ((clear) (lambda ()
		     (setq value default-color)
		     (gtk-color-button-set-color button (gdk-color-parse-interp value))))
	  ((ref) (lambda () value))
	  ((gtk-widget) button)
	  ((validp) (lambda (x)
		      (and (stringp x) (string-match "^#" x))))))))

  (define-widget-type 'color make-color-item))
