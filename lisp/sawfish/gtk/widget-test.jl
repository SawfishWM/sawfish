#| nokogiri-widget-test.jl -- test harness for widgets

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

(define-structure sawfish.gtk.widget-test

    (export test-widget)

    (open rep
	  gui.gtk-2.gtk
	  rep.system
	  sawfish.gtk.widget)

  (define (test-widget spec #!optional initial-value)
    (let (widget)
      (setq widget (make-widget spec (lambda ()
				       (when widget
					 (format standard-output
						 "changed: %s\n"
						 (widget-ref widget))))))
      (when initial-value
	(or (widget-valid-p widget initial-value)
	    (error "Value is not suitable for widget: %s" initial-value))
	(widget-set widget initial-value))
      (let ((window (gtk-window-new 'toplevel)))
	(gtk-container-add window (widget-gtk-widget widget))
	(gtk-signal-connect window "delete_event"
			    (lambda () (throw 'done t)))
	(gtk-widget-show-all window)
	(unwind-protect
	    (catch 'done
	      (recursive-edit))
	  (gtk-widget-destroy window)
	  (gdk-flush))))))
