#| nokogiri-widgets/font.jl -- font selection widget

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

(define-structure sawfish.gtk.widgets.font ()

    (open rep
	  gui.gtk
	  sawfish.gtk.widget)

  (defconst default-font "fixed")

  (define (make-font-item changed-callback)
    (let* ((box (gtk-hbox-new nil box-spacing))
	   (entry (gtk-entry-new))
	   (button (gtk-button-new-with-label (_ "Browse..."))))
      (gtk-box-pack-start box entry)
      (gtk-box-pack-start box button)
      (when changed-callback
	(gtk-signal-connect
	 entry "changed" (make-signal-callback changed-callback)))
      (gtk-signal-connect
       button "clicked"
       (lambda ()
	 (let ((fontsel (gtk-font-selection-dialog-new (_ "Select font"))))
	   (gtk-font-selection-dialog-set-font-name
	    fontsel (gtk-entry-get-text entry))
	   (gtk-signal-connect
	    (gtk-font-selection-dialog-ok-button fontsel) "clicked"
	    (lambda ()
	      (gtk-entry-set-text
	       entry (gtk-font-selection-dialog-get-font-name fontsel))
	      (gtk-widget-destroy fontsel)))
	   (gtk-signal-connect
	    (gtk-font-selection-dialog-cancel-button fontsel) "clicked"
	    (lambda () (gtk-widget-destroy fontsel)))
	   (gtk-signal-connect fontsel "delete_event"
			       (lambda () (gtk-widget-destroy fontsel)))
	   (gtk-widget-show fontsel)
	   (gtk-grab-add fontsel))))
      (gtk-widget-show box)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (gtk-entry-set-text entry (and (stringp x) x))))
	  ((clear) (lambda ()
		     (gtk-entry-set-text entry default-font)))
	  ((ref) (lambda () (gtk-entry-get-text entry)))
	  ((gtk-widget) box)
	  ((validp) (lambda (x) (stringp x)))))))

  (define-widget-type 'font make-font-item))
