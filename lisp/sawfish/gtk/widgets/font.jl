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
	  rep.system
	  gui.gtk-2.gtk
	  sawfish.gtk.widget
	  sawfish.wm.util.font)

  (defconst default-font "fixed")

  ;; FIXME: this is broken, in that only if Xrender is present
  ;; does gdk use Xft. But, it's the best I can do..
  (define use-xft (let ((x (getenv "GDK_USE_XFT")))
		    (and x (/= (string->number x) 0))))

  (define (make-font-item changed-callback)
    (let* ((box (gtk-hbox-new nil box-spacing))
	   (entry (gtk-entry-new))
	   (button (gtk-button-new-with-label (_ "Browse..."))))
      (gtk-editable-set-editable entry nil)
      (gtk-box-pack-start box entry t t)
      (gtk-box-pack-start box button)
      (when changed-callback
	(g-signal-connect
	 entry "changed" (make-signal-callback changed-callback)))
      (g-signal-connect
       button "clicked"
       (lambda ()
	 (let ((fontsel (gtk-font-selection-dialog-new (_ "Select font"))))
	   (gtk-font-selection-dialog-set-font-name
	    fontsel (gtk-entry-get-text entry))
	   (g-signal-connect
	    (gtk-font-selection-dialog-ok-button fontsel) "clicked"
	    (lambda ()
	      (gtk-entry-set-text
	       entry (gtk-font-selection-dialog-get-font-name fontsel))
	      (gtk-widget-destroy fontsel)))
	   (g-signal-connect
	    (gtk-font-selection-dialog-cancel-button fontsel) "clicked"
	    (lambda () (gtk-widget-destroy fontsel)))
	   (g-signal-connect fontsel "delete_event"
			       (lambda () (gtk-widget-destroy fontsel)))
	   (gtk-widget-show fontsel)
	   (gtk-grab-add fontsel))))
      (gtk-widget-show box)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (cond ((stringp x)
			  (gtk-entry-set-text entry x))
			 ((consp x)
			  (let ((face (cond
					((string-equal (car x) "Xft")
					 (xft-description->face (cdr x)))
					((string-equal (car x) "xlfd")
					 (xlfd-description->face (cdr x))))))
			    (when face
			      (gtk-entry-set-text
			       entry (face->pango-description face)))))
			 (t (gtk-entry-set-text entry default-font)))))
	  ((clear) (lambda ()
		     (gtk-entry-set-text entry default-font)))
	  ((ref) (lambda ()
		   ;; FIXME: Assumes we'll always be using Xft..
		   (let* ((pango-name (gtk-entry-get-text entry))
			  (face (pango-description->face pango-name)))
		     (cond ((not face) nil)
			   (use-xft (cons "Xft" (face->xft-description face)))
			   (t (cons "xlfd" (face->xlfd-description face)))))))
	  ((gtk-widget) box)
	  ((validp) (lambda (x) (stringp x)))))))

  (define-widget-type 'font make-font-item))
