#| nokogiri-widgets/icon.jl -- GNOME icon entry widget

   $Id$

   Originally written by Bruce Miller <docmad@md.prestige.net>

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

;; GNOME version of this widget

(define-structure sawfish.ui.widgets.icon ()

    (open rep
	  gui.gtk
	  gui.gnome.ui
	  rep.io.files
	  sawfish.gtk.widget)

  (define (make-icon-item changed-callback)
    (let* ((widget (gnome-icon-entry-new "IconEntry" (_ "Select Icon"))))
      (when changed-callback
	(gtk-signal-connect (gnome-icon-entry-gtk-entry widget) "changed"
			    (make-signal-callback changed-callback)))
      (gtk-widget-show widget)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (if x
		       (gnome-icon-entry-set-icon widget x)
		     (gtk-entry-set-text
		      (gnome-icon-entry-gtk-entry widget) ""))))
	  ((clear) (lambda ()
		     (gtk-entry-set-text
		      (gnome-icon-entry-gtk-entry widget) "")))
	  ((ref) (lambda ()
		   (let ((file (gtk-entry-get-text
				(gnome-icon-entry-gtk-entry widget))))
		     (and (file-regular-p file) file))))
	  ((gtk-widget) widget)
	  ((validp) (lambda (x) (and (stringp x)
				     (file-exists-p x))))))))

  (define-widget-type 'icon make-icon-item))
