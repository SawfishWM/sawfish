#| nokogiri-gnome.jl -- some code to use GNOME widgets

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

(require 'nokogiri-interfaces)

(define-structure nokogiri-gnome nokogiri-gnome-interface

    (open rep
	  gtk
	  gnomeui)

  (define (stock-button type)
    (case type
      ((ok cancel yes no close apply help)
       (gnome-stock-button type))
      ((revert)
       (gtk-button-new-with-label (_ "Revert")))))

  (define (simple-dialog title widget #!optional ok-callback main-window)

    (let ((window (gnome-dialog-new title (if ok-callback
					      '(ok cancel)
					    '(ok)))))

      (gtk-window-set-wmclass window "ok_cancel_dialog" "Nokogiri")
      (when main-window
	(gnome-dialog-set-parent window main-window))
      (gtk-container-add (gnome-dialog-vbox window) widget)

      (gtk-signal-connect window "clicked"
			  (lambda (w button)
			    (if (and (= button 0) ok-callback)
				(ok-callback))
			    (gtk-widget-destroy w)))
      (gtk-signal-connect window "delete_event" gtk-widget-destroy)

      (gtk-widget-show window)
      (gtk-window-set-modal window t)
      (gtk-widget-grab-focus widget)

      window)))
