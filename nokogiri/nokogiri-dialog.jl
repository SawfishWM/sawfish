#| nokogiri-dialog.jl -- simple dialogs

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

(define-structure nokogiri-dialog nokogiri-dialog-interface

    (open rep
	  gtk
	  nokogiri-gnome
	  nokogiri-shell		;for main-window
	  nokogiri-widget)		;for box- constants

  (define (ok-cancel-dialog widget title &optional ok-callback)

    (let ((window (gtk-window-new 'dialog))
	  (vbox (gtk-vbox-new nil box-spacing))
	  (hbbox (gtk-hbutton-box-new))
	  (ok (stock-button 'ok))
	  (cancel (and ok-cancel-dialog (stock-button 'cancel))))

      (define (on-cancel)
	(gtk-widget-destroy window))

      (define (on-ok)
	(ok-callback)
	(gtk-widget-destroy window))
	  
      (gtk-window-set-title window title)
      (gtk-window-set-wmclass window "ok_cancel_dialog" "Nokogiri")
      (gtk-container-border-width window box-border)
      (gtk-window-set-transient-for window main-window)

      (gtk-button-box-set-layout hbbox 'end)
      (gtk-box-pack-start hbbox ok)
      (when cancel
	(gtk-box-pack-end hbbox cancel))
      (gtk-box-pack-end vbox hbbox)
      (gtk-container-add window vbox)
      (gtk-widget-show-all vbox)

      (gtk-container-add vbox widget)

      (when cancel
	(gtk-signal-connect cancel "clicked" on-cancel))
      (gtk-signal-connect ok "clicked" (if ok-callback on-ok on-cancel))
      (gtk-signal-connect window "delete_event" on-cancel)

      (gtk-widget-show window)
      ;;(gtk-grab-add window)
      (gtk-widget-grab-focus widget)

      window)))
