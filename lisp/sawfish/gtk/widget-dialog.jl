;; nokogiri-widget-dialog.jl -- simple dialogs from widgets
;;
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.gtk.widget-dialog

    (export widget-dialog)

    (open rep
	  gui.gtk-2.gtk
	  sawfish.gtk.stock
	  sawfish.gtk.widget)

  (define (widget-dialog title spec callback
			 #!optional initial-value main-window)

    (let* ((widget (make-widget spec))
	   (vbox (gtk-vbox-new nil box-spacing))
	   (hbox (gtk-hbox-new nil 0)))

      (when initial-value
	(widget-set widget initial-value))

      (gtk-box-pack-start hbox (gtk-label-new title))
      (gtk-container-add vbox hbox)
      (gtk-container-add vbox (widget-gtk-widget widget))
      (gtk-widget-show-all vbox)
      (simple-dialog title vbox
		     (lambda () (callback (widget-ref widget)))
		     main-window))))
