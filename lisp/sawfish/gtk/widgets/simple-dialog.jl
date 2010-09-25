;; simple-dialog.jl
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
;; sawfish is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; History
;; This file was delete once by mistake, in the commit dbe8c2235, and
;; it had been called "lisp/sawfish/gtk/stock.jl.gtk".
;; Re-added in the commit ce7e6c, as "lisp/sawfish/gtk/stock.jl".
;; Then renamed again in commit a22557030.

(define-structure sawfish.gtk.widgets.simple-dialog

    (export simple-dialog
            widget-dialog)

    (open rep
          gui.gtk-2.gtk
          sawfish.gtk.widget)

  (define (simple-dialog title widget #!optional ok-callback main-window)

    (let ((window (gtk-window-new 'toplevel))
	  (vbox (gtk-vbox-new nil box-spacing))
	  (hbbox (gtk-hbutton-box-new))
	  (ok (gtk-button-new-from-stock "gtk-ok"))
	  (cancel (and ok-callback (gtk-button-new-from-stock "gtk-cancel"))))

      (define (on-cancel)
	(gtk-widget-destroy window))

      (define (on-ok)
	(ok-callback)
	(gtk-widget-destroy window))

      (gtk-window-set-title window title)
      (gtk-window-set-wmclass window "ok_cancel_dialog" "Nokogiri")
      (gtk-container-set-border-width window box-border)
      (when main-window
	(gtk-window-set-transient-for window main-window))

      (gtk-box-set-spacing hbbox button-box-spacing)
      (gtk-button-box-set-layout hbbox 'end)
      (when cancel
	(gtk-box-pack-end hbbox cancel))
      (gtk-box-pack-end hbbox ok)
      (gtk-box-pack-end vbox hbbox)
      (gtk-container-add window vbox)
      (gtk-widget-show-all vbox)

      (gtk-container-add vbox widget)

      (when cancel
	(g-signal-connect cancel "clicked" on-cancel))
      (g-signal-connect ok "clicked" (if ok-callback on-ok on-cancel))
      (g-signal-connect window "delete_event" on-cancel)

      (gtk-widget-show window)
      (gtk-window-set-modal window t)
      (gtk-widget-grab-focus widget)

      window))

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
