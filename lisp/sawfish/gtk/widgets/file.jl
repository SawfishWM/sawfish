;; nokogiri-widgets/file.jl -- file name widget
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

(define-structure sawfish.gtk.widgets.file

    (export )

    (open rep
          gui.gtk-2.gtk
          sawfish.gtk.widget)

  (define (make-file-item changed-callback)
    (let* ((box (gtk-vbox-new nil box-spacing))
	   (entry (gtk-entry-new))
	   (button (gtk-file-chooser-button-new '() 'open)))
      (gtk-container-set-border-width box box-border)

      (gtk-box-pack-start box button)
      (gtk-box-pack-start box entry)

      (when changed-callback
	(g-signal-connect
	 entry "changed" (make-signal-callback changed-callback)))

      (gtk-widget-show box)

      (g-signal-connect button "file-set" (lambda ()
					    (gtk-entry-set-text entry (gtk-file-chooser-get-filename button))))

      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (gtk-entry-set-text entry (or (and (stringp x) x) ""))))
	  ((clear) (lambda ()
		     (gtk-entry-set-text entry "")))
	  ((ref) (lambda () (gtk-entry-get-text entry)))
	  ((gtk-widget) box)
	  ((validp) (lambda (x) (stringp x)))))))

  (define-widget-type 'file make-file-item))
