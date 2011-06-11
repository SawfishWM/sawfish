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
	  rep.io.files
          gui.gtk-2.gtk
          sawfish.gtk.widget)

  (define (make-file-item changed-callback)
    (let* ((button (gtk-file-chooser-button-new '() 'open)))

      (when changed-callback
	(g-signal-connect
	 button "file-set" (make-signal-callback changed-callback)))

      (gtk-widget-show button)

      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (gtk-file-chooser-select-filename button (or (and (file-exists-p x) x) ""))))
	  ((clear) (lambda ()
		     (gtk-file-chooser-select-filename button "")))
	  ((ref) (lambda () (gtk-file-chooser-get-filename button)))
	  ((gtk-widget) button)
	  ((validp) (lambda (x) (stringp x)))))))

  (define-widget-type 'file make-file-item))
