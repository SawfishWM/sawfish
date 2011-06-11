;; nokogiri-widgets/scheme-boolean.jl --
;;
;; Copyright (C) 2002 John Harper <jsh@unfactored.org>
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

(define-structure sawfish.gtk.widgets.scheme-boolean

    (export )

    (open rep
          gui.gtk-2.gtk
          sawfish.gtk.widget)

  (define (make-item changed-callback)
    (let ((widget (gtk-check-button-new-with-label (_ "Enable"))))
      (gtk-label-set-justify (car (gtk-container-get-children widget)) 'left)
      (gtk-toggle-button-set-inconsistent widget t)
      (g-signal-connect
       widget "toggled"
       (lambda ()
	 (when changed-callback
	   (call-callback changed-callback))))
      (gtk-widget-show widget)
      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (gtk-toggle-button-set-active widget x)))
	  ((clear) (lambda ()
		     (gtk-toggle-button-set-inconsistent widget t)))
	  ((ref) (lambda ()
		   (unless (gtk-toggle-button-get-inconsistent widget)
		     (if (gtk-toggle-button-get-active widget) '#t '#f))))
	  ((gtk-widget) widget)
	  ((validp) (lambda () t))))))

  (define-widget-type 'scheme-boolean make-item)
  (widget-accepts-doc-string 'scheme-boolean))
