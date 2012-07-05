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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.gtk.widgets.scheme-boolean

    (export )

    (open rep
          gui.gtk-2.gtk
          sawfish.gtk.widget)

  (define (make-item changed-callback)
    (let ((widget (gtk-check-button-new-with-label (_ "Enable"))))

      (gtk-label-set-justify (car (gtk-container-get-children widget)) 'left)
      (gtk-toggle-button-set-inconsistent widget t)

      (g-signal-connect widget "toggled" (lambda (widget)
					   (gtk-toggle-button-set-inconsistent widget nil)))

      (when changed-callback
	(g-signal-connect widget "toggled" (make-signal-callback changed-callback)))

      (gtk-widget-show widget)

      (lambda (op)
	(case op
	  ((set) (lambda (x)
		   (if (not (memq x '(() #f)))
		       (gtk-toggle-button-set-active widget t)
		     (gtk-toggle-button-set-inconsistent widget nil)
		     (gtk-toggle-button-set-active widget nil))))
	  ((clear) (lambda ()
		     (gtk-toggle-button-set-inconsistent widget t)))
	  ((ref) (lambda ()
		   (unless (gtk-toggle-button-get-inconsistent widget)
		     (if (gtk-toggle-button-get-active widget) '#t '#f))))
	  ((gtk-widget) widget)
	  ((validp) (lambda () t))))))

  (define-widget-type 'scheme-boolean make-item)
  (widget-accepts-doc-string 'scheme-boolean))
