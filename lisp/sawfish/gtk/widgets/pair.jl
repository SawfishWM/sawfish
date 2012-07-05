;; nokogiri-widgets/pair.jl -- cons-cell widget
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.gtk.widgets.pair

    (export )

    (open rep
          gui.gtk-2.gtk
          sawfish.gtk.widget)

  ;; (pair CAR-SPEC CDR-SPEC)

  (define (box-packer arg)
    (case arg
      ((start) gtk-box-pack-start)
      ((end) gtk-box-pack-end)
      (t gtk-container-add)))

  (define (make-pair-item changed-callback left right
			  #!optional use-vbox reversed packing)
    (let ((hbox ((if use-vbox gtk-vbox-new gtk-hbox-new) nil box-spacing))
	  (left-widget (make-widget left changed-callback))
	  (right-widget (make-widget right changed-callback)))

      (let ((w1 (if (not reversed) left-widget right-widget))
	    (w2 (if (not reversed) right-widget left-widget)))

	((box-packer (car packing)) hbox (widget-gtk-widget w1))
	((box-packer (cdr packing)) hbox (widget-gtk-widget w2)))

      (gtk-widget-show hbox)

      (lambda (op)
	(case op
	  ((gtk-widget) hbox)
	  ((set) (lambda (x)
		   (widget-set left-widget (car x))
		   (widget-set right-widget (cdr x))))
	  ((clear) (lambda ()
		     (widget-clear left-widget)
		     (widget-clear right-widget)))
	  ((ref) (lambda ()
		   (cons (widget-ref left-widget)
			 (widget-ref right-widget))))
	  ((validp) (lambda (x)
		      (and (widget-valid-p left-widget (car x))
			   (widget-valid-p right-widget (cdr x)))))))))

  (define-widget-type 'pair make-pair-item))
