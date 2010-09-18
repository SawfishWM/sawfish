;; nokogiri-widgets/alist.jl -- alist widget
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

(define-structure sawfish.gtk.widgets.alist

    (export )

    (open rep
          gui.gtk-2.gtk
          sawfish.gtk.widget
          sawfish.gtk.widgets.simple-dialog)

  ;; (alist (KEY-SPEC "KEY-TITLE") (VALUE-SPEC "VALUE-TITLE"))

  (define (make-alist-item changed-callback key value)

    (let* ((spec `(pair ,(or (car key) key)
			,(or (car value) value)))
	   (title `(,(or (cadr key) (_ "Key"))
		    ,(or (cadr value) (_ "Value")))))

      (define (type op)
	(case op
	  ((print) (lambda (x) (list (format nil "%s" (car x))
				     (format nil "%s" (cdr x)))))
	  ((dialog) (lambda (title callback #!key value for)
		      (widget-dialog title spec callback value for)))
	  ((validp) ((make-widget spec) 'validp))))

      (make-widget `(list ,type ,title) changed-callback)))

  (define-widget-type 'alist make-alist-item))
