#| nokogiri-layout.jl -- arranging groups of slots

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

(define-structure nokogiri-layout

    (export define-layout-type
	    layout-slots
	    document-slot)

    (open rep
	  gtk
	  nokogiri-slot
	  nokogiri-widget)

  (define (define-layout-type name fun) (put name 'nokogiri-layout fun))

  (define (layout-type name)
    (or (get name 'nokogiri-layout)
	;; try to dynamically load it
	(let ((module-name (intern (concat "nokogiri-layouts/"
					   (symbol-name name)))))
	  (condition-case nil
	      (progn
		(require module-name)
		(get name 'nokogiri-layout))
	    (error (layout-type 'vbox))))))

  (define (layout-slots name slots)
    ((layout-type (or (car name) name)) name slots))

;;; basic layout styles

  (define (layout-single style slots)
    (cond ((null slots)
	   (let ((placeholder (gtk-vbox-new nil 0)))
	     (gtk-widget-show placeholder)
	     placeholder))
	  ((= (length slots) 1)
	   (let ((w (document-slot (car slots))))
	     (set-slot-layout (car slots) w)
	     w))
	  (t
	   (error "Too many slots for `single' layout"))))

  (define-layout-type 'single layout-single)

  (define (layout-box style slots)
    (let ((box ((if (eq style 'hbox)
		    gtk-hbox-new
		  gtk-vbox-new) nil box-spacing)))
      (gtk-container-border-width box box-border)
      (mapc (lambda (s)
	      (let ((w (document-slot s)))
		(set-slot-layout s w)
		(gtk-box-pack-start box w))) slots)
      (gtk-widget-show box)
      box))

  (define-layout-type 'vbox layout-box)
  (define-layout-type 'hbox layout-box)

  (define (layout-frame style slots)
    (let ((frame (gtk-frame-new (cadr style)))
	  (vbox (layout-slots 'vbox slots)))
      (gtk-container-border-width frame box-border)
      (gtk-container-add frame vbox)
      (gtk-widget-show frame)
      frame))

  (define-layout-type 'frame layout-frame)

;;; including doc strings alongside slot widgets

  (define (document-slot slot)
    (let ((doc (slot-doc slot)))
      (if (null doc)
	  (slot-gtk-widget slot)
	(setq doc (remove-newlines doc))
	(let ((hbox (gtk-hbox-new nil box-spacing))
	      (break (if (string-match "\\\\w" doc)
			 (match-start)
		       -2)))
	  (when (> break 0)
	    (gtk-box-pack-start hbox (make-label (substring doc 0 break))))
	  (gtk-box-pack-start hbox (slot-gtk-widget slot))
	  (when (< break (length doc))
	    (gtk-box-pack-start hbox (make-label (substring doc (+ break 2)))))
	  (gtk-widget-show-all hbox)
	  hbox))))

  (define (remove-newlines string)
    (let loop ((point 0)
	       (out '()))
      (if (string-match "\n" string point)
	  (loop (match-end)
		(list* #\space (substring string point (match-start)) out))
	(apply concat (nreverse (cons (substring string point) out))))))
  
  (define (make-label text)
    (let ((label (gtk-label-new text)))
      (gtk-label-set-justify label 'left)
      ;; XXX GtkLabel line wrapping sucks, but it stops the
      ;; XXX text disappearing..
      (gtk-label-set-line-wrap label t)
      label)))
