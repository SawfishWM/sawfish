#| nokogiri-color-preview.jl -- color previews

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

(define-structure sawfish.gtk.color-preview

    (export button-new-with-color
	    set-button-color)

    (open rep
	  gui.gtk)

  (defconst color-preview-width 28)
  (defconst color-preview-height 16)

  (define (set-preview-color preview color)
    (let ((buf (make-string (* color-preview-width 3))))
      (let ((red (quotient (gdk-color-red color) 256))
	    (green (quotient (gdk-color-green color) 256))
	    (blue (quotient (gdk-color-blue color) 256)))
	(do ((i 0 (1+ i)))
	    ((= i color-preview-width))
	  (aset buf (* i 3) red)
	  (aset buf (1+ (* i 3)) green)
	  (aset buf (+ 2 (* i 3)) blue)))
      (do ((i 0 (1+ i)))
	  ((= i color-preview-height))
	(gtk-preview-draw-row preview buf 0 i color-preview-width))))

  (define (button-new-with-color color-name)
    (let ((button (gtk-button-new))
	  (preview (gtk-preview-new 'color))
	  (color (and color-name (gdk-color-parse-interp color-name))))
      (gtk-preview-size preview color-preview-width color-preview-height)
      (when color
	(set-preview-color preview color))
      (gtk-container-add button preview)
      button))

  (define (set-button-color button color-name)
    (let ((color (and color-name (gdk-color-parse-interp color-name))))
      (when color
	(mapc (lambda (w)
		(when (gtk-preview-p w)
		  (set-preview-color w color)
		  (gtk-widget-draw-interp w)))
	      (gtk-container-children button))))))
