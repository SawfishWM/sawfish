#| nokogiri-no-gnome.jl -- workaround lack of GNOME widgets

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

(require 'nokogiri-interfaces)

(define-structure nokogiri-gnome nokogiri-gnome-interface

    (open rep gtk)

  (define (stock-button type)
    (gtk-button-new-with-label
     (case type
       ((ok) (_ "OK"))
       ((cancel) (_ "Cancel"))
       ((revert) (_ "Revert"))
       ((apply) (_ "Apply"))
       ((yes) (_ "Yes"))
       ((no) (_ "No"))
       ((close) (_ "Close"))
       ((help) (_ "Help"))))))

(structure () (open rep gtk nokogiri-widget)

  (define-widget-type 'workspace-geometry
    (lambda (changed)
      (make-widget `(pair (labelled ,(_ "Workspaces:"))
			  (pair (labelled ,(_ "X:") (number 1))
				(labelled ,(_ "Y:") (number 1))) t)
		   changed)))

  (define-widget-type 'icon (lambda (changed) (make-widget 'file changed))))
