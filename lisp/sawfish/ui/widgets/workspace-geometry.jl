#| nokogiri-widgets/workspace-geometry.jl

   $Id: workspace-geometry.jl,v 1.1 2000/09/01 20:03:29 john Exp $

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

;; GNOME-less version of this widget

(define-structure sawfish.ui.widgets.workspace-geometry ()

    (open rep
	  sawfish.gtk.widget)

  (define-widget-type 'workspace-geometry
    (lambda (changed)
      (make-widget `(pair (labelled ,(_ "Workspaces:") (number 1))
			  (pair (labelled ,(_ "Columns:") (number 1))
				(labelled ,(_ "Rows:") (number 1))) t)
		   changed))))
