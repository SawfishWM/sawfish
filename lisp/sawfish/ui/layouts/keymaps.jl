#| nokogiri-layouts/keymaps.jl -- shell for binding group

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

(define-structure sawfish.ui.layouts.keymaps ()

    (open rep
	  gui.gtk
	  sawfish.ui.slot
	  sawfish.ui.wm
	  sawfish.ui.layout
	  sawfish.gtk.widget)

  (define (layout-keymaps style slots)
    (let ((menu (gtk-menu-new))
	  (omenu (gtk-option-menu-new))
	  (hbox (gtk-hbox-new nil box-spacing))
	  (vbox (gtk-vbox-new nil box-spacing))
	  (active (car slots)))

      (gtk-box-pack-start hbox (gtk-label-new (_ "Context:")))
      (gtk-box-pack-start hbox omenu)
      (gtk-box-pack-start vbox hbox)

      (let loop ((rest slots)
		 (last nil))
	(when rest
	  (let* ((slot (car rest))
		 (button (gtk-radio-menu-item-new-with-label-from-widget
			  last (beautify-keymap-name (slot-name slot)))))
	    (gtk-menu-append menu button)
	    (gtk-widget-show button)
	    (gtk-signal-connect button "toggled"
				(lambda (w)
				  (when (gtk-check-menu-item-active w)
				    (when active
				      (gtk-container-remove
				       vbox (slot-gtk-widget active)))
				    (setq active slot)
				    (gtk-container-add
				     vbox (slot-gtk-widget active)))))
	    (set-slot-layout slot (slot-gtk-widget slot))
	    (loop (cdr rest) button))))

      (gtk-option-menu-set-menu omenu menu)

      (when active
	(gtk-option-menu-set-history omenu 0)
	(gtk-container-add vbox (slot-gtk-widget active)))

      (gtk-widget-show-all vbox)
      vbox))

  (define-layout-type 'keymaps layout-keymaps)

;;; utils

  (define (beautify-keymap-name symbol)
    (cond ((stringp symbol) symbol)
	  ((not (symbolp symbol)) (format "%s" symbol))
	  (t
	   (let ((name (copy-sequence (symbol-name symbol))))
	     (when (string-match "-keymap" name)
	       (setq name (substring name 0 (match-start))))
	     (while (string-match "[-:]" name)
	       (setq name (concat (substring name 0 (match-start))
				  ?  (substring name (match-end)))))
	     (aset name 0 (char-upcase (aref name 0)))
	     name)))))
