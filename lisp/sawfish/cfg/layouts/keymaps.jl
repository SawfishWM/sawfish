;; nokogiri-layouts/keymaps.jl -- shell for binding group
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

(define-structure sawfish.cfg.layouts.keymaps

    (export )

    (open rep
          gui.gtk-2.gtk
          rep.regexp
          sawfish.cfg.slot
          sawfish.cfg.wm
          sawfish.cfg.layout
          sawfish.gtk.widget)

  (define label-ptr nil)

  (define (keymap-slot-p slot)
    ;; XXX so fucking evil!
    (string-match "-keymap$" (symbol-name (slot-name slot))))

  (define (layout-keymaps style slots)
    (declare (unused style))

    (let* ((menu (gtk-menu-new))
	   (omenu (gtk-option-menu-new))
	   (hbox (gtk-hbox-new nil box-spacing))
	   (vbox (gtk-vbox-new nil box-spacing))
	   (km-vbox (gtk-vbox-new nil box-spacing))
	   (keymap-slots (filter keymap-slot-p slots))
	   (other-slots (filter (lambda (x) (not (keymap-slot-p x))) slots))
	   (active (car keymap-slots)))

      (when keymap-slots
	(setq label-ptr (gtk-label-new (_ "Context:")))
	(gtk-box-pack-start hbox label-ptr)
	(gtk-box-pack-start hbox omenu)
	(gtk-widget-relate-label omenu label-ptr)
	(gtk-box-pack-start vbox hbox)

	(let loop
	    ((rest keymap-slots)
	     (last nil))
	  (when rest
	    (let* ((slot (car rest))
		   (button (gtk-radio-menu-item-new-with-label-from-widget
			    last (beautify-keymap-name (slot-name slot)))))
	      (gtk-menu-shell-append menu button)
	      (gtk-widget-show button)
	      (g-signal-connect button "toggled"
				(lambda (w)
				  (when (gtk-check-menu-item-active w)
				    (when active
				      (gtk-container-remove
				       km-vbox (slot-gtk-widget active)))
				    (setq active slot)
				    (gtk-box-pack-start
				     km-vbox (slot-gtk-widget active) t t))))
	      (set-slot-layout slot (slot-gtk-widget slot))
	      (loop (cdr rest) button))))

	(gtk-option-menu-set-menu omenu menu)

	(when active
	  (gtk-option-menu-set-history omenu 0)
	  (gtk-box-pack-start km-vbox (slot-gtk-widget active) t t))

	(gtk-box-pack-start vbox km-vbox t t))

      (gtk-box-pack-start vbox (layout-slots 'vbox other-slots))

      (gtk-widget-show-all vbox)
      vbox))

  (define-layout-type 'keymaps layout-keymaps)

;;; utils

  ;; also in sawfish-xgettext
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
	     (_ name))))))
