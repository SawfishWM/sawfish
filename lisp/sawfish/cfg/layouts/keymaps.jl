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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, 
;; Boston, MA 02110-1301 USA. 

(define-structure sawfish.cfg.layouts.keymaps

    (export )

    (open rep
          gui.gtk-2.gtk
          rep.regexp
	  rep.util.misc
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

    (let* ((hbox (gtk-hbox-new nil box-spacing))
	   (vbox (gtk-vbox-new nil box-spacing))
	   (km-vbox (gtk-vbox-new nil box-spacing))
	   (keymap-slots (filter keymap-slot-p slots))
	   (other-slots (filter (lambda (x) (not (keymap-slot-p x))) slots))
	   (active (car keymap-slots))
           (combo (gtk-combo-box-text-new)))

      (when keymap-slots
	(setq label-ptr (gtk-label-new (_ "Context:")))
	(gtk-box-pack-start hbox label-ptr)
        (gtk-box-pack-start hbox combo)
	(gtk-box-pack-start vbox hbox)

	(let loop
	    ((rest keymap-slots))
	  (when rest
            (gtk-combo-box-text-append-text combo
              (or (_ (beautify-symbol-name (slot-name (car rest) #:cut "-keymap")))
                  (_ (cadar rest))))
	      (loop (cdr rest)))

        (gtk-combo-box-set-active combo 0)

        (g-signal-connect combo "changed"
            (lambda ()
              (let ((slot (nth (gtk-combo-box-get-active combo) keymap-slots)))
                (when active
                  (gtk-container-remove km-vbox (slot-gtk-widget active)))
                (setq active slot)
                (gtk-box-pack-start km-vbox (slot-gtk-widget active) t t)))))

	(when active
	  (gtk-box-pack-start km-vbox (slot-gtk-widget active) t t))

	(gtk-box-pack-start vbox km-vbox t t))

      (gtk-box-pack-start vbox (layout-slots 'vbox other-slots))

      (gtk-widget-show-all vbox)
      vbox))

  (define-layout-type 'keymaps layout-keymaps))
