;; nokogiri-widgets/event.jl
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

(define-structure sawfish.gtk.widgets.event

    (export )

    (open rep
          rep.regexp
          gui.gtk-2.gtk
          sawfish.gtk.widget
          sawfish.cfg.wm)

  (define (make-event-item changed)

    (let ((entry (gtk-entry-new))
	  (grab (gtk-button-new-with-label (_ "Grab...")))
	  (hbox (gtk-hbox-new nil box-spacing)))

      (gtk-container-add hbox entry)
      (gtk-box-pack-end hbox grab)
      (gtk-widget-show-all hbox)

      (g-signal-connect entry "changed" (make-signal-callback changed))
      (g-signal-connect grab "clicked"
                        (lambda ()
                          (gtk-entry-set-text entry (wm-grab-key))))

      (lambda (op)
	(case op
	  ((gtk-widget) hbox)
	  ((clear) (lambda ()
		     (gtk-entry-set-text entry "")))
	  ((set) (lambda (x)
		   (gtk-entry-set-text entry x)))
	  ((ref) (lambda ()
		   (strip-surrounding-whitespace (gtk-entry-get-text entry))))
	  ((validp) stringp)))))

  (define-widget-type 'event make-event-item)

  (define (strip-surrounding-whitespace string)
    (if (string-match "^\\s*(.*?)\\s*$" string)
	(expand-last-match "\\1")
      string)))
