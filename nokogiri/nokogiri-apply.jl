#| nokogiri-apply.jl -- setting values in the wm

   $Id: nokogiri-apply.jl,v 1.2 2000/07/01 14:07:42 john Exp $

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

(define-structure nokogiri-apply nokogiri-apply-interface

    (open rep
	  tables
	  nokogiri-slot
	  nokogiri-widget
	  nokogiri-wm)

  ;; list of slots that have changed since last apply
  (define changed-slots '())

  ;; list of slots changed since they were created
  (define changed-slots-ever '())

  (define change-handler-table (make-table eq-hash eq))

  ;; arrange for HANDLER thunk to be called each time SLOT changes
  (define (define-change-handler slot handler)
    (table-set change-handler-table slot
	       (cons handler (table-ref change-handler-table slot))))

  (define (slot-change-handlers slot) (table-ref change-handler-table slot))

  (defvar *nokogiri-apply-immediately* nil)

;;; applying changes

  (define (after-changed slot)
    (if (not *nokogiri-apply-immediately*)
	(unless (memq slot changed-slots)
	  (setq changed-slots (cons slot changed-slots)))
      (apply-slot-1 slot))
    (unless (memq slot changed-slots-ever)
      (setq changed-slots-ever (cons slot changed-slots-ever))))

  (add-hook '*nokogiri-slot-changed-hook* after-changed)

  (define (apply-changes-for slots getter)
    (when slots
      (wm-apply-changes (mapcar (lambda (slot)
				  (cons (slot-name slot) (getter slot)))
				slots))
      (mapc (lambda (slot)
	      (mapc (lambda (h) (h)) (slot-change-handlers slot))) slots)))

  (define (apply-slot-changes)
    (apply-changes-for changed-slots slot-value)
    (setq changed-slots '()))

  (define (apply-slot-1 slot)
    (apply-changes-for (list slot) slot-value))

  (define (revert-slot-changes)
    (mapc (lambda (slot)
	    (widget-set (slot-widget slot) (slot-old-value slot))
	    (update-dependences slot))
	  changed-slots-ever)
    (apply-changes-for changed-slots-ever slot-old-value)
    (setq changed-slots-ever '()))

  (define (changes-to-apply-p) changed-slots)

  (define (changes-to-revert-p)
    (> (length changed-slots-ever) (length changed-slots))))
