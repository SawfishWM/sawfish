;; nokogiri-slot.jl -- managing individual config items
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

(define-structure sawfish.cfg.slot

    (export slot-name
	    slot-widget
	    slot-old-value
	    slot-gtk-widget
	    slot-doc slot-flags
	    slot-layout set-slot-layout
	    update-dependences
	    update-all-dependences
	    get-slot
	    slot-value
	    fetch-slots
	    custom-symbol-value)

    (open rep
	  gui.gtk-2.gtk
	  rep.system
	  rep.data.tables
	  rep.data.records
	  sawfish.gtk.widget
	  sawfish.cfg.wm)

  (define-record-type :slot
    (create-slot name old-value flags)
    ;; [no predicate]
    (name slot-name)			 ;name of config item
    (widget slot-widget slot-widget-set) ;associated lisp widget
    (layout slot-layout set-slot-layout) ;gtk widget if the slot is displayed
    (old-value slot-old-value)		 ;original value of slot's config
    (doc slot-doc slot-doc-set)		 ;doc string (unless shown in widget)
    (flags slot-flags))

  (define-record-discloser :slot
    (lambda (x) (format nil "#<slot %s>" (slot-name x))))

  ;; hash table mapping names to slots
  (define slot-table (make-table symbol-hash eq))

  ;; hash table mapping slot name to list of slots depending on them
  (define dependence-table (make-table symbol-hash eq))

  (defvar *nokogiri-slot-changed-hook* '())

;;; slot creation, data structure mgmt

  (define (slot-dependences slot)
    (table-ref dependence-table (slot-name slot)))

  (define (update-dependences slot)
    (when (slot-dependences slot)
      (let ((value (slot-value slot)))
	(mapc (lambda (dep)
		(when (slot-layout dep)
		  (gtk-widget-set-sensitive (slot-layout dep) value)))
	      (slot-dependences slot)))))

  (define (update-all-dependences)
    (table-walk (lambda (dep slots)
		  (declare (unused slots))
		  (update-dependences (get-slot dep))) dependence-table))

  ;; called when the value of SLOT changes
  (define (slot-changed slot)
    (call-hook '*nokogiri-slot-changed-hook* (list slot))
    (update-dependences slot))

  (define (slot-gtk-widget slot) (widget-gtk-widget (slot-widget slot)))

  (define (make-slot #!key name value type doc depends widget-flags)
    (let ((slot (create-slot name value widget-flags)))
      (table-set slot-table (slot-name slot) slot)

      ;; install dependendences
      (when depends
	(table-set dependence-table depends
		   (cons slot (table-ref dependence-table depends))))

      ;; create the widget
      (let* ((callback (lambda () (slot-changed slot)))
	     (doc (if (or (null doc) (string= doc ""))
		      doc
		    (_ doc))))
	(if (widget-accepts-doc-string-p (or (car type) type))
	    (slot-widget-set slot (make-widget type callback doc))
	  (slot-doc-set slot doc)
	  (slot-widget-set slot (make-widget type callback))))
      (widget-set (slot-widget slot) (slot-old-value slot))

      slot))

  (define (get-slot name) (table-ref slot-table name))
  (define (slot-value slot) (widget-ref (slot-widget slot)))

  ;; Return a list of slots for variables NAMES. Will load the data
  ;; from the WM if not already cached
  (define (fetch-slots names)
    ;; don't want to request one slot at a time from wm..

    ;; SLOTS is a list of slots matching NAMES, or nil if the slot
    ;; hadn't been loaded. EXTRA is a list of slots to splice into
    ;; the nil spaces.
    (define (merge slots extra)
      (let loop ((rest slots))
	(if (null rest)
	    slots
	  (when (null (car rest))
	    (rplaca rest (apply make-slot (car extra)))
	    (setq extra (cdr extra)))
	  (loop (cdr rest)))))

    ;; find which slots still need to be loaded
    (let ((slots (mapcar get-slot names)))
      (let loop
	  ((names-rest names)
	   (slots-rest slots)
	   (to-fetch '()))
	(if (null names-rest)
	    (if to-fetch
		;; load and merge the required slots
		(merge slots (wm-load-slots (nreverse to-fetch)))
	      slots)
	  (if (null (car slots-rest))
	      ;; slot not loaded yet
	      (loop (cdr names-rest)
		    (cdr slots-rest)
		    (cons (car names-rest) to-fetch))
	    (loop (cdr names-rest)
		  (cdr slots-rest)
		  to-fetch))))))

;;; misc

  (define (custom-symbol-value symbol)
    (let ((slot (or (get-slot symbol)
		    (error "Slot not loaded yet: %s" symbol))))
      (slot-value slot))))
