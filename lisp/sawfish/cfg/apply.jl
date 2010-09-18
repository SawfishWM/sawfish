;; nokogiri-apply.jl -- setting values in the wm
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

(define-structure sawfish.cfg.apply

    (export define-change-handler
	    apply-slot-changes
	    revert-slot-changes
	    changes-to-apply-p
	    changes-to-revert-p)

    (open rep
	  rep.system
	  rep.data.tables
	  gui.gtk-2.gtk
	  sawfish.gtk.widget
	  sawfish.gtk.widgets.simple-dialog
	  sawfish.cfg.slot
	  sawfish.cfg.group
	  sawfish.cfg.wm)

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

  (defvar *nokogiri-apply-immediately* t)

;;; applying changes

  (define (after-changed slot)
    (if (not *nokogiri-apply-immediately*)
	(unless (memq slot changed-slots)
	  (setq changed-slots (cons slot changed-slots)))
      (apply-slot-1 slot))
    (unless (memq slot changed-slots-ever)
      (setq changed-slots-ever (cons slot changed-slots-ever))))

  (add-hook '*nokogiri-slot-changed-hook* after-changed)

  (define in-apply-changes (make-fluid))

  (define (apply-changes-for slots getter)
    (define (do-apply)
      (wm-apply-changes (mapcar (lambda (slot)
				  (cons (slot-name slot) (getter slot)))
				slots))
      (refresh-groups-for-slots slots))

    (define (format-error ex)
      (when (eq (car ex) 'remote-sawfish)
	(condition-case nil
	    ;; this may fail if the string contains #<..>
	    (setq ex (read-from-string (cadr ex)))
	  (error nil)))
      (format nil "%s\n\n%s: %s"
	      (format nil (_ "While changing %s:")
		      (mapconcat (lambda (x)
				   (prin1-to-string (slot-name x)))
				 slots ", "))
	      (or (get (car ex) 'error-message) (car ex))
	      (mapconcat (lambda (x) (format nil "%s" x)) (cdr ex) ", ")))

    (when slots
      (if (fluid in-apply-changes)
	  ;; avoid infinite regress..
	  (do-apply)
	(let-fluids ((in-apply-changes t))
           (condition-case data
               (do-apply)
             (remote-sawfish
              ;; An error occurred while setting the values,
              ;; back them all out, then display the error message
              (revert-slots slots)
              (let ((label (gtk-label-new (format-error data))))
                (gtk-label-set-justify label 'left)
                (gtk-label-set-line-wrap label t)
                (gtk-widget-show label)
                (simple-dialog (_ "Sawfish Error") label nil
                               (gtk-widget-get-toplevel
                                (slot-gtk-widget (car slots)))))))))
      (mapc (lambda (slot)
	      (mapc (lambda (h) (h)) (slot-change-handlers slot))) slots)))

  (define (apply-slot-changes)
    (apply-changes-for changed-slots slot-value)
    (setq changed-slots '()))

  (define (apply-slot-1 slot)
    (apply-changes-for (list slot) slot-value))

  (define (revert-slots slots)
    (mapc (lambda (slot)
	    (widget-set (slot-widget slot) (slot-old-value slot))
	    (update-dependences slot)) slots)
    (apply-changes-for slots slot-old-value))

  (define (revert-slot-changes)
    (revert-slots changed-slots-ever)
    (setq changed-slots-ever '()))

  (define (changes-to-apply-p) changed-slots)

  (define (changes-to-revert-p)
    (> (length changed-slots-ever) 0)))
