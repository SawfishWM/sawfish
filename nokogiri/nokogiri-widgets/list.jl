#| nokogiri-widgets/list.jl -- list widget

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

(define-structure nokogiri-widgets/list ()

    (open rep
	  gtk
	  nokogiri-widget
	  nokogiri-widget-dialog
	  nokogiri-shell)

  ;; (list SPEC-OR-FUNCTION [TITLE])

  ;; if a functional spec is passed, these operations will be used:

  ;; - ((SPEC 'print) VALUE) => LIST-OF-STRINGS
  ;; - ((SPEC 'dialog) TITLE CALLBACK [VALUE])
  ;; - ((SPEC 'validp) ARG) => BOOL

  ;; CALLBACK is a function that will be called with the new value
  ;; if it's accepted

  (defconst list-width 200)
  (defconst list-height -2)

  (define (make-list-item changed-callback spec &optional title)

    (let ((clist (if title
		     (gtk-clist-new-with-titles (if (stringp title)
						    (vector title)
						  title))
		   (gtk-clist-new 1)))
	  (scroller (gtk-scrolled-window-new))
	  (add (gtk-button-new-with-label (_ "Add...")))
	  (delete (gtk-button-new-with-label (_ "Delete")))
	  (edit (gtk-button-new-with-label (_ "Edit...")))
	  (vbox (gtk-vbox-new nil box-spacing))
	  (hbox (gtk-hbox-new nil box-spacing))
	  (value '())
	  (selection nil))

      (define (set-selection row)
	(setq selection row)
	(gtk-widget-set-sensitive delete selection)
	(gtk-widget-set-sensitive edit selection))

      (define (print-value x)
	(if (functionp spec)
	    ((spec 'print) x)
	  (list (format nil "%s" x))))

      (define (add-item)
	(let ((callback (lambda (new)
			  (with-clist-frozen clist
			    (if (not selection)
				(progn
				  (setq value (nconc value (list new)))
				  (gtk-clist-append clist (print-value new)))
			      (setq value (insert-after new value selection))
			      (gtk-clist-insert
			       clist (1+ selection) (print-value new))
			      (gtk-clist-select-row clist (1+ selection) 0)))
			  (call-callback changed-callback))))
	  (if (functionp spec)
	      ((spec 'dialog) (_ "Add:") callback)
	    (widget-dialog (_ "Add:") spec callback nil main-window))))

      (define (delete-item)
	(when selection
	  (let ((orig-sel selection))
	    (if (zerop selection)
		(setq value (cdr value))
	      (rplacd (nthcdr (1- selection) value)
		      (nthcdr (1+ selection) value)))
	    (with-clist-frozen clist
	      (gtk-clist-remove clist selection)
	      (if (> (gtk-clist-rows clist) orig-sel)
		  (gtk-clist-select-row clist orig-sel 0)
		(set-selection nil)))
	    (call-callback changed-callback))))

      (define (edit-item)
	(when selection
	  (let* ((orig-sel selection)
		 (cell (nthcdr orig-sel value))
		 (callback (lambda (new)
			     (rplaca cell new)
			     (with-clist-frozen clist
			       (gtk-clist-remove clist orig-sel)
			       (gtk-clist-insert
				clist orig-sel (print-value new))
			       (gtk-clist-select-row clist orig-sel 0))
			     (call-callback changed-callback))))
	    (if (functionp spec)
		((spec 'dialog) (_ "Edit:") callback (car cell))
	      (widget-dialog (_ "Edit:") spec callback
			     (car cell) main-window)))))

      (define (clear)
	(gtk-clist-clear clist)
	(setq value '()))

      (gtk-signal-connect add "clicked" add-item)
      (gtk-signal-connect delete "clicked" delete-item)
      (gtk-signal-connect edit "clicked" edit-item)
      (gtk-signal-connect clist "select_row"
			  (lambda (w row col)
			    (set-selection row)))
      (gtk-signal-connect clist "unselect_row"
			  (lambda (w row col)
			    (when (= row selection)
			      (set-selection nil))))
      (gtk-signal-connect clist "button_press_event"
			  (lambda (w ev)
			    (when (eq (gdk-event-type ev) '2button-press)
			      (edit-item))))

      (gtk-clist-set-shadow-type clist 'none)
      (gtk-clist-set-column-width clist 0 100)
      (gtk-clist-set-selection-mode clist 'browse)
      (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
      (gtk-scrolled-window-add-with-viewport scroller clist)
      (gtk-widget-set-usize scroller list-width list-height)
      (gtk-container-add vbox scroller)
      (gtk-box-pack-end vbox hbox)
      (gtk-box-pack-end hbox add)
      (gtk-box-pack-end hbox edit)
      (gtk-box-pack-end hbox delete)
      (gtk-widget-show-all vbox)
      (set-selection nil)

      (lambda (op)
	(case op
	  ((gtk-widget) vbox)
	  ((clear) (lambda ()
		     (clear)
		     (call-callback changed-callback)))
	  ((set) (lambda (x)
		   (clear)
		   (setq value x)
		   (mapc (lambda (cell)
			   (gtk-clist-append clist (print-value cell)))
			 value)
		   (call-callback changed-callback)))
	  ((ref) (lambda () value))
	  ((validp) (lambda (x)
		      (let ((validp (if (functionp spec)
					(spec 'validp)
				      ((make-widget spec) 'validp))))
			(catch 'out
			  (mapc (lambda (y)
				  (unless (validp y)
				    (throw 'out nil))) x))
			t)))))))

  (define-widget-type 'list make-list-item)

;;; utilities

  (define (insert-after x lst idx)
    (let ((cell (nthcdr idx lst)))
      (if cell
	  (rplacd cell (cons x (cdr cell)))
	(setq lst (nconc lst (list x))))
      lst))

  (defmacro with-clist-frozen (clist . body)
    `(progn
       (gtk-clist-freeze ,clist)
       (unwind-protect
	   (progn ,@body)
	 (gtk-clist-thaw ,clist)))))
