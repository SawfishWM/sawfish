;; nokogiri-widgets/match-window.jl -- match-window widget
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

(define-structure sawfish.cfg.widgets.match-window

    (export )

    (open rep
          gui.gtk-2.gtk
          rep.regexp
          sawfish.gtk.widget
          sawfish.gtk.widgets.simple-dialog
          sawfish.cfg.wm
	  sawfish.cfg.utils)

  (defconst matcher-count 3)

;;; the widget representing the `matchers' frame

  ;; (match-window:matchers x-properties)

  (define (make-match-window:matchers changed-callback x-properties)
    (declare (unused changed-callback))

    (let ((frame (gtk-frame-new (_ "Matchers")))
	  (table (gtk-table-new matcher-count 3 nil))
	  (l10n-x-properties (mapcar (lambda (x)
				       (cons (car x) (_ (cdr x))))
				     x-properties))
	  (widgets '()))

      (do ((i 0 (1+ i)))
	  ((= i matcher-count))
	(let ((combo (gtk-combo-new))
	      (entry (gtk-entry-new))
	      (button (gtk-button-new-with-label (_ "Grab..."))))
	  (gtk-combo-set-popdown-strings
	   combo (cons "" (mapcar cdr l10n-x-properties)))
	  (gtk-table-attach-defaults table button 0 1 i (1+ i))
	  (gtk-table-attach-defaults table combo 1 2 i (1+ i))
	  (gtk-table-attach-defaults table entry 2 3 i (1+ i))
	  (g-signal-connect
           button "clicked"
           (lambda ()
             (let* ((string (gtk-entry-get-text (gtk-combo-entry combo)))
                    (x-prop (and string (car (rassoc string
                                                     l10n-x-properties)))))
               (when string
                 (let ((prop (wm-grab-x-property (or x-prop (intern string)))))
                   (gtk-entry-set-text entry (if (stringp prop)
                                                 prop
                                               "")))))))
	  (setq widgets (nconc widgets (list (cons combo entry))))))
      (gtk-container-add frame table)
      (gtk-container-set-border-width table box-border)
      (gtk-table-set-row-spacings table box-spacing)
      (gtk-table-set-col-spacings table box-spacing)
      (gtk-widget-show-all frame)

      (lambda (op)
	(case op
	  ((gtk-widget) frame)
	  ((clear)
	   (lambda ()
	     (mapc (lambda (cell)
		     (gtk-entry-set-text (gtk-combo-entry (car cell)) "")
		     (gtk-entry-set-text (cdr cell) "")) widgets)))
	  ((set)
	   (lambda (x)
	     (do ((cells widgets (cdr cells))
		  (rest x (cdr rest)))
		 ((or (null cells) (null rest)))
	       (gtk-entry-set-text
		(gtk-combo-entry (caar cells))
		(or (cdr (assq (caar rest) l10n-x-properties)) (caar rest)))
	       (gtk-entry-set-text (cdar cells) (cdar rest)))))
	  ((ref)
	   (lambda ()
	     (let loop ((cells widgets)
			(out '()))
	       (if (null cells)
		   (nreverse out)
		 (let ((name (gtk-entry-get-text
			      (gtk-combo-entry (caar cells))))
		       (value (gtk-entry-get-text (cdar cells))))
		   (if (or (string= name "") (string= value ""))
		       (loop (cdr cells) out)
		     (let ((prop (rassoc name l10n-x-properties)))
		       (if prop
			   (setq name (car prop))
			 (setq name (intern name))))
		     (loop (cdr cells)
			   (cons (cons name value) out))))))))
	  ((validp) listp)))))

  (define-widget-type 'match-window:matchers make-match-window:matchers)

;;; the widget representing the `Actions' frame

  (define (make-match-window:actions changed-callback properties)
    (declare (unused changed-callback))

    (let ((frame (gtk-frame-new (_ "Actions")))
	  (book (gtk-notebook-new))
	  (widgets '()))

      (mapc
       (lambda (sub)
	 (let ((title (car sub))
	       (table (gtk-table-new (length (cdr sub)) 2 nil))
	       (vbox (gtk-vbox-new nil box-spacing)))
	   (do ((i 0 (1+ i))
		(props (cdr sub) (cdr props)))
	       ((null props))
	     (let* ((prop (car props))
		    (widget (make-widget
			     (if (eq (cadr prop) 'boolean)
				 `(optional scheme-boolean)
			       `(optional ,(cadr prop))))))
	       (gtk-table-attach-defaults
		table (make-left-label (beautify-symbol-name (car prop)))
		0 1 i (1+ i))
	       (gtk-table-attach-defaults
		table (widget-gtk-widget widget) 1 2 i (1+ i))
	       (setq widgets (cons (cons (car prop) widget) widgets))))
	   (gtk-table-set-row-spacings table box-spacing)
	   (gtk-table-set-col-spacings table box-spacing)
	   (gtk-box-pack-start vbox table)
	   (gtk-notebook-append-page book vbox (gtk-label-new title))))
       properties)

      (setq widgets (nreverse widgets))
      (gtk-container-set-border-width book box-border)
      (gtk-container-add frame book)
      (gtk-widget-show-all frame)

      (lambda (op)
	(case op
	  ((gtk-widget) frame)
	  ((clear) (lambda ()
		     (mapc (lambda (x) (widget-clear (cdr x))) widgets)))
	  ((set)
	   (lambda (x)
	     (mapc (lambda (cell)
		     (let ((widget (cdr (assq (car cell) widgets))))
		       (when widget
			 (widget-set widget (cdr cell))))) x)))
	  ((ref)
	   (lambda ()
	     (let loop ((rest widgets)
			(out '()))
	       (if (null rest)
		   (nreverse out)
		 (let ((value (widget-ref (cdar rest))))
		   (if value
		       (loop (cdr rest) (cons (cons (caar rest) value) out))
		     (loop (cdr rest) out)))))))))))

  (define-widget-type 'match-window:actions make-match-window:actions)

  (define (make-left-label string)
    (let ((hbox (gtk-hbox-new nil 0)))
      (gtk-box-pack-start hbox (gtk-label-new string))
      hbox))

;;; the main widget

  ;; (match-window ...)

  (define (make-match-window-item changed-callback properties x-properties)

    (define (print-matcher match)
      (if (stringp (cdr match)) (cdr match) "?"))

    (define (print-action action)
      (cond ((memq (cdr action) '(t #t))
	     (format nil "%s" (car action)))
	    ((eq (cdr action) '#f)
	     (format nil "%s %s" (_ "not") (car action)))
	    (t (format nil "%s=%s" (car action) (cdr action)))))

    (define (print x)
      (list (mapconcat print-matcher (car x) ", ")
	    (mapconcat print-action (cdr x) ", ")))

    (define (dialog title callback #!key for value)
      (declare (unused title))
      (let ((vbox (gtk-vbox-new nil box-spacing))
	    (matcher-widget (make-widget
			     `(match-window:matchers ,x-properties)))
	    (action-widget (make-widget
			    `(match-window:actions ,properties))))
	(gtk-container-add vbox (widget-gtk-widget matcher-widget))
	(gtk-container-add vbox (widget-gtk-widget action-widget))
	(when value
	  (widget-set matcher-widget (car value))
	  (widget-set action-widget (cdr value)))
	(gtk-widget-show vbox)

	(simple-dialog (_ "Match Window Properties") vbox
		       (lambda ()
			 (callback (cons (widget-ref matcher-widget)
					 (widget-ref action-widget))))
		       for)))

    (define (validp x) (and (consp x) (listp (car x)) (listp (cdr x))))

    (define (type op)
      (case op
	((print) print)
	((dialog) dialog)
	((validp) validp)))

    (make-widget `(list ,type (,(_ "Matchers") ,(_ "Actions")))
		 changed-callback))

  (define-widget-type 'match-window make-match-window-item))
