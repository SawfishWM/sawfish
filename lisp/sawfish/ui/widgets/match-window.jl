#| nokogiri-widgets/match-window.jl -- match-window widget

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

(define-structure nokogiri-widgets/match-window ()

    (open rep
	  gtk
	  nokogiri-widget
	  nokogiri-wm
	  nokogiri-gnome
	  nokogiri-shell)

  (defconst matcher-count 3)


;;; the widget representing the `matchers' frame

  ;; (match-window:matchers x-properties)

  (define (make-match-window:matchers changed-callback x-properties)
    (let ((frame (gtk-frame-new (_ "Matchers")))
	  (table (gtk-table-new matcher-count 3 nil))
	  (widgets '()))

      (do ((i 0 (1+ i)))
	  ((= i matcher-count))
	(let ((combo (gtk-combo-new))
	      (entry (gtk-entry-new))
	      (button (gtk-button-new-with-label (_ "Grab..."))))
	  (gtk-combo-set-popdown-strings
	   combo (cons "" (mapcar cdr x-properties)))
	  (gtk-table-attach-defaults table combo 0 1 i (1+ i))
	  (gtk-table-attach-defaults table entry 1 2 i (1+ i))
	  (gtk-table-attach-defaults table button 2 3 i (1+ i))
	  (gtk-signal-connect button "clicked"
	   (lambda ()
	     (let ((string (gtk-entry-get-text (gtk-combo-entry combo))))
	       (when string
		 (let ((prop (wm-grab-x-property string)))
		   (gtk-entry-set-text entry (if (stringp prop)
						 prop
					       "")))))))
	  (setq widgets (nconc widgets (list (cons combo entry))))))
      (gtk-container-add frame table)
      (gtk-container-border-width table box-border)
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
		(or (or (cdr (assq (caar rest) x-properties)) (caar rest))))
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
		     (let ((prop (rassoc name x-properties)))
		       (if prop
			   (setq name (car prop))
			 (setq name (intern name))))
		     (loop (cdr cells)
			   (cons (cons name value) out))))))))
	  ((validp) listp)))))

  (define-widget-type 'match-window:matchers make-match-window:matchers)


;;; the widget representing the `Actions' frame

  (define (make-match-window:actions changed-callback properties)
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
		    (is-boolean (eq (cadr prop) 'boolean))
		    (widget (make-widget
			     (if is-boolean
				 `(boolean ,(beautify-symbol-name (car prop)))
			       `(optional ,(cadr prop))))))
	       (unless is-boolean
		 (gtk-table-attach-defaults
		  table (make-left-label (beautify-symbol-name (car prop)))
		  0 1 i (1+ i)))
	       (gtk-table-attach-defaults
		table (widget-gtk-widget widget) 1 2 i (1+ i))
	       (setq widgets (cons (cons (car prop) widget) widgets))))
	   (gtk-table-set-row-spacings table box-spacing)
	   (gtk-table-set-col-spacings table box-spacing)
	   (gtk-box-pack-start vbox table)
	   (gtk-notebook-append-page book vbox (gtk-label-new title))))
       properties)

      (setq widgets (nreverse widgets))
      (gtk-container-border-width book box-border)
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
      (gtk-box-pack-end hbox (gtk-label-new string))
      hbox))
  
  (define (beautify-symbol-name symbol)
    (cond ((stringp symbol) symbol)
	  ((not (symbolp symbol)) (format "%s" symbol))
	  (t
	   (let ((name (copy-sequence (symbol-name symbol))))
	     (while (string-match "[-:]" name)
	       (setq name (concat (substring name 0 (match-start))
				  ?  (substring name (match-end)))))
	     (aset name 0 (char-upcase (aref name 0)))
	     name))))


;;; the main widget
		     
  ;; (match-window ...)

  (define (make-match-window-item changed-callback properties x-properties)

    (define (print-matcher match)
      (if (stringp (cdr match)) (cdr match) "?"))

    (define (print-action action)
      (if (eq (cdr action) t)
	  (format nil "%s" (car action))
	(format nil "%s=%s" (car action) (cdr action))))

    (define (print x)
      (list (mapconcat print-matcher (car x) ", ")
	    (mapconcat print-action (cdr x) ", ")))

    (define (dialog title callback &optional value)
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

	(simple-dialog (_ "Match window properties") vbox
		       (lambda ()
			 (callback (cons (widget-ref matcher-widget)
					 (widget-ref action-widget))))
		       main-window)))

    (define (validp x) (and (consp x) (listp (car x)) (listp (cdr x))))

    (define (type op)
      (case op
	((print) print)
	((dialog) dialog)
	((validp) validp)))

    (make-widget `(list ,type (,(_ "Matchers") ,(_ "Actions")))
		 changed-callback))

  (define-widget-type 'match-window make-match-window-item))
