#| nokogiri-widgets/keymap.jl

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

(define-structure nokogiri-widgets/keymap ()

    (open rep
	  gtk
	  nokogiri-dialog
	  nokogiri-wm
	  nokogiri-widget)

;;; widget for representing keymaps

  (define (make-keymap-item changed-callback)

    (define (print x) (list (cdr x) (beautify-symbol-name (car x))))

    (define (dialog title callback &optional value)
      (let ((widget (make-widget `(keymap:binding ,(wm-command-list)))))
	(when value
	  (widget-set widget value))
	(ok-cancel-dialog (widget-gtk-widget widget) (_ "Edit binding")
			  (lambda () (callback (widget-ref widget))))))

    (define (validp x) (and (consp x) (symbolp (car x)) (stringp (cdr x))))

    (define (type op)
      (case op
	((print) print)
	((dialog) dialog)
	((validp) validp)))

    (let ((base (make-widget `(list ,type (,(_ "Key") ,(_ "Command")))
			     changed-callback)))
      ;; mold this to accept (keymap . LIST)
      (lambda (op)
	(case op
	  ((ref) (lambda ()
		   (cons 'keymap (widget-ref base))))
	  ((set) (lambda (x)
		   (widget-set base (cdr x))))
	  ((validp) (lambda (x)
		      (and (eq (car x) 'keymap)
			   (widget-valid-p base (cdr x)))))
	  (t (base op))))))

  (define-widget-type 'keymap make-keymap-item)

;;; widget for editing individual bindings

  (define (make-keymap:binding-item changed-callback commands)
    
    (let ((clist (gtk-clist-new-with-titles (list (_ "Command"))))
	  (entry (gtk-entry-new))
	  (grab (gtk-button-new-with-label (_ "Grab...")))
	  (text (gtk-text-new))
	  (hbox (gtk-hbox-new nil box-spacing))
	  (vbox (gtk-vbox-new nil box-spacing))
	  (scroller (gtk-scrolled-window-new))
	  (scroller-2 (gtk-scrolled-window-new))
	  (selection 0))

      (define (update-doc)
	(let ((doc (remove-newlines
		    (or (wm-documentation (nth selection commands))
			(_ "Undocumented")))))
	  (gtk-text-set-point text 0)
	  (gtk-text-forward-delete text (gtk-text-get-length text))
	  (gtk-text-insert text nil nil nil doc (length doc))
	  (gtk-text-set-point text 0)))

      (mapc (lambda (c)
	      (gtk-clist-append clist (list (beautify-symbol-name c))))
	    commands)

      (gtk-signal-connect entry "changed"
			  (make-signal-callback changed-callback))
      (gtk-signal-connect clist "select_row"
			  (lambda (w row col)
			    (setq selection row)
			    (update-doc)
			    (call-callback changed-callback)))
      (gtk-signal-connect grab "clicked"
			  (lambda ()
			    (gtk-entry-set-text entry (wm-grab-key))))

      ;; seems you have to `moveto' _after_ the widget is realized..
      (gtk-signal-connect clist "map"
			  (lambda ()
			    (gtk-clist-moveto clist selection 0)))

      (gtk-text-set-word-wrap text 1)
      (gtk-widget-set-usize text -2 50)
      (gtk-clist-set-selection-mode clist 'browse)
      (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
      (gtk-scrolled-window-set-policy scroller-2 'automatic 'automatic)
      (gtk-container-add scroller clist)
      (gtk-box-pack-start hbox (gtk-label-new (_ "Key:")))
      (gtk-container-add hbox entry)
      (gtk-box-pack-end hbox grab)
      (gtk-box-pack-start vbox hbox)
      (gtk-container-add scroller-2 text)
      (gtk-box-pack-end vbox scroller-2)
      (gtk-container-add vbox scroller)
      (gtk-widget-set-usize scroller 350 350)
      (gtk-widget-show-all vbox)
      (update-doc)

      (lambda (op)
	(case op
	  ((gtk-widget) vbox)
	  ((clear) (lambda ()
		     (gtk-entry-set-text entry "")
		     (gtk-clist-select-row 0 0)
		     (gtk-clist-moveto clist 0 0)))
	  ((set) (lambda (x)
		   (let ((index (or (list-index commands (car x)) 0)))
		     (gtk-entry-set-text entry (cdr x))
		     (gtk-clist-select-row clist index 0)
		     (gtk-clist-moveto clist index 0))))
	  ((ref) (lambda ()
		   (cons (nth selection commands)
			 (gtk-entry-get-text entry))))
	  ((validp) (lambda (x)
		      (and (memq (car x) commands)
			   (stringp (cdr x)))))))))

  (define-widget-type 'keymap:binding make-keymap:binding-item)

;;; utils

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

  (define (remove-newlines string)
    (let loop ((point 0)
	       (out '()))
      (if (string-match "\n" string point)
	  (loop (match-end)
		(list* #\space (substring string point (match-start)) out))
	(apply concat (nreverse (cons (substring string point) out))))))

  (define (list-index lst x)
    (do ((i 0 (1+ i))
	 (rest lst (cdr rest)))
	((eq (car rest) x) i))))
