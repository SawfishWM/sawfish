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
	  nokogiri-gnome
	  nokogiri-wm
	  nokogiri-widget
	  nokogiri-shell)

;;; widget for representing keymaps

  (define (command-name command)
    (or (car command) command))

  (define (make-keymap-item changed-callback)

    (define (print x)
      (let ((command (car x))
	    (key (cdr x)))
	(list (cdr x)
	      (if (consp command)
		  (concat (beautify-symbol-name (command-name command))
			  ": "
			  (mapconcat (lambda (x) (format nil "%s" x))
				     (cdr command) ", "))
		(beautify-symbol-name command)))))

    (define (dialog title callback &optional value)
      (let ((widget (make-widget `(keymap:binding ,(wm-command-list)))))
	(when value
	  (widget-set widget value))
	(simple-dialog (_ "Edit binding") (widget-gtk-widget widget)
		       (lambda () (callback (widget-ref widget)))
		       main-window)))

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
	  (params-hbox (gtk-hbox-new nil box-spacing))
	  (selection 0)
	  (params-spec nil)
	  (params-widget nil))

      (define (update-doc)
	(let ((doc (remove-newlines
		    (or (wm-documentation
			 (command-name (nth selection commands)))
			(_ "Undocumented")))))
	  (gtk-text-set-point text 0)
	  (gtk-text-forward-delete text (gtk-text-get-length text))
	  (gtk-text-insert text nil nil nil doc (length doc))
	  (gtk-text-set-point text 0)))

      (define (update-params)
	(let ((new-spec (cadr (nth selection commands))))
	  (unless (equal new-spec params-spec)
	    (when params-widget
	      (gtk-container-remove params-hbox (widget-gtk-widget
						 params-widget))
	      (setq params-widget nil))
	    (setq params-spec new-spec)
	    (if (null params-spec)
		(gtk-widget-hide params-hbox)
	      (setq params-widget (make-widget params-spec changed-callback))
	      (gtk-container-add params-hbox (widget-gtk-widget params-widget))
	      (gtk-widget-show params-hbox)))))

      (mapc (lambda (c)
	      (gtk-clist-append
	       clist (list (beautify-symbol-name (command-name c)))))
	    commands)

      (gtk-signal-connect entry "changed"
			  (make-signal-callback changed-callback))
      (gtk-signal-connect clist "select_row"
			  (lambda (w row col)
			    (setq selection row)
			    (update-params)
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
      (gtk-editable-set-editable text nil)
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
      (gtk-box-pack-end vbox params-hbox)
      (gtk-widget-show-all vbox)
      (unless params-widget
	(gtk-widget-hide params-hbox))
      (gtk-widget-set-usize vbox 350 400)
      (update-doc)

      (lambda (op)
	(case op
	  ((gtk-widget) vbox)
	  ((clear) (lambda ()
		     (when params-widget
		       (widget-clear params-widget))
		     (gtk-entry-set-text entry "")
		     (gtk-clist-select-row 0 0)
		     (gtk-clist-moveto clist 0 0)))
	  ((set) (lambda (x)
		   (let ((index (or (command-index
				     commands (command-name (car x))) 0)))
		     (setq selection index)
		     (gtk-entry-set-text entry (cdr x))
		     (gtk-clist-select-row clist index 0)
		     (gtk-clist-moveto clist index 0)
		     (when (cdar x)
		       (update-params)
		       (widget-set params-widget (cdar x))))))
	  ((ref) (lambda ()
		   (cons (if params-widget
			     (cons (car (nth selection commands))
				   (widget-ref params-widget))
			   (nth selection commands))
			 (gtk-entry-get-text entry))))
	  ((validp) (lambda (x)
		      (and (memq (command-name (car x)) commands)
			   ;; XXX check params
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

  (define (command-index lst x)
    (let loop ((i 0) (rest lst))
      (cond ((null rest) nil)
	    ((eq (or (caar rest) (car rest)) x) i)
	    (t (loop (1+ i) (cdr rest)))))))
