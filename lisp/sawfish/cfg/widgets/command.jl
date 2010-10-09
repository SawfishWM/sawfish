;; nokogiri-widgets/command.jl
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

(define-structure sawfish.cfg.widgets.command

    (export )

    (open rep
          gui.gtk-2.gtk
          rep.regexp
          sawfish.gtk.widget
          sawfish.cfg.wm
	  sawfish.cfg.utils)

  (define all-commands)

  (define (command-name command) (or (car command) command))

  (define (command-type command)
    (and (listp command) (cadr (memq #:type command))))

  (define (get-command name)
    (or (memq name all-commands) (assq name all-commands)))

  (define (filter-command-list) all-commands)

  (define (command-item x) (list (beautify-symbol-name (command-name x))))

  (define (make-command-item changed)

    (unless all-commands
      (setq all-commands (wm-command-list)))

    (let ((commands (filter-command-list))
	  (clist (gtk-clist-new-with-titles (list (_ "Command"))))
	  (text-view (gtk-text-view-new))
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
			(_ "Undocumented"))))
	      (buffer (gtk-text-view-get-buffer text-view))
	      (iter (gtk-text-iter-new)))
	  (gtk-text-buffer-set-text buffer doc (length doc))
	  (gtk-text-buffer-get-start-iter buffer iter)
	  (gtk-text-buffer-place-cursor buffer iter)))

      (define (update-params)
	(let ((new-spec (command-type (nth selection commands))))
	  (unless (equal new-spec params-spec)
	    (when params-widget
	      (gtk-container-remove params-hbox (widget-gtk-widget
						 params-widget))
	      (setq params-widget nil))
	    (setq params-spec new-spec)
	    (if (null params-spec)
		(gtk-widget-hide params-hbox)
	      (setq params-widget (make-widget params-spec changed))
	      (gtk-container-add params-hbox (widget-gtk-widget params-widget))
	      (gtk-widget-show params-hbox)))))

      (mapc (lambda (c)
	      (gtk-clist-append clist (command-item c))) commands)

      (g-signal-connect clist "select_row"
                        (lambda (w row col)
                          (declare (unused w col))
                          (setq selection row)
                          (update-params)
                          (update-doc)
                          (call-callback changed)))

      ;; seems you have to `moveto' _after_ the widget is realized..
      (g-signal-connect clist "map"
                        (lambda ()
                          (gtk-clist-moveto clist selection 0)))

      (gtk-text-view-set-wrap-mode text-view 'word)
      (gtk-text-view-set-editable text-view nil)
      (gtk-widget-set-size-request text-view -1 50)
      (gtk-clist-set-selection-mode clist 'browse)
      (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
      (gtk-scrolled-window-set-policy scroller-2 'automatic 'automatic)
      (gtk-container-add scroller clist)
      (gtk-container-add scroller-2 text-view)
      (gtk-box-pack-end vbox scroller-2)
      (gtk-container-add vbox scroller)
      (gtk-box-pack-end vbox params-hbox)
      (gtk-widget-show-all vbox)
      (unless params-widget
	(gtk-widget-hide params-hbox))
      (gtk-widget-set-size-request vbox 350 350)
      (update-doc)

      (lambda (op)
	(case op
	  ((gtk-widget) vbox)
	  ((clear) (lambda ()
		     (when params-widget
		       (widget-clear params-widget))
		     (gtk-clist-select-row 0 0)
		     (gtk-clist-moveto clist 0 0)))
	  ((set) (lambda (x)
		   (let ((index (command-index commands (command-name x))))
		     (unless index
		       ;; scan in all-commands
		       (setq index (command-index
				    all-commands (command-name x)))
		       (if index
			   ;; yes, add it to the list
			   (let ((command (nth index all-commands)))
			     (setq commands (nconc commands (list command)))
			     (gtk-clist-append clist (command-item command))
			     (setq index (1- (length commands))))
			 (setq index 0)))
		     (setq selection index)
		     (gtk-clist-select-row clist index 0)
		     (gtk-clist-moveto clist index 0)
		     (when (cdr x)
		       (update-params)
		       (widget-set params-widget (cdr x))))))
	  ((ref) (lambda ()
		   (if params-widget
		       (cons (command-name (nth selection commands))
			     (widget-ref params-widget))
		     (command-name (nth selection commands)))))
	  ((validp) (lambda (x)
		      ;; XXX check params
		      (memq (command-name (car x)) commands)))))))

  (define-widget-type 'command make-command-item)

;;; utils

  (define (command-index lst x)
    (let loop ((i 0) (rest lst))
      (cond ((null rest) nil)
	    ((eq (or (caar rest) (car rest)) x) i)
	    (t (loop (1+ i) (cdr rest)))))))
