#!/bin/sh

if [ "$1" = "--backend" ]; then
  shift; exec rep "$0" "$@"
else
  sawmill-client -w -q -f customize || echo "sawmill server isn't running"
  exit
fi

!#

;; sawmill-ui -- subprocess to handle configuration user interface
;; $Id: sawmill-ui.jl,v 1.3 1999/08/13 16:56:32 john Exp $

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'gtk)

(defvar ui-active nil)

(defvar ui-window nil)
(defvar ui-root nil)
(defvar ui-apply nil)
(defvar ui-revert nil)
(defvar ui-ok nil)

;; list of (SPEC . VALUE)
(defvar ui-values-to-apply nil)

(defvar ui-apply-hook nil)
(defvar ui-apply-changed-hook nil)

;; list of (SYMBOL . ORIGINAL-VALUE)
(defvar ui-original-values nil)

(defvar ui-changed-variables nil)

(defvar ui-box-spacing 4)
(defvar ui-box-border 5)

;; XXX this doesn't always work 100%..
(defvar ui-enable-revert nil)

;; XXX this may be confusing?
(defvar ui-enable-refresh nil)

;; may be list or notebook
(defvar ui-pages-style 'list)


;; ui builder

;; elements may be:

;; (pages (LABEL ELEMENT)...)
;; (hbox ELEMENTS...)
;; (vbox ELEMENTS...)
;; (table (COLS ROWS) ((LEFT RIGHT TOP BOTTOM) ELEMENT)...)
;; (label TEXT)
;; (hsep)
;; (vsep)
;; (frame TEXT ELEMENTS...)

;; (toggle TEXT KEYS...)
;; (number KEYS...)
;; (string KEYS...)
;; (set LIST KEYS...)
;; (font KEYS...)

;; KEYS is a plist, possible keys are:

;; :feature FUNCTION
;; :variable SYMBOL
;; :value VALUE
;; :doc STRING
;; :allow-nil t

(defmacro get-key (spec key)
  `(car (cdr (memq ,key ,spec))))

(defmacro key-exists-p (spec key)
  `(memq ,key ,spec))

(defun set-key (spec key value)
  (let
      ((cell (memq key spec)))
    (if cell
	(rplaca (cdr cell) value)
      (nconc spec (list key value)))))

(defun build-ui (spec)
  (let
      ((fun (get (car spec) 'builder)))
    (if fun
	(funcall fun spec)
      (error "Unknown ui element: %S" spec))))

(put 'pages 'builder 'build-pages)
(defun build-pages (spec)
  (cond ((eq ui-pages-style 'notebook)
	 (let
	     ((book (gtk-notebook-new)))
	   (mapc #'(lambda (page)
		     (gtk-notebook-append-page book (build-ui (car (cdr page)))
					       (gtk-label-new (car page))))
		 (cdr spec))
	   book))
	((eq ui-pages-style 'list)
	 (let
	     ((hbox (gtk-hbox-new nil 0))
	      (clist (gtk-clist-new 1))
	      (frame (gtk-frame-new))
	      (contents (make-vector (length (cdr spec)))))
	   (gtk-clist-set-column-auto-resize clist 0 t)
	   (gtk-container-add hbox clist)
	   (gtk-container-add hbox frame)
	   (gtk-clist-set-selection-mode clist 'browse)
	   (mapc #'(lambda (page)
		     (let
			 ((row (gtk-clist-append clist (vector (car page))))
			  (widget (build-ui (nth 1 page))))
		       ;; (LABEL . WIDGET)
		       (gtk-widget-show-all widget)
		       (aset contents row (cons (car page) widget))))
		 (cdr spec))
	   (gtk-signal-connect clist "select_row"
			       `(lambda (clist row col)
				  (build-pages:select-row
				   ,contents row ,frame)))
	   (gtk-clist-select-row clist 0 0)
	   hbox))))

(defun build-pages:select-row (contents row frame)
  (mapc #'(lambda (w)
	    (gtk-container-remove frame w)) (gtk-container-children frame))
  (gtk-frame-set-label frame (car (aref contents row)))
  (gtk-container-add frame (cdr (aref contents row))))

(put 'vbox 'builder 'build-box)
(put 'hbox 'builder 'build-box)
(defun build-box (spec)
  (let
      ((box (if (eq (car spec) 'vbox)
		(gtk-vbox-new nil 0)
	      (gtk-hbox-new nil 0))))
    (gtk-box-set-spacing box ui-box-spacing)
    (gtk-container-border-width box ui-box-border)
    (mapc #'(lambda (widget)
	      (gtk-box-pack-start box (build-ui widget) nil nil)) (cdr spec))
    box))

(put 'table 'builder 'build-table)
(defun build-table (spec)
  (let
      ((table (gtk-table-new (car (nth 1 spec)) (nth 1 (nth 1 spec)) nil)))
    (mapc #'(lambda (cell)
	      (gtk-table-attach-defaults table
					 (build-ui (nth 1 cell))
					 (nth 0 (car cell))
					 (nth 1 (car cell))
					 (nth 2 (car cell))
					 (nth 3 (car cell)))) (nthcdr 2 spec))
    table))

(put 'label 'builder 'build-label)
(defun build-label (spec)
  (let
      ((label (gtk-label-new (nth 1 spec))))
    (gtk-label-set-justify label 'left)
    (gtk-label-set-line-wrap label t)
    label))

(put 'frame 'builder 'build-frame)
(defun build-frame (spec)
  (let
      ((frame (gtk-frame-new (nth 1 spec))))
    (when (nth 2 spec)
      (gtk-container-add frame (build-ui (nth 2 spec))))
    frame))

(put 'hsep 'builder 'build-separator)
(put 'vsep 'builder 'build-separator)
(defun build-separator (spec)
  (if (eq (car spec) 'hsep)
      (gtk-hseparator-new)
    (gtk-vseparator-new)))

(put 'toggle 'builder 'build-toggle)
(defun build-toggle (spec)
  (let
      ((toggle (gtk-check-button-new-with-label (nth 1 spec))))
    (mapc #'(lambda (w)
	      (when (gtk-label-p w)
;		(gtk-label-set-line-wrap w t)
		(gtk-label-set-justify w 'left)))
	  (gtk-container-children toggle))
    (if (key-exists-p spec ':value)
	(when (get-key spec ':value)
	  (gtk-button-clicked toggle))
      (set-key spec ':value nil))
    (gtk-signal-connect toggle "clicked"
			`(lambda (w)
			   (let
			       ((value (not (get-key ',spec ':value))))
			     (ui-set
			      ',spec ',(get-key spec ':variable) value))))
    toggle))

(put 'number 'builder 'build-entry)
(put 'string 'builder 'build-entry)
(defun build-entry (spec)
  (let
      ((entry (gtk-entry-new))
       id)
    (if (key-exists-p spec ':value)
	(gtk-entry-set-text entry (if (eq (car spec) 'number)
				      (format nil "%s" (get-key spec ':value))
				    (get-key spec ':value)))
      (set-key spec ':value nil))
    (setq id (gtk-signal-connect entry "changed"
				 `(lambda (w)
				    (build-entry:changed ',spec))))
    (setq spec (nconc spec (list ':widget entry)))
    entry))

(defun build-entry:changed (spec)
  (unless (get-key spec ':in-apply-hook)
    (add-hook 'ui-apply-changed-hook `(lambda ()
					(build-entry:set ',spec)))
    (ui-set-button-states)
    (set-key spec ':in-apply-hook nil)))

(defun build-entry:set (spec)
  (let
      ((value (gtk-entry-get-text (get-key spec ':widget))))
    (cond ((and (string= value "") (get-key spec ':allow-nil))
	   (setq value nil))
	  ((eq (car spec) 'number)
	   (condition-case nil
	       (progn
		 (setq value (read-from-string value))
		 (unless (numberp value)
		   (setq value nil)))
	     (error
	      (setq value nil)))))
    (set-key spec ':in-apply-hook nil)
    (ui-set spec (get-key spec ':variable) value)))

(put 'font 'builder 'build-font)
(defun build-font (spec)
  (let
      ((button (gtk-button-new-with-label (or (get-key spec ':value) ""))))
    (mapc #'(lambda (w)
	      (when (gtk-label-p w)
		(gtk-label-set-line-wrap w t)))
	  (gtk-container-children button))
    (unless (key-exists-p spec ':value)
      (set-key spec ':value nil))
    (gtk-signal-connect button "clicked"
			`(lambda (w)
			   (build-font:clicked ',spec)))
    (setq spec (nconc spec (list ':widget button)))
    button))

(defun build-font:clicked (spec)
  (let
      ((fontsel (gtk-font-selection-dialog-new "Select font")))
    (when (get-key spec ':value)
      (gtk-font-selection-dialog-set-font-name fontsel (get-key spec ':value)))
    (gtk-signal-connect
     (gtk-font-selection-dialog-ok-button fontsel)
     "clicked"
     `(lambda (w)
	(let
	    ((value (gtk-font-selection-dialog-get-font-name ',fontsel)))
	  (when (and (string= value "") (get-key spec ':allow-nil))
	    (setq value nil))
	  (ui-set ',spec (get-key ',spec ':variable) value)
	  (ui-set-button-label ',(get-key spec ':widget) value)
	  (gtk-widget-destroy ',fontsel))))
    (gtk-signal-connect
     (gtk-font-selection-dialog-cancel-button fontsel)
     "clicked" `(lambda (w)
		  (gtk-widget-destroy ',fontsel)))
    (gtk-signal-connect fontsel
     "delete_event" `(lambda (w)
		       (gtk-widget-destroy ',fontsel)))
    (gtk-widget-show fontsel)))

(put 'color 'builder 'build-color)
(defun build-color (spec)
  (let
      ((button (gtk-button-new-with-label (or (get-key spec ':value) ""))))
    (mapc #'(lambda (w)
	      (when (gtk-label-p w)
		(gtk-label-set-line-wrap w t)))
	  (gtk-container-children button))
    (unless (key-exists-p spec ':value)
      (set-key spec ':value nil))
    (gtk-signal-connect button "clicked"
			`(lambda (w)
			   (build-color:clicked ',spec)))
    (setq spec (nconc spec (list ':widget button)))
    button))

(defun build-color:clicked (spec)
  (let
      ((colorsel (gtk-color-selection-dialog-new "Select color")))
    (when (get-key spec ':value)
      (gtk-color-selection-set-color-interp
       (gtk-color-selection-dialog-colorsel colorsel)
       (gdk-color-parse-interp (get-key spec ':value))))
    (gtk-signal-connect
     (gtk-color-selection-dialog-ok-button colorsel)
     "clicked"
     `(lambda (w)
	(let*
	    ((color (gtk-color-selection-get-color-interp
		     ',(gtk-color-selection-dialog-colorsel colorsel)))
	     (name (and color (format nil "#%04x%04x%04x"
				      (gdk-color-red color)
				      (gdk-color-green color)
				      (gdk-color-blue color)))))
	  (when (or name (get-key ',spec ':allow-nil))
	    (ui-set ',spec (get-key ',spec ':variable) name)
	    (ui-set-button-label ',(get-key spec ':widget) name))
	  (gtk-widget-destroy ',colorsel))))
    (gtk-signal-connect
     (gtk-color-selection-dialog-cancel-button colorsel)
     "clicked" `(lambda (w)
		  (gtk-widget-destroy ',colorsel)))
    (gtk-signal-connect colorsel
     "delete_event" `(lambda (w)
		       (gtk-widget-destroy ',colorsel)))
    (gtk-widget-show colorsel)))

(put 'set 'builder 'build-set)
(defun build-set (spec)
  (let*
      ((values (nth 1 spec))
       (box (gtk-vbox-new nil 0))
       (buttons (make-vector (length values)))
       (i 0)
       group)
    (unless (key-exists-p spec ':value)
      (set-key spec ':value (car values)))
    (while values
      (aset buttons i (gtk-radio-button-new-with-label-from-widget
		       (if (zerop i)
			   nil
			 (aref buttons (1- i)))
		       (symbol-name (car values))))
      (gtk-toggle-button-set-state (aref buttons i)
				   (eq (car values) (get-key spec ':value)))
      (gtk-box-pack-start box (aref buttons i) nil nil)
      (gtk-signal-connect (aref buttons i) "toggled"
			  `(lambda (w)
			     (when (gtk-toggle-button-active w)
			       (ui-set ',spec ',(get-key spec ':variable)
				       ',(car values)))))
      (setq i (1+ i))
      (setq values (cdr values)))
    box))

(defun show-ui (spec)
  (let
      ((ui-apply-hook nil)
       (ui-apply-changed-hook nil)
       (ui-values-to-apply nil)
       (ui-original-values nil)
       (ui-changed-variables nil)
       (ui-active t)
       (ui-window (gtk-window-new 'toplevel))
       (ui-root (build-ui spec))
       (vbox (gtk-vbox-new nil 0))
       (hbox (gtk-hbutton-box-new))
       (ui-ok (gtk-button-new-with-label "OK"))
       (ui-apply (gtk-button-new-with-label "Try"))
       (ui-revert (and ui-enable-revert (gtk-button-new-with-label "Revert")))
       (refresh (and ui-enable-refresh (gtk-button-new-with-label "Refresh")))
       (cancel (gtk-button-new-with-label "Cancel")))
    (gtk-window-set-title ui-window "Sawmill configurator")
    (gtk-widget-set-name ui-window "Sawmill configurator")
    (gtk-signal-connect ui-window "delete_event" 'ui-quit)
    (gtk-container-add ui-window vbox)
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)
    (gtk-box-set-spacing hbox ui-box-spacing)
    (gtk-container-border-width hbox ui-box-border)
    (gtk-box-pack-end vbox hbox)
    (gtk-button-box-set-layout hbox 'end)
    (gtk-signal-connect ui-ok "clicked" 'ui-ok)
    (gtk-signal-connect ui-apply "clicked" 'ui-apply)
    (gtk-signal-connect cancel "clicked" 'ui-cancel)
    (when ui-enable-revert
      (gtk-signal-connect ui-revert "clicked" #'(lambda () (ui-revert))))
    (when ui-enable-refresh
      (gtk-signal-connect refresh "clicked" 'ui-refresh))
    (gtk-container-add hbox ui-apply)
    (when ui-enable-revert
      (gtk-container-add hbox ui-revert))
    (gtk-container-add hbox ui-ok)
    (gtk-container-add hbox cancel)
    (when ui-enable-refresh
      (gtk-container-add hbox refresh))
    (gtk-container-add vbox ui-root)
    (ui-set-button-states)
    (gtk-widget-show-all ui-window)
    (gtk-main)))

(defun ui-set-button-states ()
  (when ui-apply
    (gtk-widget-set-sensitive ui-apply (or ui-values-to-apply
					   ui-apply-changed-hook)))
  (when ui-revert
    (gtk-widget-set-sensitive ui-revert ui-changed-variables))
  (when ui-apply
    (gtk-widget-set-sensitive ui-ok (or ui-values-to-apply
					ui-apply-changed-hook
					ui-changed-variables))))

(defun ui-set-button-label (button text)
  (mapc #'(lambda (w)
	    (when (gtk-label-p w)
	      (gtk-label-set w text))) (gtk-container-children button)))


;; acting on settings

(defun ui-set (spec symbol value)
  (ui-set-original spec symbol)
  (setq ui-values-to-apply (delete-if #'(lambda (x)
					  (eq (car x) spec))
				      ui-values-to-apply))
  (setq ui-values-to-apply (cons (cons spec value) ui-values-to-apply))
  (set-key spec ':value value)
  (ui-set-button-states)
; (format standard-error "--| original %S\n--| to-apply %S\n--| changed %S\n"
;	  ui-original-values ui-values-to-apply ui-changed-variables)
  )

(defun ui-set-original (spec symbol)
  (unless (assq symbol ui-original-values)
    (setq ui-original-values (cons (cons symbol (get-key spec ':value))
				   ui-original-values))))

(defun ui-quit ()
  (gtk-widget-destroy ui-window)
  (gtk-main-quit))

(defun ui-set-variables (list)
  (let
      ((commands
	(mapcar #'(lambda (cell)
		    (let*
			((spec (car cell))
			 (new-value (cdr cell))
			 (symbol (get-key spec ':variable)))
		      (unless (memq symbol ui-changed-variables)
			(setq ui-changed-variables
			      (cons symbol ui-changed-variables)))
		      (when (get-key spec ':in-apply-hook)
			(set-key spec ':in-apply-hook nil))
		      `(customize-set ',symbol ',new-value)))
		ui-values-to-apply)))
    (ui-command (cons 'progn commands))
    (setq ui-values-to-apply nil)
    (setq ui-apply-changed-hook nil)
    (ui-set-button-states)
;   (format standard-error "--| original %S\n--| to-apply %S\n--| changed %S\n"
;	    ui-original-values ui-values-to-apply ui-changed-variables)
    ))

(defun ui-apply ()
  (call-hook 'ui-apply-changed-hook)
  (call-hook 'ui-apply-hook)
  (ui-set-variables ui-values-to-apply)
  (setq ui-apply-changed-hook nil))

(defun ui-ok ()
  (ui-apply)
  (ui-quit))

(defun ui-revert (&optional dont-refresh)
  (let
      ((commands
	(mapcar #'(lambda (symbol)
		    `(customize-set
		      ',symbol ',(cdr (assq symbol ui-original-values))))
		ui-changed-variables)))
    (ui-command (cons 'progn commands))
    (setq ui-values-to-apply nil)
    (setq ui-changed-variables nil)
;   (format standard-error "--| original %S\n--| to-apply %S\n--| changed %S\n"
;	    ui-original-values ui-values-to-apply ui-changed-variables)
    (unless dont-refresh
      (ui-refresh))))

(defun ui-cancel ()
  (ui-revert t)
  (ui-quit))

(defun ui-command (form)
  (format standard-output "%S\n" form)
  (when (filep standard-output)
    (flush-file standard-output)))

(defun ui-refresh ()
  (let
      ((vbox (gtk-widget-parent ui-root))
       spec)
    (gtk-container-remove vbox ui-root)
    (gtk-widget-destroy ui-root)
    (ui-command '(ui-refresh))
    (setq spec (read standard-input))
    (setq ui-changed-variables nil)
    (setq ui-values-to-apply nil)
    (setq ui-original-values nil)
    (setq ui-root (build-ui spec))
    (gtk-box-pack-start vbox ui-root)
    (gtk-widget-show-all ui-window)
    (ui-set-button-states)))


;; entry point, loop reading command forms, sending back results

(let
    ((input (read standard-input)))
  (show-ui input)
  (ui-command '(ui-exit)))

;; Local Variables:
;; major-mode: lisp-mode
;; End:
