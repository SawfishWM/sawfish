#!/bin/sh
exec rep "$0" "$@"
!#

;; sawmill-ui -- subprocess to handle configuration user interface
;; $Id: sawmill-ui.jl,v 1.63 2000/05/30 15:08:28 john Exp $

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

;(setq print-length 5)
;(setq print-depth 3)
;(setq debug-on-error t)

(require 'gtk)

(defvar sawmill-client-program "sawfish-client")

;; non-nil prevents actually changing any options, just print what
;; would be done to stderr
(defvar ui-debug nil)

;; from --group option, symbol defining customization group
(defvar ui-group t)

;; when non-nil the xid of a GtkSocket window. This prevents the list
;; of buttons being created and enables a protocol on stdin/stdout to 
;; detect when the buttons in the capplet are pressed (stdin) or when
;; there's unapplied state changes (stdout)
(defvar ui-socket-id nil)

;; top-level window or GtkPlug
(defvar ui-window nil)

;; root ui element
(defvar ui-root nil)

;; buttons if not a plug
(defvar ui-apply-widget nil)
(defvar ui-revert-widget nil)
(defvar ui-ok-widget nil)

;; list of (SPEC . VALUE) defining uncommitted option changes
(defvar ui-values-to-apply nil)

;; hook called to commit state changes (i.e. for modified text entry
;; widgets)
(defvar ui-apply-changed-hook nil)

;; list of (SYMBOL . ORIGINAL-VALUE) defining the old values of changed
;; options
(defvar ui-original-values nil)

;; list of variables changed since the last revert
(defvar ui-changed-variables nil)

;; container attributes
(defconst ui-box-spacing 4)
(defconst ui-box-border 5)

;; XXX this may be confusing?
(defvar ui-enable-refresh nil)

;; polish symbol names in keymaps
(defvar ui-beautify-keymaps t)

;; may be list or notebook
(defvar ui-pages-style 'list)

;; may be list, radio, or menu
(defvar ui-set-style 'menu)

(defvar ui-match-window-max-matchers 3)

;; for i18n
(defvar ui-lang (or (getenv "LC_ALL") (getenv "LC_MESSAGES") (getenv "LANG")))
(defvar ui-lang-base (when ui-lang
		       (if (string-match "^([^_.]+)_.*" ui-lang)
			   (expand-last-match "\\1")
			 ui-lang)))

(defvar ui-color-preview-width 28)
(defvar ui-color-preview-height 16)

(defvar ui-hack-activate-tree nil)


;; compatibility

(unless (boundp 'quotient)
  (setq quotient /))


;; wm communication

(defun sawmill-eval (form &optional read-back)
  (let*
      ((output (make-string-output-stream))
       (process (make-process output))
       (print-escape t))
    (if (zerop (call-process process nil sawmill-client-program
			     "-e" (format nil "%S" form)))
	(progn
	  (setq output (get-output-stream-string output))
	  ;; success
	  (if read-back
	      (read-from-string output)
	    output))
      (error "can't call sawfish-client"))))

(defun sawmill-eval-async (form)
  (let
      ((process (make-process nil))
       (print-escape t))
    (unless (zerop (call-process process nil sawmill-client-program
				 "-q" "-e" (format nil "%S" form)))
      (error "can't call sawfish-client"))))


;; ui builder

;; elements may be:

;; (tree . SUBTREE)
;;   SUBTREE -> (NAME ELEMENT (SUBTREES...))
;; (pages (LABEL ELEMENT)...)
;; (hbox ELEMENTS...)
;; (vbox ELEMENTS...)
;; (table (COLS ROWS) ((LEFT RIGHT TOP BOTTOM) ELEMENT)...)
;; (label TEXT)
;; (text TEXT)
;; (hsep)
;; (vsep)
;; (frame TEXT ELEMENT)

;; (toggle TEXT)
;; (number)
;; (string)
;; (symbol LIST)
;; (font)
;; (file-name)
;; (keymap)
;; (keymap-shell (PAGES...))
;; (frame-style (SYMBOLS...))

;; Most of these elements also use the tail of the list as a plist,
;; storing both working values and input parameters. Some of the inputs
;; are as follows:

;;	:feature FUNCTION
;;	:variable SYMBOL
;;	:value VALUE
;;	:doc STRING
;;	:allow-nil t
;;	:doc-path DIRECTORY-LIST		only in keymap-shell
;;	:commands COMMAND-LIST			only in keymap-shell

;; also, much work data is stored in each element's list of keys

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
  (if (null spec)
      (gtk-label-new "[empty]")
    (let
	((fun (get (car spec) 'builder)))
      (if fun
	  (fun spec)
	(error "Unknown ui element: %S" spec)))))

(defun build-tree (spec)
  (let
      ((hbox (gtk-hpaned-new))
       (tree (gtk-tree-new))
       (vbox (gtk-vbox-new nil 0))
       (scroller (gtk-scrolled-window-new)))
    (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
    (gtk-container-border-width hbox ui-box-border)
    (gtk-paned-add1 hbox scroller)
    (gtk-scrolled-window-add-with-viewport scroller tree)
    (gtk-widget-set-usize scroller 120 -2)
    (gtk-paned-add2 hbox vbox)
    (gtk-tree-set-selection-mode tree 'browse)
    ;;(gtk-container-border-width vbox 16)
    (letrec
	((iterator
	  (lambda (tree tree-widget)
	    (let
		((item (gtk-tree-item-new-with-label (nth 0 tree)))
		 (t-vbox (gtk-vbox-new nil 0))
		 (widget (build-ui (nth 1 tree))))
	      (gtk-tree-append tree-widget item)
	      (gtk-widget-show-all item)
	      (gtk-signal-connect
	       item "select"
	       (lambda ()
		 (mapc (lambda (w)
			 (gtk-container-remove vbox w))
		       (gtk-container-children vbox))
		 (gtk-widget-show-all t-vbox)
		 (gtk-container-add vbox t-vbox)))
	      (gtk-box-pack-start t-vbox widget t t)
	      (when (nth 2 tree)
		(let
		    ((subtree (gtk-tree-new)))
		  (gtk-tree-set-selection-mode subtree 'browse)
		  (gtk-tree-item-set-subtree item subtree)
		  (mapc (lambda (x)
			  (iterator x subtree)) (nth 2 tree))))))))
      (iterator (cdr spec) tree))
    (gtk-widget-show-all hbox)
    (let
	((tem (car (gtk-container-children tree))))
      (gtk-tree-item-expand tem)
      (setq ui-hack-activate-tree tree))
    hbox))
(put 'tree 'builder build-tree)

;; used by --flatten option
(defun build-tree-as-list (spec)
  (letrec
      ((scroller (gtk-scrolled-window-new))
       (top-vbox (gtk-vbox-new nil ui-box-spacing))
       (iterator (lambda (tree &optional top-level)
		   (let
		       ((root-spec (cadr tree))
			(sub-specs (caddr tree)))
		     (when root-spec
		       (let*
			   ((frame (or top-level (gtk-frame-new (car tree))))
			    (vbox (gtk-vbox-new nil 0)))
			 (if top-level
			     (gtk-container-add top-vbox vbox)
			   (gtk-container-add top-vbox frame)
			   (gtk-container-add frame vbox))
			 (gtk-box-pack-start vbox (build-ui root-spec))))
		     (mapc iterator sub-specs)))))
    (iterator (cdr spec) t)
    (gtk-container-border-width top-vbox ui-box-border)
    (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
    (gtk-widget-set-usize scroller 500 300)
    (gtk-scrolled-window-add-with-viewport scroller top-vbox)
    (gtk-viewport-set-shadow-type (gtk-widget-parent top-vbox) 'none)
    (gtk-widget-show-all scroller)
    scroller))

(defun build-pages (spec)
  (cond ((eq ui-pages-style 'notebook)
	 (let
	     ((book (gtk-notebook-new)))
	   (gtk-notebook-set-scrollable book 1)
	   (gtk-notebook-popup-enable book)
	   (mapc (lambda (page)
		   (gtk-notebook-append-page book (build-ui (car (cdr page)))
					     (gtk-label-new (car page))))
		 (cdr spec))
	   book))
	((eq ui-pages-style 'list)
	 (let
	     ((hbox (gtk-hpaned-new))
	      (clist (gtk-clist-new 1))
	      (frame (gtk-frame-new))
	      (contents (make-vector (length (cdr spec)))))
	   (gtk-container-border-width hbox ui-box-border)
	   (gtk-clist-set-column-auto-resize clist 0 t)
	   (gtk-paned-add1 hbox clist)
	   (gtk-paned-add2 hbox frame)
	   (gtk-clist-set-selection-mode clist 'browse)
	   (mapc (lambda (page)
		   (let
		       ((row (gtk-clist-append clist (vector (car page))))
			(widget (build-ui (nth 1 page))))
		     ;; (LABEL . WIDGET)
		     (gtk-widget-show-all widget)
		     (aset contents row (cons (car page) widget))))
		 (cdr spec))
	   (gtk-signal-connect clist "select_row"
			       (lambda (clist row col)
				 (build-pages:select-row
				  contents row frame)))
	   (gtk-clist-select-row clist 0 0)
	   hbox))))
(put 'pages 'builder build-pages)

(defun build-pages:select-row (contents row frame)
  (mapc (lambda (w)
	  (gtk-container-remove frame w)) (gtk-container-children frame))
  (gtk-frame-set-label frame (car (aref contents row)))
  (gtk-container-add frame (cdr (aref contents row))))

(defun build-box (spec)
  (let
      ((box (if (eq (car spec) 'vbox)
		(gtk-vbox-new nil 0)
	      (gtk-hbox-new nil 0))))
    (gtk-box-set-spacing box ui-box-spacing)
    (gtk-container-border-width box ui-box-border)
    (mapc (lambda (widget)
	    (gtk-box-pack-start box (build-ui widget) nil nil)) (cdr spec))
    box))
(put 'vbox 'builder build-box)
(put 'hbox 'builder build-box)

(defun build-table (spec)
  (let
      ((table (gtk-table-new (car (nth 1 spec)) (nth 1 (nth 1 spec)) nil)))
    (mapc (lambda (cell)
	    (gtk-table-attach-defaults table
				       (build-ui (nth 1 cell))
				       (nth 0 (car cell))
				       (nth 1 (car cell))
				       (nth 2 (car cell))
				       (nth 3 (car cell)))) (nthcdr 2 spec))
    table))
(put 'table 'builder build-table)

(defun build-label (spec)
  (let
      ((label (gtk-label-new (nth 1 spec))))
    (gtk-label-set-justify label 'left)
    (gtk-label-set-line-wrap label t)
    label))
(put 'label 'builder build-label)

(defun build-text (spec)
  (let
      ((text (gtk-text-new)))
    (gtk-text-set-editable text nil)
    (gtk-text-set-word-wrap text 1)
    (gtk-text-insert text nil nil nil (nth 1 spec) (length (nth 1 spec)))
    text))
(put 'text 'builder build-text)

(defun build-frame (spec)
  (let
      ((frame (gtk-frame-new (nth 1 spec))))
    (when (nth 2 spec)
      (gtk-container-add frame (build-ui (nth 2 spec))))
    frame))
(put 'frame 'builder build-frame)

(defun build-separator (spec)
  (if (eq (car spec) 'hsep)
      (gtk-hseparator-new)
    (gtk-vseparator-new)))
(put 'hsep 'builder build-separator)
(put 'vsep 'builder build-separator)

(defun build-toggle (spec)
  (let
      ((toggle (gtk-check-button-new-with-label (nth 1 spec))))
    (mapc (lambda (w)
	    (when (gtk-label-p w)
	      (gtk-label-set-justify w 'left)))
	  (gtk-container-children toggle))
    (if (key-exists-p spec ':value)
	(when (get-key spec ':value)
	  (gtk-button-clicked toggle))
      (set-key spec ':value nil))
    (gtk-signal-connect toggle "clicked"
			(lambda (w)
			  (let
			      ((value (not (get-key spec ':value))))
			    (ui-set spec (get-key spec ':variable) value))))
    toggle))
(put 'toggle 'builder build-toggle)

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
				 (lambda (w)
				   (build-entry:changed spec))))
    (setq spec (nconc spec (list ':widget entry)))
    entry))
(put 'string 'builder build-entry)

(defun build-number-entry (spec)
  (unless (key-exists-p spec ':value)
    (set-key spec ':value 0))
  (let*
      ((range (get-key spec ':range))
       (entry (gtk-spin-button-new (gtk-adjustment-new
				    (get-key spec ':value)
				    (or (car range) 0)
				    ;; need maxint
				    (or (cdr range) 1000000)
				    1 16 0) 1 0)))
    (gtk-signal-connect entry "changed"
			(lambda (w)
			  (build-entry:changed spec)))
    (setq spec (nconc spec (list ':widget entry)))
    entry))
(put 'number 'builder build-number-entry)

(defun build-entry:changed (spec)
  (unless (get-key spec ':in-apply-hook)
    (ui-add-apply-hook (lambda ()
			 (build-entry:set spec)))
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

(defun build-font (spec)
  (let
      ((button (gtk-button-new-with-label
		(build-font:abbrev (or (get-key spec ':value) "")))))
    (mapc (lambda (w)
	    (when (gtk-label-p w)
	      (gtk-label-set-line-wrap w t)))
	  (gtk-container-children button))
    (unless (key-exists-p spec ':value)
      (set-key spec ':value nil))
    (gtk-signal-connect button "clicked"
			(lambda (w)
			  (build-font:clicked spec)))
    (setq spec (nconc spec (list ':widget button)))
    button))
(put 'font 'builder build-font)

(defun build-font:clicked (spec)
  (let
      ((fontsel (gtk-font-selection-dialog-new (_ "Select font"))))
    (when (get-key spec ':value)
      (gtk-font-selection-dialog-set-font-name fontsel (get-key spec ':value)))
    (gtk-signal-connect
     (gtk-font-selection-dialog-ok-button fontsel)
     "clicked"
     (lambda (w)
       (let
	   ((value (gtk-font-selection-dialog-get-font-name fontsel)))
	 (when (and (string= value "") (get-key spec ':allow-nil))
	   (setq value nil))
	 (ui-set spec (get-key spec ':variable) value)
	 (ui-set-button-label (get-key spec ':widget)
			      (build-font:abbrev value))
	 (gtk-widget-destroy fontsel))))
    (gtk-signal-connect
     (gtk-font-selection-dialog-cancel-button fontsel)
     "clicked" (lambda (w)
		 (gtk-widget-destroy fontsel)))
    (gtk-signal-connect fontsel
     "delete_event" (lambda (w)
		      (gtk-widget-destroy fontsel)))
    (gtk-widget-show fontsel)
    (gtk-grab-add fontsel)))

(defun build-font:abbrev (font-name)
  (if (string-match "-[^-]+-([^-]+)-" font-name)
      (expand-last-match "\\1")
    font-name))

(defun build-color (spec)
  (let
      ((button (ui-button-new-with-color (get-key spec ':value))))
    (unless (key-exists-p spec ':value)
      (set-key spec ':value nil))
    (gtk-signal-connect button "clicked"
			(lambda (w)
			  (build-color:clicked spec)))
    (setq spec (nconc spec (list ':widget button)))
    button))
(put 'color 'builder build-color)

(defun build-color:clicked (spec)
  (let
      ((colorsel (gtk-color-selection-dialog-new (_ "Select color"))))
    (when (get-key spec ':value)
      (gtk-color-selection-set-color-interp
       (gtk-color-selection-dialog-colorsel colorsel)
       (gdk-color-parse-interp (get-key spec ':value))))
    (gtk-signal-connect
     (gtk-color-selection-dialog-ok-button colorsel)
     "clicked"
     (lambda (w)
       (let*
	   ((color (gtk-color-selection-get-color-interp
		    (gtk-color-selection-dialog-colorsel colorsel)))
	    (name (and color (format nil "#%04x%04x%04x"
				     (gdk-color-red color)
				     (gdk-color-green color)
				     (gdk-color-blue color)))))
	 (when (or name (get-key spec ':allow-nil))
	   (ui-set spec (get-key spec ':variable) name)
	   (ui-set-button-color (get-key spec ':widget) name))
	 (gtk-widget-destroy colorsel))))
    (gtk-signal-connect
     (gtk-color-selection-dialog-cancel-button colorsel)
     "clicked" (lambda (w)
		 (gtk-widget-destroy colorsel)))
    (gtk-signal-connect colorsel
     "delete_event" (lambda (w)
		      (gtk-widget-destroy colorsel)))
    (gtk-widget-hide (gtk-color-selection-dialog-help-button colorsel))
    (gtk-widget-show colorsel)
    (gtk-grab-add colorsel)))

(defun build-file (spec)
  (let
      ((button (gtk-button-new-with-label
		(file-name-nondirectory (or (get-key spec ':value) "")))))
    (mapc (lambda (w)
	    (when (gtk-label-p w)
	      (gtk-label-set-line-wrap w t)))
	  (gtk-container-children button))
    (unless (key-exists-p spec ':value)
      (set-key spec ':value nil))
    (gtk-signal-connect button "clicked"
			(lambda (w)
			  (build-file:clicked spec)))
    (setq spec (nconc spec (list ':widget button)))
    button))
(put 'file-name 'builder build-file)

(defun build-file:clicked (spec)
  (let
      ((filesel (gtk-file-selection-new (_ "Select file"))))
    (when (get-key spec ':value)
      (gtk-file-selection-set-filename filesel (get-key spec ':value)))
    (gtk-signal-connect
     (gtk-file-selection-ok-button filesel)
     "clicked"
     (lambda (w)
       (let
	   ((value (gtk-file-selection-get-filename filesel)))
	 (when (and (string= value "") (get-key spec ':allow-nil))
	   (setq value nil))
	 (ui-set spec (get-key spec ':variable) value)
	 (ui-set-button-label (get-key spec ':widget)
			      (file-name-nondirectory value))
	 (gtk-widget-destroy filesel))))
    (gtk-signal-connect
     (gtk-file-selection-cancel-button filesel)
     "clicked" (lambda (w)
		 (gtk-widget-destroy filesel)))
    (gtk-signal-connect filesel
     "delete_event" (lambda (w)
		      (gtk-widget-destroy filesel)))
    (gtk-widget-show filesel)))

(defun build-symbol (spec)
  (let*
      ((values (nth 1 spec))
       (buttons (make-vector (length values)))
       (type (or (get-key spec ':widget) ui-set-style))
       (i 0)
       group)
    (unless (key-exists-p spec ':value)
      (set-key spec ':value (car values)))
    (cond
     ((eq type 'menu)
      (let
	  ((omenu (gtk-option-menu-new))
	   (menu (gtk-menu-new))
	   history)
	(while values
	  (aset buttons i (gtk-radio-menu-item-new-with-label-from-widget
			   (if (zerop i) nil (aref buttons (1- i)))
			   (symbol-name (car values))))
	  (gtk-menu-append menu (aref buttons i))
	  (when (eq (car values) (get-key spec ':value))
	    (gtk-check-menu-item-set-state (aref buttons i) t)
	    (setq history i))
	  (gtk-widget-show (aref buttons i))
	  (gtk-signal-connect (aref buttons i) "toggled"
			      (let ((i i))
				(lambda (w)
				  (when (gtk-check-menu-item-active w)
				    (build-symbol:select-row spec i)))))
	  (setq i (1+ i))
	  (setq values (cdr values)))
	(gtk-option-menu-set-menu omenu menu)
	(when history
	  (gtk-option-menu-set-history omenu history))
	omenu))
     ((eq type 'list)
      (let
	  ((clist (gtk-clist-new 1)))
	(gtk-clist-set-column-auto-resize clist 0 t)
	(gtk-clist-set-selection-mode clist 'browse)
	(while values
	  (gtk-clist-append clist (vector (symbol-name (car values))))
	  (when (eq (car values) (get-key spec ':value))
	    (gtk-clist-select-row clist i 0)
	    (gtk-clist-moveto clist i 0 nil 0))
	  (setq i (1+ i))
	  (setq values (cdr values)))
	(gtk-signal-connect clist "select_row"
			    (lambda (clist row col)
			      (build-symbol:select-row spec row)))
	(setq spec (nconc spec (list ':clist clist)))
	clist))
     ((eq type 'radio)
      (let
	  ((box (gtk-vbox-new nil 0)))
	(while values
	  (aset buttons i (gtk-radio-button-new-with-label-from-widget
			   (if (zerop i) nil (aref buttons (1- i)))
			   (symbol-name (car values))))
	  (when (eq (car values) (get-key spec ':value))
	    (gtk-toggle-button-set-state (aref buttons i) t))
	  (gtk-box-pack-start box (aref buttons i) nil nil)
	  (gtk-signal-connect (aref buttons i) "toggled"
			      (let ((i i))
				(lambda (w)
				  (when (gtk-toggle-button-active w)
				    (build-symbol:select-row spec i)))))
	  (setq i (1+ i))
	  (setq values (cdr values)))
	box)))))
(put 'symbol 'builder build-symbol)

(defun build-symbol:select-row (spec row)
  (ui-set spec (get-key spec ':variable) (nth row (nth 1 spec))))


;; customizing keymaps

(defvar ui-keymap-shell nil)

(defun build-keymap (spec)
  (let
      ((hbox (gtk-vbox-new nil 0))
       (vbox (gtk-hbox-new nil 0))
       (vbox-2 (gtk-vbox-new nil 0))
       (label (gtk-label-new (get-key spec ':doc)))
       (insert (gtk-button-new-with-label (_ "Insert")))
       (copy (gtk-button-new-with-label (_ "Copy")))
       (deleteb (gtk-button-new-with-label (_ "Delete")))
       (clist (gtk-clist-new-with-titles (vector (_ "Key") (_ "Command"))))
       (scroller (gtk-scrolled-window-new)))

    (gtk-box-set-spacing hbox ui-box-spacing)
    (gtk-container-border-width hbox ui-box-border)
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)

    (gtk-clist-set-column-auto-resize clist 0 t)
    (gtk-clist-set-column-auto-resize clist 1 t)
    (gtk-clist-set-selection-mode clist 'browse)
    (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
    (gtk-widget-set-usize scroller 300 150)
    (gtk-container-add hbox scroller)
    (gtk-box-pack-end hbox vbox)
    (gtk-container-add scroller clist)
    (gtk-container-add vbox copy)
    (gtk-container-add vbox insert)
    (gtk-container-add vbox deleteb)
    (mapc (lambda (cell)
	    (gtk-clist-append clist (vector (cdr cell)
					    (beautify-symbol-name
					     (car cell)))))
	  (cdr (get-key spec ':value)))
    (setq spec (nconc spec (list ':shell ui-keymap-shell
				 ':clist clist
				 ':selection 0)))
    (gtk-signal-connect copy "clicked" (lambda ()
					 (build-keymap:copy spec)))
    (gtk-signal-connect insert "clicked" (lambda ()
					   (build-keymap:insert spec)))
    (gtk-signal-connect deleteb "clicked" (lambda ()
					    (build-keymap:delete spec)))
    (gtk-signal-connect clist "select_row" (lambda (w row col)
					     (set-key spec ':selection row)
					     (build-keymap:select-row spec)))
    (gtk-box-pack-start vbox-2 label)
    (gtk-label-set-justify label 'left)
    (gtk-container-add vbox-2 hbox)
    vbox-2))
(put 'keymap 'builder build-keymap)

(defun build-keymap:select-row (spec)
  (let*
      ((row (get-key spec ':selection))
       (shell (get-key spec ':shell))
       (binding (nth row (cdr (get-key spec ':value)))))
    (if binding
	(build-keymap-shell:set-binding shell (cdr binding) (car binding))
      (build-keymap-shell:set-binding shell "" 'nop))))

(defun build-keymap:insert (spec)
  (let*
      ((map (copy-sequence (get-key spec ':value)))
       (pred (nthcdr (get-key spec ':selection) map)))
    (rplacd pred (cons (cons 'nop "Null") (cdr pred)))
    (ui-set spec (get-key spec ':variable) map)
    (gtk-clist-insert (get-key spec ':clist)
		      (get-key spec ':selection)
		      (vector "Null" (beautify-symbol-name 'nop)))
    (gtk-clist-select-row (get-key spec ':clist)
			  (get-key spec ':selection) 0)))

(defun build-keymap:copy (spec)
  (let*
      ((map (copy-sequence (get-key spec ':value)))
       (binding (nth (get-key spec ':selection) (cdr map)))
       (pred (nthcdr (get-key spec ':selection) map)))
    (rplacd pred (cons (cons (car binding) (cdr binding)) (cdr pred)))
    (ui-set spec (get-key spec ':variable) map)
    (gtk-clist-insert (get-key spec ':clist)
		      (get-key spec ':selection)
		      (vector (cdr binding)
			      (beautify-symbol-name (car binding))))
    (set-key spec ':selection (1+ (get-key spec ':selection)))
    (gtk-clist-select-row (get-key spec ':clist)
			  (get-key spec ':selection) 0)))

(defun build-keymap:delete (spec)
  (let*
      ((map (copy-sequence (get-key spec ':value)))
       (pred (nthcdr (get-key spec ':selection) map)))
    (rplacd pred (nthcdr 2 pred))
    (ui-set spec (get-key spec ':variable) map)
    (gtk-clist-remove (get-key spec ':clist) (get-key spec ':selection))))

(defun ui-bind-key (spec index new)
  (let*
      ((map (copy-sequence (get-key spec ':value)))
       (cell (nthcdr (get-key spec ':selection) (cdr map))))
    (unless (equal (car cell) new)
      (rplaca cell (cons (car new) (cdr new)))
      (ui-set spec (get-key spec ':variable) map)
      (gtk-clist-set-text (get-key spec ':clist)
			  index 0 (cdr new))
      (gtk-clist-set-text (get-key spec ':clist)
			  index 1 (beautify-symbol-name (car new))))))

(defun build-keymap-shell (spec)
  (let*
      ((ui-keymap-shell spec)
       (pages (nth 1 spec))
       (hbox-1 (gtk-hbox-new nil 0))
       (vbox-2 (gtk-vbox-new nil 0))
       (vbox (gtk-vbox-new nil 0))
       (paned (gtk-vpaned-new))
       (entry (gtk-entry-new))
       (entry-hbox (gtk-hbox-new nil 0))
       (entry-button (gtk-button-new-with-label (_ "Grab key...")))
       (map-clist (gtk-clist-new-with-titles (vector (_ "Keymaps"))))
       (frame (gtk-frame-new))
       (doc-frame (gtk-frame-new))
       (doc-label (gtk-label-new ""))
       (cmd-clist (gtk-clist-new-with-titles (vector (_ "Commands"))))
       (scroller (gtk-scrolled-window-new))
       (scroller-2 (gtk-scrolled-window-new))
       ;; vector of [LABEL SPEC WIDGET] for each keymap
       (maps (make-vector (length pages))))

    (gtk-box-set-spacing hbox-1 ui-box-spacing)
    (gtk-box-set-spacing vbox-2 ui-box-spacing)
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)

    (when (get-key spec ':doc-path)
      (setq documentation-files (get-key spec ':doc-path)))

    (gtk-container-add doc-frame doc-label)
    (gtk-label-set-justify doc-label 'left)

    (rplacd spec (nthcdr 2 spec))
    (setq spec (nconc spec (list ':maps maps
				 ':frame frame
				 ':cmd-clist cmd-clist
				 ':entry entry
				 ':doc-frame doc-frame
				 ':doc-label doc-label
				 ':active-map 0)))

    ;; 1. the key and command editing widget
    (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
    (gtk-widget-set-usize scroller 300 150)
    (gtk-container-add scroller cmd-clist)
    (gtk-clist-set-column-auto-resize cmd-clist 0 t)
    ;(gtk-clist-set-selection-mode cmd-clist 'browse)
    (mapc (lambda (c)
	    (gtk-clist-append cmd-clist (vector (beautify-symbol-name c))))
	  (get-key spec ':commands))
    (gtk-container-add entry-hbox entry)
    (gtk-box-pack-end entry-hbox entry-button)
    (gtk-container-add vbox-2 scroller)
    (gtk-box-pack-end vbox-2 entry-hbox)
    (gtk-signal-connect cmd-clist "select_row"
			(lambda (w row col)
			  (set-key spec ':active-cmd row)
			  (build-keymap-shell:set-command spec)))
    (gtk-signal-connect entry "changed"
			(lambda (w)
			  (build-keymap-shell:set-event spec)))
    (gtk-signal-connect entry-button "clicked"
			(lambda ()
			  (build-keymap-shell:grab spec)))

    ;; 2. the keymap selection widget
    (gtk-scrolled-window-set-policy scroller-2 'automatic 'automatic)
    (gtk-clist-set-column-auto-resize map-clist 0 t)
    (gtk-clist-set-selection-mode map-clist 'browse)
    (gtk-widget-set-usize scroller-2 150 150)
    (gtk-container-add scroller-2 map-clist)
    (gtk-container-add hbox-1 scroller-2)
    (gtk-container-add hbox-1 vbox-2)
    (mapc (lambda (page)
	    (let*
		((name (beautify-symbol-name (intern (car page))))
		 row)
	      (when (and ui-beautify-keymaps (string-match " keymap" name))
		(setq name (substring name 0 (match-start))))
	      (setq row (gtk-clist-append map-clist (vector name)))
	      (aset maps row (vector (car page)
				     (nth 1 page)
				     (build-ui (nth 1 page))))
	      (gtk-widget-show-all (aref (aref maps row) 2))))
	  pages)
    (gtk-signal-connect map-clist "select_row"
			(lambda (w row col)
			  (build-keymap-shell:select-map spec maps row)))
    (gtk-clist-select-row map-clist 0 0)

    (gtk-box-pack-start vbox frame t t)
    (gtk-box-pack-end vbox doc-frame nil t)
    (gtk-paned-add1 paned vbox)
    (gtk-paned-add2 paned hbox-1)

    paned))
(put 'keymap-shell 'builder build-keymap-shell)

(defun build-keymap-shell:current-binding (spec)
  (let*
      ((map (aref (get-key spec ':maps) (get-key spec ':active-map)))
       (binding (get-key (aref map 1) ':selection)))
    (nth binding (cdr (get-key (aref map 1) ':value)))))

(defun build-keymap-shell:select-map (spec maps row)
  (let
      ((frame (get-key spec ':frame)))
    (set-key spec ':active-map row)
    (mapc (lambda (w)
	    (gtk-container-remove frame w)) (gtk-container-children frame))
    (gtk-frame-set-label frame (aref (aref maps row) 0))
    (gtk-container-add frame (aref (aref maps row) 2))
    (build-keymap:select-row (aref (aref maps row) 1))))

(defun build-keymap-shell:set-binding (spec event command)
  (if (symbolp command)
      (let*
	  ((commands (get-key spec ':commands))
	   (c-row (- (length commands) (length (memq command commands)))))
	(gtk-clist-select-row (get-key spec ':cmd-clist) c-row 0)
	(gtk-clist-moveto (get-key spec ':cmd-clist) c-row 0 nil 0))
    (gtk-clist-unselect-all (get-key spec ':cmd-clist)))
  (gtk-entry-set-text (get-key spec ':entry) event)
  (build-keymap-shell:update-doc spec))

(defun build-keymap-shell:set-event (spec)
  (let*
      ((text (gtk-entry-get-text (get-key spec ':entry)))
       (map (aref (get-key spec ':maps) (get-key spec ':active-map)))
       (binding (get-key (aref map 1) ':selection))
       (value (nth binding (cdr (get-key (aref map 1) ':value)))))
    (when value
      (setq value (cons (car value) text))
      (ui-bind-key (aref map 1) binding value))))
    
(defun build-keymap-shell:set-command (spec)
  (let*
      ((command (and (get-key spec ':active-cmd)
		     (nth (get-key spec ':active-cmd)
			  (get-key spec ':commands))))
       (map (aref (get-key spec ':maps) (get-key spec ':active-map)))
       (binding (get-key (aref map 1) ':selection))
       (value (nth binding (cdr (get-key (aref map 1) ':value)))))
    (when (and value command)
      (setq value (cons command (cdr value))))
    (ui-bind-key (aref map 1) binding value)
    (build-keymap-shell:update-doc spec)))

(defun build-keymap-shell:update-doc (spec)
  (let*
      ((frame (get-key spec ':doc-frame))
       (label (get-key spec ':doc-label))
       (command (car (build-keymap-shell:current-binding spec))))
    (when (consp command)
      (setq command (car command)))
    (if (and command (symbolp command))
	(let
	    ((doc (documentation command)))
	  (gtk-frame-set-label frame (beautify-symbol-name command))
	  (gtk-label-set label (or doc (_ "Undocumented"))))
      (gtk-frame-set-label frame "")
      (gtk-label-set label ""))))

(defun build-keymap-shell:grab (spec)
  (let
      ((event (sawmill-eval '(event-name (read-event)) t)))
    (gtk-entry-set-text (get-key spec ':entry) event)
    (build-keymap-shell:set-event spec)))


;; customizing frame styles

(defun build-frame-style (spec)
  (let
      ((vbox (gtk-vbox-new nil 0))
       (hbox (gtk-hbox-new nil 0))
       (combo (gtk-combo-new))
       (doc-label (gtk-label-new (get-key spec ':doc)))
       (readme-label (gtk-label-new ""))
       (readme-scroller (gtk-scrolled-window-new))
       (values (nth 1 spec))
       (i 0)
       history button previous)

    (gtk-box-set-spacing hbox ui-box-spacing)
    (gtk-container-border-width hbox ui-box-border)
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)
    (gtk-scrolled-window-add-with-viewport readme-scroller readme-label)
    (gtk-container-add hbox combo)
    (gtk-container-add hbox doc-label)
    (gtk-container-add vbox readme-scroller)
    (gtk-container-add vbox hbox)
    (gtk-label-set-justify doc-label 'left)
    (gtk-label-set-justify readme-label 'left)
    (gtk-entry-set-editable (gtk-combo-entry combo) nil)
    (gtk-scrolled-window-set-policy readme-scroller 'automatic 'automatic)
    (gtk-widget-set-usize readme-scroller -2 200)

    (unless (key-exists-p spec ':value)
      (set-key spec ':value (car values)))
    (gtk-combo-set-popdown-strings combo (mapcar symbol-name values))
    (gtk-entry-set-text (gtk-combo-entry combo)
			(symbol-name (get-key spec ':value)))
    (gtk-signal-connect (gtk-combo-entry combo) "changed"
			(lambda ()
			  (build-frame-style:set
			   spec (intern (gtk-entry-get-text
					 (gtk-combo-entry combo))))))
						 

    (setq spec (nconc spec (list ':readme readme-label)))

    (build-frame-style:update-readme spec)
    vbox))
(put 'frame-style 'builder build-frame-style)
    
(defun build-frame-style:set (spec value)
  (ui-set spec (get-key spec ':variable) value)
  (build-frame-style:update-readme spec))

(defun build-frame-style:update-readme (spec)
  (catch 'out
    (let
	((theme (symbol-name (get-key spec ':value))))
      (mapc (lambda (dir)
	      (let
		  ((full (expand-file-name theme dir)))
		(when (catch 'out
			(mapc (lambda (suf)
				(let ((dir (format nil suf full theme)))
				  (condition-case nil
				      (when (file-directory-p dir)
					(setq full dir)
					(throw 'out t))
				    (error))))
			      '("%s" "%s.tar#tar/%s" "%s.tar.gz#tar/%s"
			       "%s.tar.Z#tar/%s" "%s.tar.bz2#tar/%s"))
			nil)
		  (setq full (i18n-filename (expand-file-name "README" full)))
		  (if (file-exists-p full)
		      (let
			  ((text (make-string-output-stream))
			   (file (open-file full 'read)))
			(unwind-protect
			    (progn
			      (copy-stream file text)
			      (setq text (get-output-stream-string text))
			      (when (string-match "\\s+$" text)
				(setq text (substring text 0 (match-start))))
			      (gtk-label-set (get-key spec ':readme) text))
			  (close-file file)))
		    (gtk-label-set (get-key spec ':readme) ""))
		  (throw 'out t))))
	    (get-key spec ':theme-path))
      (gtk-label-set (get-key spec ':readme) ""))))


;; customizing window matchers

(defun match-window:edit (spec cell &optional callback)
  (let*
      ((x-properties (get-key spec ':x-properties))
       (properties (get-key spec ':properties))
       bool-props
       (window (gtk-window-new 'dialog))
       (vbox (gtk-vbox-new nil 0))
       (vbox-2 (gtk-vbox-new nil 0))
       (hbox-2 (gtk-hbutton-box-new))
       (ok (gtk-button-new-with-label (_ "OK")))
       (cancel (gtk-button-new-with-label (_ "Cancel")))
       (frame (gtk-frame-new (_ "Actions")))
       (frame-1 (gtk-frame-new (_ "Matchers")))
       table table-2
       (table-1 (gtk-table-new ui-match-window-max-matchers 3 nil))
       (match-widget-alist nil)
       (prop-widget-alist nil)

       (make-cell
	(lambda ()
	  (catch 'out
	    (let
		((read-num (lambda (string)
			     (and (string-match "^[+-]?\\d+$" string)
				  (read-from-string string))))
		 matchers actions tem)
			  

	      (mapc (lambda (cell)
		      (let
			  ((atom (gtk-entry-get-text
				  (gtk-combo-entry (car cell))))
			   (re (gtk-entry-get-text (cdr cell))))
			(unless (or (string= atom "")
				    (string= re ""))
			  (setq atom (or (car (rassoc atom x-properties))
					 (intern atom)))
			  (setq matchers (cons (cons atom re) matchers)))))
		    match-widget-alist)
	      (unless matchers
		(throw 'out nil))

	      (mapc (lambda (cell)
		      (cond ((gtk-check-button-p (cdr cell))
			     ;; boolean
			     (when (gtk-toggle-button-active (cdr cell))
			       (setq actions (cons
					      (cons (car cell) t) actions))))
			    ((gtk-entry-p (cdr cell))
			     ;; number
			     (setq tem (read-num
					(gtk-entry-get-text (cdr cell))))
			     (when tem
			       (setq actions
				     (cons (cons (car cell) tem) actions))))
			    ((gtk-combo-p (cdr cell))
			     ;; symbol
			     (setq tem (gtk-entry-get-text
					(gtk-combo-entry (cdr cell))))
			     (unless (string= tem "")
			       (setq actions
				     (cons (cons (car cell) (intern tem))
					   actions))))

			    ((consp (cdr cell))
			     ;; pair of numbers
			     (let
				 (x y)
			       (setq x (read-num (gtk-entry-get-text
						  (car (cdr cell)))))
			       (setq y (read-num (gtk-entry-get-text
						  (cdr (cdr cell)))))
			       (when (and x y)
				 (setq actions
				       (cons (cons (car cell) (cons x y))
					     actions)))))

			    (t
			     (error "Unknown widget type"))))
		    prop-widget-alist)

	      (cons (nreverse matchers) (nreverse actions))))))

       (attach-label (lambda (table label &rest args)
		       (let
			   ((box (gtk-hbox-new nil 0)))
			 (gtk-box-pack-end box label)
			 (apply gtk-table-attach-defaults table box args)))))

    (unless cell
      (setq cell (cons (list (cons 'WM_NAME "")) nil)))

    ;; move boolean properties to head of list for best effect
    (setq bool-props (filter (lambda (p)
			       (eq (nth 1 p) 'boolean)) properties))
    (setq properties (filter (lambda (p)
			       (not (eq (nth 1 p) 'boolean))) properties))

    (setq table (gtk-table-new (1+ (quotient (length bool-props) 3)) 3 nil))
    (setq table-2 (gtk-table-new (length properties) 2 nil))

    (gtk-window-set-title window (_ "Match window properties"))
    (gtk-widget-set-name window (_ "Match window properties"))
    (gtk-window-set-wmclass window "sawmill-ui" "match-window-properties")

    (gtk-box-set-spacing hbox-2 ui-box-spacing)
    (gtk-container-border-width hbox-2 ui-box-border)
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)
    (gtk-table-set-col-spacings table ui-box-spacing)
    (gtk-table-set-row-spacings table ui-box-spacing)
    (gtk-container-border-width table ui-box-border)
    (gtk-table-set-col-spacings table-2 ui-box-spacing)
    (gtk-table-set-row-spacings table-2 ui-box-spacing)
    (gtk-container-border-width table-2 ui-box-border)
    (gtk-table-set-col-spacings table-1 ui-box-spacing)
    (gtk-table-set-row-spacings table-1 ui-box-spacing)
    (gtk-container-border-width table-1 ui-box-border)
    (gtk-button-box-set-layout hbox-2 'end)

    (let
	((i 0))
      (while (< i ui-match-window-max-matchers)
	(let
	    ((combo (gtk-combo-new))
	     (entry (gtk-entry-new))
	     (button (gtk-button-new-with-label (_ "Grab..."))))
	  (gtk-combo-set-popdown-strings
	   combo (cons "" (mapcar cdr x-properties)))
	  (gtk-table-attach-defaults table-1 combo 0 1 i (1+ i))
	  (gtk-table-attach-defaults table-1 entry 1 2 i (1+ i))
	  (gtk-table-attach-defaults table-1 button 2 3 i (1+ i))
	  (setq match-widget-alist (cons (cons combo entry)
					 match-widget-alist))
	  (gtk-signal-connect button "clicked"
			      (lambda ()
				(let*
				    ((string (gtk-entry-get-text
					      (gtk-combo-entry combo)))
				     prop)
				  (when string
				    (setq prop (sawmill-eval
						`(match-window-grab-x-property
						  ,string) t))
				    (unless (stringp prop)
				      (setq prop ""))
				    (gtk-entry-set-text entry prop)))))
	  (setq i (1+ i))))
      (setq match-widget-alist (nreverse match-widget-alist))
      (setq i 0)
      (mapc (lambda (matcher)
	      (gtk-entry-set-text (gtk-combo-entry
				   (car (nth i match-widget-alist)))
				  (or (cdr (assq (car matcher) x-properties))
				      (symbol-name (car matcher))))
	      (gtk-entry-set-text (cdr (nth i match-widget-alist))
				  (cdr matcher))
	      (setq i (1+ i)))
	    (car cell)))

    (gtk-container-add window vbox)
    (gtk-box-pack-start vbox frame-1)
    (gtk-container-add vbox frame)
    (gtk-box-pack-end vbox hbox-2)
    (gtk-container-add frame vbox-2)
    (gtk-box-pack-start vbox-2 table)
    (gtk-box-pack-start vbox-2 table-2)
    (gtk-container-add frame-1 table-1)
    (gtk-container-add hbox-2 ok)
    (gtk-container-add hbox-2 cancel)

    (let
	((i 0))
      (mapc (lambda (prop)
	      (let
		  ((current (cdr (assq (car prop) (cdr cell))))
		   (widget (gtk-check-button-new-with-label
			    (symbol-name (car prop))))
		   (row (quotient i 3))
		   (col (mod i 3)))
		(when current
		  (gtk-toggle-button-set-state widget t))
		(gtk-table-attach-defaults
		 table widget col (1+ col) row (1+ row))
		(setq i (1+ i))
		(setq prop-widget-alist (cons (cons (car prop) widget)
					      prop-widget-alist))))
	    bool-props))

    (let
	((i 0))
      (mapc (lambda (prop)
	      (let
		  ((current (cdr (assq (car prop) (cdr cell)))))
		(cond
		 ((eq (nth 1 prop) 'number)
		  (let*
		      ((entry (gtk-entry-new))
		       (label (gtk-label-new (symbol-name (car prop)))))
		    (if current
			(gtk-entry-set-text entry (format nil "%d" current))
		      (gtk-entry-set-text entry ""))
		    (attach-label table-2 label 0 1 i (1+ i))
		    (gtk-table-attach-defaults table-2 entry 1 2 i (1+ i))
		    (setq prop-widget-alist (cons (cons (car prop) entry)
						  prop-widget-alist))))
		 ((eq (nth 1 prop) 'symbol)
		  (let
		      ((combo (gtk-combo-new))
		       (label (gtk-label-new (symbol-name (car prop)))))
		    (attach-label table-2 label 0 1 i (1+ i))
		    (gtk-table-attach-defaults table-2 combo 1 2 i (1+ i))
		    (gtk-combo-set-popdown-strings
		     combo (cons "" (mapcar symbol-name (nth 2 prop))))
		    (gtk-entry-set-text (gtk-combo-entry combo)
					(if current
					    (symbol-name current) ""))
		    (setq prop-widget-alist (cons (cons (car prop) combo)
						  prop-widget-alist))))
		 ((eq (nth 1 prop) 'pair)
		  (let
		      ((entry-1 (gtk-entry-new))
		       (entry-2 (gtk-entry-new))
		       (label (gtk-label-new (symbol-name (car prop))))
		       (hbox (gtk-hbox-new t ui-box-spacing)))
		    (gtk-widget-set-usize entry-1 10 -2)
		    (gtk-widget-set-usize entry-2 10 -2)
		    (gtk-box-pack-start hbox entry-1 t t)
		    (gtk-box-pack-start hbox entry-2 t t)
		    (when current
		      (gtk-entry-set-text
		       entry-1 (format nil "%d" (car current)))
		      (gtk-entry-set-text
		       entry-2 (format nil "%d" (cdr current))))
		    (attach-label table-2 label 0 1 i (1+ i))
		    (gtk-table-attach-defaults table-2 hbox 1 2 i (1+ i))
		    (setq prop-widget-alist (cons (cons (car prop)
							(cons entry-1 entry-2))
						  prop-widget-alist)))))
		(setq i (1+ i))))
	    properties))
    (setq prop-widget-alist (nreverse prop-widget-alist))

    (gtk-signal-connect ok "clicked" (lambda ()
				       (let
					   ((item (make-cell)))
					 (when (and callback item)
					   (callback item)))
				       (gtk-widget-destroy window)))
    (gtk-signal-connect cancel "clicked" (lambda ()
					   (gtk-widget-destroy window)))

    (gtk-widget-show-all window)
    (gtk-grab-add window)
    window))

(defun build-match-window (spec)
  (let*
      ((vbox (gtk-vbox-new nil 0))
       (hbox (gtk-hbox-new nil 0))
       (scroller (gtk-scrolled-window-new))
       (clist (gtk-clist-new-with-titles
	       (vector (_ "Matchers") (_ "Actions"))))
       (add-b (gtk-button-new-with-label (_ "Add...")))
       (delete-b (gtk-button-new-with-label (_ "Delete")))
       (edit-b (gtk-button-new-with-label (_ "Edit...")))

       (x-properties (get-key spec ':x-properties))
       (properties (get-key spec ':properties))

       (selection nil)
       (select-row-callback (lambda (w row col)
			      (setq selection row)))

       (format-cell
	(lambda (cell)
	  (let
	      ((matchers nil)
	       (actions nil))
	    (mapc (lambda (match)
		    (when matchers
		      (setq matchers (cons ", " matchers)))
		    (let
			((string (format nil "%s=%s"
					 (or (cdr (assq (car match)
							x-properties))
					     (car match)) (cdr match))))
		      (setq matchers (cons string matchers))))
		  (car cell))
	    (mapc (lambda (action)
		    (when actions
		      (setq actions (cons ", " actions)))
		    (let
			((string (format nil "%s=%s"
					 (car action) (cdr action))))
		      (when (string-match "=t$" string)
			(setq string (substring string 0 (match-start))))
		      (setq actions (cons string actions))))
		  (cdr cell))
	    (vector (apply concat (nreverse matchers))
		    (apply concat (nreverse actions))))))

       (add-callback
	(lambda ()
	  (match-window:edit
	   spec nil
	   (lambda (cell)
	     (let
		 ((value (get-key spec ':value)))
	       (setq value (append value (list cell)))
	       (gtk-clist-append clist (format-cell cell))
	       (ui-set spec (get-key spec ':variable) value))))))

       (delete-callback
	(lambda ()
	  (when selection
	    (let*
		((value (get-key spec ':value))
		 (item (nth selection value)))
	      (setq value (delq item (copy-sequence value)))
	      (gtk-clist-remove clist selection)
	      (when (>= selection (length value))
		(setq selection (1- (length value)))
		(when (< selection 0)
		  (setq selection nil)))
	      (ui-set spec (get-key spec ':variable) value)))))

       (edit-callback
	(lambda ()
	  (when selection
	    (let
		((old-item (nth selection (get-key spec ':value))))
	      (match-window:edit spec old-item
	       (lambda (new-item)
		 (let*
		     ((value (copy-sequence (get-key spec ':value)))
		      (ptr value)
		      (vec (format-cell new-item)))
		   (while (and ptr (not (eq (car ptr) old-item)))
		     (setq ptr (cdr ptr)))
		   (rplaca ptr new-item)
		   (ui-set spec (get-key spec ':variable) value)
		   (gtk-clist-set-text clist selection 0 (aref vec 0))
		   (gtk-clist-set-text clist selection 1 (aref vec 1))))))))))

    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)
    (gtk-box-set-spacing hbox ui-box-spacing)
    (gtk-container-border-width hbox ui-box-border)

    (gtk-clist-set-column-auto-resize clist 0 t)
    (gtk-clist-set-column-auto-resize clist 1 t)
    (gtk-clist-set-selection-mode clist 'browse)

    (gtk-scrolled-window-set-policy scroller 'automatic 'automatic)
    (gtk-widget-set-usize scroller 400 200)

    (gtk-box-pack-start vbox scroller t t)
    (gtk-box-pack-end vbox hbox)
    (gtk-container-add hbox add-b)
    (gtk-container-add hbox edit-b)
    (gtk-container-add hbox delete-b)
    (gtk-container-add scroller clist)

    (mapc (lambda (item)
	    (gtk-clist-append clist (format-cell item))
	    (unless selection
	      (setq selection 0)))
	  (get-key spec ':value))

    (when selection
      (gtk-clist-select-row clist selection 0))

    (gtk-signal-connect add-b "clicked" add-callback)
    (gtk-signal-connect delete-b "clicked" delete-callback)
    (gtk-signal-connect edit-b "clicked" edit-callback)
    (gtk-signal-connect clist "select_row" select-row-callback)
    (gtk-signal-connect clist "button_press_event"
			(lambda (w ev)
			  (when (eq (gdk-event-type ev) '2button-press)
			    (edit-callback))))
    vbox))

(put 'match-window 'builder build-match-window)
    

;; building the frame for the element tree

(defun show-ui (spec)
  (let
      ((ui-apply-changed-hook nil)
       (ui-values-to-apply nil)
       (ui-original-values nil)
       (ui-changed-variables nil)
       (ui-window (if ui-socket-id
		      (gtk-plug-new ui-socket-id)
		    (gtk-window-new 'toplevel)))
       (ui-root (build-ui spec))
       (vbox (gtk-vbox-new nil 0))
       (hbox (gtk-hbutton-box-new))
       ui-ok-widget ui-apply-widget ui-revert-widget refresh cancel)
    (gtk-window-set-policy ui-window nil t nil)
    (when (and (not ui-socket-id) (eq ui-group t))
      (gtk-widget-set-usize ui-window 600 500))
    (unless ui-socket-id
      (setq ui-ok-widget (gtk-button-new-with-label (_ "OK")))
      (setq ui-apply-widget (gtk-button-new-with-label (_ "Try")))
      (setq ui-revert-widget (gtk-button-new-with-label (_ "Revert")))
      (setq refresh (and ui-enable-refresh
			 (gtk-button-new-with-label (_ "Refresh"))))
      (setq cancel (gtk-button-new-with-label (_ "Cancel")))
      (gtk-window-set-title ui-window (_ "Sawfish configurator"))
      (gtk-widget-set-name ui-window (_ "Sawfish configurator"))
      (gtk-window-set-wmclass ui-window "sawmill-ui" "main"))
    (gtk-signal-connect ui-window "delete_event" ui-quit)
    (gtk-container-add ui-window vbox)
    (gtk-box-set-spacing vbox ui-box-spacing)
    (gtk-container-border-width vbox ui-box-border)
    (gtk-box-set-spacing hbox ui-box-spacing)
    (gtk-container-border-width hbox ui-box-border)
    (gtk-button-box-set-layout hbox 'end)
    (unless ui-socket-id
      (gtk-box-pack-end vbox hbox)
      (gtk-signal-connect ui-ok-widget "clicked" ui-ok)
      (gtk-signal-connect ui-apply-widget "clicked" ui-apply)
      (gtk-signal-connect cancel "clicked" ui-cancel)
      (gtk-signal-connect ui-revert-widget "clicked" (lambda () (ui-revert)))
      (when ui-enable-refresh
	(gtk-signal-connect refresh "clicked" ui-refresh))
      (gtk-container-add hbox ui-apply-widget)
      (gtk-container-add hbox ui-revert-widget)
      (gtk-container-add hbox ui-ok-widget)
      (gtk-container-add hbox cancel)
      (when ui-enable-refresh
	(gtk-container-add hbox refresh)))
    (gtk-container-add vbox ui-root)
    (ui-set-button-states)
    (gtk-widget-show-all ui-window)
    (when ui-hack-activate-tree
      (gtk-tree-select-item ui-hack-activate-tree 0))
    (while ui-window
      (gtk-main))))

(defun ui-set-button-states ()
  (unless ui-socket-id
    (when ui-apply-widget
      (gtk-widget-set-sensitive ui-apply-widget (or ui-values-to-apply
						    ui-apply-changed-hook)))
    (when ui-revert-widget
      (gtk-widget-set-sensitive ui-revert-widget ui-changed-variables))
    (when ui-ok-widget
      (gtk-widget-set-sensitive ui-ok-widget (or ui-values-to-apply
						 ui-apply-changed-hook
						 ui-changed-variables)))))

(defun ui-set-button-label (button text)
  (mapc (lambda (w)
	  (when (gtk-label-p w)
	    (gtk-label-set w text))) (gtk-container-children button)))


;; acting on settings

(defun ui-add-apply-hook (fun)
  (when (and ui-socket-id
	     (null ui-values-to-apply)
	     (null ui-apply-changed-hook))
    (ui-capplet-state-changed))
  (add-hook 'ui-apply-changed-hook fun))

(defun ui-set (spec symbol value)
  (when (and ui-socket-id
	     (null ui-values-to-apply)
	     (null ui-apply-changed-hook))
    (ui-capplet-state-changed))
  (ui-set-original spec symbol)
  (setq ui-values-to-apply (delete-if (lambda (x)
					(eq (car x) spec))
				      ui-values-to-apply))
  (setq ui-values-to-apply (cons (cons spec value) ui-values-to-apply))
  (set-key spec ':value value)
  (ui-set-button-states))

(defun ui-set-original (spec symbol)
  (unless (assq symbol ui-original-values)
    (setq ui-original-values (cons (cons symbol (get-key spec ':value))
				   ui-original-values))))

(defun ui-quit ()
  (sawmill-eval-async '(customize-write-user-file))
  (gtk-widget-destroy ui-window)
  (throw 'quit 0))

(defun ui-set-variables ()
  (let
      ((commands
	(mapcar (lambda (cell)
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
    (ui-set-button-states)))

(defun ui-apply ()
  (call-hook 'ui-apply-changed-hook)
  (ui-set-variables)
  (setq ui-apply-changed-hook nil))

(defun ui-ok ()
  (ui-apply)
  (ui-quit))

(defun ui-revert (&optional dont-refresh)
  (let
      ((commands
	(mapcar (lambda (symbol)
		  `(customize-set
		    ',symbol ',(cdr (assq symbol ui-original-values))))
		ui-changed-variables)))
    (ui-command (cons 'progn commands))
    (setq ui-values-to-apply nil)
    (setq ui-changed-variables nil)
    (unless dont-refresh
      (ui-refresh))))

(defun ui-cancel ()
  (ui-revert t)
  (ui-quit))

(defun ui-command (form)
  (if (null ui-debug)
      (sawmill-eval-async form)
    (format standard-error "ui-command: %S\n" form)))

(defun ui-get-spec ()
  (let ((response (sawmill-eval `(progn
				   (require 'customize)
				   (customize-ui-spec ',ui-group)))))
    (if (string-match "^error-->" response)
	(progn
	  (if ui-socket-id
	      (ui-capplet-no-group)
	    (format standard-error "No such group: %s\n" ui-group))
	  (throw 'quit 1))
      (read-from-string response))))

(defun ui-refresh ()
  (let
      ((vbox (gtk-widget-parent ui-root))
       spec)
    (gtk-container-remove vbox ui-root)
    (gtk-widget-destroy ui-root)
    (setq spec (ui-get-spec))
    (setq ui-changed-variables nil)
    (setq ui-values-to-apply nil)
    (setq ui-original-values nil)
    (setq ui-root (build-ui spec))
    (gtk-box-pack-start vbox ui-root)
    (gtk-widget-show-all ui-window)
    (ui-set-button-states)))

;; called when there's input available on stdin
(defun ui-capplet-input ()
  (let
      ((tem (read-line standard-input)))
    (condition-case nil
	(progn
	  (cond ((string-match "apply" tem)
		 (ui-apply))
		((string-match "revert" tem)
		 (ui-revert))
		((string-match "ok" tem)
		 (ui-ok))
		((string-match "cancel" tem)
		 (ui-revert t)
		 (gtk-widget-destroy ui-window)
		 (setq ui-window nil)))
	  (write standard-output ?\001)
	  (flush-file standard-output)
	  (when (null ui-window)
	    (gtk-main-quit)))
      (end-of-stream))))

(defun ui-capplet-state-changed ()
  (write standard-output ?c)
  (flush-file standard-output))

(defun ui-capplet-no-group ()
  (write standard-output ?g)
  (flush-file standard-output))


;; utilities

(defun flatten-doc-string (string)
  (let
      ((point 0))
    (while (string-match "[^\n]\n[^\n]" string point)
      (aset string (1+ (match-start)) ? )
      (setq point (match-end)))
    string))

;; redefine this to get documentation from the wm process
(defun documentation (symbol &optional is-var)
  (sawmill-eval
   `(let
	((doc (documentation ',symbol ',is-var)))
      (and doc (_ doc))) t))

;; for a file called FILE, look for one with a .LC extension LC is
;; the language code for the current language
(defun i18n-filename (file)
  (cond ((and ui-lang (file-exists-p (concat file ?. ui-lang)))
	 (concat file ?. ui-lang))
	((and ui-lang-base (file-exists-p (concat file ?. ui-lang-base)))
	 (concat file ?. ui-lang-base))
	(t
	 file)))

(defun beautify-symbol-name (symbol)
  (cond ((stringp symbol) symbol)
	((not (symbolp symbol)) (format "%s" symbol))
	(t
	 (if (not ui-beautify-keymaps)
	     (symbol-name symbol)
	   (let
	       ((name (copy-sequence (symbol-name symbol))))
	     (while (string-match "[-:]" name)
	       (setq name (concat (substring name 0 (match-start))
				  ?  (substring name (match-end)))))
	     (aset name 0 (char-upcase (aref name 0)))
	     name)))))


;; color previews

(defun ui-set-preview-color (preview color)
  (let
      ((buf (make-string (* ui-color-preview-width 3)))
       i)
    (setq i 0)
    (while (< i ui-color-preview-width)
      (aset buf (* i 3) (quotient (gdk-color-red color) 256))
      (aset buf (1+ (* i 3)) (quotient (gdk-color-green color) 256))
      (aset buf (+ 2 (* i 3)) (quotient (gdk-color-blue color) 256))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i ui-color-preview-height)
      (gtk-preview-draw-row preview buf 0 i ui-color-preview-width)
      (setq i (1+ i)))))

(defun ui-button-new-with-color (color-name)
  (let
      ((button (gtk-button-new))
       (preview (gtk-preview-new 'color))
       (color (and color-name (gdk-color-parse-interp color-name)))
       i)
    (gtk-preview-size preview ui-color-preview-width ui-color-preview-height)
    (when color
      (ui-set-preview-color preview color))
    (gtk-container-border-width button ui-box-border)
    (gtk-container-add button preview)
    button))

(defun ui-set-button-color (button color-name)
  (let
      ((color (and color-name (gdk-color-parse-interp color-name))))
    (when color
      (mapc (lambda (w)
	      (when (gtk-preview-p w)
		(ui-set-preview-color w color)
		(gtk-widget-draw-interp w)))
	    (gtk-container-children button)))))


;; entry point

(when (boundp 'gtk-set-locale)
  (gtk-set-locale))

(let
    (tem)
  (when (setq tem (get-command-line-option "--group" t))
    (setq ui-group (read-from-string tem)))
  (when (get-command-line-option "--notebook")
    (setq ui-pages-style 'notebook))
  (when (get-command-line-option "--flatten")
    (put 'tree 'builder build-tree-as-list))
  (when (setq tem (get-command-line-option "--socket-id" t))
    (setq ui-socket-id (read-from-string tem))))

(when ui-socket-id
  (set-input-handler standard-input ui-capplet-input))

;; initialise i18n
(let
    ((locale-dir (sawmill-eval
		  '(and (featurep 'gettext)
			(bindtextdomain "sawfish")) t)))
  (when (and (not (get-command-line-option "--disable-nls")) locale-dir)
    (require 'gettext)
    (bindtextdomain "sawfish" locale-dir)
    (textdomain "sawfish")))

(show-ui (ui-get-spec))

;; Local Variables:
;; major-mode: lisp-mode
;; End:
