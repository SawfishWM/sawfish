#!/bin/sh
exec rep "$0" "$@"
!#

;; sawmill-menu -- subprocess to handle menus
;; $Id$

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

(require 'gui.gtk)
(require 'rep.data.tables)


;; menus

(define menu-selected nil)

;; map radio-group ids to the last widget
(define group-table (make-fluid))

(define (make-group-table) (make-table symbol-hash eq))
(define (group-id-set id w) (table-set (fluid group-table) id w))
(define (group-id-ref id) (table-ref (fluid group-table) id))

(define (create-menu spec #!optional bar)
  (let* ((menu (if bar (gtk-menu-bar-new) (gtk-menu-new)))
	 (accels (gtk-menu-ensure-uline-accel-group menu)))
    (mapc (lambda (cell)
	    (let (label item)
	      (when (and cell (symbolp (car cell)))
		(setq cell (symbol-value (car cell))))
	      (if (null cell)
		  (setq item (gtk-menu-item-new))
		(setq label (car cell))
		(setq cell (cdr cell))
		(if (and (consp (car cell)) (stringp (car (car cell))))
		    (let ((sub (create-menu cell)))
		      (setq item (gtk-menu-item-new-with-label label))
		      (gtk-menu-item-set-submenu item sub))
		  (let ((options (cdr cell)))
		    (let* ((check (assq 'check options))
			   (group (cdr (assq 'group options)))
			   (insensitive (cdr (assq 'insensitive options)))
			   (last-widget (and group (group-id-ref group))))
		      (cond (group
			     (setq item (gtk-radio-menu-item-new-with-label-from-widget last-widget label))
			     (group-id-set group item))
			    (check
			     (setq item (gtk-check-menu-item-new-with-label label))
			     (gtk-check-menu-item-set-show-toggle item t))
			    (t (setq item (gtk-menu-item-new-with-label label))))
		      (when check
			(gtk-check-menu-item-set-state item (cdr check)))
		      (when insensitive
			(gtk-widget-set-sensitive item nil))))
		  (gtk-signal-connect
		   item "activate" (lambda ()
				     (setq menu-selected (car cell)))))
		(let ((hkey (gtk-label-parse-uline
			     (car (gtk-container-children item)) label)))
		  (gtk-widget-add-accelerator
		   item "activate_item" accels hkey 0 0)))
	      (when item
		(gtk-widget-lock-accelerators item)
		((if bar gtk-menu-bar-append gtk-menu-append) menu item)
		(gtk-widget-show item))))
	  spec)
    menu))

(define (popup-menu spec #!optional timestamp position)
  (let ((menu (let-fluids ((group-table (make-group-table)))
		(create-menu spec))))
    (gtk-signal-connect menu "deactivate" gtk-main-quit)
    (setq menu-selected nil)
    (gtk-menu-popup-interp menu nil nil 0 (or timestamp 0) position)
    (gtk-main)
    menu-selected))


;; entry point, loop reading command forms, sending back results

(gtk-set-locale)

(condition-case nil
    (while t
      (let ((input (read standard-input)))
	(format standard-output "%S\n"
		(apply (symbol-value (car input)) (cdr input)))
	(when (filep standard-output)
	  (flush-file standard-output))))
  (end-of-stream))


;; Local Variables:
;; major-mode: lisp-mode
;; End:
