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

(require 'gtk)


;; menus

(defvar menu-selected nil)

(defun create-menu (spec &optional bar)
  (let
      ((menu (if bar (gtk-menu-bar-new) (gtk-menu-new))))
    (mapc (lambda (cell)
	    (let
		(label item)
	      (when (and cell (symbolp (car cell)))
		(setq cell (symbol-value (car cell))))
	      (if (null cell)
		  (setq item (gtk-menu-item-new))
		(setq label (car cell))
		(setq cell (cdr cell))
		(if (and (consp (car cell)) (stringp (car (car cell))))
		    (let
			((sub (create-menu cell)))
		      (setq item (gtk-menu-item-new-with-label label))
		      (gtk-menu-item-set-submenu item sub))
		  (setq item (gtk-menu-item-new-with-label label))
		  (gtk-signal-connect
		   item "activate" (lambda ()
				     (setq menu-selected (car cell))))))
	      (when item
		(gtk-widget-lock-accelerators item)
		((if bar gtk-menu-bar-append gtk-menu-append) menu item)
		(gtk-widget-show item))))
	  spec)
    menu))

(defun popup-menu (spec &optional timestamp position)
  (catch 'menu-done
    (let
	((menu  (create-menu spec)))
      (gtk-signal-connect menu "deactivate" gtk-main-quit)
      (setq menu-selected nil)
      (gtk-menu-popup-interp menu nil nil 0 (or timestamp 0) position)
      (gtk-main)
      menu-selected)))


;; entry point, loop reading command forms, sending back results

(condition-case nil
    (while t
      (let
	  ((input (read standard-input)))
	(format standard-output "%S\n"
		(apply (symbol-value (car input)) (cdr input)))
	(when (filep standard-output)
	  (flush-file standard-output))))
  (end-of-stream))


;; Local Variables:
;; major-mode: lisp-mode
;; End:
