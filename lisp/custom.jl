;; custom.jl -- Emacs-like ``customizing'' (but more simple)
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

(provide 'custom)

(defvar custom-groups nil)

(defvar custom-required nil
  "List of features to load before running customize.")

;; (defcustom VARIABLE VALUE DOC &rest CUSTOM-KEYS)

;; where CUSTOM-KEYS is a plist containing any of the following:

;;	:group GROUP
;;	:require FEATURE
;;	:type TYPE
;;	:allow-nil t

;; TYPE may be `boolean', `number', `string', `(set SYMBOLS..)',
;; `file-name', `program-name'

(defmacro defcustom (symbol value doc &rest keys)
  (list 'defvar
	symbol
	(list 'custom-declare-variable
	      (list 'quote symbol) value (list 'quote keys))
	doc))

(defmacro defgroup (symbol doc)
  (list 'custom-declare-group (list 'quote symbol) doc))

(defun custom-declare-variable (symbol value keys)
  (let
      (tem)
    (while keys
      (setq tem (car keys))
      (setq keys (cdr keys))
      (cond ((eq tem ':group)
	     (put symbol 'custom-group (car keys))
	     (custom-add-to-group symbol (car keys)))
	    ((eq tem ':require)
	     (put symbol 'custom-require (car keys)))
	    ((eq tem ':type)
	     (put symbol 'custom-type (car keys)))
	    ((eq tem ':allow-nil)
	     (put symbol 'custom-allow-nil (car keys))))
      (setq keys (cdr keys)))
    value))

(defun custom-declare-group (group &optional doc)
  (unless (assq group custom-groups)
    (setq custom-groups (nconc custom-groups (list (list group)))))
  (when doc
    (put group 'custom-group-doc doc)))

(defun custom-add-to-group (symbol group)
  (let
      ((group-list (assq group custom-groups)))
    (unless group-list
      (custom-declare-group group)
      (setq group-list (assq group custom-groups)))
    (rplacd group-list (nconc (cdr group-list) (list symbol)))))

(defun custom-set-variable (symbol value &optional require)
  (when require
    (require require))
  (set symbol value))


;; default groups

(defgroup focus "Focus")
(defgroup move "Move/Resize")
(defgroup menus "Menus")
(defgroup workspace "Workspaces")
(defgroup placement "Placement")
(defgroup misc "Miscellaneous")
(defgroup customize "Customization")



(defcustom custom-user-file "~/.sawmill-custom"
  "File used to store user's configuration settings."
  :group customize
  :type file-name)

(defun custom-load-user-file ()
  (when (file-exists-p custom-user-file)
    (load custom-user-file t t t)))
