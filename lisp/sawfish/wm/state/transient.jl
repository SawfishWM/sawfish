;; transient.jl -- support transient windows
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

(provide 'transient)

(defcustom ignored-windows-re nil
  "Regular expression matching windows to ignore."
  :group misc
  :type string
  :allow-nil t)

(defcustom sticky-windows-re nil
  "Regular expression matching windows to make sticky by default."
  :group misc
  :type string
  :allow-nil t)

(defcustom transients-get-focus t
  "Mapping a transient window whose parent is currently focused transfers
the input focus to the transient window."
  :group focus
  :type boolean)

(defcustom focus-windows-when-mapped nil
  "Mapping a window gives it the focus."
  :type boolean
  :group focus)

(defvar ignored-window-names nil
  "A list of regular expressions matching windows that don't get a frame.")

(defvar sticky-window-names nil
  "A list of regular expressions matching window names that exist across
workspaces.")


;; utility functions

(defun window-type (w)
  (if (window-transient-p w)
      (if (window-shaped-p w)
	  'shaped-transient
	'transient)
    (if (window-shaped-p w)
	'shaped
      'default)))


;; hooks

;; called from the add-window-hook
(defun transient-add-window (w)
  (when (catch 'foo
	  (when (and ignored-windows-re
		     (string-match ignored-windows-re (window-name w)))
	    (throw 'foo t))
	  (mapc #'(lambda (r)
		    (when (string-match r (window-name w))
		      (throw 'foo t))) ignored-window-names)
	  nil)
    (window-put w 'ignored t)
    (set-window-frame w 'nil-frame))
  (when (catch 'foo
	  (when (and sticky-windows-re
		     (string-match sticky-windows-re (window-name w)))
	    (throw 'foo t))
	  (mapc #'(lambda (r)
		    (when (string-match r (window-name w))
		      (throw 'foo t))) sticky-window-names)
	  nil)
    (window-put w 'sticky t)))

(defun transient-map-window (w)
  (catch 'out
    (when (window-transient-p w)
      (let
	  ((parent (get-window-by-id (window-transient-p w))))
	(when (and parent transients-get-focus (eq (input-focus) parent))
	  (set-input-focus w)
	  (throw 'out t))))
    (when focus-windows-when-mapped
      (set-input-focus w))))

;; If a transient window gets unmapped that currently has the input
;; focus, pass it (the focus) to its parent
;; XXX but this only works if the pointer isn't over a different window
(defun transient-unmap-window (w)
  (when (eq (input-focus) w)
    (let
	((parent (and (window-transient-p w)
		      (get-window-by-id (window-transient-p w)))))
      (when parent
	(set-input-focus parent)))))

(add-hook 'add-window-hook 'transient-add-window t)
(add-hook 'map-notify-hook 'transient-map-window t)
(add-hook 'unmap-notify-hook 'transient-unmap-window t)
