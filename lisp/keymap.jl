;; keymap.jl -- some keymap utilities, mostly copied from jade
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


;; Map a function over all key bindings in a keymap

(defvar map-keymap-recursively t
  "When nil, the map-keymap function will only map over the first level of
keymaps, i.e. all prefix keys are ignored.")

(defvar km-prefix-string nil)
(defvar km-keymap-list nil)

;;;###autoload
(defun map-keymap (function keymap)
  "Map FUNCTION over all key bindings under the keymap or list of
keymaps KEYMAP. FUNCTION is called as (FUNCTION KEY), where KEY is a
cons cell (COMMAND . EVENT)."
  (when (keymapp keymap)
    (setq keymap (list keymap)))
  (let
      ((km-keymap-list keymap)
       (done-list nil))
    (while km-keymap-list
      (let
	  ((keymap (car km-keymap-list)))
	(setq km-keymap-list (cdr km-keymap-list))
	(when (and (not (keymapp keymap)) (consp keymap))
	  (setq keymap (car keymap)))
	(unless (memq keymap done-list)
	  (setq done-list (cons keymap done-list))
	  (when (symbolp keymap)
	    (setq keymap (symbol-value keymap)))
	  (when (keymapp keymap)
	    (mapc #'(lambda (k)
		      (funcall function k)) (cdr keymap))))))))


;; Substitute one command for another in a keymap

;;;###autoload
(defun substitute-key-definition (olddef newdef &optional keymap)
  "Substitute all occurrences of the command OLDDEF for the command NEWDEF
in the keybindings under the keymap or list of keymaps KEYMAP. When KEYMAP
is nil, the currently active keymaps used, i.e. all key bindings currently
in effect."
  (map-keymap #'(lambda (k)
		  (when (eq (car k) olddef)
		    (rplaca k newdef))) keymap))


;; Adding bindings to a feature that may not yet be loaded

;;;###autoload
(defmacro lazy-bind-keys (feature keymap &rest bindings)
  "Install the list of BINDINGS in KEYMAP, assuming that KEYMAP is available
once FEATURE has been provided. If FEATURE has not yet been loaded, arrange
for the bindings to be installed if and when it is."
  `(if (featurep ',feature)
       (bind-keys ,keymap ,@bindings)
     (eval-after-load ,(symbol-name feature) '(bind-keys ,keymap ,@bindings))))


;; Search for a named command in the current keymap configuration

(defvar km-where-is-results nil)

;;;###autoload
(defun where-is (command keymap)
  (let
      ((km-where-is-results nil))
    (map-keymap #'(lambda (k)
		    (when (eq (car k) command)
		      (setq km-where-is-results
			    (cons (event-name (cdr k)) km-where-is-results))))
		keymap)
    km-where-is-results))
