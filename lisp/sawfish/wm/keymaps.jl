;; keymaps.jl -- the default keymaps
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

(provide 'keymaps)

(put 'keymap 'custom-set 'custom-set-keymap)
(put 'keymap 'custom-get 'custom-get-keymap)
(put 'keymap 'custom-widget 'custom-keymap-widget)

(defgroup bindings "Bindings"
  :widget custom-keymap-group-widget)

(defcustom global-keymap (make-keymap)
  "Keymap containing bindings active anywhere."
  :group bindings
  :type keymap
  :before-set (lambda ()
		(ungrab-keymap global-keymap))
  :after-set (lambda ()
	       (grab-keymap global-keymap)))

(defcustom window-keymap (make-keymap)
  "Keymap containing bindings active when a client window is focused."
  :group bindings
  :type keymap
  :before-set (lambda ()
		(ungrab-keymap window-keymap))
  :after-set (lambda ()
	       (grab-keymap window-keymap)))

(defcustom root-window-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the root window
(or when no window is focused)."
  :group bindings
  :type keymap)

(defcustom title-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the title of
a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :type keymap)

(defcustom border-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the border of
a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :type keymap)

(defcustom close-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the close button
of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :type keymap)

(defcustom iconify-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the iconify
button of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :type keymap)

(defcustom maximize-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the maximize
button of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :type keymap)

(defcustom menu-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the menu button
of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :type keymap)


;; Arrange for window-keymap to be set in each window

(defun keymap-add-window (w)
  (unless (window-get w 'keymap)
    (window-put w 'keymap window-keymap)))

(add-hook 'add-window-hook 'keymap-add-window)


;; some bindings

(unless batch-mode
  (bind-keys title-keymap
    "Button3-Off" 'raise-lower-window
    "Button1-Move" 'move-window-interactively
    "Button2-Move" 'resize-window-interactively
    "Button1-Click2" 'toggle-window-shaded)

  (bind-keys border-keymap
    "Button3-Off" 'raise-lower-window
    "Button2-Move" 'move-window-interactively
    "Button1-Move" 'resize-window-interactively)

  (bind-keys window-keymap
    "C-M-Up" 'raise-window
    "C-M-Down" 'lower-window
    "C-M-Left" 'send-to-previous-workspace
    "C-M-Right" 'send-to-next-workspace
    "M-Button1-Click1" 'move-window-interactively
    "M-Button2-Click1" 'popup-window-menu
    "M-Button3-Click1" 'raise-lower-window)

  (bind-keys global-keymap
    "C-Left" 'previous-workspace
    "C-Right" 'next-workspace
    "M-Tab" 'cycle-windows)

  (bind-keys root-window-keymap
    "Button2-Click1" 'popup-root-menu
    "Button2-Off" 'nop)			;so it doesn't get proxyed

  (bind-keys close-button-keymap
    "Button1-Off" 'delete-window
    "Button3-Click1" 'popup-window-menu)

  (bind-keys iconify-button-keymap
    "Button1-Off" 'iconify-window
    "Button3-Click1" 'popup-window-menu)

  (bind-keys menu-button-keymap
    "Button1-Click1" 'popup-window-menu
    "Button3-Off" 'delete-window)

  (bind-keys maximize-button-keymap
    "Button1-Off" 'maximize-window-toggle
    "Button2-Off" 'maximize-window-vertically-toggle
    "Button3-Off" 'maximize-window-horizontally-toggle))


;; customize support

(defun custom-get-keymap (symbol)
  (cons 'keymap (mapcar #'(lambda (cell)
			    (cons (car cell) (event-name (cdr cell))))
			(cdr (symbol-value symbol)))))

(defun custom-set-keymap (symbol value &rest args)
  (when (eq (car value) 'keymap)
    (let
	((old-value (and (boundp symbol) (symbol-value symbol)))
	 (new-tail (delq nil (mapcar #'(lambda (cell)
					 (let
					     ((ev (lookup-event (cdr cell))))
					   (and ev (cons (car cell) ev))))
				     (cdr value)))))
      (if (and old-value (eq (car old-value) 'keymap))
	  ;; hijack the old keymap to preserve eq-ness
	  (progn
	    (rplacd old-value new-tail)
	    (setq value old-value))
	(setq value (cons 'keymap new-tail)))
      (apply 'custom-set-variable symbol value args))))

(defun custom-keymap-widget (symbol value doc)
  `(keymap :variable ,symbol
	   :value ,value
	   :doc ,doc))

(defun custom-keymap-group-widget (group spec)
  (let
      ((names (mapcar 'symbol-name (cdr (assq 'bindings custom-groups)))))
    `(keymap-shell ,(mapcar #'(lambda (elt)
				(list (car (prog1 names
					     (setq names (cdr names))))
				      elt)) spec)
		   :commands ,(sort (apropos "" 'commandp)
				    #'(lambda (x y)
					(< (symbol-name x)
					   (symbol-name y))))
		   :doc-path ,documentation-files)))
