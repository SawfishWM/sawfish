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

(defgroup bindings "Bindings"
  :layout keymaps)


;; Customize support

(defun customizable-command-p (x)
  (or (symbolp x)
      (and (symbolp (car x)) (get (car x) 'custom-command-args))))

(defun custom-get-keymap (symbol)
  (cons 'keymap (mapcar (lambda (cell)
			  (cons (car cell) (event-name (cdr cell))))
			(filter (lambda (cell)
				  (customizable-command-p (car cell)))
				(cdr (symbol-value symbol))))))

;; can't just call out to custom-set-variable since we side-effect VALUE
(defun custom-set-keymap (symbol value &optional req)
  (when req
    (require req))
  (when (get symbol 'custom-before-set)
    (funcall (get symbol 'custom-before-set) symbol))
  (when (eq (car value) 'keymap)
    (let
	((old-value (and (boundp symbol) (symbol-value symbol)))
	 (new-tail (delq nil (mapcar (lambda (cell)
				       (let
					   ((ev (condition-case nil
						    (lookup-event (cdr cell))
						  (error nil))))
					 (and ev (cons (car cell) ev))))
				     (cdr value)))))
      ;; add in any non-command bindings
      (setq new-tail (nconc new-tail (filter (lambda (cell)
					       (not (customizable-command-p
						     (car cell))))
					     (cdr old-value))))
      (if (and old-value (eq (car old-value) 'keymap))
	  ;; hijack the old keymap to preserve eq-ness
	  (rplacd old-value new-tail)
	(set symbol (cons 'keymap new-tail)))))
  (when (get symbol 'custom-after-set)
    (funcall (get symbol 'custom-after-set) symbol)))

(put 'keymap 'custom-set custom-set-keymap)
(put 'keymap 'custom-get custom-get-keymap)
(define-custom-setter 'custom-set-keymap custom-set-keymap)


;; Options

(defcustom global-keymap (make-keymap)
  "Keymap containing bindings active anywhere."
  :group bindings
  :type keymap
  :before-set (lambda () (ungrab-keymap global-keymap))
  :after-set (lambda () (grab-keymap global-keymap)))

(defcustom window-keymap (make-keymap)
  "Keymap containing bindings active when a client window is focused."
  :group bindings
  :type keymap
  :before-set (lambda () (ungrab-keymap window-keymap))
  :after-set (lambda () (grab-keymap window-keymap)))

(defcustom root-window-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the root window
(or when no window is focused)."
  :group bindings
  :user-level expert
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
  :user-level expert
  :type keymap)

(defcustom close-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the close button
of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :user-level expert
  :type keymap)

(defcustom iconify-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the iconify
button of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :user-level expert
  :type keymap)

(defcustom maximize-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the maximize
button of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :user-level expert
  :type keymap)

(defcustom menu-button-keymap (make-keymap)
  "Keymap containing bindings active when the pointer is in the menu button
of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :user-level expert
  :type keymap)


;; Arrange for window-keymap to be set in each window

(defun keymap-add-window (w)
  (unless (window-get w 'keymap)
    (window-put w 'keymap window-keymap)))

(add-hook 'add-window-hook keymap-add-window)


;; some bindings

(unless batch-mode
  (bind-keys title-keymap
    "Button3-Off" 'raise-lower-window-and-transients
    "Button2-Move" 'resize-window-interactively
    "Button1-Click2" 'toggle-window-shaded
    "Button1-Move" 'move-window-interactively)

  (bind-keys border-keymap
    "Button3-Off" 'raise-lower-window-and-transients
    "Button2-Move" 'move-window-interactively
    "Button1-Move" 'resize-window-interactively)

  (bind-keys window-keymap
    "C-M-Up" 'raise-window-and-transients
    "C-M-Down" 'lower-window-and-transients
    "C-M-Left" 'send-to-previous-workspace
    "C-M-Right" 'send-to-next-workspace
    "C-M-q" 'quote-event
    "M-Button3-Click1" 'raise-lower-window-and-transients
    "M-Button2-Click1" 'popup-window-menu
    "M-Button1-Move" 'move-window-interactively)

  (bind-keys global-keymap
    "C-Left" 'previous-workspace
    "C-Right" 'next-workspace
    "M-Tab" 'cycle-windows)

  (bind-keys root-window-keymap
    "Button2-Click1" 'popup-root-menu)

  (bind-keys close-button-keymap
    "Button3-Click1" 'popup-window-menu
    "Button1-Off" 'delete-window)

  (bind-keys iconify-button-keymap
    "Button3-Click1" 'popup-window-menu
    "Button1-Off" 'iconify-window)

  (bind-keys menu-button-keymap
    "Button3-Off" 'delete-window
    "Button1-Click1" 'popup-window-menu)

  (bind-keys maximize-button-keymap
    "Button3-Off" 'maximize-window-horizontally-toggle
    "Button2-Off" 'maximize-window-vertically-toggle
    "Button1-Off" 'maximize-window-toggle))
