;; keymaps.jl -- the default keymaps

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.keymaps

    (export custom-set-keymap)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.events
	  sawfish.wm.custom)

  (defgroup bindings "Bindings"
    :layout keymaps)

;;; Customize support

  (define (customizable-command-p x)
    (or (symbolp x)
	(and (symbolp (car x)) (get (car x) 'custom-command-args))))

  (define (custom-get-keymap symbol)
    (cons 'keymap (mapcar (lambda (cell)
			    (cons (car cell) (event-name (cdr cell))))
			  (filter (lambda (cell)
				    (customizable-command-p (car cell)))
				  (cdr (symbol-value symbol))))))

  ;; can't just call out to custom-set-variable since we side-effect VALUE
  (define (custom-set-keymap symbol value)
    (custom-set
     (lambda ()
       (when (eq (car value) 'keymap)
	 (let
	     ((old-value (and (boundp symbol) (symbol-value symbol)))
	      (new-tail (delq nil (mapcar (lambda (cell)
					    (let
						((ev (condition-case nil
							 (lookup-event
							  (cdr cell))
						       (error nil))))
					      (and ev (cons (car cell) ev))))
					  (cdr value)))))

	   (when (eq old-value (variable-default-value symbol))
	     ;; protect the default-value, an ugly hack..
	     (put symbol 'custom-default-value
		  (copy-sequence (variable-default-value symbol))))

	   ;; add in any non-command bindings
	   (setq new-tail (nconc new-tail
				 (filter (lambda (cell)
					   (not (customizable-command-p
						 (car cell))))
					 (cdr old-value))))
	   (if (and old-value (eq (car old-value) 'keymap))
	       ;; hijack the old keymap to preserve eq-ness
	       (rplacd old-value new-tail)
	     (set symbol (cons 'keymap new-tail))))))
     symbol))

  (define-custom-setter 'custom-set-keymap custom-set-keymap)
  (put 'keymap 'custom-set 'custom-set-keymap)
  (put 'keymap 'custom-get custom-get-keymap)

;;; Options

  (defcustom global-keymap (bind-keys (make-keymap)
			     "W-Left" 'previous-workspace
			     "W-Right" 'next-workspace
			     "W-Tab" 'cycle-windows)
    "Keymap containing bindings active anywhere."
    :group bindings
    :type keymap
    :before-set (lambda () (ungrab-keymap global-keymap))
    :after-set (lambda () (grab-keymap global-keymap)))

  (defcustom window-keymap (bind-keys (make-keymap)
			     "W-Up" 'raise-window
			     "W-Down" 'lower-window
			     "W-Button3-Click1" 'raise-lower-window
			     "W-Button2-Click1" 'popup-window-ops-menu
			     "W-Button1-Move" 'move-window-interactively
			     "Button1-Click1" 'raise-and-pass-through-click
			     "W-ISO_Left_Tab" 'tab-raise-left-window
			     "H-ISO_Left_Tab" 'tab-raise-right-window)
    "Keymap containing bindings active when a client window is focused."
    :group bindings
    :type keymap
    :before-set (lambda () (ungrab-keymap window-keymap))
    :after-set (lambda () (grab-keymap window-keymap)))

  (defcustom root-window-keymap (bind-keys (make-keymap)
				  "Button2-Click1" 'popup-root-menu)
    "Keymap containing bindings active when the pointer is in the root window
(or when no window is focused)."
    :group bindings
    :type keymap)

  (defcustom title-keymap (bind-keys (make-keymap)
			    "Button3-Off" 'raise-lower-window
			    "Button2-Move" 'resize-window-interactively
			    "Button1-Off2" 'toggle-window-shaded
			    "Button1-Move" 'move-window-interactively
			    "Button2-Off" 'tab-add-to-group)
    "Keymap containing bindings active when the pointer is in the title of
a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom border-keymap (bind-keys (make-keymap)
			     "Button3-Off" 'raise-lower-window
			     "Button2-Move" 'move-window-interactively
			     "Button1-Move" 'resize-window-interactively)
    "Keymap containing bindings active when the pointer is in the border of
a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom close-button-keymap (bind-keys (make-keymap)
				   "Button3-Click1" 'popup-window-ops-menu
				   "S-Button1-Off" 'delete-group
				   "Button1-Off" 'delete-window)
    "Keymap containing bindings active when the pointer is in the close button
of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom iconify-button-keymap (bind-keys (make-keymap)
				     "Button3-Click1" 'popup-window-ops-menu
				     "Button1-Off" 'iconify-window)
    "Keymap containing bindings active when the pointer is in the iconify
button of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom maximize-button-keymap (bind-keys (make-keymap)
				      "Button3-Off" 'maximize-window-horizontally-toggle
				      "Button2-Off" 'maximize-window-vertically-toggle
				      "Button1-Off" 'maximize-window-toggle)
    "Keymap containing bindings active when the pointer is in the maximize
button of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom menu-button-keymap (bind-keys (make-keymap)
				  "Button3-Off" 'delete-window
				  "Button1-Click1" 'popup-window-ops-menu)
    "Keymap containing bindings active when the pointer is in the menu button
of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom shade-button-keymap (bind-keys (make-keymap)
				   "Button1-Off" 'toggle-window-shaded)
    "Keymap containing bindings active when the pointer is in the shade button
of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom sticky-button-keymap (bind-keys (make-keymap)
				   "Button1-Off" 'toggle-window-sticky
				   "Button2-Off" '(call-command
				                   (lambda ()
						     (if (window-get (current-event-window) 'sticky-viewport)
						           (window-put (current-event-window) 'sticky-viewport nil)
							   (window-put (current-event-window) 'sticky-viewport t)))))
    "Keymap containing bindings active when the pointer is in the sticky button
of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom lock-button-keymap (bind-keys (make-keymap)
    "Button1-Off" '(call-command
			(lambda ()
				(if (window-get (current-event-window) 'fixed-position)
					(window-put (current-event-window) 'fixed-position nil)
					(window-put (current-event-window) 'fixed-position t))
                  (call-window-hook 'window-state-change-hook (current-event-window) (list '(fixed-position))))))


    "Keymap containing bindings active when the pointer is in the lock button
of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)


  (defcustom rename-button-keymap (bind-keys (make-keymap)
				   "Button1-Off" 'rename-window)
    "Keymap containing bindings active when the pointer is in the rename button
of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom move-resize-button-keymap (bind-keys (make-keymap)
                                        "Button1-Off" 'move-window-interactively
					"Button2-Off" 'resize-window-interactively
					"Button3-Off" 'move-window-center
					"Button4-Off" 'double-window-size
					"Button5-Off" 'halve-window-size)
    "Keymap containing bindings active when the pointer is in the move/resize button
of a window. (Only mouse-bindings are evaluated in this map.)"
    :group bindings
    :type keymap)

  (defcustom raise-lower-button-keymap (bind-keys (make-keymap)
                                         "Button1-Off" 'raise-window
					 "Button2-Off" 'lower-window
					 "Button4-Off" 'raise-window-depth
					 "Button5-Off" 'lower-window-depth)
    "Keymap containing bindings active when the pointer is in the raise/lower button
of a window. (Only mouse-bindings are evaluated in this map.)"
  :group bindings
  :type keymap)

  (defvar pointer-motion-threshold 2
    "Distance in pixels pointer must move before generating motion events.")

  (defcustom wm-modifier-value (wm-modifier)
    "Modifier key(s) used for default shortcuts."
    :group bindings
    :type modifier-list
    :after-set (lambda ()
		 (ungrab-keymap global-keymap)
		 (ungrab-keymap window-keymap)
		 (set-wm-modifier wm-modifier-value)
		 (grab-keymap window-keymap)
		 (grab-keymap global-keymap)))

;;; Arrange for window-keymap to be set in each window

  (define (keymap-add-window w)
    (unless (window-get w 'keymap)
      (window-put w 'keymap window-keymap)))

  (add-hook 'add-window-hook keymap-add-window)

;; custom support for modifiers

  (define-custom-serializer 'modifier-list
			    (lambda (value)
			      (require 'sawfish.wm.util.decode-events)
			      (decode-modifier value)))

  (define-custom-deserializer 'modifier-list
			      (lambda (value)
				(require 'sawfish.wm.util.decode-events)
				(encode-modifier value))))
