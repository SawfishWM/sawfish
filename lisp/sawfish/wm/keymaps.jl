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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

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
    "Keymap active anywhere."
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
    "Keymap active when a client window is focused."
    :group bindings
    :type keymap
    :before-set (lambda () (ungrab-keymap window-keymap))
    :after-set (lambda () (grab-keymap window-keymap)))

  (defcustom root-window-keymap (bind-keys (make-keymap)
					   "Button2-Click1" 'popup-root-menu)
    "Keymap active when the pointer is in the root window
(or when no window is focused)."
    :group bindings
    :type keymap)

  (defcustom title-keymap (bind-keys (make-keymap)
				     "Button3-Off" 'raise-lower-window
				     "Button3-Move" 'move-window-interactively
				     "Button3-Click" 'tab-release-window
				     "Button2-Move" 'resize-window-interactively
				     "Button2-Off" 'tab-window-add-to-tabgroup
				     "C-Button2-Off" 'tab-tabgroup-add-to-tabgroup
				     "Button1-Off2" 'toggle-window-shaded
				     "Button1-Move" 'move-window-interactively)
    "Keymap of window title bar. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom tabbar-keymap (bind-keys (make-keymap)
				      "Button3-Move" 'move-window-interactively
				      "Button3-Click" 'tab-release-window
				      "Button2-Off" 'tab-window-add-to-tabgroup
				      "C-Button2-Off" 'tab-tabgroup-add-to-tabgroup
				      "Button1-Off2" 'toggle-window-shaded
				      "Button1-Move" 'move-window-interactively
                      "Button1-C-Off" 'tab-move-to-left
                      "Button3-C-Off" 'tab-move-to-right
                      "Button1-Super-Off" 'tab-move-to-beginning
                      "Button3-Super-Off" 'tab-move-to-end)

    "Keymap of window tabbar. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom border-keymap (bind-keys (make-keymap)
				      "Button3-Off" 'raise-lower-window
				      "Button2-Move" 'move-window-interactively
				      "Button1-Move" 'resize-window-interactively)
    "Keymap of window border. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom close-button-keymap (bind-keys (make-keymap)
					    "Button3-Click1" 'popup-window-ops-menu
					    "S-Button1-Off" 'delete-group
					    "Button1-Off" 'delete-window)
    "Keymap of \"close\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom iconify-button-keymap (bind-keys (make-keymap)
					      "Button3-Click1" 'popup-window-ops-menu
					      "Button1-Off" 'iconify-window)
    "Keymap of \"iconify\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom maximize-button-keymap (bind-keys (make-keymap)
					       "Button3-Off" 'maximize-window-horizontally-toggle
					       "Button2-Off" 'maximize-window-vertically-toggle
					       "Button1-Off" 'maximize-window-toggle)
    "Keymap of \"maximize\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom menu-button-keymap (bind-keys (make-keymap)
					   "Button3-Off" 'delete-window
					   "Button1-Click1" 'popup-window-ops-menu)
    "Keymap of \"menu\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom shade-button-keymap (bind-keys (make-keymap)
					    "Button1-Off" 'toggle-window-shaded)
    "Keymap of \"shade\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom sticky-button-keymap (bind-keys (make-keymap)
					     "Button1-Off" 'toggle-window-sticky
					     "Button2-Off" 'toggle-window-sticky-viewport)
    "Keymap of \"sticky\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom lock-button-keymap (bind-keys (make-keymap)
					   "Button1-Off" 'toggle-fixed-postion)
    "Keymap of \"lock\" button. Mouse-bindings only."
    :group bindings
    :type keymap)


  (defcustom rename-button-keymap (bind-keys (make-keymap)
					     "Button1-Off" 'rename-window)
    "Keymap of \"rename\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom move-resize-button-keymap (bind-keys (make-keymap)
						  "Button1-Off" 'move-window-interactively
						  "Button2-Off" 'resize-window-interactively
						  "Button3-Off" 'move-window-center
						  "Button4-Off" 'double-window-size
						  "Button5-Off" 'halve-window-size)
    "Keymap of \"move/resize\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom raise-lower-button-keymap (bind-keys (make-keymap)
						  "Button1-Off" 'raise-window-depth
						  "Button2-Off" 'raise-window
						  "Button3-Off" 'lower-window-depth
						  "Button4-Off" 'raise-window
						  "Button5-Off" 'lower-window)
    "Keymap of \"raise/lower\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom next-workspace-button-keymap (bind-keys (make-keymap)
					    "Button3-Off" 'send-to-previous-workspace
					    "Button2-Click" 'popup-workspace-list
					    "Button1-Off" 'send-to-next-workspace)
    "Keymap of \"next-workspace\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defcustom previous-workspace-button-keymap (bind-keys (make-keymap)
					    "Button3-Off" 'send-to-next-workspace
					    "Button2-Click" 'popup-workspace-list
					    "Button1-Off" 'send-to-previous-workspace)
    "Keymap of \"previous-workspace\" button. Mouse-bindings only."
    :group bindings
    :type keymap)

  (defvar pointer-motion-threshold 2
    "Distance pointer must move before generating motion events, in pixels.")

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
