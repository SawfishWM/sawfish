;; focus.jl -- implement standard focus behaviour
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

(provide 'focus)

(defcustom focus-mode 'enter-exit
  "When does the mouse pointer affect the input focus."
  :type (set enter-exit enter-only click)
  :group focus
  :after-set focus-mode-changed)

(defcustom focus-proxy-click t
  "Does click-to-focus mode pass the click through to the window."
  :type boolean
  :group focus)

(defvar click-to-focus-keymap
  (bind-keys (make-keymap)
    "Any-Click1" 'focus-click))

(defun focus-click (w)
  (interactive "%w")
  (set-input-focus w)
  (window-put w 'keymap window-keymap)
  (when (or (window-get w 'focus-proxy-click) focus-proxy-click)
    ;; there's a problem here. allow-events called with replay-pointer
    ;; ignores any passive grabs on the window, thus if the wm has a
    ;; binding in the window's keymap, it would be ignored. So search
    ;; manually..
    (let
	((command (lookup-event-binding (current-event))))
      (if command
	  (call-command command)
	;; pass the event through to the client window
	(allow-events 'replay-pointer)))))

(defun focus-enter-fun (w)
  (if (eq w 'root)
      (when (eq focus-mode 'enter-exit)
	(set-input-focus nil))
    (unless (eq focus-mode 'click)
      (set-input-focus w))))

(defun focus-in-fun (w)
  (unless (eq (window-get w 'keymap) window-keymap)
    (window-put w 'keymap window-keymap))
  (when (eq focus-mode 'click)
    (mapc #'(lambda (x)
	      (unless (or (eq x w) (eq (window-get x 'keymap)
				       click-to-focus-keymap))
		(window-put x 'keymap click-to-focus-keymap))))))

(defun focus-out-fun (w)
  (when (and (eq focus-mode 'click)
	     (not (eq (window-get w 'keymap) click-to-focus-keymap)))
    (window-put w 'keymap click-to-focus-keymap)))

(defun focus-mode-changed ()
  (if (eq focus-mode 'click)
      (mapc #'(lambda (w)
		(window-put w 'keymap (if (eq (input-focus) w)
					  window-keymap
					click-to-focus-keymap)))
	    (managed-windows))
    (mapc #'(lambda (w)
	      (window-put w 'keymap window-keymap)) (managed-windows))))

(defun focus-add-window (w)
  (when (eq focus-mode 'click)
    (window-put w 'keymap click-to-focus-keymap)))

(add-hook 'enter-notify-hook 'focus-enter-fun t)
(add-hook 'focus-in-hook 'focus-in-fun t)
(add-hook 'focus-out-hook 'focus-out-fun t)
(add-hook 'add-window-hook 'focus-add-window t)

(unless batch-mode
  (mapc 'focus-add-window (managed-windows)))
