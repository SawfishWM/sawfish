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

(require 'window-order)
(provide 'focus)

(defcustom focus-mode 'enter-exit
  "When does the mouse pointer affect the input focus."
  :type symbol
  :options (enter-exit enter-only click)
  :group focus
  :after-set (lambda () (focus-mode-changed)))

(defcustom focus-click-through t
  "Does click-to-focus mode pass the click through to the window."
  :type boolean
  :group (focus advanced))

(defvar click-to-focus-keymap
  (bind-keys (make-keymap)
    ;; apparently buttons 4 & 5 are usually bound to the
    ;; wheel on some mice
    "Any-Button1-Click1" 'focus-click
    "Any-Button2-Click1" 'focus-click
    "Any-Button3-Click1" 'focus-click))

(defvar focus-dont-push nil)

(defun focus-push-map (w)
  (unless (eq (window-get w 'keymap) 'click-to-focus-keymap)
    (window-put w 'focus-saved-keymap (window-get w 'keymap))
    (window-put w 'keymap 'click-to-focus-keymap)))

(defun focus-pop-map (w)
  (when (eq (window-get w 'keymap) 'click-to-focus-keymap)
    (window-put w 'keymap (window-get w 'focus-saved-keymap))
    (window-put w 'focus-saved-keymap nil)))

(defun focus-click (w)
  (interactive "%w")
  (when (window-really-wants-input-p w)
    (set-input-focus w))
  (focus-pop-map w)
  (when (or (window-get w 'focus-click-through)
	    focus-click-through
	    (not (window-really-wants-input-p w)))
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
    (unless (or (eq focus-mode 'click)
		(not (window-really-wants-input-p w)))
      (set-input-focus w))))

(defun focus-in-fun (w)
  (focus-pop-map w)
  (when (eq focus-mode 'click)
    (mapc (lambda (x)
	    (unless (eq x w)
	      (focus-push-map x)))))
  (unless focus-dont-push
    (window-order-push w)))

(defun focus-out-fun (w)
  (when (eq focus-mode 'click)
    (focus-push-map w)))

(defun focus-mode-changed ()
  (if (eq focus-mode 'click)
      (mapc (lambda (w)
	      (if (eq (input-focus) w)
		  (focus-pop-map w)
		(focus-push-map w)))
	    (managed-windows))
    (mapc focus-pop-map (managed-windows))))

(defun focus-add-window (w)
  (when (eq focus-mode 'click)
    (focus-push-map w)))

(add-hook 'enter-notify-hook focus-enter-fun t)
(add-hook 'focus-in-hook focus-in-fun t)
(add-hook 'focus-out-hook focus-out-fun t)
(add-hook 'add-window-hook focus-add-window t)

(sm-add-saved-properties 'never-focus)
