;; shade-hover.jl -- temporarily unshade windows while hovered over
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(require 'timers)
(provide 'shade-hover)

(defgroup shade-hover "Shade Hover"
  :group focus)

(defcustom shade-hover-mode nil
  "Enable shade-hover mode.
(Temporarily unshade windows while pointer is over them.)"
  :group (focus shade-hover)
  :type boolean
  :require shade-hover)

(defcustom shade-hover-delay 250
  "Delay in milliseconds before unshading windows."
  :group (focus shade-hover)
  :type number
  :range (0 . 5000))

(defcustom shade-hover-raise nil
  "Raise windows when they are unshaded."
  :group (focus shade-hover)
  :type boolean)

(defvar shade-hover-timer nil)

(defun shade-hover-leave (w)
  (unless (eq w 'root)
    (when shade-hover-timer
      (delete-timer shade-hover-timer)
      (setq shade-hover-timer nil))
    (when (window-get w 'shade-hover-unshaded)
      (window-put w 'shade-hover-unshaded nil)
      (shade-window w))
    (when (in-hook-p 'pre-command-hook shade-hover-before)
      (remove-hook 'pre-command-hook shade-hover-before))))

(defun shade-hover-before ()
  (let
      ((command (and (current-event) (lookup-event-binding (current-event))))
       (w (current-event-window)))
    (when (and (eq command 'toggle-window-shaded) w)
      (shade-hover-leave w))))

(defun shade-hover-enter (w)
  (when (and (windowp w)
	     (or shade-hover-mode (window-get w 'shade-hover)))
    (when (window-get w 'shaded)
      (let
	  ((callback
	    (lambda ()
	      (window-put w 'shade-hover-unshaded t)
	      (let
		  ((raise-windows-when-unshaded shade-hover-raise))
		(unshade-window w)))))
	(when shade-hover-timer
	  (delete-timer shade-hover-timer))
	(if (zerop shade-hover-delay)
	    (callback)
	  (setq shade-hover-timer (make-timer callback
					      (quotient shade-hover-delay 1000)
					      (mod shade-hover-delay 1000))))
	(unless (in-hook-p 'pre-command-hook shade-hover-before)
	  (add-hook 'pre-command-hook shade-hover-before))))))

(add-hook 'enter-notify-hook shade-hover-enter)
(add-hook 'leave-notify-hook shade-hover-leave)
