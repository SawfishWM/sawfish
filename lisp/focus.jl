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

(defvar sloppy-focus nil
  "When non-nil, and the usual focus-follows-mouse behaviour is in use, the
focus is only changed when a top-level window is entered, never when the
root window is entered.")

(defvar raise-windows-on-focus nil
  "When non-nil, windows are raised after receiving focus.")

(defvar raise-window-timeout 500
  "Time in milliseconds until windows are raised if raise-windows-on-focus
is set.")

(defun focus-enter-fun (w)
  (if (eq w 'root)
      (unless sloppy-focus
	(set-input-focus nil))
    (set-input-focus w)
    (when raise-windows-on-focus
      (focus-raise w))))

(defun focus-leave-fun (w)
  (unless sloppy-focus
    (set-input-focus nil)))

(defun focus-raise (w)
  (sync-server)
  (sit-for 0 raise-window-timeout)
  (raise-window w))

(add-hook 'enter-notify-hook 'focus-enter-fun t)
(add-hook 'leave-notify-hook 'focus-leave-fun t)
