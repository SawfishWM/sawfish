;; auto-raise.jl -- auto-raise on focus
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

(require 'timers)
(provide 'auto-raise)

;;;###autoload (setq custom-required (cons 'auto-raise custom-required))

;; if a string, windows are only raised if their name matches this
;; regular expression.
(defcustom raise-windows-on-focus nil
  "Raise windows when they get the keyboard focus."
  :type boolean
  :require auto-raise
  :group focus)

(defcustom raise-window-timeout 500
  "Delay in milliseconds until focused windows are raised."
  :type number
  :group focus)

(defvar rw-timer nil)
(defvar rw-window nil)

(defun rw-disable-timer ()
  (when rw-timer
    (setq rw-window nil)
    (delete-timer rw-timer)
    (setq rw-timer nil)))

(defun rw-on-focus (w)
  (if (or (and (stringp raise-windows-on-focus)
	       (string-match raise-windows-on-focus (window-name w)))
	  raise-windows-on-focus)
      (if (<= raise-window-timeout 0)
	  (progn
	    (raise-window w)
	    (rw-disable-timer))
	(setq rw-window w)
	(if rw-timer
	    (set-timer rw-timer)
	  (setq rw-timer (make-timer 'rw-timer-callback
				     (/ raise-window-timeout 1000)
				     (mod raise-window-timeout 1000)))))
    (rw-disable-timer)))

(defun rw-out-focus (w)
  (when (and rw-timer (eq rw-window w))
    (rw-disable-timer)))

(defun rw-timer-callback ()
  (raise-window rw-window)
  (setq rw-timer nil))

(add-hook 'focus-in-hook 'rw-on-focus)
(add-hook 'focus-out-hook 'rw-out-focus)
