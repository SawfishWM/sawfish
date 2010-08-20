;; auto-raise.jl -- auto-raise on focus

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

(define-structure sawfish.wm.ext.auto-raise

    (export )

    (open rep
          rep.system
          rep.io.timers
          sawfish.wm.windows
          sawfish.wm.custom
          sawfish.wm.util.stacking)

  (define-structure-alias auto-raise sawfish.wm.ext.auto-raise)

  (defgroup auto-raise "Auto-Raise" :group focus)

  (defcustom raise-windows-on-focus nil
    "Raise windows when they are focused."
    :type boolean
    :require sawfish.wm.ext.auto-raise
    :group (focus auto-raise))

  (defcustom raise-window-timeout 500
    "Delay in milliseconds until focused windows are raised."
    :type number
    :depends raise-windows-on-focus
    :group (focus auto-raise))

  (defvar disable-auto-raise nil)

  (define rw-timer nil)
  (define rw-window nil)

  (define (rw-disable-timer)
    (when rw-timer
      (setq rw-window nil)
      (delete-timer rw-timer)
      (setq rw-timer nil)))

  (define (rw-on-focus w mode)
    (declare (unused mode))
    (when (not disable-auto-raise)
      (if (or (window-get w 'raise-on-focus) raise-windows-on-focus)
	  (progn
	    (setq rw-window w)
	    (if rw-timer
		(set-timer rw-timer)
	      (let ((timer-callback (lambda (timer)
				      (if disable-auto-raise
					  (set-timer timer)
					(setq rw-timer nil)
					(raise-window* rw-window))))
		    (delay (max 1 raise-window-timeout)))
		(setq rw-timer (make-timer timer-callback
					   (quotient delay 1000)
					   (mod delay 1000))))))
	(rw-disable-timer))))

  (define (rw-out-focus w mode)
    (declare (unused mode))
    (when (and rw-timer (eq rw-window w))
      (rw-disable-timer)))

  (add-hook 'focus-in-hook rw-on-focus)
  (add-hook 'focus-out-hook rw-out-focus))
