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

(define-structure sawfish.wm.ext.shade-hover ()

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.windows
	  sawfish.wm.custom
	  sawfish.wm.events
	  sawfish.wm.state.shading)

  (define-structure-alias shade-hover sawfish.wm.ext.shade-hover)

;;;###autoload (defgroup shade-hover "Shade Hover" :group focus :require sawfish.wm.ext.shade-hover)

  (defgroup shade-hover "Shade Hover"
    :group focus
    :require sawfish.wm.ext.shade-hover)

  (defcustom shade-hover-mode nil
    "Enable shade-hover mode."
    :tooltip "(Temporarily unshades windows while the mouse pointer is over them.)"
    :group (focus shade-hover)
    :type boolean
    :user-level novice
    :require sawfish.wm.ext.shade-hover)

  (defcustom shade-hover-delay 250
    "Delay in milliseconds before unshading windows."
    :group (focus shade-hover)
    :depends shade-hover-mode
    :type (number 0 5000))

  (defcustom shade-hover-raise nil
    "Raise windows when they are unshaded."
    :group (focus shade-hover)
    :depends shade-hover-mode
    :type boolean)

  (define shade-hover-timer nil)
  (define shade-hover-window nil)

  (define (clean-up)
    (when shade-hover-timer
      (delete-timer shade-hover-timer)
      (setq shade-hover-timer nil))
    (setq shade-hover-window nil)
    (when (in-hook-p 'pre-command-hook shade-hover-before)
      (remove-hook 'pre-command-hook shade-hover-before)))

  (define (shade-hover-leave w)
    (when (eq shade-hover-window w)
      (clean-up)
      (when (window-get w 'shade-hover-unshaded)
	(window-put w 'shade-hover-unshaded nil)
	(shade-window w))))

  (define (shade-hover-before)
    (let ((w (current-event-window)))
      (when (and (windowp w) (window-get w 'shade-hover-unshaded))
	(case this-command
	  ((toggle-window-shaded unshade-window)
	   ;; don't want window to shade then unshade
	   (clean-up)
	   (window-put w 'shade-hover-unshaded nil)
	   (setq this-command nil))
	  ((shade-window)
	   (clean-up))))))

  (define (shade-hover-enter w)
    (when (and (windowp w)
	       (or shade-hover-mode (window-get w 'shade-hover)))
      (when (window-get w 'shaded)
	(let ((callback
	       (lambda ()
		 (window-put w 'shade-hover-unshaded t)
		 (let
		     ((raise-windows-when-unshaded shade-hover-raise))
		   (unshade-window w)))))
	  (when shade-hover-timer
	    (delete-timer shade-hover-timer))
	  (setq shade-hover-window w)
	  (if (zerop shade-hover-delay)
	      (callback)
	    (setq shade-hover-timer
		  (make-timer callback
			      (quotient shade-hover-delay 1000)
			      (mod shade-hover-delay 1000))))
	  (unless (in-hook-p 'pre-command-hook shade-hover-before)
	    (add-hook 'pre-command-hook shade-hover-before))))))

  (add-hook 'enter-notify-hook shade-hover-enter)
  (add-hook 'leave-notify-hook shade-hover-leave)
  (add-hook 'focus-out-hook shade-hover-leave))
