;; transient.jl -- support transient windows
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

(define-structure sawfish.wm.state.transient

    (export transient-of-p
	    indirect-transient-of-p
	    transient-parents
	    transient-children
	    transient-group
	    map-transient-group
	    raise-window-and-transients
	    lower-window-and-transients
	    raise-lower-window-and-transients)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.windows
	  sawfish.wm.stacking
	  sawfish.wm.viewport
	  sawfish.wm.util.window-order
	  sawfish.wm.util.groups
	  sawfish.wm.frames)

  (defcustom focus-windows-when-mapped t
    "Focus on application windows when they first appear."
    :type boolean
    :group focus)

  (defcustom transients-get-focus t
    "Dialog windows inherit the focus from their parent."
    :group focus
    :type boolean)

  (defcustom decorate-transients nil
    "Decorate dialog windows similarly to application windows."
    :type boolean
    :group appearance
    :after-set after-setting-frame-option)


;;; functions

  (define (transient-of-p x y #!key allow-root)
    "Return t if window X is directly a transient for window Y."
    (let ((x-for (window-transient-p x)))
      (and x-for
	   (or (eql x-for (window-id y))
	       ;; windows that set WM_TRANSIENT_FOR to the root window are
	       ;; transients for their entire group (de facto standard).
	       ;; This only makes sense for non-transient windows
	       ;; (disable this code by default, it causes too much weirdness)
	       (and allow-root
		    (eql x-for (root-window-id))
		    (window-group-id x)
		    (not (window-transient-p y))
		    (eql (window-group-id x) (window-group-id y)))))))

  (define (indirect-transient-of-p x y #!key allow-root)
    "Return t if window X is (directly, or indirectly) a transient for window Y."
    (let loop ((x x))
      (or (transient-of-p x y #:allow-root allow-root)
	  (let ((x-for (window-transient-p x)))
	    (and x-for
		 ;; Some KDE windows set WM_TRANSIENT_FOR to their own id!
		 (not (eql x-for (window-id x)))
		 (let ((x-for-w (get-window-by-id x-for)))
		   (if x-for-w
		       (loop x-for-w)
		     nil)))))))

  (define (transient-parents w #!optional indirectly)
    "Return the list of windows that window W is a transient for."
    (filter-windows (lambda (x)
		      (and (window-mapped-p x)
			   ((if indirectly
				indirect-transient-of-p
			      transient-of-p) w x)))))

  (define (transient-children w #!optional indirectly)
    "Return the list of windows which are transients of window W."
    (filter-windows (lambda (x)
		      (and (window-mapped-p x)
			   ((if indirectly
				indirect-transient-of-p
			      transient-of-p) x w)))))

  (define (transient-group w #!optional by-depth)
    "Return the list of windows which is either a transient window for window
W, or a window which W is a transient for. This always includes W. The
`transient window for' relation holds for windows which are direct or
indirect transients of the parent window in question."
    (delete-if-not (lambda (x)
		     (and (window-mapped-p x)
			  (or (eq x w)
			      (indirect-transient-of-p x w)
			      (indirect-transient-of-p w x))))
		   (if by-depth (stacking-order) (managed-windows))))

  (define (map-transient-group fun w)
    "Map the single argument function FUN over all windows in the same
transient group as window W."
    (mapc fun (transient-group w)))


;;; commands for raising windows with their transients

  (define (raise-window-and-transients w)
    "Raise the current window to its highest allowed position in the stacking
order. Also raise any transient windows that it has."
    (raise-windows w (transient-group w t)))

  (define (lower-window-and-transients w)
    "Lower the current window to its lowest allowed position in the stacking
order. Also lower any transient windows that it has."
    (lower-windows w (transient-group w t)))

  (define (raise-lower-window-and-transients w)
    "If the window is at its highest possible position, then lower it to its
lowest possible position. Otherwise raise it as far as allowed. Also changes
the level of any transient windows it has."
    (raise-lower-windows w (transient-group w t)))

  (define-command 'raise-window-and-transients
    raise-window-and-transients #:spec "%W" #:user-level 'expert)
  (define-command 'lower-window-and-transients
    lower-window-and-transients #:spec "%W" #:user-level 'expert)
  (define-command 'raise-lower-window-and-transients
    raise-lower-window-and-transients #:spec "%W" #:user-level 'expert)


;;; displaying

  (define (transient-frame-type w type)
    (if (and decorate-transients (window-transient-p w))
	(case type
	  ((transient) 'default)
	  ((shaped-transient) 'shaped)
	  (t type))
      type))

  (define-frame-type-mapper transient-frame-type)


;;; hooks

  (define (transient-map-window w)
    (cond ((and transients-get-focus
		(window-transient-p w)
		(window-really-wants-input-p w)
		(window-visible-p w)
		(input-focus)
		(transient-of-p w (input-focus) #:allow-root t))
	   (set-input-focus w))
	  ((and (or (and focus-windows-when-mapped
			 (not (window-get w 'never-focus)))
		    (window-get w 'focus-when-mapped))
		(or (not (window-transient-p w))
		    (eql (window-transient-p w) (root-window-id)))
		(window-really-wants-input-p w)
		(window-visible-p w))
	   (set-input-focus w))))

  ;; If a transient window gets unmapped that currently has the input
  ;; focus, pass it (the focus) to its parent. Otherwise, pass the focus
  ;; to the topmost window if click-to-focus, otherwise the window under
  ;; the mouse
  (define (transient-unmap-window w)
    (when (eq (input-focus) w)
      (let ((parent (and (window-transient-p w)
			 ;; for transient windows, look for the most
			 ;; recently focused window in its group, or
			 ;; fall back to its parent window
			 (or (window-order-most-recent
			      #:windows (delq w (windows-in-group w)))
			     (get-window-by-id (window-transient-p w))))))
	(when (or (not parent)
		  (not (window-mapped-p parent))
		  (not (window-visible-p parent))
		  (window-outside-viewport-p parent)
		  (not (window-really-wants-input-p parent)))
	  ;; if no parent, choose the topmost window (if in click-to-focus
	  ;; mode) or the window under the pointer otherwise
	  (if (eq focus-mode 'click)
	      (setq parent nil)
	    (setq parent (query-pointer-window)))
	  (unless (or parent (eq focus-mode 'enter-exit))
	    (setq parent (window-order-most-recent))))
	(when (or (null parent) (window-really-wants-input-p parent))
	  (set-input-focus parent)))))

  (add-hook 'map-notify-hook transient-map-window)
  (add-hook 'unmap-notify-hook transient-unmap-window)
  (add-hook 'iconify-window-hook transient-unmap-window))
