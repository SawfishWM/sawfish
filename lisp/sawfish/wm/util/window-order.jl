;; window-order.jl -- keep track of recently accessed windows
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

(provide 'window-order)

;; window order high-water-mark
(defvar window-order-highest 1)

;; return windows in MRU order
(defun window-order (&optional workspace allow-iconified all-viewports)
  (let
      ((windows (managed-windows)))
    (setq windows (delete-if (lambda (w)
			       (or (not (window-mapped-p w))
				   (window-get w 'ignored)
				   (and (not allow-iconified)
					(window-get w 'iconified))
				   (and workspace
					(not (window-in-workspace-p
					      w workspace)))))
			     windows))
    (unless all-viewports
      (setq windows (delete-if window-outside-viewport-p windows)))
    (sort windows (lambda (x y)
		    (setq x (window-get x 'order))
		    (setq y (window-get y 'order))
		    (cond ((and x y)
			   (> x y))
			  (x t)
			  (t nil))))))

;; push window W onto the top of the cycle stack
(defun window-order-push (w)
  (window-put w 'order window-order-highest)
  (setq window-order-highest (1+ window-order-highest))
  (when (> window-order-highest 1000000)		;arbitrary big number
    (window-order-compress)))

;; remove window W from the order stack
(defun window-order-pop (w)
  (window-put w 'order nil))

;; compress the order stack
(defun window-order-compress ()
  (let
      ((order (nreverse (window-order nil t t)))	;all windows
       (i 1))
    (mapc (lambda (w)
	    (window-put w 'order nil)) (managed-windows))
    (mapc (lambda (w)
	    (window-put w 'order i)
	    (setq i (1+ i))) order)
    (setq window-order-highest i)))

(defun window-order-focus-most-recent ()
  (let
      ((win (car (window-order current-workspace nil))))
    (when win
      (set-input-focus win))))

;; The problem is that any sticky windows that have been focused once
;; will _always_ rise to the top of the order when switching workspaces
;; (since the topmost window is _always_ focused when entering a new
;; workspace). The hacky solution is to remove the order of any sticky
;; windows that aren't at the top of the stack before switching spaces

(defun window-order-pop-low-sticky-windows (space)
  (let
      ((order (window-order space t t)))
    (while (and order (window-get (car order) 'sticky))
      (setq order (cdr order)))
    (mapc (lambda (w)
	    (when (window-get w 'sticky)
	      (window-put w 'order nil))) order)))

(add-hook 'leave-workspace-hook window-order-pop-low-sticky-windows)

(sm-add-saved-properties 'order)
(add-hook 'sm-after-restore-hook window-order-compress)
(add-hook 'iconify-window-hook window-order-pop)
