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

(define-structure sawfish.wm.util.window-order

    (export window-order
	    window-order-push
	    window-order-pop
	    window-order-most-recent
	    window-order-focus-most-recent)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.session.init
	  sawfish.wm.workspace
	  sawfish.wm.viewport)

  (define-structure-alias window-order sawfish.wm.util.window-order)

  ;; window order high-water-mark
  (define window-order-highest 1)

  ;; return windows in MRU order
  (define (window-order #!optional workspace allow-iconified all-viewports)
    (let ((windows (managed-windows)))
      (setq windows (delete-if (lambda (w)
				 (or (not (window-mapped-p w))
				     (window-get w 'ignored)
				     (and (not allow-iconified)
					  (window-get w 'iconified))
				     (and workspace
					  (not (window-appears-in-workspace-p
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
  (define (window-order-push w)
    (window-put w 'order window-order-highest)
    (setq window-order-highest (1+ window-order-highest))
    (when (> window-order-highest 1000000)		;arbitrary big number
      (window-order-compress)))

  ;; remove window W from the order stack
  (define (window-order-pop w)
    (window-put w 'order nil))

  ;; compress the order stack
  (define (window-order-compress)
    (let ((order (nreverse (window-order nil t t)))	;all windows
	  (i 1))
      (map-windows (lambda (w)
		     (window-put w 'order nil)))
      (mapc (lambda (w)
	      (window-put w 'order i)
	      (setq i (1+ i))) order)
      (setq window-order-highest i)))

  (define (window-order-most-recent)
    (let ((windows (window-order current-workspace nil)))
      (while (and windows (window-get (car windows) 'never-focus))
	(setq windows (cdr windows)))
      (car windows)))

  (define (window-order-focus-most-recent)
    (set-input-focus (window-order-most-recent)))

  (sm-add-saved-properties 'order)
  (add-swapped-properties 'order)

  (add-hook 'sm-after-restore-hook window-order-compress)
  (add-hook 'iconify-window-hook window-order-pop)
  (add-hook 'viewport-moved-hook window-order-focus-most-recent))
