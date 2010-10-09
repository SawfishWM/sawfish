;; window-order.jl -- keep track of recently accessed windows

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

  (define (window-order-most-recent #!key (windows 0))
    "Return the most-recently focused window in the current workspace. If the
WINDOWS argument is given it should be a list of windows, in this case the
function will restrict its search to the elements of this list."
    (let loop ((rest (window-order current-workspace nil)))
      (cond ((null rest) nil)
	    ((or (window-get (car rest) 'never-focus)
		 (and (listp windows) (not (memq (car rest) windows))))
	     (loop (cdr rest)))
	    (t (car rest)))))

  (define (window-order-focus-most-recent)
    (set-input-focus (window-order-most-recent)))

  (define (on-viewport-change)
    ;; The problem is that any sticky windows that have been focused once
    ;; will _always_ rise to the top of the order when switching viewports
    ;; (since the topmost window is _always_ focused when entering a new
    ;; workspace). The hacky solution is to remove the order of any sticky
    ;; windows
    (let ((order (window-order current-workspace)))
      (mapc (lambda (w)
	      (when (window-get w 'sticky-viewport)
		(window-put w 'order nil))) order))
    (unless (eq focus-mode 'enter-exit)
      (window-order-focus-most-recent)))

  (sm-add-saved-properties 'order)
  (add-swapped-properties 'order)

  (add-hook 'sm-after-restore-hook window-order-compress)
  (add-hook 'iconify-window-hook window-order-pop)
  (add-hook 'viewport-moved-hook on-viewport-change))
