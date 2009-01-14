#| workspace-swapper.jl -- swapping window attributes per-workspace

   $Id: swapper.jl,v 1.6 2001/01/05 04:12:38 jsh Exp $

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.swapper ()

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.frames)

;;; default swappers

  (define (workspace-swap-out w space)
    (declare (unused space))
    (let ((props (mapcar (lambda (prop)
			   (cons prop (window-get w prop)))
			 workspace-local-properties)))
      ;; meta properties
      (if (not (window-get w 'sticky))
	  (list (cons 'position (window-absolute-position w))
		(cons 'viewport (window-viewport w))
		(cons 'dimensions (window-dimensions w))
		(cons 'properties props))
	(list (cons 'properties props)))))

  (define (workspace-swap-in w space alist)
    (declare (unused space))
    (let ((position (cdr (assq 'position alist)))
	  (viewport (cdr (assq 'viewport alist)))
	  (dimensions (cdr (assq 'dimensions alist)))
	  (properties (cdr (assq 'properties alist)))
	  (old-frame-style (window-get w 'current-frame-style)))
      (mapc (lambda (cell)
	      (window-put w (car cell) (cdr cell))) properties)
      (when (and position dimensions viewport)
	(move-resize-window-to w (+ (car position)
				    (* (car viewport) (screen-width))
				    (- viewport-x-offset))
			       (+ (cdr position)
				  (* (cdr viewport) (screen-height))
				  (- viewport-y-offset))
			       (car dimensions) (cdr dimensions)))
      (when old-frame-style
	;; special case this to help switching the default theme
	(window-put w 'current-frame-style (or (window-get w 'frame-style)
					       default-frame-style))
	(unless (eq (window-get w 'current-frame-style) old-frame-style)
	  (reframe-window w))))
    (call-hook 'after-workspace-swap-in-hook (list w space)))

  (add-hook 'workspace-swap-in-hook workspace-swap-in)
  (add-hook 'workspace-swap-out-hook workspace-swap-out))
