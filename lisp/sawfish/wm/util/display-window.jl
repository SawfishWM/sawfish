#| display-window.jl -- activating windows

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.util.display-window

    (export display-window-without-focusing
	    display-window)

    (open rep
	  sawfish.wm.misc
	  sawfish.wm.workspace
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.viewport
	  sawfish.wm.stacking
	  sawfish.wm.state.shading
	  sawfish.wm.state.iconify
	  sawfish.wm.windows
	  sawfish.wm.util.window-order)

  (defcustom display-window:uniconify-to-current-workspace t
    "Windows uniconify to the current workspace when they are selected."
    :type boolean
    :group (min-max iconify))

  (defcustom unshade-selected-windows nil
    "Unshade selected windows."
    :type boolean
    :group misc)

  (defcustom raise-selected-windows t
    "Raise selected windows (normally by the Windows menu)."
    :type boolean
    :group misc)

  (defcustom warp-to-selected-windows t
    "Warp the pointer to selected windows."
    :type boolean
    :group misc)

;;; Activating windows

  (define (display-window-without-focusing w &optional preferred-space)
    "Display the workspace/viewport containing the window W."
    (when w
      (let ((uniconify-to-current-workspace
	     display-window:uniconify-to-current-workspace))
	(uniconify-window w)
	(when (or (not preferred-space)
		  (not (window-in-workspace-p w preferred-space)))
	  (setq preferred-space
		(nearest-workspace-with-window w current-workspace)))
	(when preferred-space
	  (select-workspace preferred-space))
	(move-viewport-to-window w)
	(when (and unshade-selected-windows (window-get w 'shaded))
	  (unshade-window w)))))

  (define-command 'display-window-without-focusing
    display-window-without-focusing
    (lambda ()
      (require 'sawfish.wm.util.prompt)
      (list (prompt-for-window))))

  (define (display-window w &optional preferred-space)
    "Display the workspace containing the window W, then focus on W."
    (when w
      (display-window-without-focusing w preferred-space)
      (when raise-selected-windows
	(raise-window w))
      (when warp-to-selected-windows
	(warp-cursor-to-window w))
      (when (window-really-wants-input-p w)
	(set-input-focus w))
      (window-order-push w)))

  (define-command 'display-window display-window
    (lambda ()
      (require 'sawfish.wm.util.prompt)
      (list (prompt-for-window)))))
