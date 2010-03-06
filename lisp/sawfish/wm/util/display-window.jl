;; display-window.jl -- activating windows
;;
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.util.display-window

    (export display-window-without-focusing
	    display-window)

    (open rep
	  sawfish.wm.misc
	  sawfish.wm.focus
	  sawfish.wm.workspace
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.viewport
	  sawfish.wm.util.stacking
	  sawfish.wm.state.shading
	  sawfish.wm.state.iconify
	  sawfish.wm.windows
	  sawfish.wm.util.window-order)

  (defcustom unshade-selected-windows nil
    "Unshade window when selected."
    :type boolean
    :group min-max)

;;; Activating windows

  ;; No Sawfish code except display-window calls this function.
  ;; But this is a command, too, so don't delete it.
  (define (display-window-without-focusing
	   w #!optional preferred-space #!key will-refocus)
    "Display the window, by choosing the workspace/viewport containing
the window W, and by showing it, i.e. the window is uniconified, or
unshaded if necessary."
    (when w
      (when (window-iconified-p w)
	(uniconify-window w))
      (when (or (not preferred-space)
		(not (window-in-workspace-p w preferred-space)))
	(setq preferred-space
	      (nearest-workspace-with-window w current-workspace)))
      (when preferred-space
	(select-workspace preferred-space will-refocus))
      (move-viewport-to-window w)
      (move-window-to-current-viewport w)
      (when (and unshade-selected-windows (window-get w 'shaded))
	(unshade-window w))))

  (define-command 'display-window-without-focusing
    display-window-without-focusing
    #:spec (lambda ()
	     (require 'sawfish.wm.util.prompt)
	     (list (prompt-for-window)))
    #:doc "Display the window, but don't focus. You're prompted to type in the name."
    )

  #|
    Currently, the optional argument `preferred-space' is not used
    anywhere in the Sawfish code, so I didn't describe it in the texi.
    Consider removing it, and instead implementing many other options
    with #!key.
  |#
  (define (display-window w #!optional preferred-space)
    "Do everything necessary to make window W ready for user,
i.e. display, focus, etc."
    (when w
      (display-window-without-focusing
       w preferred-space
       #:will-refocus (window-really-wants-input-p w))
      (activate-window w)))

  (define-command 'display-window display-window
    #:spec (lambda ()
	     (require 'sawfish.wm.util.prompt)
	     (list (prompt-for-window)))
    #:doc "Display the window. You're prompted to type in the name."))
