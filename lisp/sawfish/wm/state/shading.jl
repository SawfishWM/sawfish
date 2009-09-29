;; shading.jl -- window ``shading''

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

(define-structure sawfish.wm.state.shading

    (export window-shaded-p
	    shade-window
	    unshade-window
	    toggle-window-shaded)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.frames
	  sawfish.wm.session.init
	  sawfish.wm.workspace
	  sawfish.wm.util.stacking
	  sawfish.wm.menus)

  (define-structure-alias shading sawfish.wm.state.shading)

  (defvar raise-windows-when-unshaded nil
    "Raise windows when they are unshaded.")

  (define (window-shaded-p w) (window-get w 'shaded))

  (define (shade-window w)
    "Display only the title bar of the window."
    (unless (window-get w 'shaded)
      (window-put w 'shaded t)
      (window-put w 'hide-client t)
      (reframe-window w)
      (call-window-hook 'shade-window-hook w)
      (call-window-hook 'window-state-change-hook w (list '(shaded)))))

  (define (unshade-window w)
    "If the window is shaded (see `shade-window'), restore it to it's usual
state."
    (when (window-get w 'shaded)
      (window-put w 'shaded nil)
      (window-put w 'hide-client nil)
      (reframe-window w)
      (call-window-hook 'unshade-window-hook w)
      (call-window-hook 'window-state-change-hook w (list '(shaded)))
      (when raise-windows-when-unshaded
	(raise-window* w))))

  (define (toggle-window-shaded w)
    "Toggle the shaded (only the title bar is displayed) state of the window."
    (if (window-get w 'shaded)
	(unshade-window w)
      (shade-window w)))

  (define-command 'shade-window shade-window #:spec "%W")
  (define-command 'unshade-window unshade-window #:spec "%W")
  (define-command 'toggle-window-shaded toggle-window-shaded #:spec "%W")

;;; displaying

  (define (shaded-frame-type-mapper w type)
    (if (window-get w 'shaded)
	(case type
	  ((default shaped) 'shaded)
	  ((transient shaped-transient) 'shaded-transient)
	  ((utility shaped-utility) 'shaded-utility)
	  ((menu shaped-menu) 'shaded-menu)
	  ((toolbar shaped-toolbar) 'shaded-toolbar)
	  (t type))
      type))

  (define-frame-type-mapper shaded-frame-type-mapper)

;;; hooks

  (define (shading-add-window w)
    (when (window-get w 'shaded)
      (window-put w 'hide-client t)))

  (add-hook 'add-window-hook shading-add-window t)

  (define (shading-after-swap-in w space)
    (declare (unused space))
    (unless (eq (window-get w 'shaded)
		(window-get w 'hide-client))
      (window-put w 'hide-client (window-get w 'shaded))
      (reframe-window w)))

  (add-hook 'after-workspace-swap-in-hook shading-after-swap-in)

  (sm-add-saved-properties 'shaded)
  (add-swapped-properties 'shaded)

  (add-window-menu-toggle (_ "S_haded") 'toggle-window-shaded window-shaded-p))
