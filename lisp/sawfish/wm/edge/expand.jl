;; edge/expand.jl -- window-expansion for EdgeActions

;; Copyright (C) 2011 Christopher Roy Bratusek <zanghar@freenet.de>

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

(define-structure sawfish.wm.edge.expand

    (export expand-window-half-screen-horizontal
            expand-window-half-screen-vertical
            expand-action)

    (open rep
	  rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.commands.move-resize
	  sawfish.wm.state.maximize
	  sawfish.wm.edge.util)

  (define-structure-alias edge-expand sawfish.wm.edge.expand)

  (define (expand-window-half-screen-vertical side #!optional w)
    (unless w
      (setq w (input-focus)))
    (if (eq side 'left)
        (move-window-to-corner w 'top-left)
      (move-window-to-corner w 'top-right))
    (resize-window-frame-to w (/ (screen-width) 2) (window-height w))
    (maximize-fill-window-vertically w))

  (define (expand-window-half-screen-horizontal side #!optional w)
    (unless w
      (setq w (input-focus)))
    (if (eq side 'top)
        (move-window-to-corner w 'top-left)
      (move-window-to-corner w 'bottom-left))
    (resize-window-frame-to w (window-width w) (/ (screen-height) 2))
    (maximize-fill-window-horizontally w))

  (define (expand-action edge)
    (let ((w (input-focus))
          (func (case edge
                   ((left) expand-window-half-screen-vertical)
                   ((right) expand-window-half-screen-vertical)
                   ((top) expand-window-half-screen-horizontal)
                   ((bottom) expand-window-half-screen-horizontal))))
      (allow-events 'async-both)
      (fake-release-window)
      (funcall func edge w))))
