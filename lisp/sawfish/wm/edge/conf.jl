;; edge/conf.jl -- configuration for EdgeActions

;; Copyright (C) 2011 Christopher Roy Bratusek <nano@jpberlin.de>

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.edge.conf

    (export)

    (open rep
	  sawfish.wm.custom
	  sawfish.wm.edge.actions)

  (define-structure-alias edge-conf sawfish.wm.edge.conf)

  (defvar before-edge-action-hook nil)
  (defvar after-edge-action-hook nil)

  (defcustom edge-actions-enabled nil
    "Activate edges to perform various actions."
    :group edge-actions
    :type boolean
    :after-set (lambda () (activate-edges-after-set)))

  (defcustom left-edge-action 'none/hot-spot
    "Action for the left screen-edge."
    :group edge-actions
    :type (choice none/hot-spot viewport-drag flip-workspace
		  flip-viewport show-desktop))

  (defcustom left-edge-move-action 'none/hot-move
    "Action for the left screen-edge while moving a window."
    :group edge-actions
    :type  (choice none/hot-move viewport-drag flip-workspace
		   flip-viewport expand-window maximize-window kill-window
		   iconify-window move-window-viewport move-window-workspace))

  (defcustom top-edge-action 'none/hot-spot
    "Action for the top screen-edge."
    :group edge-actions
    :type (choice none/hot-spot viewport-drag flip-workspace
		  flip-viewport show-desktop))

  (defcustom top-edge-move-action 'none/hot-move
    "Action for the top screen-edge while moving a window."
    :group edge-actions
    :type  (choice none/hot-move viewport-drag flip-workspace
		   flip-viewport expand-window maximize-window kill-window
		   iconify-window move-window-viewport move-window-workspace))

  (defcustom right-edge-action 'none/hot-spot
    "Action for the right screen-edge."
    :group edge-actions
    :type (choice none/hot-spot viewport-drag flip-workspace
		  flip-viewport show-desktop))

  (defcustom right-edge-move-action 'none/hot-move
    "Action for the right screen-edge while moving a window."
    :group edge-actions
    :type  (choice none/hot-move viewport-drag flip-workspace
		   flip-viewport expand-window maximize-window kill-window
		   iconify-window move-window-viewport move-window-workspace))

  (defcustom bottom-edge-action 'none/hot-spot
    "Action for the bottom screen-edge."
    :group edge-actions
    :type (choice none/hot-spot viewport-drag flip-workspace
		  flip-viewport show-desktop))

  (defcustom bottom-edge-move-action 'none/hot-move
    "Action for the bottom screen-edge while moving a window."
    :group edge-actions
    :type  (choice none/hot-move viewport-drag flip-workspace
		   flip-viewport expand-window maximize-window kill-window
		   iconify-window move-window-viewport move-window-workspace))

  (defcustom edge-flip-delay 250
    "Delay (in milliseconds) of flipping of viewport / workspace."
    :group edge-actions
    :type number
    :range (0 . nil))

  (defcustom hot-spot-delay 250
    "Delay (in milliseconds) before activating a hot-spot."
    :group edge-actions
    :type number
    :range (0 . nil))

  (defcustom hot-spots-corner-length 50
    "The size portion of the screen-border recognized as \"corners\",
in pixels. Applies to both x and y direction."
    :type (range (5 . 500))
    :group edge-actions)

  (defcustom viewport-drag-distance 64
    "Amount to drag the viewport (in pixel) each time the pointer hits the
screen edge."
    :group edge-actions
    :type number
    :range (1 . nil))

  (defcustom viewport-drag-cursor-distance 32
    "Amount to pull back the cursor (in pixel) after dragging the viewport."
    :group edge-actions
    :type number
    :range (1 . nil)))
