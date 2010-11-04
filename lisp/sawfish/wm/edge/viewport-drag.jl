;; viewport-drag.jl -- Smooth viewport motion with mouse

;; Copyright (C) 2008 David T. McWherter <udmcwher@mcs.drexel.edu>
;; Copyright (C) 2010 Christopher Roy Bratusek <zanghar@freent.de>

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

(define-structure sawfish.wm.edge.viewport-drag

    (export viewport-drag-activate)

    (open rep
          rep.system
          sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands.move-cursor
          sawfish.wm.viewport)

  (define-structure-alias viewport-drag sawfish.wm.edge.viewport-drag)

  (defcustom viewport-drag-distance 64
    "Amount to drag the viewport when the pointer hits the screen edge."
    :group edge-actions
    :type number
    :range (1 . nil))

  (defcustom viewport-drag-cursor-distance 32
    "Amount to pull back the cursor after dragging the viewport."
    :group edge-actions
    :type number
    :range (1 . nil))

  (define (viewport-drag-right)
    "Shifts the viewport `viewport-drag-distance' pixels to the right."
    (let ((dist viewport-drag-distance)
          (cdist viewport-drag-cursor-distance)
          (maxx (* (screen-width) (1- (car viewport-dimensions)))))
      (if
          (and (viewport-honor-workspace-edges)
               (> (+ dist viewport-x-offset) maxx))
          (setq dist (- maxx viewport-x-offset)))
      (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
      (move-cursor (- (min dist cdist)) 0)))

  (define (viewport-drag-left)
    "Shifts the viewport `viewport-drag-distance' pixels to the left."
    (let ((dist (- viewport-drag-distance))
          (cdist (- viewport-drag-cursor-distance))
          (minx 0))
      (if
          (and (viewport-honor-workspace-edges)
               (< (+ viewport-x-offset dist) minx))
          (setq dist (- minx viewport-x-offset)))
      (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
      (move-cursor (- (max dist cdist)) 0)))

  (define (viewport-drag-top)
    "Shifts the viewport `viewport-drag-distance' pixels up."
    (let ((dist (- viewport-drag-distance))
          (cdist (- viewport-drag-cursor-distance))
          (miny 0))
      (if
          (and (viewport-honor-workspace-edges)
               (< (+ viewport-y-offset dist) miny))
          (setq dist (- miny viewport-y-offset)))
      (set-viewport viewport-x-offset (+ viewport-y-offset dist))
      (move-cursor 0 (- (max dist cdist)))))

  (define (viewport-drag-bottom)
    "Shifts the viewport `viewport-drag-distance' pixels down."
    (let ((dist viewport-drag-distance)
          (cdist viewport-drag-cursor-distance)
          (maxy (* (screen-height) (1- (cdr viewport-dimensions)))))
      (if
          (and (viewport-honor-workspace-edges)
               (> (+ dist viewport-y-offset) maxy))
          (setq dist (- maxy viewport-y-offset)))
      (set-viewport viewport-x-offset (+ viewport-y-offset dist))
      (move-cursor 0 (- (min dist cdist)))))

  (define (viewport-drag-activate edge)
    "Called when a desktop flipper is triggered to shift the visible desktop."
    (case edge
      ((left) (viewport-drag-left))
      ((top) (viewport-drag-top))
      ((right) (viewport-drag-right))
      ((bottom) (viewport-drag-bottom)))))
