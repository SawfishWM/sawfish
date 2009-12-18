;; infinite-desktop.jl -- Smooth viewport motion with mouse

;; Copyright (C) 2008 David T. McWherter <udmcwher@mcs.drexel.edu>

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

(define-structure sawfish.wm.ext.infinite-desktop

    (export )

    (open rep
          rep.system
          sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands.move-cursor
          sawfish.wm.viewport
          sawfish.wm.util.prompt
          sawfish.wm.util.flippers
          sawfish.wm.ext.edge-flip)

  (define-structure-alias infinite-desktop sawfish.wm.ext.infinite-desktop)

  (defgroup infinite-desktop "Infinite Desktop"
    :group workspace)

  (defcustom infinite-desktop-p t
    "\"Infinite desktop\", or smooth viewport motion with mouse (Conflicts edge-flipping)."
    :group (workspace infinite-desktop)
    :after-set (lambda () (infinite-desktop.infinite-desktop))
    :type boolean)

  (defcustom infinite-desktop.move-distance 64
    "Amount to move the viewport when the pointer hits the screen edge."
    :group (workspace infinite-desktop)
    :type number
    :range (1 . nil))

  (defcustom infinite-desktop.move-cursor-distance 32
    "Amount to pull back the cursor after moving the viewport."
    :group (workspace infinite-desktop)
    :type number
    :range (1 . nil))

  (define (infinite-desktop.move-right)
    "Shifts the viewport `infinite-desktop.move-distance' pixels to the
right."
    (let ((dist infinite-desktop.move-distance)
          (cdist infinite-desktop.move-cursor-distance)
          (maxx (* (screen-width) (1- (car viewport-dimensions)))))
      (if
          (and (viewport-honor-workspace-edges)
               (> (+ dist viewport-x-offset) maxx))
          (setq dist (- maxx viewport-x-offset)))
      (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
      (move-cursor (- (min dist cdist)) 0)))

  (define (infinite-desktop.move-left)
    "Shifts the viewport `infinite-desktop.move-distance' pixels to the
left."
    (let ((dist (- infinite-desktop.move-distance))
          (cdist (- infinite-desktop.move-cursor-distance))
          (minx 0))
      (if
          (and (viewport-honor-workspace-edges)
               (< (+ viewport-x-offset dist) minx))
          (setq dist (- minx viewport-x-offset)))
      (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
      (move-cursor (- (max dist cdist)) 0)))

  (define (infinite-desktop.move-top)
    "Shifts the viewport `infinite-desktop.move-distance' pixels up."
    (let ((dist (- infinite-desktop.move-distance))
          (cdist (- infinite-desktop.move-cursor-distance))
          (miny 0))
      (if
          (and (viewport-honor-workspace-edges)
               (< (+ viewport-y-offset dist) miny))
          (setq dist (- miny viewport-y-offset)))
      (set-viewport viewport-x-offset (+ viewport-y-offset dist))
      (move-cursor 0 (- (max dist cdist)))))

  (define (infinite-desktop.move-bottom)
    "Shifts the viewport `infinite-desktop.move-distance' pixels down."
    (let ((dist infinite-desktop.move-distance)
          (cdist infinite-desktop.move-cursor-distance)
          (maxy (* (screen-height) (1- (cdr viewport-dimensions)))))
      (if
          (and (viewport-honor-workspace-edges)
               (> (+ dist viewport-y-offset) maxy))
          (setq dist (- maxy viewport-y-offset)))
      (set-viewport viewport-x-offset (+ viewport-y-offset dist))
      (move-cursor 0 (- (min dist cdist)))))

  (define (infinite-desktop.enter-flipper-hook w)
    "Called when a desktop flipper is triggered to shift the visible
desktop."
    (if infinite-desktop-p
        (cond ((eq w 'right) (infinite-desktop.move-right))
              ((eq w 'left) (infinite-desktop.move-left))
              ((eq w 'bottom) (infinite-desktop.move-bottom))
              ((eq w 'top) (infinite-desktop.move-top))
              (t (display-message "move-unknown")))))

  (define (infinite-desktop.infinite-desktop)
    "Turn on infinite-desktop if `infinite-desktop-p' is true."
    (if infinite-desktop-p
        (enable-flippers)))

  (unless batch-mode
    (add-hook 'enter-flipper-hook infinite-desktop.enter-flipper-hook)))
