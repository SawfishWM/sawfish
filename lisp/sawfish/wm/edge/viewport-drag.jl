;; viewport-drag.jl -- Smooth viewport motion with mouse

;; Based on dtm-infinite-desktop.jl in public domain, written by
;; David T. McWherter <udmcwher@mcs.drexel.edu>
;; 
;; Copyright (C) 2010 Christopher Roy Bratusek <zanghar@freent.de>

;; This file is part of sawfish.

;; viewport-drag is public domain. It's free in any mean for anyone.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

(define-structure sawfish.wm.edge.viewport-drag

    (export viewport-drag-invoke)

    (open rep
          rep.system
          sawfish.wm.misc
          sawfish.wm.custom
          sawfish.wm.commands.move-cursor
          sawfish.wm.viewport)

  (define-structure-alias viewport-drag sawfish.wm.edge.viewport-drag)

  (define (drag-right while-moving)
    "Shifts the viewport `viewport-drag-distance' pixels to the right."
    (let ((dist viewport-drag-distance)
          (cdist viewport-drag-cursor-distance)
          (maxx (* (screen-width) (1- (car viewport-dimensions)))))
      (if
          (and (viewport-honor-workspace-edges)
               (> (+ dist viewport-x-offset) maxx))
          (setq dist (- maxx viewport-x-offset)))
      (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
      (move-cursor (- (min dist cdist)) 0)

      (call-hook 'after-edge-action-hook (list 'viewport-drag 'right while-moving))))

  (define (drag-left while-moving)
    "Shifts the viewport `viewport-drag-distance' pixels to the left."
    (let ((dist (- viewport-drag-distance))
          (cdist (- viewport-drag-cursor-distance))
          (minx 0))
      (if
          (and (viewport-honor-workspace-edges)
               (< (+ viewport-x-offset dist) minx))
          (setq dist (- minx viewport-x-offset)))
      (set-viewport (+ viewport-x-offset dist) viewport-y-offset)
      (move-cursor (- (max dist cdist)) 0)

      (call-hook 'after-edge-action-hook (list 'viewport-drag 'left while-moving))))

  (define (drag-up while-moving)
    "Shifts the viewport `viewport-drag-distance' pixels up."
    (let ((dist (- viewport-drag-distance))
          (cdist (- viewport-drag-cursor-distance))
          (miny 0))
      (if
          (and (viewport-honor-workspace-edges)
               (< (+ viewport-y-offset dist) miny))
          (setq dist (- miny viewport-y-offset)))
      (set-viewport viewport-x-offset (+ viewport-y-offset dist))
      (move-cursor 0 (- (max dist cdist)))

      (call-hook 'after-edge-action-hook (list 'viewport-drag 'up while-moving))))

  (define (drag-down while-moving)
    "Shifts the viewport `viewport-drag-distance' pixels down."
    (let ((dist viewport-drag-distance)
          (cdist viewport-drag-cursor-distance)
          (maxy (* (screen-height) (1- (cdr viewport-dimensions)))))
      (if
          (and (viewport-honor-workspace-edges)
               (> (+ dist viewport-y-offset) maxy))
          (setq dist (- maxy viewport-y-offset)))
      (set-viewport viewport-x-offset (+ viewport-y-offset dist))
      (move-cursor 0 (- (min dist cdist)))

      (call-hook 'after-edge-action-hook (list 'viewport-drag 'down while-moving))))

  (define (viewport-drag-invoke edge while-moving)
    (call-hook 'before-edge-action-hook (list 'viewport-drag edge while-moving))
    (case edge
      ((left) (drag-left while-moving))
      ((top) (drag-up while-moving))
      ((right) (drag-right while-moving))
      ((bottom) (drag-down while-moving)))))
