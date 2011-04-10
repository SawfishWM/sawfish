;; edge/util.jl -- common utils for EdgeActions

;; Copyright (C) 2010 Christopher Roy Bratusek <zanghar@freenet.de>

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

(define-structure sawfish.wm.edge.util

    (export activate-flippers
	    get-active-corner
	    get-active-edge)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.custom
	  sawfish.wm.edge.subrs)

  (define-structure-alias edge-util sawfish.wm.edge.util)

  (define (activate-flippers enable)
    (if enable
        (progn
	  (recreate-flippers)
	  (unless (in-hook-p 'after-restacking-hook raise-flippers)
	    (add-hook 'after-restacking-hook raise-flippers))
	  (unless (in-hook-p 'randr-change-notify-hook recreate-flippers)
	    (add-hook 'randr-change-notify-hook recreate-flippers)))
      (destroy-flippers)
      (remove-hook 'after-restacking-hook raise-flippers)
      (remove-hook 'randr-change-notify-hook recreate-flippers)))

  (define (get-active-corner)
    (let ((cursor-x (car (query-pointer)))
	  (cursor-y (cdr (query-pointer))))
      (cond ((or (and (< cursor-x hot-spots-corner-length)
		      (<= cursor-y 1))
		 (and (<= cursor-x 1)
		      (< cursor-y hot-spots-corner-length)))
	     'top-left)

	    ((or (and (> cursor-x (- (screen-width) hot-spots-corner-length))
		      (<= cursor-y 1))
		 (and (>= cursor-x (- (screen-width) 1))
		      (< cursor-y hot-spots-corner-length)))
	     'top-right)

	    ((or (and (> cursor-x (- (screen-width) hot-spots-corner-length))
		      (>= cursor-y (- (screen-height) 1)))
		 (and (>= cursor-x (- (screen-width) 1))
		      (> cursor-y (- (screen-height) hot-spots-corner-length))))
	     'bottom-right)

	    ((or (and (< cursor-x hot-spots-corner-length)
		      (>= cursor-y (- (screen-height) 1)))
		 (and (<= cursor-x 1)
		      (> cursor-y (- (screen-height) hot-spots-corner-length))))
	     'bottom-left))))

  (define (get-active-edge)
    (let ((cursor (query-pointer)))
      (cond ((zerop (car cursor))
	     'left)
	    ((= (car cursor) (1- (screen-width)))
	     'right)
	    ((zerop (cdr cursor))
	     'top)
	    ((= (cdr cursor) (1- (screen-height)))
	     'bottom)))))
