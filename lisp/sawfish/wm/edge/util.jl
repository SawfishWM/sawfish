;; edge-util.jl -- common utils for EdgeActions

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

    (compound-interface
      (structure-interface sawfish.wm.edge.subrs)
      (export flippers-activate
	      get-active-corner
	      get-active-edge))

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.custom
	  sawfish.wm.edge.subrs)

  (define-structure-alias edge-util sawfish.wm.edge.util)

  (define (flippers-activate enable)
    (if enable
        (progn
	  (recreate-flippers)
	  (unless (in-hook-p 'after-restacking-hook flippers-after-restacking)
	    (add-hook 'after-restacking-hook flippers-after-restacking))
	  (unless (in-hook-p 'randr-change-notify-hook recreate-flippers)
	    (add-hook 'randr-change-notify-hook recreate-flippers)))
      (disable-flippers)
      (if (in-hook-p 'after-restacking-hook flippers-after-restacking)
	(remove-hook 'after-restacking-hook flippers-after-restacking))
      (if (in-hook-p 'randr-change-notify-hook recreate-flippers)
	(remove-hook 'randr-change-notify-hook recreate-flippers))))

  (defcustom hot-spots-area 50
    "Lenght in px (in both x and y direction) wich is used as hot-spots-area."
    :type number
    :range (5 . 500)
    :group edge-actions)

   (define (get-active-corner)
    (let ((cursor-x (car (query-pointer)))
	  (cursor-y (cdr (query-pointer))))
      (cond ((or (and (< cursor-x hot-spots-area)
		    (<= cursor-y 1))
		 (and (<= cursor-x 1)
		    (< cursor-y hot-spots-area)))
		   'top-left)

	    ((or (and (> cursor-x (- (screen-width) hot-spots-area))
		    (<= cursor-y 1))
		 (and (>= cursor-x (- (screen-width) 1))
		    (< cursor-y hot-spots-area)))
		   'top-right)

	    ((or (and (> cursor-x (- (screen-width) hot-spots-area))
		    (>= cursor-y (- (screen-height) 1)))
		 (and (>= cursor-x (- (screen-width) 1))
		    (> cursor-y (- (screen-height) hot-spots-area))))
		   'bottom-right)

	    ((or (and (< cursor-x hot-spots-area)
		    (>= cursor-y (- (screen-height) 1)))
		 (and (<= cursor-x 1)
		    (> cursor-y (- (screen-height) hot-spots-area))))
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
