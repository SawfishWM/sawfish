;; edge-util.jl -- common utils for EdgeFlip/InfiniteDesktop/HotSpots

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

(define-structure sawfish.wm.edge.util

    (export flippers-activate)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.edge.flippers)

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
	(remove-hook 'randr-change-notify-hook recreate-flippers)))))
