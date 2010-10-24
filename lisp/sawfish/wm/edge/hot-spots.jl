;; hot-spots.jl 2.1.0 -- perform actions when hitting the screen-edge

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

(define-structure sawfish.wm.edge.hot-spots

    (export hot-spots-activate)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.custom
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.workspace
	  sawfish.wm.edge.util)

  (define-structure-alias hot-spots sawfish.wm.edge.hot-spots)

  (defvar left-edge-program nil
    "The program launched when hitting the left-edge.")

  (defvar top-left-corner-program nil
    "The program launched when hitting the top-left-corner.")

  (defvar top-edge-program nil
    "The program launched when hitting the top-edge.")

  (defvar top-right-corner-program nil
    "The program launched when hitting the top-right-corner.")

  (defvar right-edge-program nil
    "The program launched when hitting the right-edge.")

  (defvar bottom-right-corner-program nil
    "The program launched when hitting the bottom-right-corner.")

  (defvar bottom-edge-program nil
    "The program launched when hitting the bottom-edge.")

  (defvar bottom-left-corner-program nil
    "The program launched when hitting the bottom-left-corner.")

  (defcustom hot-spots-enable nil
    "Whether to enable sensitive spots on the screen-edge."
    :type boolean
    :group (workspace hot-spot)
    :after-set (lambda () hot-spots-activate))

  (defcustom hot-spot-delay 150
    "Milliseconds to delay before activating hot-spot."
    :type number
    :group (workspace hot-spot))

  (define (hot-spots-activate)
    (if hot-spots-enable
        (unless (in-hook-p 'enter-flipper-hook hot-spots-hook)
	  (add-hook 'enter-flipper-hook hot-spots-hook)
	(flippers-activate t))
      (if (in-hook-p 'enter-flipper-hook hot-spots-hook)
	(remove-hook 'enter-flipper-hook hot-spots-hook))
      (flippers-activate nil)))

  (define hs-timer nil)

  (define (hot-spots-hook)
    (if (<= hot-spot-delay 0)
      (hot-spot-call)
      (setq hs-timer (make-timer (lambda ()
			(setq hs-timer nil)
			(hot-spot-call))
			  (quotient hot-spot-delay 1000)
			  (mod hot-spot-delay 1000)))))

  (define (hot-spot-call)
    (cond ((eq (get-active-corner) 'top-left)
	   (unless (eq top-left-corner-program nil)
	     (if (functionp top-left-corner-program)
	         (funcall top-left-corner-program)
	       (system (concat top-left-corner-program " &")))))

	  ((eq (get-active-corner) 'top-right)
	   (unless (eq top-right-corner-program nil)
	     (if (functionp top-right-corner-program)
	          (funcall top-right-corner-program)
	        (system (concat top-right-corner-program " &")))))

	  ((eq (get-active-corner) 'bottom-right)
	   (unless (eq bottom-right-corner-program nil)
	     (if (functionp bottom-right-corner-program)
	           (funcall bottom-right-corner-program)
	         (system (concat bottom-right-corner-program " &")))))

	  ((eq (get-active-corner) 'bottom-left)
	   (unless (eq bottom-left-corner-program nil)
	     (if (functionp bottom-left-corner-program)
	           (funcall bottom-left-corner-program)
	         (system (concat bottom-left-corner-program " &")))))

	  ((eq (get-active-edge) 'left)
           (unless (eq left-edge-program nil)
	     (if (functionp left-edge-program)
                  (funcall left-edge-program)
                (system (concat left-edge-program " &")))))

          ((eq (get-active-edge) 'top)
	   (unless (eq top-edge-program nil)
             (if (functionp top-edge-program)
                   (funcall top-edge-program)
                (system (concat top-edge-program " &")))))

          ((eq (get-active-edge) 'right)
	   (unless (eq right-edge-program nil)
             (if (functionp right-edge-program)
                   (funcall right-edge-program)
                 (system (concat right-edge-program " &")))))

          ((eq (get-active-edge) 'bottom)
	   (unless (eq bottom-edge-program nil)
             (if (functionp bottom-edge-program)
                   (funcall bottom-edge-program)
                 (system (concat bottom-edge-program " &")))))))

  (unless batch-mode
    (hot-spots-activate)))
