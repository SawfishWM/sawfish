;; hot-spots.jl -- Invoke lisp functions when hitting the screen-edge

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.edge.hot-spots

    (export hot-spot-invoke
            hot-move-invoke)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.custom
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.workspace)

  (define-structure-alias hot-spots sawfish.wm.edge.hot-spots)

  (defvar left-edge-function nil
    "The function launched when hitting the left-edge.")

  (defvar top-left-corner-function nil
    "The function launched when hitting the top-left-corner.")

  (defvar top-edge-function nil
    "The function launched when hitting the top-edge.")

  (defvar top-right-corner-function nil
    "The function launched when hitting the top-right-corner.")

  (defvar right-edge-function nil
    "The function launched when hitting the right-edge.")

  (defvar bottom-right-corner-function nil
    "The function launched when hitting the bottom-right-corner.")

  (defvar bottom-edge-function nil
    "The function launched when hitting the bottom-edge.")

  (defvar bottom-left-corner-function nil
    "The function launched when hitting the bottom-left-corner.")

  (define hot-spot-timer nil)

  (define (hot-spot-invoke spot)
    (unless hot-spot-timer
      (let ((func (case spot
                    ((top-left)
                     top-left-corner-function)
                    ((top-right)
                     top-right-corner-function)
                    ((bottom-right)
                     bottom-right-corner-function)
                    ((bottom-left)
                     bottom-left-corner-function)
                    ((left)
                     left-edge-function)
                    ((top)
                     top-edge-function)
                    ((right)
                     right-edge-function)
                    ((bottom)
                     bottom-edge-function))))
        (if (functionp func)
            (progn
              (call-hook 'before-edge-action-hook (list 'hot-spot spot nil))
	      (setq hot-spot-timer
		    (make-timer (lambda ()
				  (setq hot-spot-timer nil)
				  (funcall func)
				  (call-hook 'after-edge-action-hook (list 'hot-spot spot nil)))
				(quotient hot-spot-delay 1000)
				(mod hot-spot-delay 1000))))
          (when func
            ;; non-nil, but not a function?
            (error "In hot-spot, your configuration of spot `%s' is wrong; it should be a function." spot))))))

  (defvar left-edge-move-function nil
    "The function launched when hitting the left-edge.")

  (defvar top-edge-move-function nil
    "The function launched when hitting the top-edge.")

  (defvar right-edge-move-function nil
    "The function launched when hitting the right-edge.")

  (defvar bottom-edge-move-function nil
    "The function launched when hitting the bottom-edge.")

  (define hot-move-timer nil)

  (define (hot-move-invoke spot)
    (unless hot-move-timer
      (let ((func (case spot
                    ((left)
                     left-edge-move-function)
                    ((top)
                     top-edge-move-function)
                    ((right)
                     right-edge-move-function)
                    ((bottom)
                     bottom-edge-move-function)))
            (win (input-focus)))
        (if (functionp func)
            (progn
	      (call-hook 'before-edge-action-hook (list 'hot-move spot t))
	      (setq hot-move-timer
		    (make-timer (lambda ()
				  (setq hot-move-timer nil)
				  (allow-events 'async-both)
				  (fake-release-window)
				  (funcall func win)
				  (call-hook 'after-edge-action-hook (list 'hot-move spot t)))
				(quotient hot-spot-delay 1000)
				(mod hot-spot-delay 1000))))
          (when func
            ;; non-nil, but not a function?
            (error "In hot-move, your configuration of spot `%s' is wrong; it should be a function." spot)))))))
