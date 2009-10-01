;; viewport-linear.jl -- linear address of viewports

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

;; Commentary:

;; This was originally written by Eric Kidd <eric.kidd@pobox.com>

;; I've rewritten it to take advantage of lexical scope, and changed
;; some names

(define-structure sawfish.wm.commands.viewport-linear

    (export set-viewport-linear
	    define-linear-viewport-commands)

    (open rep
	  sawfish.wm.events
	  sawfish.wm.commands
	  sawfish.wm.viewport)

  (define-structure-alias viewport-linear sawfish.wm.commands.viewport-linear)

  (defvar viewport-linear-last 9)

  ;; Set the viewport using linear addressing
  (define (set-viewport-linear index)
    (set-screen-viewport (mod index (car viewport-dimensions))
			 (quotient index (car viewport-dimensions))))

  ;;###autoload
  (define-command 'set-viewport-linear set-viewport-linear
    #:spec "NIndex:"
    #:type `(and (labelled ,(_ "Index:") (number 0)))
    #:class 'viewport)

  ;; Move window to viewport INDEX using linear addressing
  (define (set-window-viewport-linear index)
    (set-window-viewport (current-event-window)
			 (mod index (car viewport-dimensions))
			 (quotient index (car viewport-dimensions))))

  ;;###autoload
  (define-command 'set-window-viewport-linear set-window-viewport-linear
    #:spec "NIndex:"
    #:type `(and (labelled ,(_ "Index:") (number 0)))
    #:class 'viewport)

  (define (define-linear-viewport-commands index)
    (let ((fn (lambda (base)
		(intern (format nil "%s:%d" base (1+ index))))))
      (define-command (fn "set-viewport-linear")
	(lambda ()
	  "Move to the specified linear viewport."
	  (set-viewport-linear index)))
      (define-command (fn "set-window-viewport-linear")
	(lambda ()
	  "Move the current window to the specified linear viewport."
	  (set-window-viewport-linear index)))
      (put (fn "set-viewport-linear") 'deprecated-command t)
      (put (fn "set-window-viewport-linear") 'deprecated-command t)))

  (do ((i 0 (1+ i)))
      ((= i viewport-linear-last))
    (define-linear-viewport-commands i)))

;;###autoload (autoload-command 'set-viewport-linear:1 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:1 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:2 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:2 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:3 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:3 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:4 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:4 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:5 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:5 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:6 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:6 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:7 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:7 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:8 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:8 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-viewport-linear:9 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
;;###autoload (autoload-command 'set-window-viewport-linear:9 'sawfish.wm.commands.viewport-linear #:class 'deprecated)
