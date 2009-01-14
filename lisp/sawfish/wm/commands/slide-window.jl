;; slide-window.jl -- simple code to move a window via the keyboard
;; $Id: slide-window.jl,v 1.8 2002/04/21 03:39:33 jsh Exp $

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

(define-structure sawfish.wm.commands.slide-window

    (export slide-window
	    slide-window-left
	    slide-window-right
	    slide-window-up
	    slide-window-down
	    slide-group-left
	    slide-group-right
	    slide-group-up
	    slide-group-down)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.util.groups)

  (defvar slide-window-increment 16
    "Number of pixels to move window in `slide-' commands.")

  (defvar before-slide-hook '()
    "Window hook called before a window is moved with the keyboard.")

  (define (slide-window w right down)
    (call-window-hook 'before-slide-hook w)
    (unless (window-get w 'fixed-position)
      (let ((coords (window-position w)))
	(move-window-to w (+ (car coords) right) (+ (cdr coords) down)))
      (call-window-hook 'after-move-hook w
			`((,@(if (/= right 0) '(horizontal))
			   ,@(if (/= down 0) '(vertical)))))))

;;; window commands

  (define (slide-window-left w)
    "Move the window `slide-window-increment' pixels to the left."
    (slide-window w (- slide-window-increment) 0))

  (define (slide-window-right w)
    "Move the window `slide-window-increment' pixels to the right."
    (slide-window w slide-window-increment 0))

  (define (slide-window-up w)
    "Move the window `slide-window-increment' pixels upwards."
    (slide-window w 0 (- slide-window-increment)))

  (define (slide-window-down w)
    "Move the window `slide-window-increment' pixels downwards."
    (slide-window w 0 slide-window-increment))

  ;;###autoload
  (define-command 'slide-window-left slide-window-left #:spec "%W")
  (define-command 'slide-window-right slide-window-right #:spec "%W")
  (define-command 'slide-window-up slide-window-up #:spec "%W")
  (define-command 'slide-window-down slide-window-down #:spec "%W")

;; group commands

  (define (slide-group-left w)
    "Move the window group `slide-window-increment' pixels to the left."
    (map-window-group slide-window-left w))

  (define (slide-group-right w)
    "Move the window group `slide-window-increment' pixels to the right."
    (map-window-group slide-window-right w))

  (define (slide-group-up w)
    "Move the window group `slide-window-increment' pixels upwards."
    (map-window-group slide-window-up w))

  (define (slide-group-down w)
    "Move the window group `slide-window-increment' pixels downwards."
    (map-window-group slide-window-down w))

  ;;###autoload
  (define-command 'slide-group-left slide-group-left #:spec "%W")
  (define-command 'slide-group-right slide-group-right #:spec "%W")
  (define-command 'slide-group-up slide-group-up #:spec "%W")
  (define-command 'slide-group-down slide-group-down #:spec "%W"))
