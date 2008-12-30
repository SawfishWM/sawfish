;; window-outline.jl -- drawing window `rubber-band' outlines
;; $Id: window-outline.jl,v 1.5 2000/07/28 21:51:54 john Exp $

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.util.window-outline

    (export draw-window-outline
	    erase-window-outline
	    define-window-outliner
	    autoload-window-outliner)

    (open rep
	  rep.util.autoloader)

  (define-structure-alias window-outline sawfish.wm.util.window-outline)

;;; mechanics

  (define (getter symbol) (get symbol 'window-outline))
  (define (setter symbol value) (put symbol 'window-outline value))

  (define define-window-outliner setter)
  (define autoload-window-outliner (make-autoloader getter setter))
  (define window-outliner (autoloader-ref getter))

;;; entry points

  (define (draw-window-outline mode x y width height)
    "Draw an outline of a window of dimensions (WIDTH, HEIGHT) at position
(X, Y) relative to the root window.

MODE is a symbol defining the type of outline drawn, currently it may
only be `box' for a 3x3 grid.

Use the `erase-window-outline' to erase the grid. Also note that since
these functions draw directly on the root window the server should be
grabbed until the outline is erased."

    (let ((fun (window-outliner mode)))
      (when fun
	(fun x y width height))))

  (defun erase-window-outline (mode x y width height)
    "Erase a previously drawn outline of a window of dimensions (WIDTH, HEIGHT)
at position (X, Y) relative to the root window. See `draw-window-outline'.

MODE is a symbol defining the type of outline drawn, currently it may
only be `box' for a 3x3 grid."

    (draw-window-outline mode x y width height))

;;; default outline types

  (define (draw-box-outline x y width height)
    (require 'sawfish.wm.util.x)
    (let ((gc (x-create-root-xor-gc))
	  (x-step (/ width 3))
	  (y-step (/ height 3)))
      (do ((i 0 (1+ i)))
	  ((= i 4))
	(x-draw-line 'root gc
		     (cons x (inexact->exact (round (+ y (* y-step i)))))
		     (cons (+ x width) (inexact->exact
					(round (+ y (* y-step i))))))
	(x-draw-line 'root gc
		     (cons (inexact->exact (round (+ x (* x-step i)))) y)
		     (cons (inexact->exact
			    (round (+ x (* x-step i)))) (+ y height))))
      (x-destroy-gc gc)))

  (define-window-outliner 'box draw-box-outline)

  ;; this is quite flickery.. double buffering doesn't work either..
  (define (draw-solid-outline x y width height)
    (require 'sawfish.wm.util.x)
    (let ((gc (x-create-root-xor-gc)))
      (x-fill-rectangle 'root gc (cons x y) (cons width height))
      (x-destroy-gc gc)))

  (define-window-outliner 'solid draw-solid-outline))
