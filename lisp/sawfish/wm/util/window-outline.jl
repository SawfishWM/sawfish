;; window-outline.jl -- drawing window `rubber-band' outlines

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
	  rep.util.autoloader
	  sawfish.wm.misc)

  (define-structure-alias window-outline sawfish.wm.util.window-outline)

;;; mechanics

  (define (getter symbol) (get symbol 'window-outline))
  (define (setter symbol value) (put symbol 'window-outline value))

  (define define-window-outliner setter)
  (define autoload-window-outliner (make-autoloader getter setter))
  (define window-outliner (autoloader-ref getter))

;;; entry points

  (define (draw-window-outline mode x y width height)
    "Draw an outline of a window of dimensions (WIDTH, HEIGHT) at
position (X, Y) relative to the root window.

MODE is a symbol defining the type of outline drawn, currently it may
only be `box' for a 3x3 grid.

Use the `erase-window-outline' to erase the grid. Also note that since
these functions draw directly on the root window the server should be
grabbed until the outline is erased."

    (let ((fun (window-outliner mode)))
      (when fun
	(fun x y width height))))

  (defun erase-window-outline (mode x y width height)
    "Erase a previously drawn outline of a window of dimensions (WIDTH,
HEIGHT) at position (X, Y) relative to the root window. See
`draw-window-outline'. MODE is a symbol defining the type of outline
drawn, currently it may only be `box' for a 3x3 grid."

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

  (define-window-outliner 'solid draw-solid-outline)

  (define (draw-cross-outline x y width height)
    (require 'sawfish.wm.util.x)
      (let
        ((gc (x-create-root-xor-gc))
         (ul (cons x y))                      ; upper left
         (ur (cons (+ x width) y))            ; upper right
         (ll (cons x (+ y height)))           ; lower left
         (lr (cons (+ x width) (+ y height))) ; lower right
        )
      ; perimeter outline
      (x-draw-line 'root gc ul ur)
      (x-draw-line 'root gc ll lr)
      (x-draw-line 'root gc ul ll)
      (x-draw-line 'root gc ur lr)
      ; cross
      (x-draw-line 'root gc ul lr)
      (x-draw-line 'root gc ur ll)
      (x-destroy-gc gc)))

  (define-window-outliner 'cross draw-cross-outline)

  (define (draw-elliptical-outline x y width height)
    (require 'sawfish.wm.util.x)
    (let
      ((gc (x-create-root-xor-gc))
       (height-prime (inexact->exact (floor (* height 1.4142))))
       (width-prime  (inexact->exact (floor (* width 1.4142))))
      )
    ; draw the circumscribed ellipse (the outside one)
    (x-draw-arc 'root gc (cons (inexact->exact (- x (floor (/ width 4.8))))
			       (inexact->exact (- y (floor (/ height 4.8)))))
		(cons width-prime height-prime)
		(cons 0 (* 360 64)))
    ; draw the inscribed ellipse (the inside one)
    (x-draw-arc 'root gc (cons x y)
		(cons width height)
		(cons 0 (* 360 64)))
    (x-destroy-gc gc)))

  (define-window-outliner 'elliptical draw-elliptical-outline)

  (define (draw-draft-line rw gc pta ptb dim-p arrow-p)
    (require 'sawfish.wm.util.x)
    (let ((pta-x (car pta)) ; recover the components
	(pta-y (cdr pta))
	(ptb-x (car ptb))
	(ptb-y (cdr ptb))
	(delta-x (- (car ptb) (car pta))) ; figure out the difference
	(delta-y (- (cdr ptb) (cdr pta)))
	(xah 4)  ; cope with different window scales
	(yah 3)  ; to ensure arrow heads look the same
	(x-dim-offset 5) ; how far to offset the dimension from the
	(y-dim-offset 5) ; draft line
	)
    ; first off, we know we are going to draw the line, always
    (x-draw-line rw gc pta ptb)
    ; now figure out if we drawing vertically or horizontally
    (if (= pta-x ptb-x)
	(progn ; vertical
	 (if dim-p
	     (x-draw-text rw gc (cons (+ pta-x x-dim-offset)
					(+ pta-y (quotient delta-y 2))
					)
			    (format nil "%d" delta-y)))
	 (if arrow-p
	     (progn
	       (x-draw-line rw gc pta (cons (+ pta-x xah) (+ pta-y yah)))
	       (x-draw-line rw gc pta (cons (- pta-x xah) (+ pta-y yah)))
	       (x-draw-line rw gc ptb (cons (+ ptb-x xah) (- ptb-y yah)))
	       (x-draw-line rw gc ptb (cons (- ptb-x xah) (- ptb-y yah)))
	      ))
	 )
	(progn ; horizontal
         (if dim-p
	   (x-draw-text rw gc (cons (+ pta-x (quotient delta-x 2))
				      (- pta-y y-dim-offset)
				      )
			    (format nil "%d" delta-x)))
         (if arrow-p
	     (progn
	       (x-draw-line rw gc pta (cons (+ pta-x xah) (+ pta-y yah)))
	       (x-draw-line rw gc pta (cons (+ pta-x xah) (- pta-y yah)))
	       (x-draw-line rw gc ptb (cons (- ptb-x xah) (+ ptb-y yah)))
	       (x-draw-line rw gc ptb (cons (- ptb-x xah) (- ptb-y yah)))
	       ))
        ))))

  (define (draw-draft-outline x y width height)
    (require 'sawfish.wm.util.x)
    (let
      ((gc (x-create-root-xor-gc))
       ; window Upper (Left Middle Right)
       (ul (cons x y))
       (um (cons (+ x (quotient width 2)) y))
       (ur (cons (+ x width) y))

       ; window Middle (Left Right)
       (ml (cons x           (+ y (quotient height 2))))
       (mr (cons (+ x width) (+ y (quotient height 2))))

       ; window Lower (Left Middle Right)
       (ll (cons x (+ y height)))
       (lm (cons (+ x (quotient width 2)) (+ y height)))
       (lr (cons (+ x width) (+ y height)))

       ; window Screen (Left Right Top Bottom)
       (sl (cons 0              (+ y (quotient height 2))))
       (sr (cons (screen-width) (+ y (quotient height 2))))
       (st (cons (+ x (quotient width 2)) 0))
       (sb (cons (+ x (quotient width 2)) (screen-height)))

       (offset 3) ; how much to offset the guidelines from the window
       )
    ; perimeter outline of window + frame
    ; is there an x-draw-retangle ?
    (x-draw-line 'root gc ul ur)
    (x-draw-line 'root gc ll lr)
    (x-draw-line 'root gc ul ll)
    (x-draw-line 'root gc ur lr)
    ; from screen left to left border
    (x-draw-line 'root gc (cons 0 y)            (cons (- x offset) y))
    (x-draw-line 'root gc (cons 0 (+ y height))
		 (cons (- x offset) (+ y height)))
    (draw-draft-line 'root gc sl ml t t)
    ; from screen top to top border
    (x-draw-line 'root gc (cons x 0) (cons x (- y offset)))
    (x-draw-line 'root gc (cons (+ x width) 0) (cons (+ x width) (- y offset)))
    (draw-draft-line 'root gc st um t t)
    ; from screen right to right border
    (x-draw-line 'root gc (cons (screen-width) y)
		 (cons (+ x width offset) y))
    (x-draw-line 'root gc (cons (screen-width) (+ y height))
		 (cons (+ x width offset) (+ y height)))
    (draw-draft-line 'root gc mr sr t t)
    ; from screen bottom to bottom border
    (x-draw-line 'root gc (cons x (screen-height))
		 (cons x (+ y height offset)))
    (x-draw-line 'root gc (cons (+ x width) (screen-height))
		 (cons (+ x width) (+ y height offset)))
    (draw-draft-line 'root gc lm sb 't 't)
    (x-destroy-gc gc)))

  (define-window-outliner 'draft draw-draft-outline))
