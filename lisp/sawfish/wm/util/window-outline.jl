;; window-outline.jl -- drawing window `rubber-band' outlines
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawmill.

;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'x)
(provide 'window-outline)

(defun draw-box-outline (x y width height)
  (let
      ((gc (x-create-root-xor-gc))
       (x-step (/ width 3))
       (y-step (/ height 3)))
    (letrec
	((loop (lambda (i)
		 (x-draw-line 'root gc (cons x (floor (+ y (* y-step i))))
			      (cons (+ x width) (ceiling (+ y (* y-step i)))))
		 (x-draw-line 'root gc (cons (floor (+ x (* x-step i))) y)
			      (cons (ceiling (+ x (* x-step i))) (+ y height)))
		 (unless (= i 3)
		   (loop (1+ i))))))
      (loop 0))
    (x-destroy-gc gc)))
(put 'box 'draw-window-outline draw-box-outline)

;; this is quite flickery.. double buffering doesn't work either..
(defun draw-solid-outline (x y width height)
  (let
      ((gc (x-create-root-xor-gc)))
    (x-fill-rectangle 'root gc (cons x y) (cons width height))
    (x-destroy-gc gc)))
(put 'solid 'draw-window-outline draw-solid-outline)

;;;###autoload
(defun draw-window-outline (mode x y width height)
  "Draw an outline of a window of dimensions (WIDTH, HEIGHT) at position
(X, Y) relative to the root window.

MODE is a symbol defining the type of outline drawn, currently it may
only be `box' for a 3x3 grid.

Use the `erase-window-outline' to erase the grid. Also note that since
these functions draw directly on the root window the server should be
grabbed until the outline is erased."
  (let
      ((fun (get mode 'draw-window-outline)))
    (when fun
      (fun x y width height))))

;;;###autoload
(defun erase-window-outline (mode x y width height)
  "Erase a previously drawn outline of a window of dimensions (WIDTH, HEIGHT)
at position (X, Y) relative to the root window. See `draw-window-outline'.

MODE is a symbol defining the type of outline drawn, currently it may
only be `box' for a 3x3 grid."
  (draw-window-outline mode x y width height))
