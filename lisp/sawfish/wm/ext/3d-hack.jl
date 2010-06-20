;; 3d-hack.jl -- too much spare time..

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

;; Commentary:

;; Load this, then do `(3d-init 3d-make-cube)', or `(3d-init 3d-make-saw)'
;; then `(3d-destroy)' when you've had enough

(define-structure sawfish.wm.ext.3d-hack

    (export 3d-init
	    3d-destroy
	    3d-hack

	    ;; the available objects
	    3d-make-cube
	    3d-make-saw)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.events
	  sawfish.wm.util.x
	  sawfish.wm.colors
	  sawfish.wm.commands
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.util.decode-events)

  (define 3d-vertices nil)
  (define 3d-rotated-vertices nil)
  (define 3d-flattened-vertices nil)
  (define 3d-faces nil)
  (define 3d-face-gcs nil)

  (define 3d-window nil)
  (define 3d-white-gc nil)
  (define 3d-black-gc nil)

  (define 3d-rot [0.0 0.0 0.0])
  (define 3d-rot-speed [0.02 0.04 -0.1])
  (define 3d-frame-delay 50)
  (define 3d-horizon -10.0)
  (define 3d-observer 0)

  (define 3d-remove-hidden-faces nil)

  (define 3d-animator nil)

  (define 3d-timer nil)

  (define 3d-2-pi (* 2 3.14159265358979323846))

  (define (3d-make-saw)
    (let* ((outer-radius 1.5)
	   (inner-radius 1.3)
	   (center-radius 0.3)
	   (teeth 14)
	   (segments 10)
	   (faces (make-vector 2))
	   step angle points face)

      (setq angle 0)
      (setq step (/ 3d-2-pi teeth))
      (setq face nil)
      (do ((i 0 (1+ i)))
	  ((= i teeth))
	(let ((out-x (* outer-radius (cos (+ angle step))))
	      (out-y (* outer-radius (sin (+ angle step))))
	      (in-x (* inner-radius (cos angle)))
	      (in-y (* inner-radius (sin angle))))
	  (setq points (list* (vector out-x out-y 0)
			      (vector in-x in-y 0)
			      points))
	  (setq face (list* (1+ (* i 2)) (* i 2) face))
	  (setq angle (+ angle step))))
      (setq face (cons 0 face))
      (aset faces 0 (apply vector (nreverse face)))

      (setq angle 0)
      (setq step (/ 3d-2-pi segments))
      (setq face nil)
      (do ((i 0 (1+ i)))
	  ((= i segments))
	(setq points (cons (vector (* center-radius (cos angle))
				   (* center-radius (sin angle)) 0)
			   points))
	(setq face (cons (+ i (* teeth 2)) face))
	(setq angle (+ angle step)))
      (setq face (cons (* 2 teeth) face))
      (aset faces 1 (apply vector (nreverse face)))

      (setq 3d-vertices (apply vector (nreverse points)))
      (setq 3d-faces faces)
      (setq 3d-face-gcs (vector 3d-white-gc 3d-black-gc))
      (setq 3d-remove-hidden-faces nil)
      (setq 3d-rot-speed [0.02 0.04 -0.1])))

  (define (3d-make-cube)
    (let ((red-gc (x-create-gc 3d-window
                               `((foreground . ,(get-color "red")))))
	  (blue-gc (x-create-gc 3d-window
                                `((foreground . ,(get-color "blue")))))
	  (green-gc (x-create-gc 3d-window
                                 `((foreground . ,(get-color "green"))))))
      (setq 3d-vertices [[-1 -1 1]
			 [1 -1 1]
			 [1 1 1]
			 [-1 1 1]
			 [-1 -1 -1]
			 [1 -1 -1]
			 [1 1 -1]
			 [-1 1 -1]])
      (setq 3d-faces [[0 1 2 3 0]
		      [5 4 7 6 5]
		      [1 5 6 2 1]
		      [4 0 3 7 4]
		      [4 5 1 0 4]
		      [2 6 7 3 2]])
      (setq 3d-remove-hidden-faces t)
      (setq 3d-face-gcs (vector red-gc red-gc
				green-gc green-gc
				blue-gc blue-gc))
      (setq 3d-rot-speed [0.05 0.1 -0.05])))

  (define (3d-redraw)
    (define (face-visible-p points)
      (or (not 3d-remove-hidden-faces)
	  (let ((p0 (aref 3d-flattened-vertices (aref points 0)))
		(p1 (aref 3d-flattened-vertices (aref points 1)))
		(p2 (aref 3d-flattened-vertices (aref points 2)))
		tem1-x tem1-y tem2-x tem2-y)
	    (setq tem1-x (- (car p1) (car p0)))
	    (setq tem1-y (- (cdr p1) (cdr p0)))
	    (setq tem2-x (- (car p2) (car p1)))
	    (setq tem2-y (- (cdr p2) (cdr p1)))
	    (> (- (* tem2-x tem1-y) (* tem1-x tem2-y)) 0))))

    (define (draw-face points gc)
      (when (face-visible-p points)
	(let ((len (1- (length points)))
	      coords)
	  (do ((i 0 (1+ i)))
	      ((= i len))
	    (setq coords (cons (aref 3d-flattened-vertices
				     (aref points i)) coords)))
	  (x-fill-polygon (x-window-back-buffer 3d-window)
			  gc (nreverse coords) 'non-convex))))

    (define (draw-all)
      (do ((i 0 (1+ i))
	   (len (length 3d-faces)))
	  ((= i len))
	(draw-face (aref 3d-faces i) (if 3d-face-gcs
					 (aref 3d-face-gcs i)
				       3d-white-gc))))

    (draw-all)
    )

  (define (3d-rotate)
    (let* ((len (length 3d-vertices))
	   (new (make-vector len))
	   (new-flat (make-vector len)))

      (define (trunc angle)
	(cond ((> angle 3d-2-pi) (- angle 3d-2-pi))
	      ((< angle 0) (+ angle 3d-2-pi))
	      (t angle)))

      (define (flatten x y z)
	(let ((fact (/ (- (- z) 3d-horizon)
		       (- 3d-observer 3d-horizon))))
	  (setq x (* x fact))
	  (setq y (* y fact))
	  (cons (inexact->exact (truncate (+ (* x 100) 200)))
		(inexact->exact (truncate (+ (* y 100) 200))))))

      (aset 3d-rot 0 (trunc (+ (aref 3d-rot 0) (aref 3d-rot-speed 0))))
      (aset 3d-rot 1 (trunc (+ (aref 3d-rot 1) (aref 3d-rot-speed 1))))
      (aset 3d-rot 2 (trunc (+ (aref 3d-rot 2) (aref 3d-rot-speed 2))))
      (do ((i 0 (1+ i)))
	  ((= i len))
	(let ((x (aref (aref 3d-vertices i) 0))
	      (y (aref (aref 3d-vertices i) 1))
	      (z (aref (aref 3d-vertices i) 2))
	      (x-rot (aref 3d-rot 0))
	      (y-rot (aref 3d-rot 1))
	      (z-rot (aref 3d-rot 2))
	      tem0 tem1)

	  (setq tem0 (+ (* x (cos z-rot)) (* y (sin z-rot))))
	  (setq tem1 (- (* y (cos z-rot)) (* x (sin z-rot))))
	  (setq x tem0)
	  (setq y tem1)

	  (setq tem0 (+ (* x (cos y-rot)) (* z (sin y-rot))))
	  (setq tem1 (- (* z (cos y-rot)) (* x (sin y-rot))))
	  (setq x tem0)
	  (setq z tem1)

	  (setq tem0 (+ (* y (cos x-rot)) (* z (sin x-rot))))
	  (setq tem1 (- (* z (cos x-rot)) (* y (sin x-rot))))
	  (setq y tem0)
	  (setq z tem1)

	  (aset new i (vector x y z))

	  (aset new-flat i (flatten x y z))))
      (setq 3d-rotated-vertices new)
      (setq 3d-flattened-vertices new-flat)))

  (define (3d-init fun)
    (setq 3d-window (x-create-window (cons (- (/ (screen-width) 2) 200)
					   (- (/ (screen-height) 2) 200))
				     '(400 . 400) 1
				     `((background . ,(get-color "#021954")))
				     3d-redraw))
    (setq 3d-white-gc (x-create-gc 3d-window
				   `((foreground . ,(get-color "white")))))
    (setq 3d-black-gc (x-create-gc 3d-window
				   `((foreground . ,(get-color "#021954")))))
    (setq 3d-rot [0 0 0])
    (setq 3d-remove-hidden-faces nil)
    (setq 3d-animator nil)
    (setq 3d-face-gcs nil)
    (when fun
      (fun))
    (when 3d-animator
      (3d-animator))
    (3d-rotate)
    (x-map-window 3d-window)
    (setq 3d-timer (make-timer (lambda ()
				 (when 3d-animator
				   (3d-animator))
				 (3d-rotate)
				 (3d-redraw)
				 (x-window-swap-buffers 3d-window)
				 (set-timer 3d-timer)) 0 3d-frame-delay)))

  (define (3d-destroy)
    (x-destroy-window 3d-window)
    (delete-timer 3d-timer)
    (setq 3d-timer nil)
    (setq 3d-window nil)
    (setq 3d-white-gc nil)
    (setq 3d-black-gc nil))

  (define (3d-hack)
    (let ((thrower (lambda ()
		     (let ((event (and (current-event)
				       (decode-event (current-event)))))
		       (when (and (eq (car event) 'key)
				  (not (memq 'release (cadr event))))
			 (throw '3d-out nil))))))
      (catch '3d-out
	(when (grab-keyboard)
	  (unwind-protect
	      (progn
		(add-hook 'unbound-key-hook thrower)
		(3d-init 3d-make-saw)
		(recursive-edit))
	    (ungrab-keyboard)
	    (remove-hook 'unbound-key-hook thrower)
	    (3d-destroy))))))

  (define-command '3d-hack 3d-hack #:class 'advanced))
