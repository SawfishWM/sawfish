;; stacking.jl -- window stacking
;; $Id$

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

(provide 'stacking)

;; Commentary:

;; Each window will have a `depth' property--an integer, zero
;; represents the level of normal windows, negative for windows below
;; this level, and positive for windows above the normal level

(defun restack-by-layer ()
  (let
      ((order (sort (stacking-order)
		    #'(lambda (x y)
			(> (window-get x 'depth) (window-get y 'depth))))))
    (restack-windows order)))

(defun set-window-depth (w depth)
  (let
      ((old (window-get w 'depth)))
    (window-put w 'depth depth)
    (restack-by-layer)
    (call-window-hook 'window-depth-change-hook w (list depth))
    (call-window-hook 'window-state-change-hook w)))

(defun stacking-add-window (w)
  (let
      ((depth (window-get w 'depth)))
    (unless depth
      (setq depth 0)
      (window-put w 'depth depth))))

(defun stacking-order-in-layer (depth)
   (filter #'(lambda (w)
	       (= (window-get w 'depth) depth)) (stacking-order)))


;; Commands

(defun lower-window (w)
  (interactive "W")
  (let
      ((order (stacking-order))
       (depth (window-get w 'depth))
       tem)
    (setq tem (memq w order))
    (while (and (cdr tem) (= (window-get (car (cdr tem)) 'depth) depth))
      (rplaca tem (car (cdr tem)))
      (rplaca (cdr tem) w)
      (setq tem (cdr tem)))
    (restack-windows order)))

(defun raise-window (w)
  (interactive "W")
  (let
      ((order (stacking-order))
       (depth (window-get w 'depth))
       tem)
    (setq order (delq w order))
    (if (<= (window-get (car order) 'depth) depth)
	(setq order (cons w order))
      (setq tem order)
      (while (and (cdr tem) (> (window-get (car (cdr tem)) 'depth) depth))
	(setq tem (cdr tem)))
      (rplacd tem (cons w (cdr tem))))
    (restack-windows order)))

(defun raise-lower-window (w)
  (interactive "W")
  (let*
      ((order (stacking-order))
       (depth (window-get w 'depth))
       tem)
    (setq tem order)
    (while (and tem (< (window-get (car tem) 'depth) depth))
      (setq tem (cdr tem)))
    (while (and tem (not (eq (car tem) w))
		(= (window-get (car tem) 'depth) depth)
		(not (eq (window-get w 'workspace)
			 (window-get (car tem) 'workspace))))
      (setq tem (cdr tem)))
    (if (eq (car tem) w)
	(lower-window w)
      (raise-window w))))

(defun lower-window-layer (w)
  (interactive "W")
  (set-window-depth w (1- (window-get w 'depth))))

(defun raise-window-layer (w)
  (interactive "W")
  (set-window-depth w (1+ (window-get w 'depth))))

(add-hook 'add-window-hook 'stacking-add-window t)
(add-hook 'map-notify-hook 'restack-by-layer t)
