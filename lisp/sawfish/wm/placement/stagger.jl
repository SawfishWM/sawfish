;; stagger-placement.jl -- for jwz

;; $Id$

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

(defcustom stagger-placement-step 32
  "Distance between successive placements in `stagger' placement mode."
  :type number
  :range (1)
  :group placement)

(define place-window-stagger
  (let ((last-x 0)
	(last-y 0))
    (lambda (w)
      (let ((dims (window-frame-dimensions w)))
	(setq last-x (+ last-x stagger-placement-step))
	(setq last-y (+ last-y stagger-placement-step))
	(when (>= (+ last-x (car dims)) (screen-width))
	  (setq last-x 0))
	(when (>= (+ last-y (cdr dims)) (screen-height))
	  (setq last-y 0))
	(move-window-to w last-x last-y)))))

(define-placement-mode 'stagger place-window-stagger)

;;;###autoload (autoload 'place-window-stagger "stagger-placement")
;;;###autoload (define-placement-mode 'stagger place-window-stagger)
