;; viewport-linear.jl -- linear address of viewports
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

;; Commentary:

;; This was originally written by Eric Kidd <eric.kidd@pobox.com>

;; I've rewritten it to take advantage of lexical scope, and changed
;; some names

(require 'viewport)

(defvar viewport-linear-last 9)

;; Set the viewport using linear addressing
;;;###autoload
(defun set-viewport-linear (index)
  (set-screen-viewport
   (mod index viewport-columns) (/ index viewport-columns)))

;; Move window to viewport INDEX using linear addressing
;;;###autoload
(defun set-window-viewport-linear (window index)
  (set-window-viewport
   window (mod index viewport-columns) (/ index viewport-columns)))

;;;###autoload
(defun define-linear-viewport-commands (index)
  (let
      ((fn (lambda (base)
	     (intern (format nil "%s:%d" base (1+ index))))))
    (define-value
     (fn "set-viewport-linear")
     (lambda ()
       "Move to the specified linear viewport."
       (interactive)
       (set-viewport-linear index)))
    (define-value
     (fn "set-window-viewport-linear")
     (lambda (window)
       "Move the current window to the specified linear viewport."
       (interactive "%W")
       (set-window-viewport-linear window index)))))

(let
    ((i 0))
  (while (< i viewport-linear-last)
    (define-linear-viewport-commands i)
    (setq i (1+ i))))
