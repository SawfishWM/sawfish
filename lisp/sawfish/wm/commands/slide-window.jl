;; slide-window.jl -- simple code to move a window via the keyboard
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

(defcustom slide-window-increment 16
  "Number of pixels to move window in `slide-' commands."
  :group misc
  :type number
  :range (1 . nil))

(defun slide-window (w right down)
  (let
      ((coords (window-position w)))
    (move-window-to w (+ (car coords) right) (+ (cdr coords) down))))


;; window commands

;;;###autoload
(defun slide-window-left (w)
  (interactive "%W")
  (slide-window w (- slide-window-increment) 0))

;;;###autoload
(defun slide-window-right (w)
  (interactive "%W")
  (slide-window w slide-window-increment 0))

;;;###autoload
(defun slide-window-up (w)
  (interactive "%W")
  (slide-window w 0 (- slide-window-increment)))

;;;###autoload
(defun slide-window-down (w)
  (interactive "%W")
  (slide-window w 0 slide-window-increment))


;; group commands

;;;###autoload
(defun slide-group-left (w)
  (interactive "%W")
  (map-window-group slide-window-left w))

;;;###autoload
(defun slide-group-right (w)
  (interactive "%W")
  (map-window-group slide-window-right w))

;;;###autoload
(defun slide-group-up (w)
  (interactive "%W")
  (map-window-group slide-window-up w))

;;;###autoload
(defun slide-group-down (w)
  (interactive "%W")
  (map-window-group slide-window-down w))
