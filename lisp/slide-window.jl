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
  "Move the window `slide-window-increment' pixels to the left."
  (interactive "%W")
  (slide-window w (- slide-window-increment) 0))

;;;###autoload
(defun slide-window-right (w)
  "Move the window `slide-window-increment' pixels to the right."
  (interactive "%W")
  (slide-window w slide-window-increment 0))

;;;###autoload
(defun slide-window-up (w)
  "Move the window `slide-window-increment' pixels upwards."
  (interactive "%W")
  (slide-window w 0 (- slide-window-increment)))

;;;###autoload
(defun slide-window-down (w)
  "Move the window `slide-window-increment' pixels downwards."
  (interactive "%W")
  (slide-window w 0 slide-window-increment))


;; group commands

;;;###autoload
(defun slide-group-left (w)
  "Move the window group `slide-window-increment' pixels to the left."
  (interactive "%W")
  (map-window-group slide-window-left w))

;;;###autoload
(defun slide-group-right (w)
  "Move the window group `slide-window-increment' pixels to the right."
  (interactive "%W")
  (map-window-group slide-window-right w))

;;;###autoload
(defun slide-group-up (w)
  "Move the window group `slide-window-increment' pixels upwards."
  (interactive "%W")
  (map-window-group slide-window-up w))

;;;###autoload
(defun slide-group-down (w)
  "Move the window group `slide-window-increment' pixels downwards."
  (interactive "%W")
  (map-window-group slide-window-down w))
