;; move-cursor.jl -- commands to move the mouse pointer
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

(provide 'move-cursor)

(defcustom move-cursor-increment 16
  "Number of pixels to move pointer in `move-cursor-' commands."
  :group misc
  :type number
  :range (1 . nil))

(defun move-cursor (right down)
  (let
      ((coords (query-pointer)))
    (warp-cursor (+ (car coords) right) (+ (cdr coords) down))))

;;;###autoload
(defun move-cursor-left ()
  "Move the cursor `move-cursor-increment' pixels to the left."
  (interactive)
  (move-cursor (- move-cursor-increment) 0))

;;;###autoload
(defun move-cursor-right ()
  "Move the cursor `move-cursor-increment' pixels to the right."
  (interactive)
  (move-cursor move-cursor-increment 0))

;;;###autoload
(defun move-cursor-up ()
  "Move the cursor `move-cursor-increment' pixels upwards."
  (interactive)
  (move-cursor 0 (- move-cursor-increment)))

;;;###autoload
(defun move-cursor-down ()
  "Move the cursor `move-cursor-increment' pixels downwards."
  (interactive)
  (move-cursor 0 move-cursor-increment))

;;;###autoload
(defun move-cursor-left-fine ()
  "Move the cursor 1 pixel to the left."
  (interactive)
  (move-cursor -1 0))

;;;###autoload
(defun move-cursor-right-fine ()
  "Move the cursor 1 pixel to the right."
  (interactive)
  (move-cursor 1 0))

;;;###autoload
(defun move-cursor-up-fine ()
  "Move the cursor 1 pixel upwards."
  (interactive)
  (move-cursor 0 -1))

;;;###autoload
(defun move-cursor-down-fine ()
  "Move the cursor 1 pixel downwards."
  (interactive)
  (move-cursor 0 1))
