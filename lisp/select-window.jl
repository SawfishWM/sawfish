;; select-window.jl -- click on a window, any window..
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

(defvar select-window-map (bind-keys (make-keymap)
			    "Any-Click1" 'select-window-finished))

(defvar select-window-cursor-shape 'crosshair)

;; this function waits for the user to select a client window, then
;; returns that window
;;;###autoload
(defun select-window ()
  (allow-events 'async-pointer)
  (when (grab-pointer nil select-window-cursor-shape)
    (unwind-protect
	(let
	    ((override-keymap select-window-map))
	  (catch 'select-window
	    (recursive-edit)))
      (ungrab-pointer))))

(defun select-window-finished ()
  (interactive)
  (throw 'select-window (query-pointer-window)))
