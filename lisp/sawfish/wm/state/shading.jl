;; shading.jl -- window ``shading''
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

(defun shade-window (w)
  "Display only the title bar of the window."
  (interactive "W")
  (unless (window-get w 'shaded)
    (window-put w 'shaded t)
    (window-put w 'hide-client t)
    (window-put w 'shaded-old-type (window-type w))
    (let
	((type (window-type w)))
      (cond ((eq type 'default)
	     (setq type 'shaped))
	    ((memq type '(transient unframed))
	     (setq type 'shaped-transient)))
      (set-window-frame-style
       w (window-get w 'current-frame-style) type nil)
      (call-window-hook 'shade-window-hook w)
      (call-window-hook 'window-state-change-hook w))))

(defun unshade-window (w)
  "If the window is shaded (see `shade-window'), restore it to it's usual
state."
  (interactive "W")
  (when (window-get w 'shaded)
    (window-put w 'shaded nil)
    (window-put w 'hide-client nil)
    (set-window-frame-style w (window-get w 'current-frame-style)
			    (window-get w 'shaded-old-type) nil)
    (window-put w 'shaded-old-type nil)
    (call-window-hook 'unshade-window-hook w)
    (call-window-hook 'window-state-change-hook w)))

(defun toggle-window-shaded (w)
  "Toggle the shaded (only the title bar is displayed) state of the window."
  (interactive "W")
  (if (window-get w 'shaded)
      (unshade-window w)
    (shade-window w)))

(defun shading-add-window (w)
  (when (window-get w 'shaded)
    (window-put w 'shaded nil)
    (shade-window w)))

(add-hook 'add-window-hook 'shading-add-window t)
