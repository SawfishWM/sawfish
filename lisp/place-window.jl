;; place-window.jl -- decide where to initially place a window
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

(provide 'place-window)

(defcustom place-window-mode 'best-fit
  "Method of selecting the position of a freshly-mapped window."
  :type (set random interactive first-fit best-fit none)
  :group placement)

(defcustom place-transient-mode 'random
  "Method of selecting the position of a freshly-mapped transient window."
  :type (set random interactive first-fit best-fit none)
  :group placement)

(defcustom ignore-program-positions nil
  "Ignore program-specified window positions."
  :type boolean
  :group placement)

(defun place-window-randomly (w)
  (let
      ((dims (window-frame-dimensions w))
       x y)
    (if (< (car dims) (screen-width))
	(setq x (random (- (screen-width) (car dims))))
      (setq x 0))
    (if (< (cdr dims) (screen-height))
	(setq y (random (- (screen-height) (cdr dims))))
      (setq y 0))
    (move-window-to w x y)))

(defun place-window-interactively (w)
  (require 'move-resize)
  (let
      ((move-outline-mode nil)
       (ptr (query-pointer))
       (dims (window-frame-dimensions w)))
    (move-window-to w (- (car ptr) (/ (car dims) 2))
		    (- (cdr ptr) (/ (cdr dims) 2)))
    (move-window-interactively w)))

;; called from the place-window-hook
(defun place-window (w)
  (let
      ((hints (window-size-hints w)))
    (if (or (cdr (assq 'user-position hints))
	    (and (not (window-get w 'ignore-program-position))
		 (not ignore-program-positions)
		 (cdr (assq 'program-position hints))))
	nil
      (let
	  ((mode (or (window-get w 'place-mode)
		     (if (window-transient-p w)
			 place-transient-mode
		       place-window-mode))))
	(cond ((eq mode 'interactive)
	       (place-window-interactively w))
	      ((eq mode 'random)
	       (place-window-randomly w))
	      ((eq mode 'first-fit)
	       (place-window-first-fit w))
	      ((eq mode 'best-fit)
	       (place-window-best-fit w)))
	t))))

(add-hook 'place-window-hook 'place-window t)
