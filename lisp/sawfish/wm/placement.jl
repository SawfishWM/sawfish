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

(defcustom place-window-mode 'random
  "Method of selecting the position of a freshly-mapped window."
  :type (set random interactive smart)
  :group placement)

(defcustom ignore-program-positions nil
  "Ignore program-specified window positions."
  :type boolean
  :group placement)

;; called from the place-window-hook
(defun place-window (w)
  (let
      ((hints (window-size-hints w)))
    (if (or (window-transient-p w)
	    (cdr (assq 'user-position hints))
	    (and (not ignore-program-positions)
		 (cdr (assq 'program-position hints))))
	nil
      (let
	  ((mode (or (window-get w 'place-mode) place-window-mode)))
	(cond ((eq mode 'smart)
	       ;; XXX implement this..
	       (setq mode 'random))
	      ((eq mode 'interactive)
	       ;; XXX this doesn't work; why not?
	       (let
		   ((move-outline-mode nil))
		 (move-window-interactively w)))
	      ((eq mode 'random)
	       (move-window-to
		w
		(random (max 0 (- (screen-width)
				  (car (window-dimensions w)))))
		(random (max 0 (- (screen-height)
				  (cdr (window-dimensions w))))))))
	t))))

(add-hook 'place-window-hook 'place-window t)
