;; open-look.jl -- handle some ol hints
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

(provide 'open-look)

;; Commentary:

;; This suffers from the same limitations as the Motif hints. See
;; mwm.jl for more details

(defun ol-add-window (w)
  (let*
      ((type (window-type w))
       (orig-type type)
       prop data)
    (when (setq prop (get-x-property w '_OL_WIN_ATTR))
      (setq data (nth 2 prop))
      (let
	  ((ol-type (cond ((= (length data) 3)
			   (aref data 0))
			  ((= (length data) 5)
			   (aref data 1))
			  ((> (length data) 0)
			   ;; wordperfect gives us [_OL_WT_OTHER]
			   ;; i.e. a _single_ atom
			   (aref data 0)))))
	(when (memq ol-type '(_OL_WT_NOTICE _OL_WT_OTHER))
	  (setq type (window-type-remove-title type)))))
    (when (setq prop (get-x-property w '_OL_DECOR_ADD))
      (setq data (nth 2 prop))
      (let
	  ((i 0))
	(while (< i (length data))
	  (when (eq (aref data i) '_OL_DECOR_HEADER)
	    (setq type (window-type-add-title type)))
	  (setq i (1+ i)))))
    (when (setq prop (get-x-property w '_OL_DECOR_DEL))
      (setq data (nth 2 prop))
      (let
	  ((i 0))
	(while (< i (length data))
	  (when (eq (aref data i) '_OL_DECOR_HEADER)
	    (setq type (window-type-remove-title type)))
	  (setq i (1+ i)))))
    (unless (eq type orig-type)
      (window-put w 'type type))))

(add-hook 'before-add-window-hook 'ol-add-window)
