;; open-look.jl -- handle some ol hints

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

;; Commentary:

;; This suffers from the same limitations as the Motif hints. See
;; mwm.jl for more details

(define-structure sawfish.wm.state.open-look

    (export )

    (open rep
          rep.system
          sawfish.wm.windows
          sawfish.wm.misc
          sawfish.wm.frames)

  (define (ol-add-window w)
    (let* ((type (window-type w))
	   prop data)
      (when (setq prop (get-x-property w '_OL_WIN_ATTR))
	(setq data (nth 2 prop))
	(let ((ol-type (cond ((= (length data) 3)
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
	(do ((i 0 (1+ i)))
	    ((= i (length data)))
	  (when (eq (aref data i) '_OL_DECOR_HEADER)
	    (setq type (window-type-add-title type)))))
      (when (setq prop (get-x-property w '_OL_DECOR_DEL))
	(setq data (nth 2 prop))
	(do ((i 0 (1+ i)))
	    ((= i (length data)))
	  (when (eq (aref data i) '_OL_DECOR_HEADER)
	    (setq type (window-type-remove-title type)))))
      (set-window-type w type)))

  (add-hook 'before-add-window-hook ol-add-window))
