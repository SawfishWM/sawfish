;; gnome-match.jl -- match-window settings for when GNOME's available
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

(define-structure sawfish.wm.gnome.match-window ()

    (open rep
	  sawfish.wm.ext.match-window
	  sawfish.wm.commands)

  (define-structure-alias gnome-match sawfish.wm.gnome.match-window)

  ;; Originally from Ben Liblit <liblit@cs.berkeley.edu>

  ;; the SKIP_WINLIST hint is now tied to the window-list-skip property

  (define-match-window-property 'skip-tasklist 'other 'boolean)

  (define-match-window-setter 'skip-tasklist
    (lambda (window property value)
      (declare (unused property))
      (apply-command (if value
			 'gnome-set-skip-tasklist
		       'gnome-clear-skip-tasklist) (list window)))))
