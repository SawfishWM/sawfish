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

(require 'match-window)
(provide 'gnome-match)

;; Originally from Ben Liblit <liblit@cs.berkeley.edu>

(define-match-window-property 'skip-tasklist 'other 'boolean)
(define-match-window-property 'skip-winlist 'other 'boolean)

(define-match-window-setter 'skip-tasklist
 (lambda (window property value)
   ((if value
	gnome-set-skip-tasklist
      gnome-clear-skip-tasklist)
    window)))

(define-match-window-setter 'skip-winlist
 (lambda (window property value)
   ((if value
	gnome-set-skip-winlist
      gnome-clear-skip-winlist)
    window)))
