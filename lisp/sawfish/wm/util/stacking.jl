;; stacking.jl -- customizable stacking functions
;;
;; Copyright (C) 2000 Eazel, Inc
;;
;; Author: John Harper <jsh@eazel.com>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.util.stacking

    (export raise-window*
	    lower-window*
	    raise-lower-window*)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.events
	  sawfish.wm.misc
	  sawfish.wm.stacking
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.state.transient
	  sawfish.wm.commands.groups)

  (defcustom user-raise-type 'transients
    "When raising a window, also raise its: \\w"
    :type (choice none transients group)
    :group (misc stacking))

  (define (raise-window* w)
    "Raise the window to its highest allowed position in the stacking
order."
    (case user-raise-type
      ((transients) (raise-window-and-transients w))
      ((group) (raise-group w))
      (t (raise-window w))))

  (define (lower-window* w)
    "Lower the window to its lowest allowed position in the stacking
order."
    (case user-raise-type
      ((transients) (lower-window-and-transients w))
      ((group) (lower-group w))
      (t (lower-window w))))

  (define (raise-lower-window* w)
    "If the window is at its highest possible position, then lower it
to its lowest possible position. Otherwise raise it as far as
allowed."
    (case user-raise-type
      ((transients) (raise-lower-window-and-transients w))
      ((group) (raise-lower-group w))
      (t (raise-lower-window w))))

  (define-command 'raise-window raise-window* #:spec "%W")
  (define-command 'lower-window lower-window* #:spec "%W")
  (define-command 'raise-lower-window raise-lower-window* #:spec "%W")

  )
