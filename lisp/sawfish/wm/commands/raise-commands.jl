;; raise-commands.jl -- some commands for raising windows
;; $Id$

;; Copyright (C) 1999-2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.commands.raise-commands ()

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.events
	  sawfish.wm.stacking
	  sawfish.wm.state.transient
	  sawfish.wm.commands.groups
	  sawfish.wm.commands)

  (define (window-and-pass-through-click w)
    "Raise the window that received the current event, then replay any pointer
events that invoked the command."
    (when (windowp w)
      (raise-window w))
    (allow-events 'replay-pointer)
    (unless (clicked-frame-part)
      (forget-button-press)))

  (define (group-and-pass-through-click w)
    "Raise the group of windows that received the current event, then replay
any pointer events that invoked the command."
    (when (windowp w)
      (raise-group w))
    (allow-events 'replay-pointer)
    (unless (clicked-frame-part)
      (forget-button-press)))

  (define (transients-and-pass-through-click w)
    "Raise the window that received the current event and any transients it
has, then replay any pointer events that invoked the command."
    (when (windowp w)
      (raise-window-and-transients w))
    (allow-events 'replay-pointer)
    (unless (clicked-frame-part)
      (forget-button-press)))

  (define (and-pass-through-click-if-focused w)
    "Raise the window that received the current event (if it's focused), then
replay any pointer events that invoked the command."
    (when (and (windowp w) (eq w (input-focus)))
      (raise-window w))
    (allow-events 'replay-pointer)
    (unless (clicked-frame-part)
      (forget-button-press)))

  (define (or-pass-through-click w)
    (if (and (windowp w) (not (window-on-top-p w)))
	(raise-window w)
      (allow-events 'replay-pointer)
      (unless (clicked-frame-part)
	(forget-button-press))))

  ;;###autoload
  (define-command 'raise-window-and-pass-through-click
    window-and-pass-through-click #:spec "%w")
  (define-command 'raise-group-and-pass-through-click
    group-and-pass-through-click #:spec "%w")
  (define-command 'raise-transients-and-pass-through-click
    transients-and-pass-through-click #:spec "%w")
  (define-command 'raise-and-pass-through-click-if-focused
    and-pass-through-click-if-focused #:spec "%w")
  (define-command 'raise-or-pass-through-click
    or-pass-through-click #:spec "%w"))
