;; raise-commands.jl -- some commands for raising windows

;; Copyright (C) 1999-2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.commands.raise-commands

    (export )

    (open rep
          sawfish.wm.windows
          sawfish.wm.events
          sawfish.wm.stacking
          sawfish.wm.util.stacking
          sawfish.wm.state.transient
          sawfish.wm.commands.groups
          sawfish.wm.commands
          sawfish.wm.focus)

  (define (replay-pointer w)
    ;; click-to-focus mode sets this to t when it calls a command
    ;; from within a button press event
    (unless (and (fluid focus-within-click-event)
		 (not (or focus-click-through
			  (window-get w 'focus-click-through)
			  (not (window-really-wants-input-p w)))))
      (allow-events 'replay-pointer)
      (unless (clicked-frame-part)
	(forget-button-press))))

  (define (and-pass-through-click w)
    "Raise the window that received the current event, then replay any pointer
events that invoked the command."
    (when (windowp w)
      (raise-window* w))
    (replay-pointer w))

  (define (and-pass-through-click-if-focused w)
    "Raise the window that received the current event (if it's focused), then
replay any pointer events that invoked the command."
    (when (and (windowp w) (eq w (input-focus)))
      (raise-window* w))
    (replay-pointer w))

  (define (or-pass-through-click w)
    (if (and (windowp w) (not (window-on-top-p w)))
	(raise-window* w)
      (replay-pointer w)))

  ;;###autoload
  (define-command 'raise-and-pass-through-click
    and-pass-through-click #:spec "%W")
  (define-command 'raise-and-pass-through-click-if-focused
    and-pass-through-click-if-focused #:spec "%w")
  (define-command 'raise-or-pass-through-click
    or-pass-through-click #:spec "%w")

;;; these should probably be considered obsolete

  (define (window-and-pass-through-click w)
    "Raise the window that received the current event, then replay any pointer
events that invoked the command."
    (when (windowp w)
      (raise-window w))
    (replay-pointer w))

  (define (group-and-pass-through-click w)
    "Raise the group of windows that received the current event, then replay
any pointer events that invoked the command."
    (when (windowp w)
      (raise-group w))
    (replay-pointer w))

  (define (transients-and-pass-through-click w)
    "Raise the window that received the current event and any transients it
has, then replay any pointer events that invoked the command."
    (when (windowp w)
      (raise-window-and-transients w))
    (replay-pointer w))

  ;;###autoload
  (define-command 'raise-window-and-pass-through-click
    window-and-pass-through-click #:spec "%w" #:class 'advanced)
  (define-command 'raise-group-and-pass-through-click
    group-and-pass-through-click #:spec "%w" #:class 'advanced)
  (define-command 'raise-transients-and-pass-through-click
    transients-and-pass-through-click #:spec "%w" #:class 'advanced))
