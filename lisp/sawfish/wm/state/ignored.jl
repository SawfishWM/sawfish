;; ignore-window.jl -- controlling the ignored property
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

;; originally by Julian Missig <julian@linuxpower.org> (X-ViRGE), with
;; some extra hacking/renaming

(define-structure sawfish.wm.state.ignored

    (export make-window-ignored
	    make-window-not-ignored
	    toggle-window-ignored
	    toggle-window-never-focus)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.util.window-order
	  sawfish.wm.frames)

  (define (make-window-ignored w)
    "Ignore the window."
    (unless (window-get w 'ignored)
      (set-window-frame w nil-frame)
      (window-put w 'current-frame-style nil)
      (window-put w 'ignored t)
      (call-window-hook 'window-state-change-hook w (list '(ignored)))
      (call-hook 'workspace-state-change-hook)))

  (define (make-window-not-ignored w)
    "Unignore the window."
    (when (window-get w 'ignored)
      (window-put w 'ignored nil)
      (reframe-window w)
      (call-window-hook 'window-state-change-hook w (list '(ignored)))
      (call-hook 'workspace-state-change-hook)))

  (define (toggle-window-ignored w)
    "Toggle whether a window is ignored or not."
    (if (window-get w 'ignored)
	(make-window-not-ignored w)
      (make-window-ignored w)))

  (define (toggle-window-never-focus w)
    "Toggle whether a window is focusable or not."
    (if (window-get w 'never-focus)
	(window-put w 'never-focus nil)
      (window-put w 'never-focus t))
    (when (eq (input-focus) w)
      (window-order-focus-most-recent))
    (call-window-hook 'window-state-change-hook w (list '(never-focus))))

  ;;###autoload
  (define-command 'make-window-ignored make-window-ignored "%W")
  (define-command 'make-window-not-ignored make-window-not-ignored "%W")
  (define-command 'toggle-window-ignored toggle-window-ignored "%W")
  (define-command 'toggle-window-never-focus toggle-window-never-focus "%W"))
