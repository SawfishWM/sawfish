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

    (export window-ignored-p
	    make-window-ignored
	    make-window-not-ignored
	    toggle-window-ignored
	    toggle-window-never-focus
	    toggle-window-cycle-skip
	    toggle-window-list-skip
	    toggle-task-list-skip)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.util.window-order
	  sawfish.wm.frames
	  sawfish.wm.menus)

  (define (window-ignored-p w) (window-get w 'ignored))

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
    (window-put w 'never-focus (not (window-get w 'never-focus)))
    (when (eq (input-focus) w)
      (window-order-focus-most-recent))
    (call-window-hook 'window-state-change-hook w (list '(never-focus))))

  (define (toggle-window-cycle-skip w)
    "Toggle whether a window is ignored while window cycling."
    (window-put w 'cycle-skip (not (window-get w 'cycle-skip)))
    (call-window-hook 'window-state-change-hook w (list '(cycle-skip))))

  (define (toggle-window-list-skip w)
    "Toggle whether a window will be include in the window list."
    (window-put w 'window-list-skip (not (window-get w 'window-list-skip)))
    (call-window-hook 'window-state-change-hook w (list '(window-list-skip))))

  (define (toggle-task-list-skip w)
    "Toggle whether a window will be included in the task-list."
    (window-put w 'task-list-skip (not (window-get w 'task-list-skip)))
    (call-window-hook 'window-state-change-hook w (list '(task-list-skip))))

  (define-command 'make-window-ignored make-window-ignored #:spec "%W")
  (define-command 'make-window-not-ignored make-window-not-ignored #:spec "%W")
  (define-command 'toggle-window-ignored toggle-window-ignored #:spec "%W")
  (define-command 'toggle-window-never-focus toggle-window-never-focus #:spec "%W")
  (define-command 'toggle-window-cycle-skip toggle-window-cycle-skip #:spec "%W")
  (define-command 'toggle-window-list-skip toggle-window-list-skip #:spec "%W")
  (define-command 'toggle-task-list-skip toggle-task-list-skip #:spec "%W")

  (add-window-menu-toggle (_ "_Ignored") 'toggle-window-ignored
			  window-ignored-p)
  (add-window-menu-toggle (_ "_Focusable") 'toggle-window-never-focus
			  (lambda (w) (not (window-get w 'never-focus))))
  (add-window-menu-toggle (_ "_Cyclable") 'toggle-window-cycle-skip
			  (lambda (w) (not (window-get w 'cycle-skip))))
  (add-window-menu-toggle (_ "In _window list") 'toggle-window-list-skip
			  (lambda (w) (not (window-get w 'window-list-skip))))
  (add-window-menu-toggle (_ "In _task list") 'toggle-task-list-skip
                         (lambda (w) (not (window-get w 'task-list-skip)))))

