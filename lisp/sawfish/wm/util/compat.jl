;; compat.jl -- aliases for obsolete functions
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

(define-structure sawfish.wm.util.compat

    (export show-message
	    ws-copy-window
	    ws-move-window
	    ws-insert-workspace
	    ws-remove-workspace
	    sawmill-directory
	    sawmill-lisp-lib-directory
	    sawmill-site-lisp-directory
	    sawmill-exec-directory
	    sawmill-version
	    custom-set-color
	    custom-set-font
	    custom-set-frame-style)

    (open rep
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.workspace)

;;; obsolete functions

  (define (show-message #!optional text font fg bg position)
    (let ((attrs nil))
      (when font
	(setq attrs (cons (cons 'font font) attrs)))
      (when fg
	(setq attrs (cons (cons 'fg fg) attrs)))
      (when bg
	(setq attrs (cons (cons 'bg bg) attrs)))
      (when position
	(setq attrs (cons (cons 'position position) attrs)))
      (display-message text attrs)))

  (define ws-copy-window copy-window-to-workspace)
  (define ws-move-window move-window-to-workspace)
  (define ws-insert-workspace insert-workspace)
  (define ws-remove-workspace remove-workspace)

;;; obsolete variables

  (define sawmill-directory sawfish-directory)
  (define sawmill-lisp-lib-directory sawfish-lisp-lib-directory)
  (define sawmill-site-lisp-directory sawfish-site-lisp-directory)
  (define sawmill-exec-directory sawfish-exec-directory)
  (define sawmill-version sawfish-version)

;;; obsolete commands

  (define (define-commands index)
    (let ((fn (lambda (base)
		(intern (format nil "%s:%d" base (1+ index))))))
      (define-command (fn "select-workspace")
	(lambda () (select-workspace-from-first index)))
      (define-command (fn "send-to-workspace")
	(lambda (w) (send-window-to-workspace-from-first w index)) "%W")
      (define-command (fn "copy-to-workspace")
	(lambda (w) (send-window-to-workspace-from-first w index t)) "%W")
      (put (fn "select-workspace") 'deprecated-command t)
      (put (fn "send-to-workspace") 'deprecated-command t)
      (put (fn "copy-to-workspace") 'deprecated-command t)))

  (do ((i 0 (1+ i)))
      ((= i 9))
    (define-commands i))

  (define-command 'insert-workspace (command-ref 'insert-workspace-after))
  (put 'insert-workspace 'deprecated-command t)

;;; obsolete options

  (put 'viewport-columns 'custom-obsolete t)
  (put 'viewport-rows 'custom-obsolete t)
  (put 'viewport-dimensions 'custom-obsolete t)
  (put 'preallocated-workspaces 'custom-obsolete t)
  (put 'iconify-whole-group 'custom-obsolete t)
  (put 'uniconify-whole-group 'custom-obsolete t)
  (put 'always-update-frames 'custom-obsolete t)

;;; obsolete custom setters

  (define (custom-set-color var value #!optional req)
    (custom-set-typed-variable var value 'color req))
  (define (custom-set-font var value #!optional req)
    (custom-set-typed-variable var value 'font req))
  (define (custom-set-frame-style var value #!optional req)
    (custom-set-typed-variable var value 'frame-style req))

  (define-custom-setter 'custom-set-color custom-set-color)
  (define-custom-setter 'custom-set-font custom-set-font)
  (define-custom-setter 'custom-set-frame-style custom-set-frame-style))
