;; groups.jl -- commands for manipulating window groups
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

(define-structure sawfish.wm.commands.groups

    (export iconify-group
	    uniconify-group
	    iconify-transient-group
	    uniconify-transient-group
	    make-group-sticky
	    make-group-unsticky
	    toggle-group-sticky
	    send-group-to-workspace
	    send-group-to-current-workspace
	    send-group-to-next-workspace
	    send-group-to-previous-workspace
	    move-group-to-current-viewport
	    move-group-viewport
	    move-group-left
	    move-group-right
	    move-group-up
	    move-group-down
	    raise-group
	    lower-group
	    raise-lower-group
	    raise-group-depth
	    lower-group-depth
	    set-group-frame-style)

    (open rep
	  sawfish.wm.util.groups
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.state.iconify
	  sawfish.wm.state.transient
	  sawfish.wm.workspace
	  sawfish.wm.viewport
	  sawfish.wm.stacking
	  sawfish.wm.frames)

  (define-structure-alias groups sawfish.wm.commands.groups)

  ;; iconification

  (define (call-with-iconify-mode mode fun w)
    (let ((iconify-group-mode mode)
	  (uniconify-group-mode mode))
      (fun w)))

  (define (iconify-group w)
    (call-with-iconify-mode 'group iconify-window w))
  (define (uniconify-group w)
    (call-with-iconify-mode 'group uniconify-window w))

  (define (iconify-transient-group w)
    (call-with-iconify-mode 'transients iconify-window w))
  (define (uniconify-transient-group w)
    (call-with-iconify-mode 'transients uniconify-window w))

  (define-command 'iconify-group iconify-group #:spec "%W")
  (define-command 'uniconify-group uniconify-group #:spec "%W")
  (define-command 'iconify-transient-group iconify-transient-group #:spec "%W")
  (define-command 'uniconify-transientgroup uniconify-transient-group #:spec "%W")
  
  ;; sticky

  (define (make-group-sticky w) (map-window-group make-window-sticky w))
  (define (make-group-unsticky w) (map-window-group make-window-unsticky w))

  (defun toggle-group-sticky (w)
    (if (window-get w 'sticky)
	(make-group-unsticky w)
      (make-group-sticky w)))

  ;;###autoload
  (define-command 'make-group-sticky make-group-sticky #:spec "%W")
  (define-command 'make-group-unsticky make-group-unsticky #:spec "%W")
  (define-command 'toggle-group-sticky toggle-group-sticky #:spec "%W")

  ;; workspaces

  (define (send-group-to-workspace w send-group-dest-space)
    (map-window-group
     (lambda (x)
       (unless (window-get x 'sticky)
	 (move-window-to-workspace
	  x (nearest-workspace-with-window x current-workspace)
	  send-group-dest-space (eq x (input-focus))))) w))

  (define (send-group-to-current-workspace w)
    (send-group-to-workspace w current-workspace))

  (define (send-group-to-next-workspace send-group-window count)
    (ws-call-with-workspace (lambda (space)
			      (send-group-to-workspace send-group-window space)
			      (select-workspace space))
			    count workspace-send-boundary-mode))

  (define (send-group-to-previous-workspace w count)
    (send-group-to-next-workspace w (- count)))

  ;;###autoload
  (define-command 'send-group-to-current-workspace
    send-group-to-current-workspace #:spec "%W")
  (define-command 'send-group-to-next-workspace
    send-group-to-next-workspace #:spec "%W\np")
  (define-command 'send-group-to-previous-workspace
    send-group-to-previous-workspace #:spec "%W\np")

  ;; viewports

  (define (move-group-to-current-viewport w)
    (map-window-group move-window-to-current-viewport w))

  (define (move-group-viewport w col row)
    (map-window-group (lambda (x)
			(move-window-viewport x col row)) w)
    (move-viewport-to-window w))

  (define (move-group-left w) (move-group-viewport w -1 0))
  (define (move-group-right w) (move-group-viewport w 1 0))
  (define (move-group-up w) (move-group-viewport w 0 -1))
  (define (move-group-down w) (move-group-viewport w 0 1))

  ;;###autoload
  (define-command 'move-group-to-current-viewport
    move-group-to-current-viewport #:spec "%W")

  ;;###autoload
  (define-command 'move-group-left move-group-left #:spec "%W")
  (define-command 'move-group-right move-group-right #:spec "%W")
  (define-command 'move-group-up move-group-up #:spec "%W")
  (define-command 'move-group-down move-group-down #:spec "%W")

  ;; stacking

  (define (raise-group w) (raise-windows w (windows-in-group w t)))
  (define (lower-group w) (lower-windows w (windows-in-group w t)))
  (define (raise-lower-group w) (raise-lower-windows w (windows-in-group w t)))
  (define (raise-group-depth w) (map-window-group raise-window-depth w))
  (define (lower-group-depth w) (map-window-group lower-window-depth w))

  ;;###autoload
  (define-command 'raise-group raise-group
    #:spec "%W" #:user-level 'expert)
  (define-command 'lower-group lower-group
    #:spec "%W" #:user-level 'expert)
  (define-command 'raise-lower-group raise-lower-group
    #:spec "%W" #:user-level 'expert)
  (define-command 'raise-group-depth raise-group-depth #:spec "%W")
  (define-command 'lower-group-depth lower-group-depth #:spec "%W")

  ;; framing

  (define (set-group-frame-style w style)
    (map-window-group (lambda (x)
			(set-frame-style x style)) w)))
