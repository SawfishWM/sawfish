;; groups.jl -- commands for manipulating window groups

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

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
    "Minimize the window and all windows from the same group."
    (call-with-iconify-mode 'group iconify-window w))
  (define (uniconify-group w)
    "Restore the window and all windows from the same group from its minimized state."
    (call-with-iconify-mode 'group uniconify-window w))

  (define (iconify-transient-group w)
    "Minimize the transient window and all transient windows from the same group."
    (call-with-iconify-mode 'transients iconify-window w))
  (define (uniconify-transient-group w)
    "Restore the transient window and all transient windows from the same group from its minimized state."
    (call-with-iconify-mode 'transients uniconify-window w))

  (define-command 'iconify-group
    iconify-group #:spec "%W")
  (define-command 'uniconify-group
    uniconify-group #:spec "%W")
  (define-command 'iconify-transient-group
    iconify-transient-group #:spec "%W")
  (define-command 'uniconify-transient-group
    uniconify-transient-group #:spec "%W")

  ;; sticky

  (define (make-group-sticky w)
    "Make the window and all windows from the same group appears in all workspaces and viewports."
    (map-window-group make-window-sticky w))
  (define (make-group-unsticky w)
    "Make the window and all windows from the same group appears only in the
current workspaces or viewports."
    (map-window-group make-window-unsticky w))
  
  (defun toggle-group-sticky (w)
    "Toggle the `stickiness' from the window and all windows from the same group,
i.e. make it appear in all workspaces and viewports."
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
    "Send the window and all windows from the same group to the current workspace."
    (send-group-to-workspace w current-workspace))

  (define (send-group-to-next-workspace send-group-window count)
    "Send the window and all windows from the same group to the next workspace."
    (ws-call-with-workspace (lambda (space)
			      (send-group-to-workspace send-group-window space)
			      (select-workspace space))
			    count workspace-send-boundary-mode))

  (define (send-group-to-previous-workspace w count)
    "Send the window and all windows from the same group to the previous workspace."
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
    "Move the window and all windows from the same group
to the current viewport."
    (map-window-group move-window-to-current-viewport w))

  (define (move-group-viewport w col row)
    (map-window-group (lambda (x)
			(move-window-viewport x col row)) w)
    (move-viewport-to-window w))

  (define (move-group-left w) 
    "Move the window and all windows from the same group
to the viewport on the left, and switch to that viewport."
    (move-group-viewport w -1 0))

  (define (move-group-right w) 
    "Move the window and all windows from the same group
to the viewport on the right, and switch to that viewport."
    (move-group-viewport w 1 0))

  (define (move-group-up w) 
    "Move the window and all windows from the same group
to the viewport above, and switch to that viewport."
    (move-group-viewport w 0 -1))

  (define (move-group-down w) 
    "Move the window and all windows from the same group
to the viewport below, and switch to that viewport."
    (move-group-viewport w 0 1))

  ;;###autoload
  (define-command 'move-group-to-current-viewport
    move-group-to-current-viewport #:spec "%W")

  ;;###autoload
  (define-command 'move-group-left
    move-group-left #:spec "%W")
  (define-command 'move-group-right
    move-group-right #:spec "%W")
  (define-command 'move-group-up
    move-group-up #:spec "%W")
  (define-command 'move-group-down
    move-group-down #:spec "%W")

  ;; stacking

  (define (raise-group w) 
    "Raise the window and all windows from the same group to its highest
allowed position in the stacking order."
    (raise-windows w (windows-in-group w t)))

  (define (lower-group w) 
    "Lower the window and all windows from the same group to its lowest
allowed position in the stacking order."
    (lower-windows w (windows-in-group w t)))

  (define (raise-lower-group w) 
    "If the window is at its highest possible position, then lower the window and all
windows from the same group to its lowest possible position. Otherwise raise the window
and all windows from the group as far as allowed."
    (raise-lower-windows w (windows-in-group w t)))

  (define (raise-group-depth w) 
    "Put the window and all windows from the same group in the stacking level above its current level."
    (map-window-group raise-window-depth w))

  (define (lower-group-depth w) 
    "Put the window and all windows from the same group in the stacking level beneath its current level."
    (map-window-group lower-window-depth w))

  ;;###autoload
  (define-command 'raise-group raise-group
    #:spec "%W")
  (define-command 'lower-group lower-group
    #:spec "%W")
  (define-command 'raise-lower-group raise-lower-group
    #:spec "%W")
  (define-command 'raise-group-depth raise-group-depth #:spec "%W")
  (define-command 'lower-group-depth lower-group-depth #:spec "%W")

  ;; framing

  (define (set-group-frame-style w style)
    (map-window-group (lambda (x)
			(set-frame-style x style)) w))

  ;; deleting windows

  (define (delete-group w)
    "Delete all windows in the group that the current window is a member of."
    (map-window-group (lambda (x)
			;; Don't delete the desktop-window. It should
			;; probably be in a separate group, but isn't..
			(unless (desktop-window-p x)
			  (delete-window x))) w))

  ;;###autoload
  (define-command 'delete-group delete-group #:spec "%W"))
