;; iconify.jl -- handling window state
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

(define-structure sawfish.wm.state.iconify

    (export iconify-window
	    uniconify-window
	    toggle-window-iconified
	    iconify-workspace-windows
	    make-window-sticky
	    make-window-unsticky
	    toggle-window-sticky
	    toggle-single-window-mode)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.workspace
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.workspace
	  sawfish.wm.stacking
	  sawfish.wm.viewport
	  sawfish.wm.util.groups
	  sawfish.wm.commands.groups)
  
  ;; Commentary:

  ;; Iconification (but without icons)

  ;; If iconified, a window has its `iconified' property set to t

  (defcustom focus-windows-on-uniconify nil
    "Windows are focused after being uniconified."
    :type boolean
    :group (min-max iconify))

  (defcustom raise-windows-on-uniconify t
    "Windows are raised after being uniconified."
    :type boolean
    :user-level expert
    :group (min-max iconify))

  (defcustom iconify-ignored nil
    "Unmanaged windows may be iconified."
    :type boolean
    :user-level expert
    :group (min-max iconify))

  (defcustom uniconify-to-current-workspace t
    "Windows uniconify to the current workspace."
    :type boolean
    :user-level expert
    :group (min-max iconify))

  (defcustom iconify-group-mode 'transients
    "Iconifying a window also iconifies the: \\w"
    :type (choice none transients group)
    :group (min-max iconify))

  (defcustom uniconify-group-mode 'transients
    "Uniconifying a window also uniconifies the: \\w"
    :type (choice none transients group)
    :group (min-max iconify))

  (define (iconify-window w)
    "Iconify the window."
    (when (and (not (window-get w 'iconified))
	       (or iconify-ignored (not (window-get w 'ignored))))
      (window-put w 'iconified t)
      (when (window-visible-p w)
	(hide-window w))
      (call-window-hook 'iconify-window-hook w)
      (call-window-hook 'window-state-change-hook w (list '(iconified)))
      (case iconify-group-mode
	((transients) (iconify-transient-group w))
	((group) (iconify-group w)))))

  (define (uniconify-window w)
    "Return the window from its iconified state."
    (when (window-get w 'iconified)
      (window-put w 'iconified nil)
      (cond ((window-get w 'sticky)
	     (show-window w))
	    ((window-in-workspace-p w current-workspace)
	     (show-window w))
	    (uniconify-to-current-workspace
	     (ws-remove-window w t)
	     (ws-add-window-to-space w current-workspace)))
      (when raise-windows-on-uniconify
	(raise-window w))
      (when (and focus-windows-on-uniconify (window-really-wants-input-p w))
	(set-input-focus w))
      (call-window-hook 'uniconify-window-hook w)
      (call-window-hook 'window-state-change-hook w (list '(iconified)))
      (case uniconify-group-mode
	((transients) (uniconify-transient-group w))
	((group) (uniconify-group w)))))

  (define (toggle-window-iconified w)
    "Toggle the iconification of window W."
    (if (window-get w 'iconified)
	(uniconify-window w)
      (iconify-window w)))

  (define-command 'iconify-window iconify-window "%W")
  (define-command 'uniconify-window uniconify-window "%W")
  (define-command 'toggle-window-iconified toggle-window-iconified "%W")

  (define (iconify-workspace-windows)
    "Iconify all windows on the current workspace."
    (map-windows (lambda (w)
		   (when (and (not (window-get w 'ignored))
			      (window-in-workspace-p w current-workspace))
		     (iconify-window w)))))

  (define-command 'iconify-workspace-windows iconify-workspace-windows)


;;; sticky-ness, could be in a separate file..

  (define (make-window-sticky w)
    (unless (and (window-get w 'sticky) (window-get w 'sticky-viewport))
      (ws-remove-window w t)
      (when (window-outside-viewport-p w)
	(move-window-to-current-viewport w))
      (window-put w 'sticky t)
      (window-put w 'sticky-viewport t)
      (call-window-hook 'window-state-change-hook w (list '(sticky)))))
  
  (define (make-window-unsticky w)
    (when (or (window-get w 'sticky) (window-get w 'sticky-viewport))
      (window-put w 'sticky nil)
      (window-put w 'sticky-viewport nil)
      (ws-add-window-to-space w current-workspace)
      (call-window-hook 'window-state-change-hook w (list '(sticky)))))
  
  (define (toggle-window-sticky w)
    "Toggle the `stickiness' of the window--whether or not it is a member of
all workspaces."
    (if (or (window-get w 'sticky) (window-get w 'sticky-viewport))
	(make-window-unsticky w)
      (make-window-sticky w)))

  (define-command 'make-window-sticky make-window-sticky "%W")
  (define-command 'make-window-unsticky make-window-unsticky "%W")
  (define-command 'toggle-window-sticky toggle-window-sticky "%W")


;;; MacOS X single-window mode stuff

  (define (toggle-single-window-mode w)
    (let ((iconify-whole-group nil)
	  (raise-windows-on-uniconify nil)
	  fun)
      (map-other-window-groups
       (lambda (x)
	 (when (and (windows-share-workspace-p w x)
		    (not (window-get x 'ignored)))
	   (unless fun
	     (setq fun (if (window-get x 'iconified)
			   uniconify-window
			 iconify-window)))
	   (fun x))) w)))

  (define-command 'toggle-single-window-mode toggle-single-window-mode "%W")


;;; hooks

  (define (ws-client-msg-handler w prop data)
    (cond ((and (windowp w)
		(eq prop 'WM_CHANGE_STATE)
		(= (aref data 0) IconicState))
	   (iconify-window w)
	   t)))

  (define (ws-honour-client-state w)
    (let ((state (get-x-property w 'WM_STATE)))
      (when state
	(setq state (aref (nth 2 state) 0))
	(when (eq state IconicState)
	  (window-put w 'iconified t)))))

  (define (ws-set-client-state w)
    (set-x-property w 'WM_STATE
		    (vector (if (window-get w 'iconified)
				IconicState
			      NormalState) 0)
		    'WM_STATE 32))

  (add-hook 'client-message-hook ws-client-msg-handler)
  (add-hook 'before-add-window-hook ws-honour-client-state)
  (add-hook 'map-notify-hook ws-set-client-state t)
  (call-after-state-changed '(iconified) ws-set-client-state))
