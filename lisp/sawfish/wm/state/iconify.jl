;; iconify.jl -- iconification & (bah) stickiness

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; By historical reason, stickiness is here, too.

(define-structure sawfish.wm.state.iconify

    (export window-iconified-p
	    window-iconifiable-p
	    iconify-window
	    uniconify-window
	    toggle-window-iconified
	    iconify-workspace-windows
	    window-sticky-p/workspace
	    make-window-sticky/workspace
	    make-window-unsticky/workspace
	    window-sticky-p/viewport
	    make-window-sticky/viewport
	    make-window-unsticky/viewport
	    window-sticky-p
	    make-window-sticky
	    make-window-unsticky
	    toggle-window-sticky)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.workspace
	  sawfish.wm.custom
	  sawfish.wm.commands
	  sawfish.wm.workspace
	  sawfish.wm.stacking
	  sawfish.wm.viewport
	  sawfish.wm.util.groups
	  sawfish.wm.commands.groups
	  sawfish.wm.state.transient
	  sawfish.wm.menus)

  ;; Commentary:

  ;; Iconification (but without icons)

  ;; If iconified, a window has its `iconified' property set to t

  (defvar focus-windows-on-uniconify nil
    "Windows are focused after being minimized.")

  (defvar raise-windows-on-uniconify t
    "Windows are raised after being unminimized.")

  (defvar uniconify-to-current-workspace t
    "Move windows to the current workspace when they are unminimized.")

  (defvar iconify-ignored nil
    "Unmanaged windows may be iconified.")

  (defcustom iconify-group-mode 'transients
    "Minimizing a window also removes its: \\w"
    :type (choice none transients group)
    :group min-max)

  (defcustom uniconify-group-mode 'transients
    "Unminimizing a window also restores its: \\w"
    :type (choice none transients group)
    :group min-max)

  (define (window-iconified-p w) (window-get w 'iconified))

  (define (window-iconifiable-p w)
    (and (not (window-get w 'never-iconify))
	 (not (window-get w 'iconified))
	 (not (desktop-window-p w))
	 (or iconify-ignored (not (window-get w 'ignored)))))

  (define (iconify-window-1 w)
    (when (window-iconifiable-p w)
      (window-put w 'iconified t)
      (when (window-visible-p w)
	(hide-window w))
      (call-window-hook 'iconify-window-hook w)
      (call-window-hook 'window-state-change-hook w (list '(iconified)))))

  (define (uniconify-window-1 w)
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
	(activate-window w))
      (call-window-hook 'uniconify-window-hook w)
      (call-window-hook 'window-state-change-hook w (list '(iconified)))))

  (define (windows-to-change w type)
    (case type
      ((transients)
       (filter (lambda (x)
		 (or (eq x w)
		     ;; Only include transients which would have
		     ;; no parents
		     (let ((parents
			    (delete-if window-iconified-p
				       (delq w (transient-parents x)))))
		       (null parents))))
	       (transient-group w)))
      ((group) (windows-in-group w))
      (t (list w))))

  (define (iconify-window w)
    "Minimize the window."
    (mapc iconify-window-1 (windows-to-change w iconify-group-mode)))

  (define (uniconify-window w)
    "Restore the window from its minimized state."
    (mapc uniconify-window-1 (windows-to-change w uniconify-group-mode)))

  (define (toggle-window-iconified w)
    "Toggle the iconification of window W."
    (if (window-get w 'iconified)
	(uniconify-window w)
      (iconify-window w)))

  (define-command 'iconify-window iconify-window #:spec "%W")
  (define-command 'uniconify-window uniconify-window #:spec "%W")
  (define-command 'toggle-window-iconified toggle-window-iconified #:spec "%W")

  (define (iconify-workspace-windows)
    "Minimize all windows on the current workspace."
    (map-windows (lambda (w)
		   (when (and (not (window-get w 'ignored))
			      (window-in-workspace-p w current-workspace))
		     (iconify-window w)))))

  (define-command 'iconify-workspace-windows iconify-workspace-windows)

;;; sticky-ness, could be in a separate file..

  ;; workspace sticky code

  (define (window-sticky-p/workspace w) (window-get w 'sticky))

  (define (emit-sticky-hook w)
    (call-window-hook 'window-state-change-hook w (list '(sticky))))

  (define (make-window-sticky/workspace w #!key no-hooks)
    (unless (window-sticky-p/workspace w)
      (ws-remove-window w t)
      (window-put w 'sticky t)
      (unless no-hooks
	(emit-sticky-hook w))))

  (define (make-window-unsticky/workspace w #!key no-hooks)
    (when (window-sticky-p/workspace w)
      (window-put w 'sticky nil)
      (ws-add-window-to-space w current-workspace)
      (unless no-hooks
	(emit-sticky-hook w))))

  ;; viewport sticky code

  (define (window-sticky-p/viewport w) (window-get w 'sticky-viewport))

  (define (make-window-sticky/viewport w #!key no-hooks)
    (unless (window-sticky-p/viewport w)
      (when (window-outside-viewport-p w)
	(move-window-to-current-viewport w))
      (window-put w 'sticky-viewport t)
      (unless no-hooks
	(emit-sticky-hook w))))

  (define (make-window-unsticky/viewport w #!key no-hooks)
    (when (window-sticky-p/viewport w)
      (window-put w 'sticky-viewport nil)
      (unless no-hooks
	(emit-sticky-hook w))))

  ;; combined code

  (define (window-sticky-p w)
    (or (window-sticky-p/workspace w) (window-sticky-p/viewport w)))

  (define (make-window-sticky w)
    (unless (window-sticky-p w)
      (make-window-sticky/workspace w #:no-hooks t)
      (make-window-sticky/viewport w #:no-hooks t)
      (emit-sticky-hook w)))

  (define (make-window-unsticky w)
    (when (window-sticky-p w)
      (make-window-unsticky/workspace w #:no-hooks t)
      (make-window-unsticky/viewport w #:no-hooks t)
      (emit-sticky-hook w)))

  (define (toggle-window-sticky w)
    "Toggle the `stickiness' of the window--whether or not it is a member of
all workspaces."
    (if (window-sticky-p w)
	(make-window-unsticky w)
      (make-window-sticky w)))

  (define-command 'make-window-sticky make-window-sticky #:spec "%W")
  (define-command 'make-window-unsticky make-window-unsticky #:spec "%W")
  (define-command 'toggle-window-sticky toggle-window-sticky #:spec "%W")

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
  (call-after-state-changed '(iconified) ws-set-client-state)

  (add-window-menu-toggle (_ "_Sticky") 'toggle-window-sticky window-sticky-p))
