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

(require 'workspace)
(provide 'iconify)

;; Commentary:

;; Iconification (but without icons)

;; If iconified, a window has its `iconified' property set to t

(defun iconify-window (w)
  "Iconify the window."
  (interactive "%W")
  (unless (window-get w 'iconified)
    (window-put w 'iconified t)
    (when (window-visible-p w)
      (hide-window w))
    (call-window-hook 'iconify-window-hook w)
    (call-window-hook 'window-state-change-hook w)
    (when iconify-whole-group
      (iconify-group w))))

(defun uniconify-window (w)
  "Return the window from its iconified state."
  (interactive "%W")
  (when (window-get w 'iconified)
    (window-put w 'iconified nil)
    (cond ((window-get w 'sticky)
	   (show-window w))
	  (uniconify-to-current-workspace
	   (ws-remove-window w)
	   (ws-add-window-to-space w current-workspace))
	  ((window-in-workspace-p w current-workspace)
	   (show-window w)))
    (when raise-windows-on-uniconify
      (raise-window w))
    (when (and focus-windows-on-uniconify (window-really-wants-input-p w))
      (set-input-focus w))
    (call-window-hook 'uniconify-window-hook w)
    (call-window-hook 'window-state-change-hook w)
    (when uniconify-whole-group
      (uniconify-group w))))

(defun display-window (w &optional preferred-space)
  "Display the workspace containing the window W, then focus on W."
  (interactive "%W")
  (when w
    (if (and (window-get w 'iconified)
	     (or uniconify-to-current-workspace (window-get w 'sticky)))
	(uniconify-window w)
      (when (or (not preferred-space)
		(not (window-in-workspace-p w preferred-space)))
	(setq preferred-space
	      (nearest-workspace-with-window w current-workspace)))
      (when preferred-space
	(select-workspace preferred-space))
      (move-viewport-to-window w)
      (uniconify-window w)
      (when (and unshade-selected-windows (window-get w 'shaded))
	(unshade-window w))
      (when raise-selected-windows
	(raise-window w))
      (when warp-to-selected-windows
	(warp-cursor-to-window w))
      (when (window-really-wants-input-p w)
	(set-input-focus w))
      (window-order-push w))))


;; sticky-ness, could be in a separate file..

(defun make-window-sticky (w)
  (interactive "%W")
  (unless (window-get w 'sticky)
    (ws-remove-window w t)
    (window-put w 'sticky t)
    (window-put w 'sticky-viewport t)
    (call-window-hook 'window-state-change-hook w)))
  
(defun make-window-unsticky (w)
  (interactive "%W")
  (when (window-get w 'sticky)
    (window-put w 'sticky nil)
    (window-put w 'sticky-viewport nil)
    (ws-add-window-to-space w current-workspace)
    (call-window-hook 'window-state-change-hook w)))
  
(defun toggle-window-sticky (w)
  "Toggle the `stickiness' of the window--whether or not it is a member of
all workspaces."
  (interactive "%W")
  (if (window-get w 'sticky)
      (make-window-unsticky w)
    (make-window-sticky w)))


;; MacOS X single-window mode stuff

;;;###autoload
(defun toggle-single-window-mode (w)
  (interactive "%W")
  (let
      ((iconify-whole-group nil)
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


;; hooks

(defun ws-client-msg-handler (w prop data)
  (cond ((and (windowp w)
	      (eq prop 'WM_CHANGE_STATE)
	      (= (aref data 0) IconicState))
	 (iconify-window w)
	 t)))

(defun ws-honour-client-state (w)
  (let
      ((state (get-x-property w 'WM_STATE)))
    (when state
      (setq state (aref (nth 2 state) 0))
      (when (eq state IconicState)
	(window-put w 'iconified t)))))

(defun ws-set-client-state (w)
  (set-x-property w 'WM_STATE
		  (vector (if (window-get w 'iconified)
			      IconicState
			    NormalState) 0)
		  'WM_STATE 32))


;; init

(add-hook 'client-message-hook ws-client-msg-handler)
(add-hook 'before-add-window-hook ws-honour-client-state)
(add-hook 'add-window-hook ws-set-client-state t)
(add-hook 'window-state-change-hook ws-set-client-state)
