;; workspace.jl -- similar to virtual desktops
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

(provide 'workspace)

;; Commentary:

;; Sawmill's workspace are organised as a dynamic 1-dimensional
;; structure. Workspaces are added and deleted as required (subject to
;; the options defined below)

;; If you want to approximate the common N-by-N grid approach to
;; virtual desktops, then you could set `preallocated-workspaces' to
;; NxN and add something like the following to your .sawmillrc:

;; (defvar workspace-grid-width 2)

;; (bind-keys global-keymap
;;   "C-Down" '(select-workspace (+ (workspace-index) workspace-grid-width))
;;   "C-Up" '(select-workspace (- (workspace-index) workspace-grid-width)))

;; substituting the 2 for suitable values of N. This won't change the
;; way that the GNOME hints portray the workspace list, but it's a start..


;; Options and variables

(defcustom cycle-through-workspaces nil
  "Moving through workspaces is cyclical."
  :type boolean
  :group workspace)

(defcustom delete-workspaces-when-empty t
  "Workspaces are deleted when they contain no windows."
  :type boolean
  :group workspace)

(defcustom uniconify-to-current-workspace t
  "Windows are uniconified onto the current workspace."
  :type boolean
  :group workspace)

(defcustom preallocated-workspaces 1
  "The number of workspaces that are allocated automatically at startup."
  :type number
  :range (1 . nil)
  :group workspace
  :after-set (lambda ()
	       (when (< total-workspaces preallocated-workspaces)
		 (setq total-workspaces preallocated-workspaces))
	       (call-hook workspace-state-change-hook)))

(defcustom raise-windows-on-uniconify t
  "Windows are raised after being uniconified."
  :type boolean
  :group misc)

(defcustom transients-on-parents-workspace nil
  "Transient windows are opened on the same workspace as their parent window."
  :type boolean
  :group workspace)

(defcustom raise-selected-windows t
  "Windows selected (normally by the Windows menu) are raised."
  :type boolean
  :group misc)

(defcustom unshade-selected-windows nil
  "Unshade selected windows."
  :type boolean
  :group misc)

(defcustom warp-to-selected-windows t
  "Warp the mouse pointer to selected windows."
  :type boolean
  :group misc)

;; Currently active workspace, an integer >= 0
(defvar current-workspace 0)
(defvar total-workspaces preallocated-workspaces)

(defvar static-workspace-menus
  '(("Insert" insert-workspace)
    ("Next" next-workspace)
    ("Previous" previous-workspace)
    ("Merge next" merge-next-workspace)
    ("Merge previous" merge-previous-workspace)
    ("Move right" move-workspace-forwards)
    ("Move left" move-workspace-backwards)))

(defconst NormalState 1)
(defconst IconicState 3)


;; Low level functions

(defun ws-workspace-empty-p (space)
  (catch 'out
    (mapc #'(lambda (w)
	      (when (and (window-get w 'workspace)
			 (= (window-get w 'workspace) space))
		(throw 'out nil))) (managed-windows))
    t))

(defun ws-insert-workspace (&optional before)
  (unless before
    (setq before (1+ current-workspace)))
  (mapc #'(lambda (w)
	    (let
		((space (window-get w 'workspace)))
	      (when (and space (> space before))
		(window-put w 'workspace (1+ space)))))
	(managed-windows))
  (when (> current-workspace before)
    (setq current-workspace (1+ current-workspace)))
  (setq total-workspaces (1+ total-workspaces))
  (call-hook 'add-workspace-hook (list (1+ before)))
  (call-hook 'workspace-state-change-hook)
  (1+ before))

;; moves any windows to the next workspace
(defun ws-remove-workspace (&optional index)
  (unless index
    (setq index current-workspace))
  (setq total-workspaces (1- total-workspaces))
  (when (> current-workspace index)
    (setq current-workspace (1- current-workspace)))
  (mapc #'(lambda (w)
	    (let
		((space (window-get w 'workspace)))
	      (when space
		(when (> space index)
		  (setq space (1- space))
		  (window-put w 'workspace space))
		(when (and (= space current-workspace)
			   (not (window-get w 'iconified)))
		  (show-window w)))))
	(managed-windows))
  (call-hook 'delete-workspace-hook (list index))
  (call-hook 'workspace-state-change-hook))

(defun ws-move-workspace (index count)
  (let
      ((windows (managed-windows)))
    (while (and (> count 0) (< index (1- total-workspaces)))
      (mapc #'(lambda (w)
		(let
		    ((space (window-get w 'workspace)))
		  (when space
		   (cond ((= space index)
			  (window-put w 'workspace (1+ space)))
			 ((= space (1+ index))
			  (window-put w 'workspace index))))))
	    windows)
      (setq count (1- count))
      (setq index (1+ index)))
    (while (and (< count 0) (> index 0))
      (mapc #'(lambda (w)
		(let
		    ((space (window-get w 'workspace)))
		  (when space
		    (cond ((= space index)
			   (window-put w 'workspace (1- space)))
			  ((= space (1- index))
			   (window-put w 'workspace index))))))
	    windows)
      (setq count (1+ count))
      (setq index (1- index)))
    (call-hook 'move-workspace-hook (list index))
    (call-hook 'workspace-state-change-hook)))

(defun ws-after-removing-window (w space)
  (when (and delete-workspaces-when-empty
	     (ws-workspace-empty-p space)
	     (> total-workspaces 1))
    ;; workspace is now empty
    (ws-remove-workspace space)))

(defun ws-remove-window (w &optional dont-hide)
  (let
      ((space (window-get w 'workspace)))
    (when space
      (window-put w 'workspace nil)
      (ws-after-removing-window w space)
      (when (and (not dont-hide) (windowp w))
	(hide-window w))
      (call-hook 'workspace-state-change-hook))))

(defun ws-move-window (w new)
  (let
      ((space (window-get w 'workspace)))
    (when space
      (window-put w 'workspace new)
      (when (>= new total-workspaces)
	(setq total-workspaces (1+ new)))
      (cond ((= space current-workspace)
	     (hide-window w))
	    ((and (= new current-workspace) (not (window-get w 'iconified)))
	     (show-window w)))
      (ws-after-removing-window w space)
      (call-hook 'workspace-state-change-hook))))

(defun ws-workspace-windows (space &optional include-iconified)
  (filter #'(lambda (w)
	      (and (equal (window-get w 'workspace) space)
		   (or include-iconified
		       (not (window-get w 'iconified)))))
	  (managed-windows)))


;; slightly higher level

;; window shouldn't be in any workspace
(defun ws-add-window-to-space (w space)
  (unless (or (window-get w 'sticky) (window-get w 'workspace))
    (window-put w 'workspace space)
    (when (>= space total-workspaces)
      (setq total-workspaces (1+ space)))
    (if (and (= space current-workspace) (not (window-get w 'iconified)))
	(show-window w)
      (hide-window w))
    (call-window-hook 'add-to-workspace-hook w)
    (call-hook 'workspace-state-change-hook)))

;; usually called from the add-window-hook
(defun ws-add-window (w)
  (unless (or (window-get w 'sticky)
	      (window-get w 'workspace))
    (let
	(parent)
      (if (and transients-on-parents-workspace
	       (window-transient-p w)
	       (setq parent (get-window-by-id (window-transient-p w)))
	       (window-get parent 'workspace)
	       (/= (window-get parent 'workspace) current-workspace))
	  ;; put the window on its parents workspace
	  (ws-add-window-to-space w (window-get parent 'workspace))
	(window-put w 'workspace current-workspace)
	(unless (window-visible-p w)
	  (show-window w))
	(call-window-hook 'add-to-workspace-hook w))
      (call-hook 'workspace-state-change-hook))))

;; called from the map-notify hook
(defun ws-window-mapped (w)
  (let
      (parent)
    (when (and transients-on-parents-workspace
	       (window-transient-p w)
	       (not (window-get w 'sticky))
	       (setq parent (get-window-by-id (window-transient-p w)))
	       (window-get parent 'workspace)
	       (/= (window-get w 'workspace) (window-get parent 'workspace)))
      (ws-remove-window w)
      (ws-add-window-to-space w (window-get parent 'workspace)))
    (raise-window w)))

(defun ws-switch-workspace (space)
  (unless (= current-workspace space)
    (when current-workspace
      (mapc #'(lambda (w)
		(when (and (window-get w 'workspace)
			   (= (window-get w 'workspace) current-workspace))
		  (hide-window w)))
	    (managed-windows))
      (call-hook 'leave-workspace-hook (list current-workspace)))
    (setq current-workspace space)
    (when current-workspace
      (mapc #'(lambda (w)
		(when (and (window-get w 'workspace)
			   (= (window-get w 'workspace) current-workspace)
			   (not (window-get w 'iconified)))
		  (show-window w)))
	    (managed-windows))
      (call-hook 'enter-workspace-hook (list current-workspace))
      (call-hook 'workspace-state-change-hook))))


;; Menu constructors

(defun workspace-menu ()
  (let
      ((i 0)
       menu)
    (while (< i total-workspaces)
      (setq menu (cons (list (format nil "space %d%s"
				     (1+ i)
				     (if (= i current-workspace) " *" ""))
			     `(lambda ()
				(ws-switch-workspace ,i)))
		       menu))
      (setq i (1+ i)))
    (nconc (nreverse menu) (list nil) static-workspace-menus)))

(defun popup-workspace-list ()
  "Display the menu containing the list of all workspaces."
  (interactive)
  (popup-menu (workspace-menu)))

(defun window-menu ()
  (let
      ((i 0)
       (windows (managed-windows))
       menu name)
    (while (< i total-workspaces)
      (mapc #'(lambda (w)
		(when (and (equal (window-get w 'workspace) i)
			   (window-mapped-p w))
		  (setq name (window-name w))
		  (setq menu (cons (list (concat
					  (and (window-get w 'iconified) ?\[)
					  (if (> (length name) 20)
					      (concat
					       (substring name 0 20) "...")
					    name)
					  (and (window-get w 'iconified)  ?\])
					  (and (eq (input-focus) w) " *"))
					 `(lambda ()
					    (display-window
					     (get-window-by-id
					      ,(window-id w)))))
				   menu))))
	    windows)
      (setq i (1+ i))
      (unless (= i total-workspaces)
	(setq menu (cons nil menu))))
    ;; search for any iconified windows that aren't anywhere else in the menu
    (let
	(extra)
      (mapc #'(lambda (w)
		(when (and (window-get w 'iconified)
			   (not (window-get w 'workspace)))
		  (setq extra (cons (list (window-name w)
					  `(lambda ()
					     (display-window
					      (get-window-by-id
					       ,(window-id w)))))
				    extra))))
	    windows)
      (when extra
	(setq menu (nconc extra (list nil) menu))))
    (nreverse menu)))

(defun popup-window-list ()
  "Display the menu of all managed windows."
  (interactive)
  (popup-menu (window-menu)))


;; Commands

(defun next-workspace ()
  "Display the next workspace."
  (interactive)
  (when (< current-workspace (1- total-workspaces))
    (ws-switch-workspace (1+ current-workspace))))

(defun send-to-next-workspace (window)
  "Move the window to the next workspace. If no next workspace exists, one
will be created."
  (interactive "f")
  (let
      ((space (window-get window 'workspace)))
    (when space
      (ws-move-window window (1+ space)))))

(defun previous-workspace ()
  "Display the previous workspace."
  (interactive)
  (when (> current-workspace 0)
    (ws-switch-workspace (1- current-workspace))))

(defun send-to-previous-workspace (window)
  "Move the window to the previous workspace. If no such workspace exists, one
will be created."
  (interactive "f")
  (let
      ((space (window-get window 'workspace)))
    (when space
      (if (zerop space)
	  (ws-insert-workspace -1)	;cheeky!
	(setq space (1- space)))
      (ws-move-window window space))))

(defun next-workspace-window ()
  "Focus on the next window of the current workspace."
  (interactive)
  (let
      ((windows (ws-workspace-windows current-workspace)))
    (display-window (or (nth 1 (memq (input-focus) windows)) (car windows)))))

(defun next-window ()
  "Focus on the next window, cycling through all possible workspaces."
  (interactive)
  (catch 'out
    (let*
	((space current-workspace)
	 (windows (ws-workspace-windows space))
	 (win (nth 1 (memq (input-focus) windows))))
      (while (not win)
	(setq space (1+ space))
	(when (= space total-workspaces)
	  (setq space 0))
	(when (= space current-workspace)
	  (throw 'out nil))
	(setq windows (ws-workspace-windows space))
	(setq win (car windows)))
      (when win
	(display-window win)))))

(defun select-workspace (index)
  "Activate workspace number INDEX (from zero)."
  (interactive "p")
  (when (and (>= index 0) (< index total-workspaces))
    (ws-switch-workspace index)))

(defun merge-next-workspace ()
  "Delete the current workspace. Its member windows are relocated to the next
workspace."
  (interactive)
  (when (< current-workspace (1- total-workspaces))
    (ws-remove-workspace current-workspace)))

(defun merge-previous-workspace ()
  "Delete the current workspace. Its member windows are relocated to the
previous workspace."
  (interactive)
  (when (and (> current-workspace 0) (> total-workspaces 1))
    (ws-remove-workspace (1- current-workspace))))

(defun insert-workspace ()
  "Create a new workspace following the current workspace."
  (interactive)
  (ws-insert-workspace current-workspace)
  (ws-switch-workspace (1+ current-workspace)))

(defun move-workspace-forwards (&optional count)
  "Move the current workspace one place to the right."
  (interactive)
  (ws-move-workspace current-workspace (or count 1)))

(defun move-workspace-backwards (&optional count)
  "Move the current workspace one place to the left."
  (interactive)
  (ws-move-workspace current-workspace (- (or count 1))))


;; some commands for moving directly to a workspace

(let
    ((i 1))
  (while (< i 10)
    (fset (intern (format nil "select-workspace:%s" i))
	  `(lambda ()
	     (interactive)
	     (select-workspace ,(1- i))))
    (setq i (1+ i))))


;; Iconification (but without icons)

;; If iconified, a window has its `iconified' property set to t

(defun iconify-window (w)
  "Iconify the window."
  (interactive "f")
  (unless (window-get w 'iconified)
    (window-put w 'iconified t)
    (when (window-visible-p w)
      (hide-window w))
    (call-window-hook 'iconify-window-hook w)
    (call-window-hook 'window-state-change-hook w)))

(defun uniconify-window (w)
  "Return the window from its iconified state."
  (interactive "f")
  (when (window-get w 'iconified)
    (window-put w 'iconified nil)
    (cond ((or (not (window-get w 'workspace))
	       (= (window-get w 'workspace) current-workspace))
	   (show-window w))
	  (uniconify-to-current-workspace
	   (ws-remove-window w)
	   (ws-add-window w)))
    (when raise-windows-on-uniconify
      (raise-window w))
    (call-window-hook 'uniconify-window-hook w)
    (call-window-hook 'window-state-change-hook w)))

(defun display-window (w)
  "Display the workspace containing the window W, then focus on W."
  (interactive "f")
  (when w
    (if (and (window-get w 'iconified)
	     (or uniconify-to-current-workspace
		 (not (window-get w 'workspace))))
	(uniconify-window w)
      (let
	  ((space (window-get w 'workspace)))
	(when (and space (not (= space current-workspace)))
	  (ws-switch-workspace space))
	(uniconify-window w)
	(when (and unshade-selected-windows (window-get w 'shaded))
	  (unshade-window w))
	(when raise-selected-windows
	  (raise-window w))
	(when warp-to-selected-windows
	  (warp-cursor-to-window w))
	(set-input-focus w)))))

(defun toggle-window-sticky (w)
  "Toggle the `stickiness' of the window--whether or not it is a member of
all workspaces."
  (interactive "f")
  (if (window-get w 'sticky)
      (progn
	(window-put w 'sticky nil)
	(ws-add-window w))
    (ws-remove-window w t)
    (window-put w 'sticky t))
  (call-window-hook 'window-state-change-hook w))

(defun ws-client-msg-handler (w atom data)
  (cond ((and (windowp w)
	      (eq atom 'WM_CHANGE_STATE)
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
			    NormalState))
		  'WM_STATE 32))


;; Initialisation

(unless (or batch-mode (memq 'ws-add-window add-window-hook))
  (add-hook 'add-window-hook 'ws-add-window t)
  (add-hook 'map-notify-hook 'ws-window-mapped t)
  (add-hook 'client-message-hook 'ws-client-msg-handler t)
  (add-hook 'before-add-window-hook 'ws-honour-client-state)
  (add-hook 'add-window-hook 'ws-set-client-state t)
  (add-hook 'window-state-change-hook 'ws-set-client-state t)
  (sm-add-saved-properties 'sticky 'iconified 'workspace)
  (mapc 'ws-honour-client-state (managed-windows))
  (mapc 'ws-add-window (managed-windows)))
