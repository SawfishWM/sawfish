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
	       (while (< (length workspace-list) preallocated-workspaces)
		 (ws-add-workspace t))))

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

(defcustom warp-to-selected-windows t
  "Warp the mouse pointer to selected windows."
  :type boolean
  :group misc)

;; List of all workspaces; a workspace is `(workspace WINDOWS...)'.
;; Each window has its `workspace' property set to the workspace it's
;; a member of. [src/gnome.c accesses this variable]
(defvar workspace-list nil)

;; Currently active workspace. [src/gnome.c accesses this variable]
(defvar current-workspace nil)

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

;; window shouldn't be in any workspace
(defun ws-add-window-to-space (w space)
  (unless (or (window-get w 'sticky)
	      (window-get w 'workspace))
    (rplacd space (nconc (cdr space) (list w)))
    (window-put w 'workspace space)
    (if (and current-workspace
	     (eq space current-workspace)
	     (not (window-get w 'iconified)))
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
	       (not (eq (window-get parent 'workspace) current-workspace)))
	  ;; put the window on its parents workspace
	  (ws-add-window-to-space w (window-get parent 'workspace))
	(if (null current-workspace)
	    (progn
	      ;; initialisation
	      (setq current-workspace (list 'workspace w))
	      (setq workspace-list (list current-workspace))
	      (call-hook 'add-workspace-hook (list current-workspace)))
	  (rplacd current-workspace
		  (nconc (delq w (cdr current-workspace)) (list w))))
	(window-put w 'workspace current-workspace)
	(unless (window-visible-p w)
	  (show-window w))
	(call-window-hook 'add-to-workspace-hook w))
      (call-hook 'workspace-state-change-hook))))

(defun ws-add-window-to-space-by-index (w index)
  (while (>= index (length workspace-list))
    (ws-add-workspace t))
  (ws-add-window-to-space w (nth index workspace-list)))

;; called from the map-notify hook
(defun ws-window-mapped (w)
  (let
      (parent)
    (when (and transients-on-parents-workspace
	       (window-transient-p w)
	       (not (window-get w 'sticky))
	       (setq parent (get-window-by-id (window-transient-p w)))
	       (window-get parent 'workspace)
	       (not (eq (window-get w 'workspace)
			(window-get parent 'workspace))))
      (ws-remove-window w)
      (ws-add-window-to-space w (window-get parent 'workspace)))))

(defun ws-remove-window (w &optional dont-hide)
  (let
      ((space (window-get w 'workspace)))
    (when space
      (rplacd space (delq w (cdr space)))
      (when (and delete-workspaces-when-empty
		 (null (cdr space))
		 (/= (length workspace-list) 1))
	;; workspace is now empty
	(when (eq current-workspace space)
	  (ws-switch-workspace (or (nth 1 (memq space workspace-list))
				   ;; deleted the last workspace
				   (nth (- (length workspace-list) 2)
					workspace-list))))
	(setq workspace-list (delq space workspace-list))
	(call-hook 'delete-workspace-hook (list space)))
      (window-put w 'workspace nil)
      (call-window-hook 'remove-from-workspace-hook w (list space))
      (when (and (not dont-hide) (windowp w))
	(hide-window w))
      (call-hook 'workspace-state-change-hook))))

(defun ws-add-workspace (at-end)
  (let
      ((space (list 'workspace)))
    (unless current-workspace
      (setq current-workspace space))
    (if at-end
	(setq workspace-list (nconc workspace-list (list space)))
      (setq workspace-list (cons space workspace-list)))
    (call-hook 'add-workspace-hook (list space))
    space))

(defun ws-insert-workspace ()
  (if (null workspace-list)
      (ws-add-workspace t)
    (let
	((space (list 'workspace))
	 (join (memq current-workspace workspace-list)))
      (rplacd join (cons space (cdr join)))
      (call-hook 'add-workspace-hook (list space))
      (call-hook 'workspace-state-change-hook)
      space)))

(defun ws-find-next-workspace (&optional cycle)
  (when (cdr workspace-list)
    (let
	((tem (nth 1 (memq current-workspace workspace-list))))
      (or tem (and cycle (car workspace-list))))))

(defun ws-find-previous-workspace (&optional cycle)
  (when (cdr workspace-list)
    (let
	((tem workspace-list))
      (while (and (cdr tem) (not (eq (nth 1 tem) current-workspace)))
	(setq tem (cdr tem)))
      (and (or cycle (cdr tem))
	   (car tem)))))

(defun ws-switch-workspace (space)
  (unless (eq current-workspace space)
    (when current-workspace
      (mapc 'hide-window (cdr current-workspace))
      (call-hook 'leave-workspace-hook (list current-workspace)))
    (setq current-workspace space)
    (when current-workspace
      (mapc #'(lambda (w)
		(unless (window-get w 'iconified)
		  (show-window w))) (cdr current-workspace))
      (call-hook 'enter-workspace-hook (list current-workspace))
      (call-hook 'workspace-state-change-hook))))

(defun ws-merge-workspaces (src dest)
  ;; XXX doing this causes a nasty flicker of windows that
  ;; XXX get unmapped, moved, then re-mapped
  (when (eq current-workspace src)
    (ws-switch-workspace dest))
  (while (cdr src)
    (let
	((w (nth 1 src)))
      (ws-remove-window w)
      (ws-add-window-to-space w dest)))
  (setq workspace-list (delq src workspace-list))
  (call-hook 'delete-workspace-hook (list src))
  (call-hook 'workspace-state-change-hook))

(defun ws-move-workspace (space count)
  (let*
      ((current (- (length workspace-list)
		   (length (memq space workspace-list))))
       (desired (min (max 0 (+ current count)) (1- (length workspace-list))))
       tem)
    (unless (= current desired)
      (setq workspace-list (delq space workspace-list))
      (if (zerop desired)
	  (setq workspace-list (cons space workspace-list))
	(setq tem (nthcdr (1- desired) workspace-list))
	(rplacd tem (cons space (cdr tem))))
      (call-hook 'move-workspace-hook (list space))
      (call-hook 'workspace-state-change-hook))))

(defun workspace-index (&optional space)
  (unless space
    (setq space current-workspace))
  (- (length workspace-list) (length (memq space workspace-list))))


;; Menu constructors

(defun workspace-menu ()
  (let
      ((tem workspace-list)
       (i 0)
       menu)
    (while tem
      (setq menu (cons (list (format nil "space %d%s"
				     (1+ i)
				     (if (eq (car tem) current-workspace)
					 " *" ""))
			     `(lambda ()
				(ws-switch-workspace (nth ,i workspace-list))))
		       menu))
      (setq tem (cdr tem))
      (setq i (1+ i)))
    (nconc (nreverse menu) (list nil) static-workspace-menus)))

(defun popup-workspace-list ()
  "Display the menu containing the list of all workspaces."
  (interactive)
  (popup-menu (workspace-menu)))

(defun window-menu ()
  (let
      (menu space win name)
    (setq space workspace-list)
    (while space
      (setq win (cdr (car space)))
      (while win
	(when (window-mapped-p (car win))
	  (setq name (window-name (car win)))
	  (setq menu (cons (list (concat
				  (and (window-get (car win) 'iconified) ?\[)
				  (if (> (length name) 20)
				      (concat (substring name 0 20) "...")
				    name)
				  (and (window-get (car win) 'iconified)  ?\])
				  (and (eq (input-focus) (car win)) " *"))
				 `(lambda ()
				    (display-window
				     (get-window-by-id
				      ,(window-id (car win))))))
			   menu)))
	(setq win (cdr win)))
      (setq space (cdr space))
      (when space
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
	    (managed-windows))
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
  (let
      ((space (ws-find-next-workspace cycle-through-workspaces)))
    (when space
      (ws-switch-workspace space))))

(defun send-to-next-workspace (window)
  "Move the window to the next workspace. If no next workspace exists, one
will be created."
  (interactive "f")
  (let
      ((space (or (ws-find-next-workspace)
		  (ws-add-workspace t))))
    (ws-remove-window window)
    (ws-add-window-to-space window space)))

(defun previous-workspace ()
  "Display the previous workspace."
  (interactive)
  (let
      ((space (ws-find-previous-workspace cycle-through-workspaces)))
    (when space
      (ws-switch-workspace space))))

(defun send-to-previous-workspace (window)
  "Move the window to the previous workspace. If no such workspace exists, one
will be created."
  (interactive "f")
  (let
      ((space (or (ws-find-previous-workspace)
		  (ws-add-workspace nil))))
    (ws-remove-window window)
    (ws-add-window-to-space window space)))

(defun next-workspace-window ()
  "Focus on the next window of the current workspace."
  (interactive)
  (let
      ((windows (filter 'window-visible-p (cdr current-workspace))))
    (display-window (or (nth 1 (memq (input-focus) windows))
			(car windows)))))

(defun next-window ()
  "Focus on the next window, cycling through all possible workspaces."
  (interactive)
  (catch 'out
    (let*
	((space current-workspace)
	 (windows (filter 'window-visible-p (cdr space)))
	 (win (nth 1 (memq (input-focus) windows))))
      (while (not win)
	(setq space (or (nth 1 (memq space workspace-list))
			(car workspace-list)))
	(when (or (not space) (eq space current-workspace))
	  (throw 'out nil))
	(setq windows (filter #'(lambda (w)
				  (not (window-get w 'iconified)))
			      (cdr space)))
	(setq win (car windows)))
      (when win
	(display-window win)))))

(defun select-workspace (index)
  "Activate workspace number INDEX (from zero)."
  (interactive "p")
  (when (>= index 0)
    (let
	((space (nth index workspace-list)))
      (when (and space (not (eq space current-workspace)))
	(ws-switch-workspace space)))))

(defun merge-next-workspace ()
  "Delete the current workspace. Its member windows are relocated to the next
workspace."
  (interactive)
  (when (> (length workspace-list) 1)
    (ws-merge-workspaces current-workspace (ws-find-next-workspace t))))

(defun merge-previous-workspace ()
  "Delete the current workspace. Its member windows are relocated to the
previous workspace."
  (interactive)
  (when (> (length workspace-list) 1)
    (ws-merge-workspaces current-workspace (ws-find-previous-workspace t))))

(defun insert-workspace ()
  "Create a new workspace following the current workspace."
  (interactive)
  (ws-switch-workspace (ws-insert-workspace)))

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
    (cond ((or (not (window-get w 'worspace))
	       (eq (window-get w 'workspace) current-workspace))
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
	(when (and space (not (eq space current-workspace)))
	  (ws-switch-workspace space))
	(uniconify-window w)
	(when (window-get w 'shaded)
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

(defun ws-set-client-state (w)
  (set-x-property w 'WM_STATE
		  (vector (if (window-get w 'iconified)
			      IconicState
			    NormalState))
		  'WM_STATE 32))


;; Session manager hooks for workspace information

(defun ws-save-session (w)
  (let
      ((space (window-get w 'workspace)))
    (when space
      `((workspace . ,(workspace-index space))))))

(defun ws-restore-session (w alist)
  (let
      ((index (cdr (assq 'workspace alist))))
    (when (and index (numberp index) (>= index 0))
      (ws-add-window-to-space-by-index w index))))


;; Initialisation

(unless (or batch-mode (memq 'ws-add-window add-window-hook))
  (add-hook 'add-window-hook 'ws-add-window t)
  (add-hook 'destroy-notify-hook 'ws-remove-window t)
  (add-hook 'map-notify-hook 'ws-window-mapped t)
  (add-hook 'client-message-hook 'ws-client-msg-handler t)
  (add-hook 'add-window-hook 'ws-set-client-state t)
  (add-hook 'window-state-change-hook 'ws-set-client-state t)
  (add-hook 'sm-window-save-functions 'ws-save-session)
  (add-hook 'sm-restore-window-hook 'ws-restore-session)
  (sm-add-saved-properties 'sticky 'iconified)
  (mapc 'ws-add-window (managed-windows)))
