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

(defcustom cycle-through-workspaces nil
  "Moving through workspaces is cyclical."
  :type boolean
  :group workspace)

(defcustom delete-workspaces-when-empty t
  "Workspaces are deleted when they contain no windows."
  :type boolean
  :group workspace)

(defcustom uniconify-to-current-workspace t
  "Windows that are uniconified appear on the current workspace."
  :type boolean
  :group workspace)

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
(defvar ws-workspaces nil)

;; Currently active workspace. [src/gnome.c accesses this variable]
(defvar ws-current-workspace nil)

(defvar static-workspace-menus
  '(("Next" next-workspace)
    ("Previous" previous-workspace)
    ("Insert" insert-workspace)
    ("Merge next" merge-next-workspace)
    ("Merge previous" merge-previous-workspace)
    ("Move right" move-workspace-forwards)
    ("Move left" move-workspace-backwards)))


;; Low level functions

;; window shouldn't be in any workspace
(defun ws-add-window-to-space (w space)
  (unless (window-get w 'sticky)
    (rplacd space (nconc (cdr space) (list w)))
    (window-put w 'workspace space)
    (when (and ws-current-workspace
	       (eq space ws-current-workspace)
	       (not (window-get w 'iconified)))
      (show-window w))
    (call-window-hook 'add-to-workspace-hook w)
    (call-hook 'workspace-state-change-hook)))

;; usually called from the add-window-hook
(defun ws-add-window (w)
  (unless (window-get w 'sticky)
    (let
	(parent)
      (if (and transients-on-parents-workspace
	       (window-transient-p w)
	       (setq parent (get-window-by-id (window-transient-p w)))
	       (window-get parent 'workspace)
	       (not (eq (window-get parent 'workspace) ws-current-workspace)))
	  ;; put the window on its parents workspace
	  (ws-add-window-to-space w (window-get parent 'workspace))
	(if (null ws-current-workspace)
	    (progn
	      ;; initialisation
	      (setq ws-current-workspace (list 'workspace w))
	      (setq ws-workspaces (list ws-current-workspace))
	      (call-hook 'add-workspace-hook (list ws-current-workspace)))
	  (rplacd ws-current-workspace
		  (nconc (delq w (cdr ws-current-workspace)) (list w))))
	(window-put w 'workspace ws-current-workspace)
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
	       (not (eq (window-get w 'workspace)
			(window-get parent 'workspace))))
      (ws-remove-window w)
      (ws-add-window-to-space w (window-get parent 'workspace)))))

(defun ws-remove-window (w &optional dont-hide)
  (let
      ((space (window-get w 'workspace)))
    (when space
      (rplacd space (delq w (cdr space)))
      (when (and delete-workspaces-when-empty (null (cdr space)))
	;; workspace is now empty
	(when (eq ws-current-workspace space)
	  (if (= (length ws-workspaces) 1)
	      ;; deleting the sole workspace!
	      (progn
		(setq ws-workspaces nil)
		(setq ws-current-workspace nil))
	    (ws-switch-workspace (or (nth 1 (memq space ws-workspaces))
				     ;; deleted the last workspace
				     (nth (- (length ws-workspaces) 2)
					  ws-workspaces)))))
	(setq ws-workspaces (delq space ws-workspaces))
	(call-hook 'delete-workspace-hook (list space)))
      (window-put w 'workspace nil)
      (call-window-hook 'remove-from-workspace-hook w (list space))
      (when (and (not dont-hide) (windowp w))
	(hide-window w))
      (call-hook 'workspace-state-change-hook))))

(defun ws-add-workspace (at-end)
  (let
      ((space (list 'workspace)))
    (if at-end
	(setq ws-workspaces (nconc ws-workspaces (list space)))
      (setq ws-workspaces (cons space ws-workspaces)))
    (call-hook 'add-workspace-hook (list space))
    space))

(defun ws-insert-workspace ()
  (if (null ws-workspaces)
      (ws-add-workspace t)
    (let
	((space (list 'workspace))
	 (join (memq ws-current-workspace ws-workspaces)))
      (rplacd join (cons space (cdr join)))
      (call-hook 'add-workspace-hook (list space))
      (call-hook 'workspace-state-change-hook)
      space)))

(defun ws-find-next-workspace (&optional cycle)
  (when (cdr ws-workspaces)
    (let
	((tem (nth 1 (memq ws-current-workspace ws-workspaces))))
      (or tem (and cycle (car ws-workspaces))))))

(defun ws-find-previous-workspace (&optional cycle)
  (when (cdr ws-workspaces)
    (let
	((tem ws-workspaces))
      (while (and (cdr tem) (not (eq (nth 1 tem) ws-current-workspace)))
	(setq tem (cdr tem)))
      (and (or cycle (cdr tem))
	   (car tem)))))

(defun ws-switch-workspace (space)
  (unless (eq ws-current-workspace space)
    (when ws-current-workspace
      (mapc 'hide-window (cdr ws-current-workspace))
      (call-hook 'leave-workspace-hook (list ws-current-workspace)))
    (setq ws-current-workspace space)
    (when ws-current-workspace
      (mapc #'(lambda (w)
		(unless (window-get w 'iconified)
		  (show-window w))) (cdr ws-current-workspace))
      (call-hook 'enter-workspace-hook (list ws-current-workspace))
      (call-hook 'workspace-state-change-hook))))

(defun ws-merge-workspaces (src dest)
  ;; XXX doing this causes a nasty flicker of windows that
  ;; XXX get unmapped, moved, then re-mapped
  (when (eq ws-current-workspace src)
    (ws-switch-workspace dest))
  (while (cdr src)
    (let
	((w (nth 1 src)))
      (ws-remove-window w)
      (ws-add-window-to-space w dest)))
  (setq ws-workspaces (delq src ws-workspaces))
  (call-hook 'delete-workspace-hook (list src))
  (call-hook 'workspace-state-change-hook))

(defun ws-move-workspace (space count)
  (let*
      ((current (- (length ws-workspaces)
		   (length (memq space ws-workspaces))))
       (desired (min (max 0 (+ current count)) (1- (length ws-workspaces))))
       tem)
    (unless (= current desired)
      (setq ws-workspaces (delq space ws-workspaces))
      (if (zerop desired)
	  (setq ws-workspaces (cons space ws-workspaces))
	(setq tem (nthcdr (1- desired) ws-workspaces))
	(rplacd tem (cons space (cdr tem))))
      (call-hook 'move-workspace-hook (list space))
      (call-hook 'workspace-state-change-hook))))


;; Menu constructors

(defun workspace-menu ()
  (let
      ((tem ws-workspaces)
       (i 0)
       menu)
    (while tem
      (setq menu (cons (list (format nil "space %d%s"
				     (1+ i)
				     (if (eq (car tem) ws-current-workspace)
					 " *" ""))
			     `(lambda ()
				(ws-switch-workspace (nth ,i ws-workspaces))))
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
    (setq space ws-workspaces)
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
      ((windows (filter 'window-visible-p (cdr ws-current-workspace))))
    (display-window (or (nth 1 (memq (input-focus) windows))
			(car windows)))))

(defun next-window ()
  "Focus on the next window, cycling through all possible workspaces."
  (interactive)
  (catch 'out
    (let*
	((space ws-current-workspace)
	 (windows (filter 'window-visible-p (cdr space)))
	 (win (nth 1 (memq (input-focus) windows))))
      (while (not win)
	(setq space (or (nth 1 (memq space ws-workspaces))
			(car ws-workspaces)))
	(when (or (not space) (eq space ws-current-workspace))
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
  (let
      ((space (nth index ws-workspaces)))
    (when (and space (not (eq space ws-current-workspace)))
      (ws-switch-workspace space))))

(defun merge-next-workspace ()
  "Delete the current workspace. Its member windows are relocated to the next
workspace."
  (interactive)
  (when (> (length ws-workspaces) 1)
    (ws-merge-workspaces ws-current-workspace (ws-find-next-workspace t))))

(defun merge-previous-workspace ()
  "Delete the current workspace. Its member windows are relocated to the
previous workspace."
  (interactive)
  (when (> (length ws-workspaces) 1)
    (ws-merge-workspaces ws-current-workspace (ws-find-previous-workspace t))))

(defun insert-workspace ()
  "Create a new workspace following the current workspace."
  (interactive)
  (ws-switch-workspace (ws-insert-workspace)))

(defun move-workspace-forwards (&optional count)
  "Move the current workspace one place to the right."
  (interactive)
  (ws-move-workspace ws-current-workspace (or count 1)))

(defun move-workspace-backwards (&optional count)
  "Move the current workspace one place to the left."
  (interactive)
  (ws-move-workspace ws-current-workspace (- (or count 1))))


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
    (cond ((eq (window-get w 'workspace) ws-current-workspace)
	   (show-window w))
	  (uniconify-to-current-workspace
	   (ws-remove-window w)
	   (ws-add-window w)))
    (when raise-windows-on-uniconify
      (raise-window w))
    (call-window-hook 'uniconify-window-hook w)
    (call-window-hook 'window-state-change-hook w)))

(defun display-window (w)
  "Display the workspace containing the window."
  (interactive "f")
  (when w
    (if (and (window-get w 'iconified) uniconify-to-current-workspace)
	(uniconify-window w)
      (let
	  ((space (window-get w 'workspace)))
	(when (and space (not (eq space ws-current-workspace)))
	  (ws-switch-workspace space))
	(uniconify-window w)
	(when raise-selected-windows
	  (raise-window w))
	(when warp-to-selected-windows
	  (warp-cursor-to-window w))))))

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
  (cond ((and (windowp w) (eq atom 'WM_CHANGE_STATE) (= (aref data 0) 3))
	 ;; IconicState
	 (iconify-window w)
	 t)))


;; Initialisation

(unless (memq 'ws-add-window add-window-hook)
  (add-hook 'add-window-hook 'ws-add-window t)
  (add-hook 'destroy-notify-hook 'ws-remove-window t)
  (add-hook 'map-notify-hook 'ws-window-mapped t)
  (add-hook 'client-message-hook 'ws-client-msg-handler t)
  (mapc 'ws-add-window (managed-windows)))
