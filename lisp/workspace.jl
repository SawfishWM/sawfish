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

(defvar cycle-through-workspaces nil
  "When non-nil, moving through workspaces is cyclical, instead of stopping
when the first or last has been reached.")

(defvar delete-workspaces-when-empty t
  "When non-nil, workspaces are immediately deleted once they contain no
windows.")

(defvar uniconify-to-current-workspace t
  "When non-nil, windows that are uniconified appear on the current
workspace.")

(defvar raise-windows-on-uniconify t
  "When non-nil, windows are raised after being uniconified.")

(defvar transients-on-parents-workspace nil
  "When non-nil transient windows are opened on the same workspace as
their parent window.")

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
    ("Merge previous" merge-previous-workspace)))


;; Low level functions

;; window shouldn't be in any workspace
(defun ws-add-window-to-space (w space)
  (rplacd space (nconc (cdr space) (list w)))
  (window-put w 'workspace space)
  (when (and ws-current-workspace
	     (eq space ws-current-workspace)
	     (not (window-get w 'iconified)))
    (show-window w))
  (call-window-hook 'add-to-workspace-hook w))

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
	(call-window-hook 'add-to-workspace-hook w)))))

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
				     (car ws-workspaces)))))
	(setq ws-workspaces (delq space ws-workspaces))
	(call-hook 'delete-workspace-hook (list space)))
      (window-put w 'workspace nil)
      (call-window-hook 'remove-from-workspace-hook w (list space))
      (when (and (not dont-hide) (windowp w))
	(hide-window w)))))

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
      (call-hook 'enter-workspace-hook (list ws-current-workspace)))))

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
  (setq ws-workspaces (delq src ws-workspaces)))


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
  "Move WINDOW to the next workspace. If no next workspace exists, one will be
created."
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
  "Move WINDOW to the previous workspace. If no such workspace exists, one
will be created."
  (interactive "f")
  (let
      ((space (or (ws-find-previous-workspace)
		  (ws-add-workspace nil))))
    (ws-remove-window window)
    (ws-add-window-to-space window space)))

(defun next-workspace-window ()
  "Focus on the next window of the current workspace"
  (interactive)
  (let
      ((windows (filter 'window-visible-p (cdr ws-current-workspace))))
    (set-input-focus (or (nth 1 (memq (input-focus) windows))
			 (car windows)))))

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
  "Delete the current workspace. Its member windows are relocated to the next
workspace."
  (interactive)
  (when (> (length ws-workspaces) 1)
    (ws-merge-workspaces ws-current-workspace (ws-find-previous-workspace t))))

(defun insert-workspace ()
  "Create a new workspace following the current workspace."
  (interactive)
  (ws-switch-workspace (ws-insert-workspace)))


;; Iconification (but without icons)

;; If iconified, a window has its `iconified' property set to t

(defun iconify-window (w)
  (interactive "f")
  (unless (window-get w 'iconified)
    (window-put w 'iconified t)
    (when (window-visible-p w)
      (hide-window w))
    (call-window-hook 'iconify-window-hook w)
    (call-window-hook 'window-state-change-hook w)))

(defun uniconify-window (w)
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
  "Display the workspace containing window W."
  (interactive "f")
  (when w
    (if (and (window-get w 'iconified) uniconify-to-current-workspace)
	(uniconify-window w)
      (let
	  ((space (window-get w 'workspace)))
	(when (and space (not (eq space ws-current-workspace)))
	  (ws-switch-workspace space))
	(uniconify-window w)
	(warp-cursor-to-window w)))))

(defun toggle-window-sticky (w)
  (interactive "f")
  (if (window-get w 'sticky)
      (progn
	(window-put w 'sticky nil)
	(ws-add-window w))
    (ws-remove-window w t)
    (window-put w 'sticky t))
  (call-window-hook 'window-state-change-hook w))


;; Initialisation

(unless (memq 'ws-add-window add-window-hook)
  (add-hook 'add-window-hook 'ws-add-window t)
  (add-hook 'destroy-notify-hook 'ws-remove-window t)
  (add-hook 'map-notify-hook 'ws-window-mapped t)
  (mapc 'ws-add-window (managed-windows)))
