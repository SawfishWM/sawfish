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

;; Sawmill's workspace are organised as a 1-dimensional continuum, from
;; -infinity to +infinity. Normally the user only sees a small part of
;; this continuum, from the first non-empty workspace to the last
;; non-empty workspace

;; So that we never run out of workspace ids the non-empty portion is
;; intermittently normalised to start with index zero

;; Inserting and deleting workspaces just involves shuffling the
;; workspace index of each window (stored in the `workspace' property,
;; nil for sticky windows)

;; The intention is that whatever structure the user wants to see (e.g.
;; grid, cube, ...) is built on top of the underlying 1d structure

;; If a window is added with its `workspace' property set, then it's
;; added to that (logical) space


;; Options and variables

(defcustom workspace-boundary-mode 'stop
  "Action when passing the first or last workspace when moving."
  :type (set stop wrap-around keep-going)
  :group workspace)

(defcustom workspace-send-boundary-mode 'keep-going
  "Action when passing the first or last workspace when moving windows."
  :type (set stop wrap-around keep-going)
  :group workspace)

(defcustom delete-workspaces-when-empty nil
  "Workspaces are merged with the next when their last window is removed."
  :type boolean
  :group workspace)

(defcustom preallocated-workspaces 1
  "The minimum number of workspaces that may exist."
  :type number
  :range (1 . nil)
  :group workspace
  :after-set (lambda ()
	       (call-hook 'workspace-state-change-hook)))

(defcustom uniconify-to-current-workspace t
  "Windows are uniconified onto the current workspace."
  :type boolean
  :group workspace)

(defcustom raise-windows-on-uniconify t
  "Windows are raised after being uniconified."
  :type boolean
  :group misc)

(defcustom focus-windows-on-uniconify nil
  "Windows are focused after being uniconified."
  :type boolean
  :group focus)

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

;; XXX should be a defcustom, need a string-list type
(defvar workspace-names nil
  "List of workspace names.")

(defvar auto-workspace-alist nil
  "List of `(REGEXP . INDEX)' mapping window names to the workspace to open
that window on (counting from zero).")

;; Currently active workspace, an integer
(defvar current-workspace 0)

(defvar static-workspace-menus
  '(("Insert" insert-workspace)
    ("Next" next-workspace)
    ("Previous" previous-workspace)
    ("Merge next" merge-next-workspace)
    ("Merge previous" merge-previous-workspace)
    ("Move right" move-workspace-forwards)
    ("Move left" move-workspace-backwards)))

;; X constants
(defconst NormalState 1)
(defconst IconicState 3)


;; Low level functions

(defun ws-workspace-empty-p (space)
  (catch 'out
    (mapc #'(lambda (w)
	      (when (equal (window-get w 'workspace) space)
		(throw 'out nil)))
	  (managed-windows))
    t))

;; returns (FIRST-INDEX . LAST-INDEX) defining the subset of the
;; continuum that is `interesting' to the user
(defun ws-workspace-limits ()
  (let
      ((max-w current-workspace)
       (min-w current-workspace)
       tem)
    (mapc #'(lambda (w)
	      (when (setq tem (window-get w 'workspace))
		(when (> tem max-w)
		  (setq max-w tem))
		(when (< tem min-w)
		  (setq min-w tem)))) (managed-windows))
    (cons min-w (max max-w (1- (+ preallocated-workspaces min-w))))))

;; renormalize the interesting workspaces so they being at index zero
(defun ws-normalize-indices ()
  (let
      ((limits (ws-workspace-limits))
       tem)
    (mapc #'(lambda (w)
	      (when (setq tem (window-get w 'workspace))
		(window-put w 'workspace (- tem (car limits)))))
	  (managed-windows))
    (setq current-workspace (- current-workspace (car limits)))))

;; insert a new workspace (returning its index) so that the workspace
;; before it has index BEFORE
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
  (call-hook 'workspace-state-change-hook)
  (1+ before))

;; merge workspace INDEX with workspace INDEX+1
(defun ws-remove-workspace (&optional index)
  (unless index
    (setq index current-workspace))
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
  (call-hook 'workspace-state-change-hook))

;; move workspace INDEX COUNT positions forwards (+ve or -ve)
(defun ws-move-workspace (index count)
  (let
      ((windows (managed-windows)))
    (while (> count 0)
      (mapc #'(lambda (w)
		(let
		    ((space (window-get w 'workspace)))
		  (when space
		   (cond ((= space index)
			  (window-put w 'workspace (1+ space)))
			 ((= space (1+ index))
			  (window-put w 'workspace index))))))
	    windows)
      (when (= current-workspace index)
	(setq current-workspace (1+ current-workspace)))
      (setq count (1- count))
      (setq index (1+ index)))
    (while (< count 0)
      (mapc #'(lambda (w)
		(let
		    ((space (window-get w 'workspace)))
		  (when space
		    (cond ((= space index)
			   (window-put w 'workspace (1- space)))
			  ((= space (1- index))
			   (window-put w 'workspace index))))))
	    windows)
      (when (= current-workspace index)
	(setq current-workspace (1- current-workspace)))
      (setq count (1+ count))
      (setq index (1- index)))
    (call-hook 'workspace-state-change-hook)))

(defun ws-after-removing-window (w space)
  (when (and delete-workspaces-when-empty
	     (ws-workspace-empty-p space))
    ;; workspace is now empty
    (ws-remove-workspace space)
    (ws-normalize-indices)
    (let
	((limits (ws-workspace-limits)))
      (when (and (= current-workspace (cdr limits))
		 (/= (car limits) (cdr limits)))
	(select-workspace (1- current-workspace))))))

;; called when window W is destroyed
(defun ws-remove-window (w &optional dont-hide)
  (let
      ((space (window-get w 'workspace)))
    (when space
      (window-put w 'workspace nil)
      (ws-after-removing-window w space)
      (when (and (not dont-hide) (windowp w))
	(hide-window w))
      (call-hook 'workspace-state-change-hook))))

;; move window W to workspace index NEW
(defun ws-move-window (w new)
  (let
      ((space (window-get w 'workspace)))
    (if (null space)
	(ws-add-window-to-space w new)
      (window-put w 'workspace new)
      (cond ((= space current-workspace)
	     (hide-window w))
	    ((and (= new current-workspace) (not (window-get w 'iconified)))
	     (show-window w)))
      (ws-after-removing-window w space)
      (call-hook 'workspace-state-change-hook))))

;; display workspace index SPACE
(defun select-workspace (space)
  "Activate workspace number SPACE (from zero)."
  (interactive "p")
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

;; return a list of all windows on workspace index SPACE
(defun workspace-windows (space &optional include-iconified)
  (filter #'(lambda (w)
	      (and (equal (window-get w 'workspace) space)
		   (or include-iconified
		       (not (window-get w 'iconified)))))
	  (managed-windows)))


;; slightly higher level

;; add window W to workspace index SPACE; window shouldn't be in any
;; workspace
(defun ws-add-window-to-space (w space)
  (unless (or (window-get w 'sticky) (window-get w 'workspace))
    (window-put w 'workspace space)
    (if (and (= space current-workspace) (not (window-get w 'iconified)))
	(show-window w)
      (hide-window w))
    (call-window-hook 'add-to-workspace-hook w)
    (call-hook 'workspace-state-change-hook)))

;; usually called from the add-window-hook; adds window W to the
;; current workspace (or wherever else it should go)
(defun ws-add-window (w)
  (if (window-get w 'sticky)
      (progn
	(window-put w 'workspace nil)
	(show-window w))
    (unless (window-get w 'workspace)
      (let
	  ((tem (assoc-regexp (window-name w) auto-workspace-alist)))
	(when tem
	  (window-put w 'workspace (cdr tem)))))
    (if (window-get w 'workspace)
	(let
	    ((space (- (window-get w 'workspace) (car (ws-workspace-limits)))))
	  (window-put w 'workspace nil)
	  (ws-add-window-to-space w space))
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
	  (call-window-hook 'add-to-workspace-hook w)
	  (call-hook 'workspace-state-change-hook))))))

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


;; Menu constructors

(defun workspace-menu ()
  (let*
      ((limits (ws-workspace-limits))
       (i (car limits))
       menu)
    (while (<= i (cdr limits))
      (setq menu (cons (list (or (nth (- i (car limits)) workspace-names)
				 (format nil "space %d%s"
					 (1+ (- i (car limits)))
					 (if (= i current-workspace) " *" "")))
			     `(lambda ()
				(select-workspace ,i)))
		       menu))
      (setq i (1+ i)))
    (nconc (nreverse menu) (list nil) static-workspace-menus)))

(defun popup-workspace-list ()
  "Display the menu containing the list of all workspaces."
  (interactive)
  (popup-menu (workspace-menu)))

(defun window-menu ()
  (let*
      ((limits (ws-workspace-limits))
       (windows (managed-windows))
       (i (car limits))
       menu name)
    (while (<= i (cdr limits))
      (mapc #'(lambda (w)
		(when (and (equal (window-get w 'workspace) i)
			   (window-mapped-p w)
			   (not (window-get w 'ignored)))
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
      (unless (or (= i (cdr limits)) (null (car menu)))
	(setq menu (cons nil menu)))
      (setq i (1+ i)))
    ;; search for any iconified windows that aren't anywhere else in the menu
    (let
	(extra)
      (mapc #'(lambda (w)
		(when (and (window-get w 'iconified)
			   (not (window-get w 'workspace)))
		  (setq extra (cons (list (concat ?[ (window-name w) ?])
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

(defun ws-call-with-workspace (fun count mode)
  (let
      ((limits (ws-workspace-limits))
       (target (+ current-workspace count)))
    (if (and (>= target (car limits)) (<= target (cdr limits)))
	(funcall fun target)
      (cond ((eq mode 'stop))
	    ((eq mode 'wrap-around)
	     (funcall fun (+ (car limits)
			     (mod (- target (car limits))
				  (1+ (- (cdr limits) (car limits)))))))
	    ((eq mode 'keep-going)
	     (funcall fun target))))))

(defun next-workspace (count)
  "Display the next workspace."
  (interactive "p")
  (ws-call-with-workspace 'select-workspace count workspace-boundary-mode))

(defun send-to-next-workspace (window count)
  "Move the window to the next workspace. If no next workspace exists, one
will be created."
  (interactive "%W\np")
  (ws-call-with-workspace #'(lambda (space)
			      (select-workspace space)
			      (ws-move-window window current-workspace))
			  count workspace-send-boundary-mode))

(defun append-workspace-and-send (window)
  "Create a new workspace at the end of the list, and move the window to it."
  (interactive "%W")
  (let
      ((limits (ws-workspace-limits)))
    (when (window-get window 'workspace)
      (select-workspace (1+ (cdr limits)))
      (ws-move-window window current-workspace))))

(defun previous-workspace (count)
  "Display the previous workspace."
  (interactive "p")
  (next-workspace (- count)))

(defun send-to-previous-workspace (window count)
  "Move the window to the previous workspace. If no such workspace exists, one
will be created."
  (interactive "%W\np")
  (send-to-next-workspace window (- count)))

(defun prepend-workspace-and-send (window)
  "Create a new workspace at the start of the list, and move the window to it."
  (interactive "%W")
  (let
      ((limits (ws-workspace-limits)))
    (when (window-get window 'workspace)
      (select-workspace (1- (car limits)))
      (ws-move-window window current-workspace))))

(defun merge-next-workspace ()
  "Delete the current workspace. Its member windows are relocated to the next
workspace."
  (interactive)
  (ws-remove-workspace current-workspace))

(defun merge-previous-workspace ()
  "Delete the current workspace. Its member windows are relocated to the
previous workspace."
  (interactive)
  (ws-remove-workspace (1- current-workspace)))

(defun insert-workspace ()
  "Create a new workspace following the current workspace."
  (interactive)
  (ws-insert-workspace current-workspace)
  (select-workspace (1+ current-workspace)))

(defun insert-workspace-before ()
  "Create a new workspace before the current workspace."
  (interactive)
  (ws-insert-workspace (1- current-workspace))
  (select-workspace (- current-workspace 2)))

(defun move-workspace-forwards (&optional count)
  "Move the current workspace one place to the right."
  (interactive)
  (ws-move-workspace current-workspace (or count 1)))

(defun move-workspace-backwards (&optional count)
  "Move the current workspace one place to the left."
  (interactive)
  (ws-move-workspace current-workspace (- (or count 1))))

(defun select-workspace-from-first (count)
  (let
      ((limits (ws-workspace-limits)))
    (select-workspace (+ count (car limits)))))

(defun send-window-to-workspace-from-first (window count)
  (let
      ((limits (ws-workspace-limits)))
    (select-workspace (+ count (car limits)))
    (ws-move-window window current-workspace)))


;; some commands for moving directly to a workspace

(let
    ((i 1))
  (while (< i 10)
    (fset (intern (format nil "select-workspace:%s" i))
	  `(lambda ()
	     (interactive)
	     (select-workspace-from-first ,(1- i))))
    (fset (intern (format nil "send-to-workspace:%s" i))
	  `(lambda (w)
	     (interactive "%W")
	     (send-window-to-workspace-from-first w ,(1- i))))
    (setq i (1+ i))))


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
    (call-window-hook 'window-state-change-hook w)))

(defun uniconify-window (w)
  "Return the window from its iconified state."
  (interactive "%W")
  (when (window-get w 'iconified)
    (window-put w 'iconified nil)
    (cond ((or (not (window-get w 'workspace))
	       (= (window-get w 'workspace) current-workspace))
	   (show-window w))
	  (uniconify-to-current-workspace
	   (ws-remove-window w)
	   (ws-add-window-to-space w current-workspace)))
    (when raise-windows-on-uniconify
      (raise-window w))
    (when (and focus-windows-on-uniconify (window-really-wants-input-p w))
      (set-input-focus w))
    (call-window-hook 'uniconify-window-hook w)
    (call-window-hook 'window-state-change-hook w)))

(defun display-window (w)
  "Display the workspace containing the window W, then focus on W."
  (interactive "%W")
  (when w
    (if (and (window-get w 'iconified)
	     (or uniconify-to-current-workspace
		 (not (window-get w 'workspace))))
	(uniconify-window w)
      (let
	  ((space (window-get w 'workspace)))
	(when space
	  (select-workspace space))
	(move-viewport-to-window w)
	(uniconify-window w)
	(when (and unshade-selected-windows (window-get w 'shaded))
	  (unshade-window w))
	(when raise-selected-windows
	  (raise-window w))
	(when warp-to-selected-windows
	  (warp-cursor-to-window w))
	(when (window-really-wants-input-p w)
	  (set-input-focus w))))))

(defun toggle-window-sticky (w)
  "Toggle the `stickiness' of the window--whether or not it is a member of
all workspaces."
  (interactive "%W")
  (if (window-get w 'sticky)
      (progn
	(window-put w 'sticky nil)
	(window-put w 'fixed-position nil)
	(ws-add-window-to-space w current-workspace))
    (ws-remove-window w t)
    (window-put w 'sticky t)
    (window-put w 'fixed-position t))
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


;; session management

(defun ws-saved-state (w)
  (let
      ((limits (ws-workspace-limits))
       (space (window-get w 'workspace)))
    (when space
      `((workspace . ,(- (car limits) space))))))

(defun ws-load-state (w alist)
  (let
      ((limits (ws-workspace-limits))
       (space (cdr (assq 'workspace alist))))
    (when space
      (window-put w 'workspace (+ (car limits) space)))))


;; Initialisation

(unless (or batch-mode (memq 'ws-add-window add-window-hook))
  (add-hook 'add-window-hook 'ws-add-window)
  (add-hook 'map-notify-hook 'ws-window-mapped)
  (add-hook 'destroy-notify-hook 'ws-remove-window)
  (add-hook 'client-message-hook 'ws-client-msg-handler)
  (add-hook 'before-add-window-hook 'ws-honour-client-state)
  (add-hook 'add-window-hook 'ws-set-client-state t)
  (add-hook 'window-state-change-hook 'ws-set-client-state)
  (sm-add-saved-properties 'sticky 'iconified)
  (add-hook 'sm-window-save-functions 'ws-saved-state)
  (add-hook 'sm-restore-window-hook 'ws-load-state))
