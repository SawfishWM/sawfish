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

(require 'window-order)
(provide 'workspace)

;; Commentary:

;; Sawmill's workspace are organised as a 1-dimensional continuum, from
;; -infinity to +infinity. Normally the user only sees a small part of
;; this continuum, from the first non-empty workspace to the last
;; non-empty workspace

;; So that we never run out of workspace ids the non-empty portion is
;; intermittently normalised to start with index zero

;; Inserting and deleting workspaces just involves shuffling the
;; workspace index of each window

;; The intention is that whatever structure the user wants to see (e.g.
;; grid, cube, ...) is built on top of the underlying 1d structure

;; Each window can be a member of any (positive) number of workspaces;
;; the `workspaces' property contains a list of workspace ids. Sticky
;; windows appear on all workspaces, and have their `sticky' property
;; set (with a null `workspaces' property)

;; When a window appears on more than one workspaces most of it's
;; properties are swapped in and out on demand when the current
;; workspace is changed. Use the `workspace-local-properties' and the
;; `workspace-swap-{in,out}-hook' variables

;; If a window is added with its `workspaces' property set, then it's
;; added to those (logical) spaces

;; Private functions are prefixed by ws-


;; Options and variables

(defcustom workspace-boundary-mode 'stop
  "Action when passing the first or last workspace when moving."
  :type symbol
  :options (stop wrap-around keep-going)
  :group (workspace advanced))

(defcustom workspace-send-boundary-mode 'keep-going
  "Action when passing the first or last workspace when moving windows."
  :type symbol
  :options (stop wrap-around keep-going)
  :group (workspace advanced))

(defcustom delete-workspaces-when-empty nil
  "Workspaces are merged with the next when their last window is closed."
  :type boolean
  :group (workspace advanced))

(defcustom preallocated-workspaces 1
  "The minimum number of workspaces that may exist."
  :type number
  :range (1 . nil)
  :group workspace
  :after-set (lambda () (call-hook 'workspace-state-change-hook)))

(defcustom lock-first-workspace t
  "Empty workspaces before or after the active workspace aren't hidden."
  :type boolean
  :group (workspace advanced)
  :after-set (lambda () (call-hook 'workspace-state-change-hook)))

(defcustom uniconify-to-current-workspace t
  "Windows are uniconified onto the current workspace."
  :type boolean
  :group iconify)

(defcustom raise-windows-on-uniconify t
  "Windows are raised after being uniconified."
  :type boolean
  :group iconify)

(defcustom focus-windows-on-uniconify nil
  "Windows are focused after being uniconified."
  :type boolean
  :group iconify)

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

(defcustom iconify-whole-group nil
  "Iconifying a window that's a member of a group removes the whole group."
  :type boolean
  :group iconify)

(defcustom uniconify-whole-group nil
  "Uniconifying a window that's a member of a group restores the whole group."
  :type boolean
  :group iconify)

;; XXX should be a defcustom, need a string-list type
(defvar workspace-names nil
  "List of workspace names.")

;; Currently active workspace, an integer
(defvar current-workspace 0)

(defvar first-interesting-workspace nil)
(defvar last-interesting-workspace nil)

(defvar static-workspace-menus
  `((,(_ "Insert workspace") insert-workspace)
    (,(_ "Select next workspace") next-workspace)
    (,(_ "Select previous workspace") previous-workspace)
    (,(_ "Merge with next") merge-next-workspace)
    (,(_ "Merge with previous") merge-previous-workspace)
    (,(_ "Move workspace right") move-workspace-forwards)
    (,(_ "Move workspace left") move-workspace-backwards)))

;; X constants
(defconst NormalState 1)
(defconst IconicState 3)

;; window properties whose values may differ on different workspaces
(defvar workspace-local-properties nil)

(defvar workspace-swap-in-hook nil)
(defvar workspace-swap-out-hook nil)


;; Workspace ``swapping''

;; Property swapping is done on demand, and for each window
;; individually. This ensures that only the minimum required data is
;; ever swapped

;; Each window has a `swapped-in' property and may have a `swapped'
;; property. `swapped-in' is a workspace id defining which of the
;; window's configurations is stored in the window itself (as if there
;; was no swapping). The `swapped' property is an alist associating
;; workspace ids with an alist of swapped attributes 

;; this doesn't need to be called explicitly for every switch, just to
;; fork the data
(defun ws-swap-out (w space)
  (let
      ((alist (apply nconc (mapcar (lambda (fun)
				     (fun w space)) workspace-swap-out-hook)))
       (swapped (assq space (window-get w 'swapped))))
    (if swapped
	(rplacd swapped alist)
      (window-put w 'swapped (cons (cons space alist)
				   (window-get w 'swapped))))))

;; swap in data for workspace id SPACE to window W
(defun ws-swap-in (w space)
  (let
      ((current (window-get w 'swapped-in)))
    (unless (eq current space)
      (when current
	;; swap out the current data
	(ws-swap-out w current))
      (let
	  ((alist (assq space (window-get w 'swapped))))
	(when alist
	  (call-hook 'workspace-swap-in-hook (list w space (cdr alist)))
	  (window-put w 'swapped (delq alist (window-get w 'swapped))))
	(window-put w 'swapped-in space)))))


;; Low level functions

;; return list of all workspaces containing window W
(defmacro window-workspaces (w)
  `(window-get ,w 'workspaces))

;; set list of all workspaces containing window W to LST
(defmacro ws-window-set-workspaces (w lst)
  `(window-put ,w 'workspaces ,lst))

;; return t if window W is a member of workspace SPACE
(defmacro window-in-workspace-p (w space)
  `(memq ,space (window-get ,w 'workspaces)))

;; map FUN over all workspace ids containing window W
(defmacro map-window-workspaces (fun w)
  `(mapc ,fun (window-workspaces ,w)))

(defmacro window-appears-in-workspace-p (w space)
  `(or (window-get ,w 'sticky) (window-in-workspace-p ,w ,space)))

;; add window W to those in workspace SPACE
(defun window-add-to-workspace (w space)
  (ws-window-set-workspaces w (cons space (delq space (window-workspaces w))))
  (cond ((= space current-workspace)
	 (ws-swap-in w space))
	((and (window-get w 'swapped-in)
	      (/= (window-get w 'swapped-in) space))
	 ;; write out the current state for when we switch
	 (ws-swap-out w space))
	(t
	 (window-put w 'swapped-in space))))

;; remove window W from those in workspace SPACE
(defun window-remove-from-workspace (w space)
  (ws-window-set-workspaces w (delq space (window-workspaces w)))
  (window-put w 'swapped (delete-if (lambda (cell)
				      (= (car cell) space))
				    (window-get w 'swapped)))
  (when (eq (window-get w 'swapped-in) space)
    (window-put w 'swapped-in nil)))

;; FUN is a function transforming an old workspace id to a new id.
;; Run this function over all workspaces that window W is a member of.
(defun transform-window-workspaces (fun w)
  (let
      ((workspaces (window-get w 'workspaces))
       (swapped (window-get w 'swapped))
       (swapped-in (window-get w 'swapped-in))
       (new-workspaces '())
       (new-swapped '()))
    (while workspaces
      (let*
	  ((space (car workspaces))
	   (swap (assq space swapped))
	   (new (fun space)))
	(unless (memq new new-workspaces)
	  (setq new-workspaces (cons new new-workspaces))
	  (when swap
	    (setq new-swapped (cons (cons new (cdr swap)) new-swapped))))
	(when (eq swapped-in space)
	  (window-put w 'swapped-in new))
	(setq workspaces (cdr workspaces))))
    (ws-window-set-workspaces w (nreverse new-workspaces))
    (window-put w 'swapped (nreverse new-swapped))))

;; return t if workspace SPACE contains zero (non-sticky) windows
(defun workspace-empty-p (space)
  (catch 'out
    (map-windows (lambda (w)
		   (when (window-in-workspace-p w space)
		     (throw 'out nil))))
    t))

;; return t if windows W-1 and W-2 both appear on the same workspace
(defun windows-share-workspace-p (w-1 w-2)
  (if (or (window-get w-1 'sticky) (window-get w-2 'sticky))
      t
    (let
	((spaces-1 (window-workspaces w-1))
	 (spaces-2 (window-workspaces w-2)))
      (catch 'out
	(mapc (lambda (space)
		(when (memq space spaces-2)
		  (throw 'out t))) spaces-1)
	nil))))

;; return a list of all workspace indices that contain windows
(defun all-workspaces ()
  (let
      (spaces)
    (map-windows
     (lambda (w)
       (map-window-workspaces (lambda (space)
				(unless (memq space spaces)
				  (setq spaces (cons space spaces)))) w)))
    spaces))

;; return the nearest workspace to SPACE that contains window W
(defun nearest-workspace-with-window (w space)
  (if (window-get w 'sticky)
      space
    (let
	((all (window-workspaces w))
	 (min-diff 10000)
	 (min-space nil)
	 tem)
      (while all
	(setq tem (max (- space (car all)) (- (car all) space)))
	(when (< tem min-diff)
	  (setq min-diff tem)
	  (setq min-space (car all)))
	(setq all (cdr all)))
      min-space)))

;; returns (FIRST-INDEX . LAST-INDEX) defining the subset of the
;; continuum that is `interesting' to the user
(defun workspace-limits ()
  (let*
      ((all-spaces (all-workspaces))
       (max-w (if (and lock-first-workspace last-interesting-workspace)
		  (max last-interesting-workspace current-workspace)
		current-workspace))
       (min-w (if (and lock-first-workspace first-interesting-workspace)
		  (min first-interesting-workspace current-workspace)
		current-workspace)))
    (cond ((cdr all-spaces)
	   (setq max-w (max max-w (apply max all-spaces)))
	   (setq min-w (min min-w (apply min all-spaces))))
	  (all-spaces
	   (setq max-w (max max-w (car all-spaces)))
	   (setq min-w (min min-w (car all-spaces)))))
    (setq max-w (max max-w (1- (+ preallocated-workspaces min-w))))
    (setq first-interesting-workspace min-w)
    (setq last-interesting-workspace max-w)
    (cons min-w max-w)))

(defun workspace-id-to-logical (space &optional limits)
  (unless limits
    (setq limits (workspace-limits)))
  (- space (car limits)))

(defun workspace-id-from-logical (offset &optional limits)
  (unless limits
    (setq limits (workspace-limits)))
  (+ offset (car limits)))

;; renormalize the interesting workspaces so they begin at index zero
(defun ws-normalize-indices ()
  (let*
      ((first-space (car (workspace-limits))))
    (map-windows (lambda (w)
		   (transform-window-workspaces (lambda (space)
						  (- space first-space)) w)))
    (setq current-workspace (- current-workspace first-space))
    (when first-interesting-workspace
      (setq first-interesting-workspace
	    (- first-interesting-workspace first-space)))
    (when last-interesting-workspace
      (setq last-interesting-workspace
	    (- last-interesting-workspace first-space)))))

;; insert a new workspace (returning its index) so that the workspace
;; before it has index BEFORE
(defun ws-insert-workspace (&optional before)
  (unless before
    (setq before (1+ current-workspace)))
  (map-windows
   (lambda (w)
     (transform-window-workspaces (lambda (space)
				    (if (> space before)
					(1+ space)
				      space)) w)))
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
  (when (and first-interesting-workspace (> first-interesting-workspace index))
    (setq first-interesting-workspace (1- first-interesting-workspace)))
  (when (and last-interesting-workspace (> last-interesting-workspace index))
    (setq last-interesting-workspace (1- last-interesting-workspace)))
  (map-windows
   (lambda (w)
     (transform-window-workspaces (lambda (space)
				    (if (> space index)
					(1- space)
				      space)) w)
     (when (and (window-in-workspace-p w current-workspace)
		(not (window-get w 'iconified)))
       (show-window w))))
  (call-hook 'workspace-state-change-hook))

;; move workspace INDEX COUNT positions forwards (+ve or -ve)
(defun ws-move-workspace (index count)
  (cond ((> count 0)
	 (map-windows
	  (lambda (w)
	    (transform-window-workspaces (lambda (space)
					   (cond ((< space index)
						  space)
						 ((= space index)
						  (+ space count))
						 ((<= space (+ index count))
						  (1- space))
						 (t
						  space))) w)))
	 (cond ((= current-workspace index)
		(setq current-workspace (+ current-workspace count)))
	       ((> current-workspace index)
		(setq current-workspace (1- current-workspace)))))
	((< count 0)
	 (map-windows
	  (lambda (w)
	    (transform-window-workspaces (lambda (space)
					   (cond ((> space index)
						  space)
						 ((= space index)
						  (+ space count))
						 ((>= space (+ index count))
						  (1+ space))
						 (t
						  space))) w)))
	 (cond ((= current-workspace index)
		(setq current-workspace (+ current-workspace count)))
	       ((< current-workspace index)
		(setq current-workspace (1+ current-workspace))))))
  (call-hook 'workspace-state-change-hook))

;; called when workspace with id SPACE may contain no windows
(defun ws-workspace-may-be-empty (space)
  (when (and delete-workspaces-when-empty (workspace-empty-p space))
    ;; workspace is now empty
    (let*
	((limits (workspace-limits))
	 (need-to-move (and (= space current-workspace)
			    (/= space (car limits))
			    (= space (cdr limits)))))
      (ws-remove-workspace space)
      (ws-normalize-indices)
      (when need-to-move
	(select-workspace (1- current-workspace))))))

;; called when window W is destroyed
(defun ws-remove-window (w &optional dont-hide)
  (let
      ((spaces (window-workspaces w)))
    (mapc (lambda (space)
	    (window-remove-from-workspace w space)) spaces)
    (mapc (lambda (space)
	    (ws-workspace-may-be-empty space)) (sort spaces >))
    (when (and (not dont-hide) (windowp w))
      (hide-window w))
    (call-hook 'workspace-state-change-hook)))

;; move window W from workspace id OLD to workspace NEW
(defun ws-move-window (w old new &optional was-focused)
  (or (window-in-workspace-p w old)
      (error
       "ws-move-window--window isn't in original workspace: %s, %s" w old))
  (if (window-in-workspace-p w new)
      ;; just remove from the source workspace
      (window-remove-from-workspace w old)
    ;; need to move it..
    (transform-window-workspaces (lambda (space)
				   (if (= space old)
				       new
				     space)) w)
    (cond ((= old current-workspace)
	   (hide-window w))
	  ((and (= new current-workspace) (not (window-get w 'iconified)))
	   (show-window w))))
  (ws-workspace-may-be-empty old)
  ;; the window may lose the focus when switching spaces
  (when (and was-focused (window-visible-p w))
    (set-input-focus w))
  (call-hook 'workspace-state-change-hook))

;; arrange it so that window W appears on both OLD and NEW workspaces
(defun ws-copy-window (w old new &optional was-focused)
  (or (window-in-workspace-p w old)
      (error
       "ws-copy-window--window isn't in original workspace: %s, %s" w old))
  (unless (window-in-workspace-p w new)
    (window-add-to-workspace w new))
  (when (and (= new current-workspace) (not (window-get w 'iconified)))
    (show-window w))
  ;; the window may lose the focus when switching spaces
  (when (and was-focused (window-visible-p w))
    (set-input-focus w))
  (call-hook 'workspace-state-change-hook))

;; switch to workspace with id SPACE
(defun select-workspace (space &optional dont-focus)
  "Activate workspace number SPACE (from zero)."
  (interactive "p")
  (unless (= current-workspace space)
    (when current-workspace
      (call-hook 'leave-workspace-hook (list current-workspace))
      ;; the server grabs are just for optimisation (each call to
      ;; show-window or hide-window may also grab the server semaphore)
      (with-server-grabbed
       (map-windows
	(lambda (w)
	  (when (and (not (window-get w 'sticky))
		     (window-in-workspace-p w current-workspace)
		     (not (window-in-workspace-p w space))
		     (window-get w 'placed))
	    (hide-window w))))))
    (setq current-workspace space)
    (when current-workspace
      (with-server-grabbed
       (map-windows
	(lambda (w)
	  (when (and (not (window-get w 'sticky))
		     (window-in-workspace-p w current-workspace)
		     (window-get w 'placed))
	    (ws-swap-in w current-workspace)
	    (if (window-get w 'iconified)
		(hide-window w)
	      (show-window w))))))
      (unless (or dont-focus (eq focus-mode 'enter-exit))
	(window-order-focus-most-recent))
      (call-hook 'enter-workspace-hook (list current-workspace))
      (call-hook 'workspace-state-change-hook))))

;; return a list of all windows on workspace index SPACE
(defun workspace-windows (space &optional include-iconified)
  (delete-if-not
   (lambda (w)
     (and (window-in-workspace-p w space)
	  (or include-iconified (not (window-get w 'iconified)))))
   (managed-windows)))

;; add window W to workspace index SPACE; window shouldn't be in any
;; workspace
(defun ws-add-window-to-space (w space)
  (unless (window-get w 'sticky)
    (window-add-to-workspace w space)
    (cond ((window-get w 'iconified)
	   (hide-window w))
	  ((= space current-workspace)
	   (show-window w))
	  ((not (window-in-workspace-p w current-workspace))
	   (hide-window w)))
    (call-window-hook 'add-to-workspace-hook w (list space))
    (call-hook 'workspace-state-change-hook)))

;; usually called from the add-window-hook; adds window W to the
;; current workspace (or wherever else it should go)
(defun ws-add-window (w)
  (if (window-get w 'sticky)
      (progn
	(ws-window-set-workspaces w nil)
	(if (window-get w 'iconified)
	    (hide-window w)
	  (show-window w)))
    (if (window-workspaces w)
	(let
	    ((spaces (window-workspaces w))
	     (limits (workspace-limits)))
	  (mapc (lambda (space)
		  (ws-add-window-to-space
		   w (workspace-id-from-logical space limits))) spaces))
      (let
	  (parent)
	(if (and transients-on-parents-workspace
		 (window-transient-p w)
		 (setq parent (get-window-by-id (window-transient-p w)))
		 (not (window-get parent 'sticky)))
	    ;; put the window on its parents workspaces
	    (mapc (lambda (space)
		    (ws-add-window-to-space w space))
		  (window-workspaces parent))
	  ;; add it to the current workspace
	  (ws-add-window-to-space w current-workspace))))))

;; called from the unmap-notify hook
(defun ws-window-unmapped (w)
  (unless (window-get w 'sticky)
    (ws-remove-window w t)))

;; called from the map-notify-hook
(defun ws-window-mapped (w)
  (unless (or (window-get w 'sticky) (window-workspaces w))
    (ws-add-window w)))


;; Menu constructors

(defun workspace-menu ()
  (let*
      ((limits (workspace-limits))
       (i (car limits))
       menu)
    (while (<= i (cdr limits))
      (setq menu (cons (list (or (nth (- i (car limits)) workspace-names)
				 (format nil (_ "space %d%s")
					 (1+ (- i (car limits)))
					 (if (= i current-workspace) " *" "")))
			     `(select-workspace ,i))
		       menu))
      (setq i (1+ i)))
    (nconc (nreverse menu) (list nil) static-workspace-menus)))

(defun popup-workspace-list ()
  "Display the menu containing the list of all workspaces."
  (interactive)
  (popup-menu (workspace-menu)))

(defun window-menu ()
  (let*
      ((make-label (lambda (w)
		     (let
			 ((name (window-name w)))
		       (concat (and (window-get w 'iconified) ?\[)
			       (if (> (length name) 20)
				   (concat
				    (substring name 0 20) "...")
				 name)
			       (and (window-get w 'iconified)  ?\])
			       (and (eq (input-focus) w) " *")))))
       (limits (workspace-limits))
       (windows (managed-windows))
       (i (car limits))
       menu)
    (while (<= i (cdr limits))
      (mapc (lambda (w)
	      (when (and (window-in-workspace-p w i)
			 (window-mapped-p w)
			 (or (not (window-get w 'ignored))
			     (window-get w 'iconified)))
		(setq menu (cons (list (make-label w)
				       `(display-window
					 (get-window-by-id ,(window-id w)) ,i))
				 menu))))
	    windows)
      (unless (or (= i (cdr limits)) (null (car menu)))
	(setq menu (cons nil menu)))
      (setq i (1+ i)))
    ;; search for any iconified windows that aren't anywhere else in the menu
    (let
	(extra)
      (mapc (lambda (w)
	      (when (and (window-get w 'iconified) (window-get w 'sticky))
		(setq extra (cons (list (make-label w)
					`(display-window
					  (get-window-by-id ,(window-id w))))
				  extra))))
	    windows)
      (when extra
	(setq menu (if menu (nconc extra (list nil) menu) extra))))
    (nreverse menu)))

(defun popup-window-list ()
  "Display the menu of all managed windows."
  (interactive)
  (popup-menu (window-menu)))


;; Commands

(defun ws-call-with-workspace (fun count mode)
  (let
      ((limits (workspace-limits))
       (target (+ current-workspace count)))
    (if (and (>= target (car limits)) (<= target (cdr limits)))
	(fun target)
      (cond ((eq mode 'stop))
	    ((eq mode 'wrap-around)
	     (fun (+ (car limits)
		     (mod (- target (car limits))
			  (1+ (- (cdr limits) (car limits)))))))
	    ((eq mode 'keep-going)
	     (fun target))))))

(defun next-workspace (count)
  "Display the next workspace."
  (interactive "p")
  (ws-call-with-workspace select-workspace count workspace-boundary-mode))

(defun previous-workspace (count)
  "Display the previous workspace."
  (interactive "p")
  (next-workspace (- count)))

(defun send-to-next-workspace (w count &optional copy)
  "Move the window to the next workspace. If no next workspace exists, one
will be created."
  (interactive "%W\np")
  (ws-call-with-workspace (lambda (space)
			    (let
				((was-focused (eq w (input-focus)))
				 (orig-space (if (window-in-workspace-p
						  w current-workspace)
						 current-workspace
					       (car (window-workspaces w)))))
			      (when orig-space
				(ws-copy-window w orig-space space was-focused)
				(select-workspace space was-focused)
				(unless copy
				  (ws-move-window
				   w orig-space space was-focused)))))
			  count workspace-send-boundary-mode))

(defun send-to-previous-workspace (w count &optional copy)
  "Move the window to the previous workspace. If no such workspace exists, one
will be created."
  (interactive "%W\np")
  (send-to-next-workspace w (- count) copy))

(defun copy-to-next-workspace (w count)
  (interactive "%W\np")
  (send-to-next-workspace w count t))

(defun copy-to-previous-workspace (w count)
  (interactive "%W\np")
  (send-to-previous-workspace w count t))

(defun append-workspace-and-send (w)
  "Create a new workspace at the end of the list, and move the window to it."
  (interactive "%W")
  (let
      ((limits (workspace-limits))
       (was-focused (eq (input-focus) w))
       (orig-space (if (window-in-workspace-p w current-workspace)
		       current-workspace
		     (car (window-workspaces w)))))
    (when orig-space
      (select-workspace (1+ (cdr limits)) was-focused)
      (ws-move-window w orig-space current-workspace was-focused))))

(defun prepend-workspace-and-send (w)
  "Create a new workspace at the start of the list, and move the window to it."
  (interactive "%W")
  (let
      ((limits (workspace-limits))
       (was-focused (eq (input-focus) w))
       (orig-space (if (window-in-workspace-p w current-workspace)
		       current-workspace
		     (car (window-workspaces w)))))
    (when orig-space
      (select-workspace (1- (car limits)) was-focused)
      (ws-move-window w orig-space current-workspace was-focused))))

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
  (select-workspace (workspace-id-from-logical count)))

(defun send-window-to-workspace-from-first (w count &optional copy)
  (let*
      ((was-focused (eq (input-focus) w))
       (orig-space (if (window-in-workspace-p w current-workspace)
		       current-workspace
		     (car (window-workspaces w))))
       (new-space (workspace-id-from-logical count)))
    (when orig-space
      (ws-copy-window w orig-space new-space was-focused)
      (select-workspace new-space was-focused)
      (unless copy
	(ws-move-window w orig-space new-space was-focused)))))

(defun delete-empty-workspaces ()
  "Delete any workspaces that don't contain any windows."
  (interactive)
  (let*
      ((limits (workspace-limits))
       (space (car limits)))
    (while (<= space (cdr limits))
      (if (workspace-empty-p space)
	  (cond ((= space (car limits))
		 (when (= first-interesting-workspace space)
		   (setq first-interesting-workspace (1+ space)))
		 (setq space (1+ space)))
		((= space (cdr limits))
		 (when (= last-interesting-workspace space)
		   (setq last-interesting-workspace (1- space)))
		 (setq space (1+ space)))
		(t
		 (ws-remove-workspace space)
		 (setq limits (workspace-limits))))
	(setq space (1+ space))))
    (when (> first-interesting-workspace last-interesting-workspace)
      (setq first-interesting-workspace last-interesting-workspace))))

(defun delete-window-instance (w)
  "Remove the copy of the window on the current workspace. If this is the last
instance remaining, then delete the actual window."
  (interactive "%W")
  (let
      ((spaces (window-workspaces w)))
    (if (cdr spaces)
	;; not the last instance
	(let
	    ((space (if (memq current-workspace spaces)
			current-workspace
		      (car spaces))))
	  (window-remove-from-workspace w space)
	  (when (= space current-workspace)
	    (hide-window w))
	  (ws-workspace-may-be-empty space)
	  (call-hook 'workspace-state-change-hook))
      (delete-window w))))


;; some commands for moving directly to a workspace

(let
    ((define-commands
      (lambda (index)
	(let
	    ((fn (lambda (base)
		   (intern (format nil "%s:%d" base (1+ index))))))
	  (define-value (fn "select-workspace")
			(lambda ()
			  (interactive)
			  (select-workspace-from-first index)))
	  (define-value (fn "send-to-workspace")
			(lambda (w)
			  (interactive "%W")
			  (send-window-to-workspace-from-first w index)))
	  (define-value (fn "copy-to-workspace")
			(lambda (w)
			  (interactive "%W")
			  (send-window-to-workspace-from-first w index t))))))
     (i 0))
  (while (< i 9)
    (define-commands i)
    (setq i (1+ i))))


;; session management

(defun ws-saved-state (w)
  (unless (window-get w 'sticky)
    (let
	((limits (workspace-limits))
	 (spaces (window-workspaces w)))
      (when spaces
	`((workspaces . ,(mapcar (lambda (space)
				   (workspace-id-to-logical space limits))
				 spaces)))))))

(defun ws-load-state (w alist)
  (cond ((cdr (assq 'workspaces alist))
	 (ws-window-set-workspaces w (cdr (assq 'workspaces alist))))
	((cdr (assq 'workspace alist))
	 ;; backwards compatibility..
	 (ws-window-set-workspaces w (list (cdr (assq 'workspace alist)))))))


;; default swappers

(defun workspace-swap-out (w space)
  (let
      ((props (mapcar (lambda (prop)
			(cons prop (window-get w prop)))
		      workspace-local-properties)))
    ;; meta properties
    (list (cons 'position (window-absolute-position w))
	  (cons 'viewport (window-viewport w))
	  (cons 'dimensions (window-dimensions w))
	  (cons 'properties props))))

(defun workspace-swap-in (w space alist)
  (let
      ((position (cdr (assq 'position alist)))
       (viewport (cdr (assq 'viewport alist)))
       (dimensions (cdr (assq 'dimensions alist)))
       (properties (cdr (assq 'properties alist)))
       (old-type (window-type w))
       (old-frame-style (window-get w 'current-frame-style))
       new-frame-style)
    (mapc (lambda (cell)
	    (window-put w (car cell) (cdr cell))) properties)
    (when dimensions
      (resize-window-to w (car dimensions) (cdr dimensions)))
    (when (and position viewport)
      (move-window-to w (+ (car position)
			   (* (car viewport) (screen-width))
			   (- viewport-x-offset))
		      (+ (cdr position)
			 (* (cdr viewport) (screen-height))
			 (- viewport-y-offset))))
    ;; special case this to help switching the default theme
    (setq new-frame-style (or (window-get w 'frame-style) default-frame-style))
    (unless (and (eq old-frame-style new-frame-style)
		 (eq (window-type w) old-type))
      (set-window-frame-style w new-frame-style))))

(add-hook 'workspace-swap-in-hook workspace-swap-in)
(add-hook 'workspace-swap-out-hook workspace-swap-out)

;; Note that all of PROPS (symbols) should be saved and restored
;; automatically when swapping window states
(defun add-swapped-properties (&rest props)
  (mapc (lambda (p)
	  (or (memq p workspace-local-properties)
	      (setq workspace-local-properties
		    (cons p workspace-local-properties))))
	props))


;; Initialisation

(sm-add-saved-properties 'sticky 'iconified)

;; some of these should really be added by other files
(add-swapped-properties 'frame-style 'type 'hide-client 'iconified)

(add-hook 'add-window-hook ws-add-window)
(add-hook 'map-notify-hook ws-window-mapped)
(add-hook 'unmap-notify-hook ws-window-unmapped)
(add-hook 'destroy-notify-hook ws-remove-window)
(add-hook 'sm-window-save-functions ws-saved-state)
(add-hook 'sm-restore-window-hook ws-load-state)
