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
;; `workspace-swap-{in,out}-hook' variables (see workspace-swapper module)

;; If a window is added with its `workspaces' property set, then it's
;; added to those (logical) spaces

;; Private functions are prefixed by ws-

(eval-when-compile '(require 'sawfish.wm.menus))

(define-structure sawfish.wm.workspace

    (export current-workspace
	    NormalState IconicState
	    window-workspaces
	    set-window-workspaces
	    window-in-workspace-p
	    map-window-workspaces
	    window-appears-in-workspace-p
	    transform-window-workspaces
	    workspace-empty-p
	    windows-share-workspace-p
	    all-workspaces
	    nearest-workspace-with-window
	    workspace-limits
	    workspace-id-to-logical
	    workspace-id-from-logical
	    move-window-to-workspace
	    copy-window-to-workspace
	    insert-workspace
	    remove-workspace
	    move-workspace
	    select-workspace
	    workspace-windows
	    workspace-menu
	    popup-workspace-list
	    popup-window-list
	    next-workspace
	    previous-workspace
	    send-to-next-workspace
	    send-to-previous-workspace
	    copy-to-next-workspace
	    copy-to-previous-workspace
	    merge-next-workspace
	    merge-previous-workspace
	    insert-workspace-before
	    insert-workspace-after
	    move-workspace-forwards
	    move-workspace-backwards
	    select-workspace-from-first
	    send-window-to-workspace-from-first
	    delete-empty-workspaces
	    delete-window-instance
	    add-swapped-properties
	    workspace-local-properties

	    ;; XXX rename these..?
	    ws-remove-window
	    ws-add-window-to-space
	    ws-call-with-workspace)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.session.init)


;;; Options and variables

  (defcustom workspace-geometry '(1 . (1 . 1))
    "Virtual desktop configuration."
    :type workspace-geometry
    :user-level novice
    :group workspace
    :after-set (lambda () (call-hook 'workspace-geometry-changed)))

  (defcustom workspace-boundary-mode 'stop
    "When passing the first or last workspace: \\w"
    :type symbol
    :options (stop wrap-around keep-going)
    :user-level expert
    :group workspace)

  (defcustom workspace-send-boundary-mode 'keep-going
    "When passing the first or last workspace, while moving a window: \\w"
    :type symbol
    :options (stop wrap-around keep-going)
    :user-level expert
    :group workspace)

  (defcustom delete-workspaces-when-empty nil
    "Workspaces are deleted when their last window closes."
    :type boolean
    :user-level expert
    :group workspace)

  (defvar preallocated-workspaces 1
    "Minimum number of workspaces.")

  (defcustom lock-first-workspace t
    "Preserve empty workspaces in pager."
    :type boolean
    :group workspace
    :user-level expert
    :after-set (lambda () (call-hook 'workspace-state-change-hook)))

  (defcustom transients-on-parents-workspace nil
    "Dialogs appear on the same workspace as their application."
    :type boolean
    :group workspace)

  (defcustom workspace-names nil
    nil
    :type* `(list string ,(_ "Workspace names"))
    :widget-flags (expand-vertically)
    :group workspace)

  ;; Currently active workspace, an integer
  (define current-workspace 0)

  (define first-interesting-workspace nil)
  (define last-interesting-workspace nil)

  (defvar static-workspace-menus
    `((,(_ "Insert workspace") insert-workspace-after)
      (,(_ "Select next workspace") next-workspace)
      (,(_ "Select previous workspace") previous-workspace)
      (,(_ "Merge with next") merge-next-workspace)
      (,(_ "Merge with previous") merge-previous-workspace)
      (,(_ "Move workspace right") move-workspace-forwards)
      (,(_ "Move workspace left") move-workspace-backwards)))

  ;; X constants
  (defconst NormalState 1)
  (defconst IconicState 3)

  (defvar workspace-swap-in-hook '())
  (defvar workspace-swap-out-hook '())

  (defvar enter-workspace-hook '())
  (defvar leave-workspace-hook '())
  (defvar workspace-state-change-hook '())
  (defvar add-to-workspace-hook '())
  (defvar remove-from-workspace-hook '())

  ;; window properties whose values may differ on different workspaces
  (define workspace-local-properties '())


;;; Workspace ``swapping''

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
  (define (swap-out w space)
    (let ((alist (apply nconc (mapcar (lambda (fun)
					(fun w space))
				      workspace-swap-out-hook)))
	  (swapped (assq space (window-get w 'swapped))))
      (if swapped
	  (rplacd swapped alist)
	(window-put w 'swapped (cons (cons space alist)
				     (window-get w 'swapped))))))

  ;; swap in data for workspace id SPACE to window W
  (define (swap-in w space)
    (let ((current (window-get w 'swapped-in)))
      (unless (eq current space)
	(when current
	  ;; swap out the current data
	  (swap-out w current))
	(let ((alist (assq space (window-get w 'swapped))))
	  (when alist
	    (call-hook 'workspace-swap-in-hook (list w space (cdr alist)))
	    (window-put w 'swapped (delq alist (window-get w 'swapped))))
	  (window-put w 'swapped-in space)))))


;;; Low level functions

  ;; return list of all workspaces containing window W
  (defmacro window-workspaces (w)
    `(window-get ,w 'workspaces))

  ;; set list of all workspaces containing window W to LST
  (defmacro set-window-workspaces (w lst)
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
  (define (window-add-to-workspace w space)
    (set-window-workspaces
     w (cons space (delq space (window-workspaces w))))
    (cond ((= space current-workspace)
	   (swap-in w space))
	  ((and (window-get w 'swapped-in)
		(/= (window-get w 'swapped-in) space))
	   ;; write out the current state for when we switch
	   (swap-out w space))
	  (t
	   (window-put w 'swapped-in space))))

  ;; remove window W from those in workspace SPACE
  (define (window-remove-from-workspace w space)
    (set-window-workspaces w (delq space (window-workspaces w)))
    (window-put w 'swapped (delete-if (lambda (cell)
					(= (car cell) space))
				      (window-get w 'swapped)))
    (when (eq (window-get w 'swapped-in) space)
      (window-put w 'swapped-in nil)))

  ;; FUN is a function transforming an old workspace id to a new id.
  ;; Run this function over all workspaces that window W is a member of.
  (define (transform-window-workspaces fun w)
    (let ((workspaces (window-get w 'workspaces))
	  (swapped (window-get w 'swapped))
	  (swapped-in (window-get w 'swapped-in))
	  (new-workspaces '())
	  (new-swapped '()))
      (while workspaces
	(let* ((space (car workspaces))
	       (swap (assq space swapped))
	       (new (fun space)))
	  (unless (memq new new-workspaces)
	    (setq new-workspaces (cons new new-workspaces))
	    (when swap
	      (setq new-swapped (cons (cons new (cdr swap)) new-swapped))))
	  (when (eq swapped-in space)
	    (window-put w 'swapped-in new))
	  (setq workspaces (cdr workspaces))))
      (set-window-workspaces w (nreverse new-workspaces))
      (window-put w 'swapped (nreverse new-swapped))))

  ;; return t if workspace SPACE contains zero (non-sticky) windows
  (define (workspace-empty-p space)
    (catch 'out
      (map-windows (lambda (w)
		     (when (window-in-workspace-p w space)
		       (throw 'out nil))))
      t))

  ;; return t if windows W-1 and W-2 both appear on the same workspace
  (define (windows-share-workspace-p w-1 w-2)
    (if (or (window-get w-1 'sticky) (window-get w-2 'sticky))
	t
      (let ((spaces-1 (window-workspaces w-1))
	    (spaces-2 (window-workspaces w-2)))
	(catch 'out
	  (mapc (lambda (space)
		  (when (memq space spaces-2)
		    (throw 'out t))) spaces-1)
	  nil))))

  ;; return a list of all workspace indices that contain windows
  (define (all-workspaces)
    (let (spaces)
      (map-windows
       (lambda (w)
	 (map-window-workspaces (lambda (space)
				  (unless (memq space spaces)
				    (setq spaces (cons space spaces)))) w)))
      spaces))

  ;; return the nearest workspace to SPACE that contains window W
  (define (nearest-workspace-with-window w space)
    (if (window-get w 'sticky)
	space
      (let ((all (window-workspaces w))
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
  (define (workspace-limits)
    (let* ((all-spaces (all-workspaces))
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

  (define (workspace-id-to-logical space #!optional limits)
    (unless limits
      (setq limits (workspace-limits)))
    (- space (car limits)))

  (define (workspace-id-from-logical offset #!optional limits)
    (unless limits
      (setq limits (workspace-limits)))
    (+ offset (car limits)))

  ;; renormalize the interesting workspaces so they begin at index zero
  (define (normalize-indices)
    (let* ((first-space (car (workspace-limits))))
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
  (define (insert-workspace #!optional before)
    (unless before
      (setq before current-workspace))
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
  (define (remove-workspace #!optional index)
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
  (define (move-workspace index count)
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
						   (t space))) w)))
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
						   (t space))) w)))
	   (cond ((= current-workspace index)
		  (setq current-workspace (+ current-workspace count)))
		 ((< current-workspace index)
		  (setq current-workspace (1+ current-workspace))))))
    (call-hook 'workspace-state-change-hook))

  ;; called when workspace with id SPACE may contain no windows
  (define (ws-workspace-may-be-empty space)
    (when (and delete-workspaces-when-empty (workspace-empty-p space))
      ;; workspace is now empty
      (let* ((limits (workspace-limits))
	     (need-to-move (and (= space current-workspace)
				(/= space (car limits))
				(= space (cdr limits)))))
	(remove-workspace space)
	(normalize-indices)
	(when need-to-move
	  (select-workspace (1- current-workspace))))))

  ;; called when window W is destroyed
  (define (ws-remove-window w #!optional dont-hide)
    (let ((spaces (window-workspaces w)))
      (mapc (lambda (space)
	      (window-remove-from-workspace w space)) spaces)
      (mapc (lambda (space)
	      (ws-workspace-may-be-empty space)) (sort spaces >))
      (when (and (not dont-hide) (windowp w))
	(hide-window w))
      (mapc (lambda (space)
	      (call-window-hook
	       'remove-from-workspace-hook w (list space))) spaces)
      (call-hook 'workspace-state-change-hook)))

  ;; move window W from workspace id OLD to workspace NEW
  (define (move-window-to-workspace w old new #!optional was-focused)
    (or (window-in-workspace-p w old)
	(error
	 "move-window-to-workspace--window isn't in original workspace: %s, %s" w old))
    (unless (= old new)
      (cond ((window-in-workspace-p w new)
	     ;; just remove from the source workspace
	     (window-remove-from-workspace w old)
	     (call-window-hook 'remove-from-workspace-hook w (list old)))
	    (t
	     ;; need to move it..
	     (transform-window-workspaces (lambda (space)
					    (if (= space old)
						new
					      space)) w)))
      (cond ((= old current-workspace)
	     (hide-window w))
	    ((and (= new current-workspace) (not (window-get w 'iconified)))
	     (show-window w)))
      (ws-workspace-may-be-empty old)
      ;; the window may lose the focus when switching spaces
      (when (and was-focused (window-visible-p w))
	(set-input-focus w))
      (call-window-hook 'add-to-workspace-hook w (list new))
      (call-hook 'workspace-state-change-hook)))

  ;; arrange it so that window W appears on both OLD and NEW workspaces
  (define (copy-window-to-workspace w old new #!optional was-focused)
    (or (window-in-workspace-p w old)
	(error
	 "copy-window-to-workspace--window isn't in original workspace: %s, %s" w old))
    (unless (= old new)
      (unless (window-in-workspace-p w new)
	(window-add-to-workspace w new))
      (when (and (= new current-workspace) (not (window-get w 'iconified)))
	(show-window w))
      ;; the window may lose the focus when switching spaces
      (when (and was-focused (window-visible-p w))
	(set-input-focus w))
      (call-window-hook 'add-to-workspace-hook w (list new))
      (call-hook 'workspace-state-change-hook)))

  ;; switch to workspace with id SPACE
  (define (select-workspace space #!optional dont-focus inner-thunk)
    "Activate workspace number SPACE (from zero)."
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
      (when inner-thunk
	(inner-thunk))
      (when current-workspace
	(with-server-grabbed
	 (map-windows
	  (lambda (w)
	    (when (or (window-get w 'sticky)
		      (window-in-workspace-p w current-workspace))
	      (swap-in w current-workspace))
	    (when (and (not (window-get w 'sticky))
		       (window-in-workspace-p w current-workspace)
		       (window-get w 'placed))
	      (if (window-get w 'iconified)
		  (hide-window w)
		(show-window w))))))
	(unless (or dont-focus (eq focus-mode 'enter-exit))
	  (require 'sawfish.wm.util.window-order)
	  (window-order-focus-most-recent))
	(when current-workspace
	  (call-hook 'enter-workspace-hook (list current-workspace)))
	(call-hook 'workspace-state-change-hook))))

  ;; return a list of all windows on workspace index SPACE
  (define (workspace-windows space #!optional include-iconified)
    (filter-windows
     (lambda (w)
       (and (window-in-workspace-p w space)
	    (or include-iconified (not (window-get w 'iconified)))))))

  ;; add window W to workspace index SPACE; window shouldn't be in any
  ;; workspace
  (define (ws-add-window-to-space w space)
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
  (define (ws-add-window w)
    (if (window-get w 'sticky)
	(progn
	  (set-window-workspaces w nil)
	  (window-put w 'swapped-in current-workspace)
	  (if (window-get w 'iconified)
	      (hide-window w)
	    (show-window w)))
      (if (window-workspaces w)
	  (let ((spaces (window-workspaces w))
		(limits (workspace-limits)))
	    (set-window-workspaces w '())
	    (mapc (lambda (space)
		    (ws-add-window-to-space
		     w (workspace-id-from-logical space limits))) spaces))
	(let (parent)
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
  (define (ws-window-unmapped w)
    (unless (window-get w 'sticky)
      (ws-remove-window w t)))

  ;; called from the map-notify-hook
  (define (ws-window-mapped w)
    (unless (or (window-get w 'sticky) (window-workspaces w))
      (ws-add-window w)))


;;; Menu constructors

  (define (workspace-menu)
    (let ((limits (workspace-limits))
	  menu)
      (do ((i (car limits) (1+ i)))
	  ((> i (cdr limits)))
	(let ((ws-name (or (nth (- i (car limits)) workspace-names)
			   (format nil (_ "space %d")
				   (1+ (- i (car limits)))))))
	  (setq menu (cons (list (quote-menu-item ws-name)
				 (lambda () (select-workspace i))
				 (cons 'check (= i current-workspace))
				 '(group . current-workspace))
			   menu))))
      (nconc (nreverse menu) (list nil) static-workspace-menus)))

  (define (popup-workspace-list)
    "Display the menu containing the list of all workspaces."
    (require 'sawfish.wm.menus)
    (popup-menu (workspace-menu)))

  (define (popup-window-list)
    "Display the menu of all managed windows."
    (require 'sawfish.wm.menus)
    (popup-menu (window-menu)))

  (define-command 'popup-workspace-list popup-workspace-list)
  (define-command 'popup-window-list popup-window-list)


;;; Commands

  (define (ws-call-with-workspace fun count mode)
    (let ((limits (workspace-limits))
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

  (define (next-workspace count)
    "Display the next workspace."
    (ws-call-with-workspace select-workspace count workspace-boundary-mode))

  (define (previous-workspace count)
    "Display the previous workspace."
    (next-workspace (- count)))

  (define-command 'next-workspace next-workspace #:spec "p")
  (define-command 'previous-workspace previous-workspace #:spec "p")

  (define (send-to-next-workspace w count #!optional copy select)
    "Move the window to the next workspace."
    (ws-call-with-workspace
     (lambda (space)
       (let ((was-focused (eq w (input-focus)))
	     (orig-space (if (window-in-workspace-p
			      w current-workspace)
			     current-workspace
			   (car (window-workspaces w)))))
	 (when orig-space
	   (copy-window-to-workspace w orig-space space was-focused)
	   (when select
	     (select-workspace space was-focused))
	   (unless copy
	     (move-window-to-workspace w orig-space space was-focused)))))
     count workspace-send-boundary-mode))

  (define (send-to-previous-workspace w count #!optional copy select)
    "Move the window to the previous workspace."
    (send-to-next-workspace w (- count) copy select))

  (define-command 'send-to-next-workspace send-to-next-workspace
    #:spec "%W\np\n\nt")
  (define-command 'send-to-previous-workspace send-to-previous-workspace
    #:spec "%W\np\n\nt")

  (define (copy-to-next-workspace w count select)
    "Copy the window to the next workspace."
    (send-to-next-workspace w count t select))

  (define (copy-to-previous-workspace w count #!optional select)
    "Copy the window to the previous workspace."
    (send-to-previous-workspace w count t select))

  (define-command 'copy-to-next-workspace copy-to-next-workspace
    #:spec "%W\np\nt")
  (define-command 'copy-to-previous-workspace copy-to-previous-workspace
    #:spec "%W\np\nt")

  (define (append-workspace-and-send w #!optional select)
    "Create a new workspace at the end of the list, and move the window to it."
    (let ((limits (workspace-limits))
	  (was-focused (eq (input-focus) w))
	  (orig-space (if (window-in-workspace-p w current-workspace)
			  current-workspace
			(car (window-workspaces w)))))
      (when orig-space
	(if select
	    (progn
	      (select-workspace (1+ (cdr limits)) was-focused)
	      (move-window-to-workspace
	       w orig-space current-workspace was-focused))
	  (move-window-to-workspace
	   w orig-space (1+ (cdr limits)) was-focused)))))

  (define (prepend-workspace-and-send w #!optional select)
    "Create a new workspace at the start of the list, and move the window to it."
    (let ((limits (workspace-limits))
	  (was-focused (eq (input-focus) w))
	  (orig-space (if (window-in-workspace-p w current-workspace)
			  current-workspace
			(car (window-workspaces w)))))
      (when orig-space
	(if select
	    (progn
	      (select-workspace (1- (car limits)) was-focused)
	      (move-window-to-workspace
	       w orig-space current-workspace was-focused))
	  (move-window-to-workspace
	   w orig-space (1- (car limits)) was-focused)))))

  (define-command 'append-workspace-and-send append-workspace-and-send
    #:spec "%W\nt" #:user-level 'expert)
  (define-command 'prepend-workspace-and-send prepend-workspace-and-send
    #:spec "%W\nt" #:user-level 'expert)

  (define (merge-next-workspace)
    "Delete the current workspace. Its member windows are relocated to the next
workspace."
    (remove-workspace current-workspace))

  (define (merge-previous-workspace)
    "Delete the current workspace. Its member windows are relocated to the
previous workspace."
    (remove-workspace (1- current-workspace)))

  (define-command 'merge-next-workspace merge-next-workspace
    #:user-level 'expert)
  (define-command 'merge-previous-workspace merge-previous-workspace
    #:user-level 'expert)

  (define (insert-workspace-after)
    "Create a new workspace following the current workspace."
    (insert-workspace current-workspace)
    (select-workspace (1+ current-workspace)))

  (define (insert-workspace-before)
    "Create a new workspace before the current workspace."
    (insert-workspace (1- current-workspace))
    (select-workspace (- current-workspace 2)))

  (define-command 'insert-workspace-after insert-workspace-after
    #:user-level 'expert)
  (define-command 'insert-workspace-before insert-workspace-before
    #:user-level 'expert)

  (define (move-workspace-forwards #!optional count)
    "Move the current workspace one place to the right."
    (move-workspace current-workspace (or count 1)))

  (define (move-workspace-backwards #!optional count)
    "Move the current workspace one place to the left."
    (move-workspace current-workspace (- (or count 1))))

  (define-command 'move-workspace-forwards move-workspace-forwards
    #:user-level 'expert)
  (define-command 'move-workspace-backwards move-workspace-backwards
    #:user-level 'expert)

  (define (select-workspace-from-first count)
    (select-workspace (workspace-id-from-logical count)))

  (define (send-window-to-workspace-from-first w count #!optional copy)
    (let* ((was-focused (eq (input-focus) w))
	   (orig-space (if (window-in-workspace-p w current-workspace)
			   current-workspace
			 (car (window-workspaces w))))
	   (new-space (workspace-id-from-logical count)))
      (when orig-space
	(copy-window-to-workspace w orig-space new-space was-focused)
	(select-workspace new-space was-focused)
	(unless copy
	  (move-window-to-workspace w orig-space new-space was-focused)))))

  (define (delete-empty-workspaces)
    "Delete any workspaces that don't contain any windows."
    (let* ((limits (workspace-limits))
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
		   (remove-workspace space)
		   (setq limits (workspace-limits))))
	  (setq space (1+ space))))
      (when (> first-interesting-workspace last-interesting-workspace)
	(setq first-interesting-workspace last-interesting-workspace))))

  (define-command 'delete-empty-workspaces delete-empty-workspaces
    #:user-level 'expert)

  (define (delete-window-instance w)
    "Remove the copy of the window on the current workspace. If this is the
last instance remaining, then delete the actual window."
    (let ((spaces (window-workspaces w)))
      (if (cdr spaces)
	  ;; not the last instance
	  (let ((space (if (memq current-workspace spaces)
			   current-workspace
			 (car spaces))))
	    (window-remove-from-workspace w space)
	    (when (= space current-workspace)
	      (hide-window w))
	    (ws-workspace-may-be-empty space)
	    (call-window-hook 'remove-from-workspace-hook w (list space))
	    (call-hook 'workspace-state-change-hook))
	(delete-window w))))

  (define-command 'delete-window-instance delete-window-instance #:spec "%W")


;; some commands for moving directly to a workspace

  (define (activate-workspace n)
    "Select the N'th workspace."
    (select-workspace-from-first (1- n)))

  (define-command 'activate-workspace activate-workspace
    #:spec "NWorkspace:"
    #:type `(and (labelled ,(_ "Workspace:") (number 1))))

  (define (send-to-workspace n)
    "Move the current window to the N'th workspace."
    (send-window-to-workspace-from-first (current-event-window) (1- n)))

  (define-command 'send-to-workspace send-to-workspace
    #:spec "NWorkspace:"
    #:type `(and (labelled ,(_ "Workspace:") (number 1))))

  (define (copy-to-workspace n)
    "Copy the current window to the N'th workspace."
    (send-window-to-workspace-from-first (current-event-window) (1- n) t))

  (define-command 'copy-to-workspace copy-to-workspace
    #:spec "NWorkspace:"
    #:type `(and (labelled ,(_ "Workspace:") (number 1))))

  (define (select-workspace-interactively)
    "Prompt for a workspace and switch to it."
    (require 'sawfish.wm.util.prompt)
    (let ((ws (prompt-for-workspace)))
      (when ws
	(select-workspace-from-first ws))))

  (define-command 'select-workspace-interactively
    select-workspace-interactively #:spec "%S")


;;; session management

  (define (ws-saved-state w)
    (unless (window-get w 'sticky)
      (let ((limits (workspace-limits))
	    (spaces (window-workspaces w)))
	(when spaces
	  `((workspaces . ,(mapcar (lambda (space)
				     (workspace-id-to-logical space limits))
				   spaces)))))))

  (define (ws-load-state w alist)
    (cond ((cdr (assq 'workspaces alist))
	   (set-window-workspaces w (cdr (assq 'workspaces alist))))
	  ((cdr (assq 'workspace alist))
	   ;; backwards compatibility..
	   (set-window-workspaces w (list (cdr (assq 'workspace alist)))))))

  ;; Note that all of PROPS (symbols) should be saved and restored
  ;; automatically when swapping window states
  (define (add-swapped-properties #!rest props)
    (mapc (lambda (p)
	    (or (memq p workspace-local-properties)
		(setq workspace-local-properties
		      (cons p workspace-local-properties))))
	  props))


;;; configuration

  (add-hook 'workspace-geometry-changed
	    (lambda ()
	      (setq preallocated-workspaces (car workspace-geometry))
	      (call-hook 'workspace-state-change-hook)))


;;; Initialisation

  (sm-add-saved-properties 'sticky 'iconified 'fixed-position)

  ;; some of these should really be added by other files
  (add-swapped-properties 'frame-style 'type 'hide-client 'iconified)

  (add-hook 'add-window-hook ws-add-window)
  (add-hook 'map-notify-hook ws-window-mapped)
  (add-hook 'unmap-notify-hook ws-window-unmapped)
  (add-hook 'destroy-notify-hook ws-remove-window)
  (add-hook 'sm-window-save-functions ws-saved-state)
  (add-hook 'sm-restore-window-hook ws-load-state))
