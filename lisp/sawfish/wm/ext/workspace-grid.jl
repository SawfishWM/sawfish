#| workspace-grid.jl -- Workspace Grid

   $Id$

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

   Author: Michael Toomim <toomim@cs.berkeley.edu>
|#

;; Commentary:

;; Provides a virtual desktop "grid" abstraction, using workspaces. In
;; effect, this emulates viewports with workspaces
  
;; Gets desktop layout information from the _NET_DESKTOP_LAYOUT root
;; property according to a proposed ICWWM EWMH spec.  Accounts for bugs
;; in gnome2's implementation.

(define-structure sawfish.wm.ext.workspace-grid

    (export workspace-up
	    workspace-down
	    workspace-right
	    workspace-left

	    send-to-workspace-up
	    send-to-workspace-down
	    send-to-workspace-left
	    send-to-workspace-right

	    ;; Useful for edge-flip functions, etc.
	    get-workspace-up
	    get-workspace-down
	    get-workspace-left
	    get-workspace-right)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.workspace
	  sawfish.wm.commands)

;;; Public Variables

  (defvar overriden-pager-vertical-p nil
    "Set this to t if you have no _NET_DESKTOP_LAYOUT root property and
want your workspaces to behave as if they were on a vertical pager.")

  (defvar overriden-num-workspace-rows nil
    "If you have no _NET_DESKTOP_LAYOUT root property, set this to the
number of rows your workspaces are arranged in")

;;; Public Commands

  ;; Move current workspace commands
  (define (workspace-down)
    "View the workspace below the current workspace"
    (safe-select-workspace
     (get-workspace-down current-workspace 1)))

  (define (workspace-up)
    "View the workspace above the current workspace"
    (safe-select-workspace
     (get-workspace-up current-workspace 1)))

  (define (workspace-left)
    "View the workspace to the left of the current workspace"
    (safe-select-workspace
     (get-workspace-left current-workspace 1)))

  (define (workspace-right)
    "View the workspace to the right of the current workspace"
    (safe-select-workspace
     (get-workspace-right current-workspace 1)))

  ;;###autoload
  (define-command 'workspace-down workspace-down)
  (define-command 'workspace-up workspace-up)
  (define-command 'workspace-left workspace-left)
  (define-command 'workspace-right workspace-right)

  ;; "Send-window" commands
  (define (send-to-workspace-down window)
    "Send window to the workspace below the current workspace"
    (safe-send-to-workspace
     window
     (get-workspace-down current-workspace 1)))
     
  (define (send-to-workspace-up window)
    "Send window to the workspace above the current workspace"
    (safe-send-to-workspace
     window
     (get-workspace-up current-workspace 1)))
     
  (define (send-to-workspace-left window)
    "Send window to the workspace left of the current workspace"
    (safe-send-to-workspace
     window
     (get-workspace-left current-workspace 1)))
     
  (define (send-to-workspace-right window)
    "Send window to the workspace right of the current workspace"
    (safe-send-to-workspace
     window
     (get-workspace-right current-workspace 1)))

  ;;###autoload
  (define-command 'send-to-workspace-down send-to-workspace-down #:spec "%W")
  (define-command 'send-to-workspace-up send-to-workspace-up #:spec "%W")
  (define-command 'send-to-workspace-left send-to-workspace-left #:spec "%W")
  (define-command 'send-to-workspace-right send-to-workspace-right #:spec "%W")
     
;;; Public Procedures

  ;; The workspace-switcher makes a desktop grid by splitting a sequence of
  ;; workspaces into rows and columns.
  ;;
  ;; With a horizontal panel, workspaces line up as
  ;;    1 2 3
  ;;    4 5 6
  ;; but with a vertical panel, they line up as
  ;;    1 4
  ;;    2 5
  ;;    3 6
  ;;
  ;; I take the convention that a "row" is a sequence of consecutive
  ;; workspaces (horizontal for a horizontal panel, vertical for a vertical
  ;; panel), and a "column" is the opposite (vertical on a horizontal panel,
  ;; horizontal on a vertical panel).  This way, I can just think about what
  ;; the coordinate code is supposed to do on horizontal panels, and then
  ;; translate it to "up", "down", "left", "right", etc. later.
  ;;
  ;; A `coordinates' object consists of the pair (column . row).

  ;; Get workspace in user-coordinate directions
  (define (get-workspace-down workspace distance)
    "Returns the workspace `distance' squares below `workspace' in the
pager, or nil of none exists."
    (let ((coords (workspace->coordinates workspace)))
      (coordinates->workspace
       (if (pager-vertical-p)
	   (cons (+ distance (car coords))
		 (cdr coords))
	 (cons (car coords)
	       (+ distance (cdr coords)))))))
      
  (define (get-workspace-right workspace distance)
    "Returns the workspace `distance' squares right of `workspace' in
the pager, or nil of none exists."
    (let ((coords (workspace->coordinates workspace)))
      (coordinates->workspace
       (if (pager-vertical-p)
	   (cons (car coords)
		 (+ distance (cdr coords)))
	 (cons (+ distance (car coords))
	       (cdr coords))))))

  (define (get-workspace-up workspace distance)
    "Returns the workspace `distance' squares above `workspace' in the
pager, or nil of none exists."
    (get-workspace-down workspace (- distance)))

  (define (get-workspace-left workspace distance)
    "Returns the workspace `distance' squares left of `workspace' in
the pager, or nil of none exists."
    (get-workspace-right workspace (- distance)))

;;; Private Procedures

  (define (num-workspaces)
    (let ((limits (workspace-limits)))
      (- (cdr limits) (car limits) -1)))

  ;; Defaults to horizontal, in case there is no _NET_DESKTOP_LAYOUT property
  (define (pager-vertical-p #!optional property)
    (or overriden-pager-vertical-p
	(let ((prop (or property
			(get-x-property 'root '_NET_DESKTOP_LAYOUT))))
	  (and prop
	       (= (aref (caddr prop) 0)
		  1)))))

  (define-macro (swap x y)
    `(setq ,x (prog1 ,y (setq ,y ,x))))

  (define (num-workspace-rows)
    (or overriden-num-workspace-rows

	(let ((layout-property (get-x-property 'root '_NET_DESKTOP_LAYOUT)))

	  ;; Property is of the form (CARDINAL 32 [orientation x y])
	  ;; Orientation horizontal=0, vert=1
	  ;; x is always sideways
	  ;; y is always vertical
	  ;; x or y are optional (can be zero)
	  
	  ;; Bugs in gnome2: x and y are switched, orientation is always
	  ;; horizontal
	  (if (not layout-property)
	      1				;Default to 1 row

	    (let ((cols (aref (caddr layout-property) 1))
		  (rows (aref (caddr layout-property) 2)))
	      
	      ;; Since we're translating (x,y) to (cols,rows), swap if values
	      ;; are for a vertical pager,
  	      (when (= (aref (caddr layout-property) 0) 1)
  		(swap cols rows))

	      (if (= rows 0)
		  ;; Compute rows from colums and num-workspaces
		  (let ((total (num-workspaces)))
		    (+ (quotient total cols)
		       (if (= (remainder total cols) 0)
			   0
			 1)))
		rows))))))

  (define (num-workspaces-per-row)
    (quotient (num-workspaces) (num-workspace-rows)))

  ;; Existence
  (define (coordinates-exist-p coords)
    (and (>= (car coords) 0)
	 (< (car coords) (num-workspaces-per-row))
	 (>= (cdr coords) 0)
	 (< (cdr coords) (num-workspace-rows))))

  ;; Coordinate Mapping
  (define (workspace->coordinates workspace)
    "Returns zero-based (column . row) coordinates of `workspace' in pager."
    (let ((normalized-workspace-index (- workspace (car (workspace-limits))))
	  (row-size (num-workspaces-per-row)))
      (cons (modulo normalized-workspace-index row-size)
	    (quotient normalized-workspace-index row-size))))

  (define (coordinates->workspace coordinates)
    "Returns the workspace at `coordinates', or nil if coordinates are invalid"
    (when (coordinates-exist-p coordinates)
      (+ (car (workspace-limits))	;min real workspace
	 (car coordinates)		;column
	 (* (cdr coordinates)		;row
	    (num-workspaces-per-row)))))

  ;; Misc
  (define (safe-select-workspace workspace)
    "Selects `workspace' iff non-nil"
    (when workspace
      (select-workspace workspace)))

  (define (safe-send-to-workspace window workspace)
    "Sends `window' to `workspace' and selects `workspace' iff
workspace is non-nil."
    (when workspace
      (send-to-next-workspace window
			      (- workspace current-workspace) nil t))))
