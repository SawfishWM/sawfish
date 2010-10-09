;; nokogiri-group.jl -- group management
;;
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.cfg.group

    (export group-real-name
	    group-slots
	    group-sub-groups
	    group-layout
	    root-group
	    top-group
	    set-top-group
	    get-group
	    fetch-group
	    update-group
	    get-sub-groups
	    refresh-groups-for-slots
	    make-group-tree
	    select-group
	    redisplay-group)

    (open rep
	  gui.gtk-2.gtk
	  rep.system
	  rep.data.records
	  rep.data.tables
	  sawfish.cfg.slot
	  sawfish.cfg.wm)

  (define-record-type :group
    (make-group name)
    ;; [no predicate]
    (name group-name)					;full name (a list)
    (real-name group-real-name group-real-name-set)	;human-readable name
    (loaded group-loaded-p group-loaded-set)		;t iff members read
    (slots group-slots group-slots-set)			;list of slots
    (sub-groups group-sub-groups group-sub-groups-set)	;((SYMBOL . REAL)..)
    (tree group-tree group-tree-set)			;GtkTree of sub groups
    (layout group-layout group-layout-set))

  (define-record-discloser :group
    (lambda (g) (format nil "#<:group %s>" (group-name g))))

  ;; hash table of all group objects
  (define group-table (make-table equal-hash equal))

  (define root-group '(root))		;XXX should be a constant

  (define top-group root-group)

  (define (set-top-group g) (setq top-group g))

  (define current-group nil)

  (defvar *nokogiri-group-selected-hook* '())
  (defvar *nokogiri-group-deselected-hook* '())

  (define (get-key lst key) (cadr (memq key lst)))

;;; group name manipulation

  ;; return the name of the parent of the group called GROUP, or
  ;; nil if this is the topmost group
  (define (group-name-above group)
    (if (null (cdr group))
	'()
      (let ((name (copy-sequence group)))
	(rplacd (nthcdr (- (length name) 2) name) '())
	name)))

  (define (group-name-local group) (last group))

  ;; return the name of the child called CHILD of the group called GROUP
  (define (group-name-add group child)
    (append group (list child)))

  (define group-name= equal)

;;; group creation and loading

  ;; return the group called NAME
  (define (get-group name)
    (let ((group (table-ref group-table name)))
      (unless group
	(setq group (make-group name))
	(table-set group-table name group))
      group))

  ;; ensure that all data for GROUP has been read
  (define (fetch-group group #!key force)
    (when (or force (not (group-loaded-p group)))
      (update-group group)))

  ;; forcibly reread data for GROUP
  (define (update-group group)
    (let ((data (wm-load-group (group-name group))))
      ;; DATA is (LAST-NAME-COMPONENT "REAL-NAME" (ITEMS...) OPTIONS...)
      ;; ITEMS are CUSTOM-NAME, or (SUB-GROUP-NAME REAL-NAME)
      (let ((real-name (cadr data))
	    (items (caddr data))
	    (layout (get-key (cdddr data) #:layout)))
	(group-real-name-set group real-name)
	(group-slots-set group (fetch-slots (filter atom items)))
	(group-sub-groups-set group (filter consp items))
	(group-layout-set group (or layout 'vbox))
	(group-loaded-set group t)
	(mapc update-dependences (group-slots group)))))

  ;; return a list containing the sub-groups of GROUP
  (define (get-sub-groups group)
    (mapcar (lambda (cell)
	      (get-group (group-name-add (group-name group) (car cell))))
	    (group-sub-groups group)))

  ;; return the parent group of GROUP, or nil
  (define (group-parent group)
    (let ((parent-name (group-name-above (group-name group))))
      (and parent-name (get-group parent-name))))

  ;; if the data for GROUP has been loaded, reload it and resync all state
  (define (refresh-group group)
    (when (group-loaded-p group)
      (let ((old-slots (length (group-slots group))))
	;; reload the group data from the wm
	(fetch-group group #:force t)
	(when (group-tree group)
	  ;; if necessary update the sub-trees of the group
	  (let ((old (gtk-container-get-children (group-tree group))))
	    (populate-branch group)
	    (mapc (lambda (x)
		    (gtk-tree-remove-item (group-tree group) x)) old)))
	;; if this is the currently displayed group, then
	;; make sure the display is consistent with the new state
	(when (and (eq group current-group)
		   (/= (length (group-slots group)) old-slots))
	  (select-group group #:force t)))))

  ;; Return the list of (unique) groups containing the list of SLOTS
  (define (locate-groups slots)
    (let ((out '()))
      (table-walk (lambda (name group)
		    (declare (unused name))
		    (when (unionq slots (group-slots group))
		      (setq out (cons group out))))
		  group-table)
      out))

  ;; Reload all groups containing the list of SLOTS
  (define (refresh-groups-for-slots slots)
    (mapc refresh-group (locate-groups slots)))

;;; group widgetry

  ;; creates the top-level tree node
  (define (make-group-tree group)
    (fetch-group group)
    (let ((tree (gtk-tree-new))
	  (item (make-tree-item (group-name-above (group-name group))
				(group-name-local (group-name group))
				(group-real-name group))))
      (gtk-tree-set-selection-mode tree 'browse)
      (gtk-tree-append tree item)
      (gtk-widget-show-all tree)
      tree))

  ;; creates the tree-item for a named group
  (define (make-tree-item parent-name name real-name)
    (let ((item (gtk-tree-item-new-with-label (_ real-name))))
      (g-signal-connect
       item "select" (group-selected parent-name name))
      (g-signal-connect
       item "deselect" (group-deselected parent-name name))
      item))

  ;; fills the contents of the tree associated with GROUP
  (define (populate-branch group)
    ;; check for sub groups
    (fetch-group group)
    (when (group-sub-groups group)
      (mapc (lambda (sub)
	      (let ((sgroup (get-group
			     (group-name-add (group-name group) (car sub))))
		    (item (make-tree-item (group-name group)
					  (car sub) (cadr sub))))
		(gtk-tree-append (group-tree group) item)
		(when (group-tree sgroup)
		  ;; rebuild the sub-tree of this item
		  (make-branch item sgroup))))
	    (group-sub-groups group))
      (gtk-widget-show-all (group-tree group))))

  ;; adds a sub-tree to ITEM representing GROUP
  (define (make-branch item group)
    (fetch-group group)
    (when (group-tree group)
      (group-tree-set group nil))
    (when (group-sub-groups group)
      (group-tree-set group (gtk-tree-new))
      (populate-branch group)
      (gtk-tree-item-set-subtree item (group-tree group))
      (gtk-tree-item-expand item)))

  (define (group-selected parent-name name)
    ;; called when a tree node is selected
    (lambda (item)
      (let ((group (get-group (group-name-add parent-name name))))
	(setq current-group group)

	;; fill the contents of the branch
	(unless (group-tree group)
	  (make-branch item group))

	;; display the slots for this group
	(call-hook '*nokogiri-group-selected-hook* (list group)))))

  (define (group-deselected parent-name name)
    (lambda (item)
      (declare (unused item))
      (let ((group (get-group (group-name-add parent-name name))))
	(call-hook '*nokogiri-group-deselected-hook* (list group))
	(setq current-group nil))))

  (define (select-group group #!key force)
    (when (or force (not (eq current-group group)))
      (when current-group
	(call-hook '*nokogiri-group-deselected-hook* (list current-group)))
      (setq current-group group)
      (call-hook '*nokogiri-group-selected-hook* (list current-group))))

  (define (redisplay-group)
    (when current-group
      (call-hook '*nokogiri-group-deselected-hook* (list current-group))
      (call-hook '*nokogiri-group-selected-hook* (list current-group))))

;;; util

  ;; return the union of lists X and Y, using `eq' for comparisons
  (define (unionq x y)
    (let loop
	((rest x)
	 (out '()))
      (cond ((null rest) (nreverse out))
	    ((memq (car rest) y) (loop (cdr rest) (cons (car rest) out)))
	    (t (loop (cdr rest) out))))))
