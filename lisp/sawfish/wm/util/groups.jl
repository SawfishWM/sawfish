;; group-funs.jl -- ever-present window-group support
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

;; Each window may only be a member of a single group, each group is
;; defined by its group id, either an integer or a symbol. Positive
;; integers are used for application-defined groups (window ids, as
;; returned by window-group-id), negative integers are used for
;; user-defined anonymous groups, symbols are used for user-defined
;; named groups.

;; To put a window in a different group to which its application
;; originally put it, set its `group' property (preferably by calling
;; the add-window-to-group function)

;; Only named groups are saved when saving sessions (anything else may
;; not work properly when the windows are reopened, and therefore their
;; ids change)

;; See groups.jl for the commands that manipulate groups of windows

(define-structure sawfish.wm.util.groups

    (export window-actual-group-id
	    windows-by-group
	    windows-in-group
	    map-window-group
	    map-other-window-groups
	    window-group-ids
	    add-window-to-group
	    add-window-to-new-group
	    window-group-menu)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.windows
	  sawfish.wm.custom
	  sawfish.wm.gaol)

  (define-structure-alias group-funs sawfish.wm.util.groups)

  (defcustom transients-are-group-members t
    "Group transient windows with their parents."
    :type boolean
    :user-level expert
    :group misc)

  (defcustom persistent-group-ids nil
    "Persistent group ids: \\w"
    :type (list symbol)
    :user-level expert
    :group misc)

  (define (window-actual-group-id w)
    "Return the id of the group that window W is a member of."
    (let (tem)
      (or (window-get w 'group)
	  (window-group-id w)
	  (and transients-are-group-members
	       (window-transient-p w)
	       (setq tem (get-window-by-id (window-transient-p w)))
	       (or (window-get tem 'group) (window-group-id tem)))
	  (window-id w))))

  (define (windows-by-group group-id #!optional by-depth)
    "Return the list of windows in the group with id GROUP-ID. If BY-DEPTH is
non-nil, then return the windows in order of stacking, from topmost to
bottommost."
    (delete-if-not (lambda (x)
		     (eq (window-actual-group-id x) group-id))
		   (if by-depth (stacking-order) (managed-windows))))

  (define (windows-in-group w #!optional by-depth)
    "Return the list of windows in the same group as window W."
    (windows-by-group (window-actual-group-id w) by-depth))

  (define (map-window-group fun w)
    "Map the single argument function FUN over all windows in the same group as
window W."
    (mapc fun (windows-in-group w)))

  (define (map-other-window-groups fun w)
    "Map the single argument function FUN over all windows not in the same
group as window W."
    (let ((group (windows-in-group w)))
      (map-windows (lambda (x)
		     (unless (memq x group)
		       (fun x))))))

  (define (window-group-ids)
    "Return the list of all group ids."
    (let ((ids (copy-sequence persistent-group-ids))
	  id)
      (map-windows (lambda (w)
		     (setq id (window-actual-group-id w))
		     (unless (memq id ids)
		       (setq ids (cons id ids)))))
      ids))

  (define (add-window-to-group w group-id)
    "Put window W in group with id GROUP-ID; returns GROUP-ID."
    (window-put w 'group group-id))

  (define (add-window-to-new-group w)
    "Add window W to a new group (i.e. has W as its sole member); returns the
id of the new group."
    (let ((ids (window-group-ids))
	  (i -1))
      (while (memq i ids)
	(setq i (1- i)))
      (add-window-to-group w i)))


;;; menu constructor

  (define (window-group-menu #!optional w)
    (unless w
      (setq w (or (current-event-window) (input-focus))))
    (let ((group-names (mapcar (lambda (x)
				 (cons (window-actual-group-id x)
				       (window-name x)))
			       (managed-windows)))
	  (group-ids (window-group-ids)))
      (nconc (mapcar
	      (lambda (id)
		(let ((name (if (symbolp id)
				(symbol-name id)
			      (cdr (assq id group-names)))))
		  (when (> (length name) 20)
		    (setq name (concat
				(substring name 0 20) "...")))
		  (list (quote-menu-item name)
			(lambda ()
			  (add-window-to-group w id))
			(cons 'check (eql (window-actual-group-id w) id))
			'(group . window-group))))
	      group-ids)
	     (list '())
	     (list (list (_ "New group")
			 (lambda ()
			   (add-window-to-new-group w)))))))


;;; session management -- only save group-ids that are _symbols_

  (define (group-saved-state w)
    (let ((group-id (window-get w 'group)))
      (when (and group-id (symbolp group-id))
	`((group . ,group-id)))))

  (define (group-load-state w alist)
    (let ((group-id (cdr (assq 'group alist))))
      (when group-id
	(add-window-to-group w group-id))))

  (add-hook 'sm-window-save-functions group-saved-state)
  (add-hook 'sm-restore-window-hook group-load-state)

;;; gaol init

  (gaol-add window-actual-group-id windows-by-group windows-in-group
	    map-window-group window-group-ids))
