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

(provide 'group-funs)

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

(defcustom transients-are-group-members t
  "Group transient windows with their parents."
  :type boolean
  :group misc)

(defvar persistent-group-ids nil
  "List of group ids that always exist, even when they have no members.")

;; return the id of the group that window W is a member of
(defun window-actual-group-id (w)
  (let
      (tem)
    (or (window-get w 'group)
	(window-group-id w)
	(and transients-are-group-members
	     (window-transient-p w)
	     (setq tem (get-window-by-id (window-transient-p w)))
	     (or (window-get tem 'group) (window-group-id tem)))
	(window-id w))))

;; return the list of windows in the group with id GROUP-ID
(defun windows-by-group (group-id)
  (delete-if-not (lambda (x)
		   (eq (window-actual-group-id x) group-id))
		 (managed-windows)))

;; return the list of windows in the same group as window W
(defun windows-in-group (w)
  (let
      (group-id tem)
    (setq group-id (window-actual-group-id w))
    (windows-by-group group-id)))

;; map FUN over all windows in the same group as window W
(defun map-window-group (fun w)
  (mapc fun (windows-in-group w)))

(defun map-other-window-groups (fun w)
  (let
      ((group (windows-in-group w)))
    (map-windows (lambda (x)
		   (unless (memq x group)
		     (fun x))))))

;; return list of all group ids
(defun window-group-ids ()
  (let
      ((ids (copy-sequence persistent-group-ids))
       id)
    (mapc (lambda (w)
	    (setq id (window-actual-group-id w))
	    (unless (memq id ids)
	      (setq ids (cons id ids)))) (managed-windows))
    ids))

;; put window W in group with id GROUP-ID, returns GROUP-ID
(defun add-window-to-group (w group-id)
  (window-put w 'group group-id))

;; returns the id of the new group
(defun add-window-to-new-group (w)
  (let
      ((ids (window-group-ids))
       (i -1))
    (while (memq i ids)
      (setq i (1- i)))
    (add-window-to-group w i)))


;; menu constructor

(defun window-group-menu (&optional w)
  (unless w
    (setq w (or (current-event-window) (input-focus))))
  (let
      ((group-id (window-actual-group-id w))
       (group-names (mapcar (lambda (x)
			      (cons (window-actual-group-id x)
				    (window-name x)))
			    (managed-windows)))
       (group-ids (window-group-ids))
       menus)
    (setq group-ids (cons group-id (delq group-id group-ids)))
    (setq menus (mapcar (lambda (id)
			  (let
			      ((name (if (symbolp id)
					 (symbol-name id)
				       (cdr (assq id group-names)))))
			    (when (> (length name) 20)
			      (setq name (concat
					  (substring name 0 20) "...")))
			    (when (eq id group-id)
			      (setq name (concat name " *")))
			    (list name
				  `(add-window-to-group
				    (get-window-by-id
				     ,(window-id w)) ',id))))
			group-ids))
    (rplacd menus (cons '() (cdr menus)))
    `(,@menus
      ()
      (,(_ "New group") (add-window-to-new-group
			 (get-window-by-id ,(window-id w)))))))


;; session management -- only save group-ids that are _symbols_

(defun group-saved-state (w)
  (let
      ((group-id (window-get w 'group)))
    (when (and group-id (symbolp group-id))
      `((group . ,group-id)))))

(defun group-load-state (w alist)
  (let
      ((group-id (cdr (assq 'group alist))))
    (when group-id
      (add-window-to-group w group-id))))

(add-hook 'sm-window-save-functions group-saved-state)
(add-hook 'sm-restore-window-hook group-load-state)
