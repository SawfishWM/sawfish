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

(defcustom transients-are-group-members t
  "Transient windows are in the same group as their parent."
  :type boolean
  :group misc)

(defun window-real-group-id (w)
  (or (window-get w 'group)
      (window-group-id w)
      (window-id w)))

;; return the list of windows in the group with id GROUP-ID
(defun windows-by-group (group-id)
  (let*
      ((windows (managed-windows))
       (group (filter  #'(lambda (x)
			   (eq (window-real-group-id x) group-id))
		       windows)))
    (when transients-are-group-members
      (mapc #'(lambda (w)
		(when (window-transient-p w)
		  (let
		      ((parent (get-window-by-id
				(window-transient-p w) windows)))
		    (when (and parent (memq parent group) (not (memq w group)))
		      (setq group (cons w group))))))
	    windows))
    group))

;; return the list of windows in the same group as window W
(defun windows-in-group (w)
  (let
      (group-id tem)
    (setq group-id (window-real-group-id w))
    (when (and (eq group-id (window-id w))
	       transients-are-group-members
	       (window-transient-p w)
	       (setq tem (get-window-by-id (window-transient-p w))))
      (setq group-id (window-real-group-id w)))
    (windows-by-group group-id)))

;; map FUN over all windows in the same group as window W
(defun map-window-group (fun w)
  (mapc fun (windows-in-group w)))

;; return list of all group ids
(defun window-group-ids ()
  (let
      (ids id)
    (mapc #'(lambda (w)
	      (setq id (window-real-group-id w))
	      (unless (memq id ids)
		(setq ids (cons id ids)))) (managed-windows))
    ids))

;; put window W in group with id GROUP-ID
(defun add-window-to-group (w group-id)
  (window-put w 'group group-id))

;; returns the id of the new group
(defun add-window-to-new-group (w)
  (let
      ((ids (window-group-ids))
       (i -1))
    (while (memq i ids)
      (setq i (1- i)))
    (window-put w 'group i)))


;; menu constructor

(defun window-group-menu (w)
  (let
      ((group-id (window-real-group-id w))
       (group-names (mapcar #'(lambda (x)
				(cons (window-real-group-id x)
				      (window-name x)))
			    (managed-windows)))
       (group-ids (window-group-ids))
       menus)
    (setq group-ids (cons group-id (delq group-id group-ids)))
    (setq menus (mapcar #'(lambda (id)
			    (let
				((name (cdr (assq id group-names))))
			      (when (> (length name) 20)
				(setq name (concat
					    (substring name 0 20) "...")))
			      (when (eq id group-id)
				(setq name (concat name " *")))
			      (list name
				    `(lambda ()
				       (add-window-to-group
					(get-window-by-id
					 ,(window-id w)) ',id)))))
			group-ids))
    (rplacd menus (cons '() (cdr menus)))
    `(,@menus
      ()
      ("New group" (lambda ()
		     (add-window-to-new-group
		      (get-window-by-id ,(window-id w))))))))


;; initialisation

(sm-add-saved-properties 'group)
