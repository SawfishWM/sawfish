;; beos-window-menu.jl -- hack to change window-menu to approximate BeOS

;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.ext.beos-window-menu

    (export beos-window-menu)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.workspace
	  sawfish.wm.util.groups
	  sawfish.wm.util.display-window
	  sawfish.wm.menus)

  (define-structure-alias beos-window-menu sawfish.wm.ext.beos-window-menu)

  (defcustom beos-window-menu-simplifies t
    "The hierarchical window menu raises singleton submenus."
    :type boolean
    :group misc
    :user-level expert)

  (define (abbreviate name #!optional len)
    (unless len (setq len 20))
    (if (> (length name) len)
	(concat (substring name 0 len) "...")
      name))

  (define (make-label w)
    (let ((name (window-name w)))
      (quote-menu-item (concat (cond ((window-get w 'iconified) ?\[)
				     ((not (window-appears-in-workspace-p
					    w current-workspace)) ?\())
			       (abbreviate name 48)
			       (cond ((window-get w 'iconified)  ?\])
				     ((not (window-appears-in-workspace-p
					    w current-workspace)) ?\)))))))

  (define (make-item w)
    (list (make-label w)
	  (lambda ()
	    (when (windowp w)
	      (display-window w)))
	  (cons 'check (and (eq (input-focus) w)))
	  '(group . beos-window-menu)))

  (define (group-name id)
    (cond ((symbolp id) (symbol-name id))
	  ((integerp id)
	   (let ((name (or (and (> id 0) (window-class id))
			   (and (> id 0) (nth 2 (get-x-property id 'WM_NAME)))
			   (window-class (car (windows-by-group id)))
			   "Unnamed")))
	     (abbreviate name)))))

  (define (make-group-item id)
    (let loop ((menu '())
	       (windows (windows-by-group id)))
      (if (null windows)
	  (cons (group-name id) (nreverse menu))
	(if (and (window-mapped-p (car windows))
		 (not (window-get (car windows) 'window-list-skip))
		 (not (window-get (car windows) 'ignored)))
	    (loop (cons (make-item (car windows)) menu) (cdr windows))
	  (loop menu (cdr windows))))))

  (define (delete-group-windows id lst)
    (let ((windows (windows-by-group id)))
      (delete-if (lambda (x)
		   (memq x windows)) lst)))

  (define (cleanup-menu menu)
    ;; sort the group names..
    (setq menu (sort menu (lambda (x y)
			    (not (string-lessp (car x) (car y))))))
    ;; ..then merge any identically named sub-menus
    (let loop ((rest menu))
      (when (cdr rest)
	(if (string= (caar rest) (caadr rest))
	    (progn
	      (rplacd (car rest) (nconc (cdar rest) (cdadr rest)))
	      (rplacd rest (cddr rest))
	      (loop rest))
	  (loop (cdr rest)))))
    menu)

  (define (make-menu)
    (let loop-1 ((menu '())
		 (groups (window-group-ids))
		 (windows (managed-windows)))
      (if (null groups)
	  (progn
	    (setq menu (cleanup-menu menu))
	    (if windows
		(let loop-2 ((menu (cons '() menu))
			     (rest windows))
		  (if (null rest)
		      menu
		    (loop-2 (cons (make-item (car rest)) menu) (cdr rest))))
	      menu))
	(loop-1 (let ((item (make-group-item (car groups))))
		  (if (cdr item)
		      (cons item menu)
		    menu))
		(cdr groups)
		(delete-group-windows (car groups) windows)))))

  (define (simplify menu)
    (let loop ((rest menu)
	       (out '()))
      (if (null rest)
	  out
	(let ((next (cdr rest)))
	  ;; reverse the pointers
	  (rplacd rest out)
	  (when (= (length (cdar rest)) 1)
	    (rplaca rest (cadar rest)))
	  (loop next rest)))))

  (define (beos-window-menu)
    (if beos-window-menu-simplifies
	(simplify (make-menu))
      (nreverse (make-menu))))

  (setq window-menu beos-window-menu))
