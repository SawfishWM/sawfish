;; functions.jl -- miscellaneous stuff
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

(provide 'functions)

(defcustom ignore-window-input-hint t
  "Give focus to windows even when they haven't asked for it."
  :type boolean
  :group focus)

(defcustom avoided-windows-re nil
  "Regular expression matching windows to avoid overlapping."
  :group misc
  :type string
  :allow-nil t)

(defvar dont-avoid-ignored t)
(defvar avoid-by-default nil)

(defcustom transients-are-group-members t
  "Transient windows are in the same group as their parent."
  :type boolean
  :group misc)

(defvar xterm-program "xterm")
(defvar xterm-args nil)

;; return a window called NAME, or nil
(defun get-window-by-name (name &optional list)
  (catch 'foo
    (mapc #'(lambda (w)
	      (when (string= (window-name w) name)
		(throw 'foo w))) (or list (managed-windows)))
    nil))

;; return the window with id ID, or nil
(defun get-window-by-id (id &optional list)
  (catch 'foo
    (mapc #'(lambda (w)
	      (when (= (window-id w) id)
		(throw 'foo w))) (or list (managed-windows)))))

;; execute FORMS with the server grabbed
(defmacro with-server-grabbed (&rest forms)
  `(progn
     (grab-server)
     (unwind-protect
	 (progn ,@forms)
       (ungrab-server))))

;; execute FORMS, then reinstall the original stacking order
(defmacro save-stacking-order (&rest forms)
  (let
      ((tem (gensym)))
    `(let
	 ((,tem (stacking-order)))
       (unwind-protect
	   (progn ,@forms)
	 (restack-windows ,tem)))))

;; try to create dir and all nonexistent parent dirs (like mkdir -p)
(defun make-directory-recursively (dir)
  (while (not (file-exists-p dir))
    (let
	((tem dir))
      (while (not (file-exists-p (expand-file-name ".." tem)))
	(setq tem (expand-file-name ".." tem)))
      (make-directory tem))))

(defun xterm ()
  "Start a new xterm."
  (interactive)
  (system (format nil "%s %s >/dev/null 2>&1 </dev/null &"
		  xterm-program (or xterm-args ""))))

(defun window-really-wants-input-p (w)
  (or ignore-window-input-hint
      (window-get w 'ignore-window-input-hint)
      (window-wants-input-p w)))

;; remove all duplicates from list, tests using eq, order is lost
(defun uniquify-list (list)
  (let
      (out)
    (mapc #'(lambda (x)
	      (unless (memq x out)
		(setq out (cons x out)))) list)
    out))

(defun window-avoided-p (w)
  (cond ((or (not (window-mapped-p w))
	     (not (window-visible-p w)))
	 nil)
	((or (window-get w 'avoid)
	     (and avoided-windows-re
		  (string-match avoided-windows-re (window-name w))))
	 t)
	((and dont-avoid-ignored (window-get w 'ignored))
	 nil)
	(t
	 avoid-by-default)))

(defun avoided-windows (&optional window)
  (delete-if #'(lambda (w)
		 (or (eq w window) (not (window-avoided-p w))))
	     (managed-windows)))

(defun windows-in-group (w)
  (let
      ((windows (managed-windows))
       group-id group tem)
    (setq group-id (or (window-group-id w)
		       (and transients-are-group-members
			    (window-transient-p w)
			    (setq tem (get-window-by-id
				       (window-transient-p w) windows))
			    (or (window-group-id tem) (window-id tem)))
		       (window-id w)))
    (setq group (filter  #'(lambda (x)
			     (eq (or (window-group-id x)
				     (window-id x)) group-id)) 
				 windows))
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

(defun map-window-group (fun w)
  (mapc fun (windows-in-group w)))
