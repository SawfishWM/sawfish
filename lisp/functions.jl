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

(defvar xterm-program "xterm")
(defvar xterm-args nil)

;; return a window called NAME, or nil
(defun get-window-by-name (name &optional list)
  (catch 'foo
    (mapc #'(lambda (w)
	      (when (string= (window-name w) name)
		(throw 'foo w))) (or list (managed-windows)))
    nil))

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


;; property changed interface

(let
    (prop-changes)

  (defun call-after-property-changed (prop fun)
    (or (closurep fun) (error "Non-closure to call-after-property-changed"))
    (setq prop-changes (cons (cons prop fun) prop-changes)))

  (add-hook 'property-notify-hook
	    #'(lambda (w atom state)
		(mapc #'(lambda (cell)
			  (when (eq (car cell) atom)
			    (funcall (cdr cell) w atom state)))
		      prop-changes))))


;; avoided (i.e. non-overlapped) windows

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

