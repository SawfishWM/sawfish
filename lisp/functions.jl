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
  :group (focus advanced))

(defvar dont-avoid-ignored t)
(defvar avoid-by-default nil)

(defvar xterm-program "xterm")
(defvar xterm-args nil)

;; return a window called NAME, or nil
(defun get-window-by-name (name &optional lst)
  (catch 'foo
    (mapc (lambda (w)
	    (when (string= (window-name w) name)
	      (throw 'foo w))) (or lst (managed-windows)))
    nil))

;; map FUN over all managed windows
(defmacro map-windows (fun)
  `(mapc ,fun (managed-windows)))

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
  (and (not (window-get w 'never-focus))
       (or ignore-window-input-hint
	   (window-get w 'ignore-window-input-hint)
	   (window-wants-input-p w))))

;; remove all duplicates from list, tests using eq, order is lost
(defun uniquify-list (lst)
  (let
      (out)
    (mapc (lambda (x)
	    (unless (memq x out)
	      (setq out (cons x out)))) lst)
    out))

;; Move the mouse pointer to position (X, Y) relative to the client
;; window associated with object WINDOW.
;; If X and Y are nil, then they are taken as the top-left corner of
;; the window frame.
(defun warp-cursor-to-window (w &optional x y)
  (let
      ((coords (window-position w))
       (foff (window-frame-offset w)))
    (unless x
      (setq x -1))
    (unless y
      (setq y -1))
    (warp-cursor (+ x (car coords) (- (car foff)))
		 (+ y (cdr coords) (- (cdr foff))))))

(defun resize-window-with-hints (w cols rows &optional hints)
  (unless hints
    (setq hints (window-size-hints w)))
  (let
      ((x-base (or (cdr (or (assq 'base-width hints)
			    (assq 'min-width hints))) 1))
       (x-inc (or (cdr (assq 'width-inc hints)) 1))
       (y-base (or (cdr (or (assq 'base-height hints)
			    (assq 'min-height hints))) 1))
       (y-inc (or (cdr (assq 'height-inc hints)) 1))
       (x-max (cdr (assq 'max-width hints)))
       (y-max (cdr (assq 'max-height hints)))
       (scale (lambda (x base inc maximum)
		(min (+ base (* inc (max 0 x))) (or maximum 65535)))))
    (resize-window-to w (scale cols x-base x-inc x-max)
		      (scale rows y-base y-inc y-max))))

(defun get-window-wm-protocols (w)
  (let*
      ((prop (get-x-property w 'WM_PROTOCOLS))
       (data (and prop (eq (car prop) 'ATOM) (nth 2 prop))))
    (when data
      (let ((i 0)
	    out)
	(while (< i (length data))
	  (setq out (cons (aref data i) out))
	  (setq i (1+ i)))
	(nreverse out)))))

(defun delete-window (w &optional safely)
  "Delete the window."
  (interactive "%W")
  (cond
   ((memq 'WM_DELETE_WINDOW (get-window-wm-protocols w))
    (send-client-message w 'WM_PROTOCOLS (vector (x-atom 'WM_DELETE_WINDOW)
						 (x-server-timestamp)) 32))
   (safely
    (beep))
   (t
    (x-kill-client w))))

(defun delete-window-safely (w)
  "Delete the window, or beep if the window can't be closed safely."
  (interactive "%W")
  (delete-window w t))


;; property and window-state changed interface

(let
    (prop-changes)

  ;; PROP may be single X property name, or list of names
  (defun call-after-property-changed (prop fun)
    (setq prop-changes (cons (cons (if (listp prop)
				       prop
				     (list prop)) fun) prop-changes)))
  
  (add-hook 'property-notify-hook
	    (lambda (w prop state)
	      (mapc (lambda (cell)
                      (when (memq prop (car cell))
			((cdr cell) w prop state)))
		    prop-changes))))

(let
    (state-changes)
  
  (defun call-after-state-changed (states fun)
    (setq state-changes (cons (cons states fun) state-changes)))
  
  (add-hook
   'window-state-change-hook
   (lambda (w states)
     (mapc (lambda (cell)
	     (let
		  ((relevant (filter (lambda (state)
					(memq state (car cell))) states)))
	       (when relevant
		 ((cdr cell) w relevant))))
	    state-changes))))


;; avoided (i.e. non-overlapped) windows

(defun window-avoided-p (w)
  (cond ((or (not (window-mapped-p w))
	     (not (window-visible-p w)))
	 nil)
	((window-get w 'avoid)
	 t)
	((and dont-avoid-ignored (window-get w 'ignored))
	 nil)
	(t
	 avoid-by-default)))

(defun avoided-windows (&optional window)
  (delete-if (lambda (w)
	       (or (eq w window) (not (window-avoided-p w))))
	     (managed-windows)))
