;; stacking.jl -- window stacking
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

(provide 'stacking)

;; Commentary:

;; Each window will have a `depth' property--an integer, zero
;; represents the level of normal windows, negative for windows below
;; this level, and positive for windows above the normal level

(defcustom transients-above 'all
  "Keep transient windows stacked above."
  :group misc
  :type (set all parents none))

;; minimum depth of transient windows
(defvar transient-depth 2)

(defvar auto-depth-alist nil
  "A list of (REGEXP . DEPTH) matching window names to the depth of the
stacking level to place them in.")

;; Resort the stacking order to ensure that the windows' depth attributes
;; are adhered to. No change is made to windows in the same depth
(defun restack-by-depth ()
  (let*
      ((old-order (stacking-order))
       (new-order (sort (copy-sequence old-order)
			#'(lambda (x y)
			    (> (window-get x 'depth) (window-get y 'depth))))))
    ;; try to optimise..
    (unless (equal old-order new-order)
      ;; look for common prefix to both lists
      (when (eq (car old-order) (car new-order))
	(while (and (cdr old-order) (cdr new-order)
		    (eq (car (cdr old-order)) (car (cdr new-order))))
	  (setq old-order (cdr old-order))
	  (setq new-order (cdr new-order))))
      (restack-windows new-order))))

(defun stacking-order-by-depth (depth)
  (let
      ((order (stacking-order)))
    (delete-if #'(lambda (x)
		   (/= (window-get x 'depth) depth)) order)))

;; Set the stacking depth of W to DEPTH
(defun set-window-depth (w depth)
  (let
      ((old (window-get w 'depth)))
    (window-put w 'depth depth)
    (restack-by-depth)
    (call-window-hook 'window-depth-change-hook w (list depth))
    (call-window-hook 'window-state-change-hook w)))

;; Called from the add-window-hook
(defun stacking-add-window (w)
  (let
      ((depth (window-get w 'depth)))
    (unless depth
      (setq depth (or (cdr (assoc-regexp (window-name w) auto-depth-alist)) 0))
      (window-put w 'depth depth))))

;; Return t if W is at the top of its level
(defun window-on-top-p (w)
  (or (eq (window-visibility w) 'unobscured)
      (let*
	  ((depth (window-get w 'depth))
	   (space (or (window-get w 'workspace) current-workspace))
	   (order (delete-if
		   #'(lambda (x)
		       (or (/= (window-get x 'depth) depth)
			   (and (window-get x 'workspace)
				(not (eq (window-get x 'workspace) space)))))
		   (stacking-order))))
	(eq (car order) w))))

;; Change stacking of window BELOW so that it is immediately below
;; window ABOVE
(defun stack-window-below (below above)
  (when (= (window-get above 'depth) (window-get below 'depth))
    (restack-windows (list above below))))

;; Change stacking of window ABOVE so that it is immediately above
;; window BELOW
(defun stack-window-above (above below)
  (when (= (window-get above 'depth) (window-get below 'depth))
    (let
	((order (stacking-order)))
      (if (eq (car order) below)
	  (x-raise-window above)
	(setq order (delete-if #'(lambda (x)
				   (/= (window-get x 'depth)
				       (window-get above 'depth))) order))
	(while (and (cdr order) (not (eq (car (cdr order)) below)))
	  (setq order (cdr order)))
	(restack-windows (list (car order) above))))))

(defun stacking-after-map (w)
  ;; if a transient window, maybe put it in a higher layer
  (if (window-transient-p w)
      (let
	  ((parent (get-window-by-id (window-transient-p w))))
	(if (or (eq transients-above 'all)
		(and (eq transients-above 'parents) parent))
	    (set-window-depth w (if parent
				    (max (1+ (window-get parent 'depth))
					 transient-depth)
				  transient-depth))
	  (restack-by-depth)))
    (restack-by-depth))
  ;; raise the window within its layer
  (raise-window w))


;; Commands

(defun lower-window (w)
  "Lower the window to the bottom of its stacking level."
  (interactive "%W")
  (let
      ((order (stacking-order))
       (depth (window-get w 'depth))
       tem)
    (setq tem (memq w order))
    (while (and (cdr tem) (= (window-get (car (cdr tem)) 'depth) depth))
      (rplaca tem (car (cdr tem)))
      (rplaca (cdr tem) w)
      (setq tem (cdr tem)))
    (restack-windows order)))

(defun raise-window (w)
  "Raise the window to the top of its stacking level."
  (interactive "%W")
  (let
      ((order (stacking-order))
       (depth (window-get w 'depth))
       tem)
    (setq order (delq w order))
    (cond ((null order))		;no other windows
	  ((<= (window-get (car order) 'depth) depth)
	   ;; no windows above DEPTH, so just raise to the top
	   ;; of the global stack
	   (x-raise-window w))
	  (t
	   (setq tem order)
	   (while (and (cdr tem) (> (window-get (car (cdr tem)) 'depth) depth))
	     (setq tem (cdr tem)))
	   (rplacd tem (cons w (cdr tem)))
	   (restack-windows order)))))

(defun raise-lower-window (w)
  "If the window is the highest window in its stacking level, lower it to the
bottom of this level, otherwise raise it to the top of its level."
  (interactive "%W")
  (if (window-on-top-p w)
      (lower-window w)
    (raise-window w)))

(defun lower-window-depth (w)
  "Put the window in the stacking level beneath its current level."
  (interactive "%W")
  (set-window-depth w (1- (window-get w 'depth))))

(defun raise-window-depth (w)
  "Put the window in the stacking level above its current level."
  (interactive "%W")
  (set-window-depth w (1+ (window-get w 'depth))))

(add-hook 'after-initialization-hook 'restack-by-depth)
(add-hook 'add-window-hook 'stacking-add-window t)
(add-hook 'map-notify-hook 'stacking-after-map t)
(sm-add-saved-properties 'depth)
