;; x-cycle.jl -- ``intelligent'' window cycling
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

;; each window may have an x-cycle-order property, an integer id
;; defining its position in the window stack, higher numbers equal
;; higher up the display. The ids may not be contiguous

;; window order high-water-mark
(defvar x-cycle-highest 1)

(defvar x-cycle-current nil)

;; associate modifier names with their keys
(defvar x-modifier-alist '(("A" "Alt_L" "Alt_R")
			   ("M" "Meta_L" "Meta_R")
			   ("S" "Shift_L" "Shift_R")
			   ("C" "Control_L" "Control_R")))

(defun x-cycle-order (&optional workspace allow-iconified)
  (let
      ((windows (managed-windows)))
    (setq windows (delete-if #'(lambda (w)
				 (or (not (window-mapped-p w))
				     (and (not allow-iconified)
					  (window-get w 'iconified))
				     (and workspace
					  (window-get w 'workspace)
					  (not (equal (window-get w 'workspace)
						      workspace)))))
			     windows))
    (nreverse (sort windows #'(lambda (x y)
				(setq x (window-get x 'x-cycle-order))
				(setq y (window-get y 'x-cycle-order))
				(cond ((and x y)
				       (< x y))
				      (x nil)
				      (y t)
				      (t nil)))))))

;;;###autoload
(defun x-cycle-entry (event)
  (interactive "e")
  (let*
      ((event-name (event-name event))
       (eval-modifier-events t)
       (eval-key-release-events t)
       (override-keymap (make-keymap))
       (x-cycle-current nil)
       mod tem)

    ;; First of all, use the name of the event that invoked us to
    ;; contruct the keymap we'll use
    (bind-keys override-keymap event-name 'x-cycle-next)
    (unless (and (string-match "(.*)-.+" event-name)
		 (setq mod (expand-last-match "\\1"))
		 (setq tem (assoc mod x-modifier-alist)))
      (error "%s must be bound to a modified event" this-command))
    (mapc #'(lambda (k)
	      (bind-keys override-keymap
		(concat mod "-Release-" k) 'x-cycle-exit)) (cdr tem))

    (when (grab-keyboard)
      (unwind-protect
	  (progn
	    ;; do the first step
	    (x-cycle-next)
	    (catch 'x-cycle-exit
	      (recursive-edit))
	    (when x-cycle-current
	      (window-put x-cycle-current 'x-cycle-order x-cycle-highest)
	      (setq x-cycle-highest (1+ x-cycle-highest))
	      (display-window x-cycle-current)))
	(show-message nil)
	(ungrab-keyboard)))))

(defun x-cycle-next ()
  (interactive)
  (let
      ((win (x-cycle-order current-workspace)))
    (when win
      (unless x-cycle-current
	(setq x-cycle-current (input-focus)))
      (when x-cycle-current
	(setq win (or (cdr (memq x-cycle-current win)) win)))
      (setq win (car win))
      (setq x-cycle-current win)
      (show-message (window-name win))
      (set-input-focus win))))

(defun x-cycle-exit ()
  (interactive)
  (throw 'x-cycle-exit t))
