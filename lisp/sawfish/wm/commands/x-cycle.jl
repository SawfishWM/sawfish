;; x-cycle.jl -- stack-based window cycling
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

;; Cycles through windows in MRU order. Whichever key is used to invoke
;; `cycle-windows' will continue to the next window on the stack when
;; pressed again. Releasing the initial modifier ends the cycling and
;; selects the current window

;; Thanks to Kuba Winnicki <blackwine@optimus.wroc.pl> for the idea:

;; [ assumes command invoked by MOD-KEY, i.e. M-TAB by default ]

;; MOD held;

;; * window 1            window 1            window 1          * window 3
;;   window 2    ==>   * window 2    ==>     window 2    ==>     window 1
;;   window 3    KEY     window 3    KEY   * window 3  release   window 2
;;   window 4            window 4            window 4    MOD     window 4

;; If cycle-raise-windows is enabled, focused window is brought to
;; front, but gets back to original placement in order when it's
;; defocused.

;; Also it'd be nice to be able to cycle through hidden windows as
;; well, like here:

;; MOD held;

;; * window 1            window 1            window 1          * window 3
;;  [window 2]   ==>   * window 2    ==>    [window 2]   ==>     window 1
;;  [window 3]   KEY    [window 3]   KEY   * window 3  release  [window 2]
;;   window 4            window 4            window 4    MOD     window 4

;; Each window may have an x-cycle-order property, an integer id
;; defining its position in the window stack, higher numbers equal
;; more recently selected. The ids may not be contiguous

;; Obviously there would be a problem when we overflow rep's integers, but
;; every now and then we compress the stack to make the ids contiguous

;; It might seem as though it should be possible to use the actual
;; window stacking to define MRU order. But since there are multiple
;; layers of windows this wouldn't work (selected windows may not reach
;; the top of the stack)


;; customization options

(defgroup cycle "Window cycling")

(defcustom cycle-show-window-names t
  "Display window names while cycling through windows."
  :group cycle
  :type boolean)

(defcustom cycle-include-iconified t
  "Include iconified windows when cycling."
  :group cycle
  :type boolean)

(defcustom cycle-all-workspaces nil
  "Include windows on all workspaces when cycling."
  :group cycle
  :type boolean)

(defcustom cycle-raise-windows t
  "Raise windows while they're temporarily selected during cycling."
  :group cycle
  :type boolean)


;; variables

;; window order high-water-mark
(defvar x-cycle-highest 1)

(defvar x-cycle-current nil)
(defvar x-cycle-stacking nil)

;; associate modifier names with their keys
(defvar x-cycle-modifier-alist `(("A" ,@alt-keysyms)
				 ("M" ,@meta-keysyms)
				 ("S" "Shift_L" "Shift_R")
				 ("C" "Control_L" "Control_R")))


;; code

;;;###autoload
(defun cycle-windows (event)
  "Cycle through all windows in order of recent selections."
  (interactive "e")
  (let*
      ((event-name (event-name event))
       (eval-modifier-events t)
       (eval-key-release-events t)
       (override-keymap (make-keymap))
       (x-cycle-current nil)
       (x-cycle-stacking nil)
       mod tem)

    ;; First of all, use the name of the event that invoked us to
    ;; contruct the keymap we'll use
    (bind-keys override-keymap event-name 'x-cycle-next)
    (unless (and (string-match "(.*)-.+" event-name)
		 (setq mod (expand-last-match "\\1"))
		 (setq tem (cdr (assoc mod x-cycle-modifier-alist))))
      (error "%s must be bound to a singly-modified event" this-command))
    (mapc #'(lambda (k)
	      (bind-keys override-keymap
		(concat mod "-Release-" k) 'x-cycle-exit)) tem)

    (when (grab-keyboard)
      (unwind-protect
	  (progn
	    ;; do the first step
	    (x-cycle-next)
	    (catch 'x-cycle-exit
	      (recursive-edit))
	    (when x-cycle-current
	      (x-cycle-push x-cycle-current)
	      (display-window x-cycle-current)))
	(show-message nil)
	(ungrab-keyboard)))))

(defun x-cycle-next ()
  (interactive)
  (let
      ((win (x-cycle-order (if cycle-all-workspaces
			       nil
			     current-workspace)
			   cycle-include-iconified)))
    (when win
      (if x-cycle-current
	  (when (or (window-get x-cycle-current 'iconified)
		    (and (window-get x-cycle-current 'workspace)
			 (not (equal (window-get x-cycle-current 'workspace)
				     current-workspace))))
	    (hide-window x-cycle-current))
	;; first call, push the currently focused window onto
	;; the top of the stack
	(when (input-focus)
	  (setq x-cycle-current (input-focus))
	  (x-cycle-push x-cycle-current)
	  (setq win (cons x-cycle-current (delq x-cycle-current win)))))
      (when x-cycle-stacking
	(restack-windows x-cycle-stacking)
	(setq x-cycle-stacking nil))
      (when x-cycle-current
	(setq win (or (cdr (memq x-cycle-current win)) win)))
      (setq win (car win))
      (setq x-cycle-current win)
      (when (window-get win 'workspace)
	(select-workspace (window-get win 'workspace)))
      (when (window-get win 'iconified)
	(show-window win))
      (when cycle-raise-windows
	(setq x-cycle-stacking (stacking-order))
	(raise-window win))
      (when warp-to-selected-windows
	(warp-cursor-to-window win))
      (when cycle-show-window-names
	(show-message (concat (and (window-get win 'iconified) ?[)
			      (window-name win)
			      (and (window-get win 'iconified) ?]))))
      (when (window-wants-input-p win)
	(set-input-focus win)))))

(defun x-cycle-exit ()
  (interactive)
  (throw 'x-cycle-exit t))

;; return windows to cycle through in MRU order
(defun x-cycle-order (&optional workspace allow-iconified)
  (let
      ((windows (managed-windows)))
    (setq windows (delete-if #'(lambda (w)
				 (or (not (window-mapped-p w))
				     (window-get w 'ignored)
				     (and (not allow-iconified)
					  (window-get w 'iconified))
				     (and workspace
					  (window-get w 'workspace)
					  (not (equal (window-get w 'workspace)
						      workspace)))))
			     windows))
    (sort windows #'(lambda (x y)
		      (setq x (window-get x 'x-cycle-order))
		      (setq y (window-get y 'x-cycle-order))
		      (cond ((and x y)
			     (> x y))
			    (x t)
			    (t nil))))))

;; push window W onto the top of the cycle stack
(defun x-cycle-push (w)
  (window-put w 'x-cycle-order x-cycle-highest)
  (setq x-cycle-highest (1+ x-cycle-highest))
  (when (> x-cycle-highest 1000000)			;arbitrary big number
    (x-cycle-compress)))

;; remove window W from the cycle stack
(defun x-cycle-pop (w)
  (window-put w 'x-cycle-order nil))

;; compress the cycle stack
(defun x-cycle-compress ()
  (let
      ((order (nreverse (x-cycle-order nil t)))		;all windows
       (i 1))
    (mapc #'(lambda (w)
	      (when (window-get w 'x-cycle-order)
		(window-put w 'x-cycle-order i)
		(setq i (1+ i)))) order)
    (setq x-cycle-highest i)))

(add-hook 'iconify-window-hook 'x-cycle-pop)
