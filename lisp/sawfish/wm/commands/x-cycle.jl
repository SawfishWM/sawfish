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

(require 'window-order)
(eval-when-compile (require 'auto-raise))	;we bind to disable-auto-raise
(eval-when-compile (require 'tooltips))		;we bind to tooltips-enabled
(provide 'x-cycle)

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

(defgroup cycle "Window Cycling" :group focus)

(defcustom cycle-show-window-names t
  "Display window names while cycling through windows."
  :group (focus cycle)
  :type boolean)

(defcustom cycle-include-iconified t
  "Include iconified windows when cycling."
  :group (focus cycle)
  :type boolean)

(defcustom cycle-all-workspaces nil
  "Include windows on all workspaces when cycling."
  :group (focus cycle)
  :type boolean)

(defcustom cycle-all-viewports nil
  "Include windows on all viewports when cycling."
  :group (focus cycle)
  :type boolean)

(defcustom cycle-raise-windows t
  "Raise windows while they're temporarily selected during cycling."
  :group (focus cycle)
  :type boolean)

(defcustom cycle-warp-pointer t
  "Warp the mouse pointer to windows as they're temporarily selected."
  :group (focus cycle)
  :type boolean)

(defcustom cycle-focus-windows t
  "Focus windows when they're temporarily selected during cycling."
  :group (focus cycle)
  :type boolean)

(defcustom cycle-disable-auto-raise nil
  "Disable auto-raising while temporarily selecting windows."
  :group (focus cycle)
  :type boolean)


;; variables

(defvar x-cycle-current nil)
(defvar x-cycle-stacking nil)
(defvar x-cycle-windows t)

;; associate modifier names with their keys
(defvar x-cycle-modifier-alist `(("A" ,@alt-keysyms)
				 ("M" ,@meta-keysyms)
				 ("H" ,@hyper-keysyms)
				 ("S" "Shift_L" "Shift_R")
				 ("C" "Control_L" "Control_R")))


;; code

;;;###autoload
(defun cycle-windows (event)
  "Cycle through all windows in order of recent selections."
  (interactive "e")
  (let*
      ((name (event-name event))
       (eval-modifier-events t)
       (eval-key-release-events t)
       (override-keymap (make-keymap))
       (focus-dont-push t)
       (disable-auto-raise cycle-disable-auto-raise)
       (tooltips-enabled nil)
       (x-cycle-current nil)
       (x-cycle-stacking nil)
       modifier tem)

    ;; First of all, use the name of the event that invoked us to
    ;; contruct the keymap we'll use
    (bind-keys override-keymap name 'x-cycle-next)
    (unless (and (string-match "(.*)-.+" name)
		 (setq modifier (expand-last-match "\\1"))
		 (setq tem (cdr (assoc modifier x-cycle-modifier-alist))))
      (error "%s must be bound to a singly-modified event" this-command))
    (mapc (lambda (k)
	    (bind-keys override-keymap
	      (concat "Any-Release-" k) 'x-cycle-exit)) tem)

    (when (grab-keyboard (input-focus))
      (unwind-protect
	  (progn
	    (catch 'x-cycle-exit
	      ;; do the first step
	      (x-cycle-next)
	      (recursive-edit))
	    (when x-cycle-current
	      (display-window x-cycle-current)))
	(display-message nil)
	(ungrab-keyboard)))))

;;;###autoload
(defun cycle-group (event w)
  (interactive "e\n%W")
  (let
      ((x-cycle-windows (windows-in-group w)))
    (cycle-windows event)))

(defun x-cycle-next ()
  (interactive)
  (let
      ((win (window-order (if cycle-all-workspaces
			      nil
			    current-workspace)
			  cycle-include-iconified cycle-all-viewports)))
    (unless (eq x-cycle-windows t)
      (setq win (delete-if (lambda (w)
			     (not (memq w x-cycle-windows))) win)))
    (setq win (delete-if (lambda (w)
			   (window-get w 'never-focus)) win))
    (unless win
      (throw 'x-cycle-exit t))
    (if x-cycle-current
	(when (or (window-get x-cycle-current 'iconified)
		  (not (window-appears-in-workspace-p
			x-cycle-current current-workspace)))
	  (hide-window x-cycle-current))
      ;; first call, push the currently focused window onto
      ;; the top of the stack
      (when (input-focus)
	(setq x-cycle-current (input-focus))
	(window-order-push x-cycle-current)
	(setq win (cons x-cycle-current (delq x-cycle-current win)))))
    (when x-cycle-stacking
      (restack-windows x-cycle-stacking)
      (setq x-cycle-stacking nil))
    (when x-cycle-current
      (setq win (or (cdr (memq x-cycle-current win)) win)))
    (setq win (car win))
    (setq x-cycle-current win)
    (when (not (window-get win 'sticky))
      (select-workspace (nearest-workspace-with-window win current-workspace)))
    (move-viewport-to-window win)
    (when (window-get win 'iconified)
      (show-window win))
    (when cycle-raise-windows
      (setq x-cycle-stacking (stacking-order))
      (raise-window win))
    (when cycle-warp-pointer
      (warp-cursor-to-window win))
    (when cycle-show-window-names
      (display-message (concat (and (window-get win 'iconified) ?[)
			       (window-name win)
			       (and (window-get win 'iconified) ?]))))
    (when (and cycle-focus-windows (window-really-wants-input-p win))
      (set-input-focus win))))

(defun x-cycle-exit ()
  (interactive)
  (throw 'x-cycle-exit t))
