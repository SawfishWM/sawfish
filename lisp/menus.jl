;; menus.jl -- popup menus
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

(provide 'menus)

;; Suppress annoying compiler warnings
(eval-when-compile (require 'timers))

(defvar menu-program (expand-file-name "sawmill-menu" sawmill-exec-directory)
  "Location of the program implementing sawmill's menu interface.")

(defvar menu-program-stays-running 60
  "When non-nil, the user-interface program is never stopped. If a number,
then this is taken as the number of seconds to let the process hang around
unused before killing it.")

;; the active user interface process
(defvar menu-process nil)

;; output from the user-interface process that's received but not
;; yet processed
(defvar menu-pending nil)

;; non-nil when we're waiting for a response from the ui process
;; if a window, then it's the window that received the event causing
;; the menu to be shown
(defvar menu-active nil)

;; if menu-program-stays-running is a number, this may be a timer
;; waiting to kill the process
(defvar menu-timer nil)

(defvar window-ops-menu
  `((,(_ "Iconify") iconify-window)
    (,(_ "Delete") delete-window)
    (,(_ "Destroy") destroy-window)
    (,(_ "Toggle") . window-ops-toggle-menu)
    (,(_ "Maximize")
     (,(_ "Vertically") maximize-window-vertically)
     (,(_ "Horizontally") maximize-window-horizontally)
     (,(_ "Both") maximize-window)
     ()
     (,(_ "Fill vertically") maximize-fill-window-vertically)
     (,(_ "Fill horizontally") maximize-fill-window-horizontally)
     (,(_ "Fill both") maximize-fill-window))
    (,(_ "Un-maximize") unmaximize-window)
    (,(_ "In group") . window-group-menu)
    (,(_ "Send window to")
     (,(_ "Previous workspace") send-to-previous-workspace)
     (,(_ "Next workspace") send-to-next-workspace)
     (,(_ "Copy to previous") copy-to-previous-workspace)
     (,(_ "Copy to next") copy-to-next-workspace)
     ()
     (,(_ "Left") move-window-left)
     (,(_ "Right") move-window-right)
     (,(_ "Up") move-window-up)
     (,(_ "Down") move-window-down))
    (,(_ "Depth")
     (,(_ "Raise") raise-window)
     (,(_ "Lower") lower-window)
     (,(_ "Upper layer") raise-window-depth)
     (,(_ "Lower layer") lower-window-depth))
    (,(_ "Frame type")
     (,(_ "Normal") set-frame:default)
     (,(_ "Title-only") set-frame:shaped)
     (,(_ "Border-only") set-frame:transient)
     (,(_ "Top-border") set-frame:shaped-transient)
     (,(_ "None") set-frame:unframed))
    (,(_ "Frame style") . frame-style-menu)))

(defvar window-ops-toggle-menu
  `((,(_ "Sticky") toggle-window-sticky)
    (,(_ "Shaded") toggle-window-shaded)
    (,(_ "Ignored") toggle-window-ignored)
    (,(_ "Focusable") toggle-window-never-focus)
    ,@(when (featurep 'gnome)
	`((,(_ "In window list") gnome-toggle-skip-winlist)
	  (,(_ "In task list") gnome-toggle-skip-tasklist)))))

(defvar root-menu
  `((,(_ "Windows") . window-menu)
    (,(_ "Workspaces") . workspace-menu)
    (,(_ "Programs") . apps-menu)
    (,(_ "Customize") . custom-menu)
    ()
    (,(_ "Restart") restart)
    (,(_ "Quit") quit)))

(defvar apps-menu
  `(("xterm" (system "xterm &"))
    ("Emacs" (system "emacs &"))
    ("Netscape" (system "netscape &"))
    ("The GIMP" (system "gimp &"))
    ("XFIG" (system "xfig &"))
    ("GV" (system "gv &"))
    ("xcalc" (system "xcalc &"))))

(defun menu-start-process ()
  (when menu-timer
    (delete-timer menu-timer)
    (setq menu-timer nil))
  (unless (and menu-process (process-in-use-p menu-process))
    (when menu-process
      (kill-process menu-process))
    (let
	((menu-sentinel (lambda (process)
			  (when (and menu-process
				     (not (process-in-use-p menu-process)))
			    (setq menu-process nil))
			  (when menu-timer
			    (delete-timer menu-timer)
			    (setq menu-timer nil))))
	 (menu-filter (lambda (output)
			(setq output (concat menu-pending output))
			(setq menu-pending nil)
			(condition-case nil
			    (let
				((result (read-from-string output)))
			      ;; GTK takes the focus for its menu,
			      ;; but later returns it to the original
			      ;; window. We want the focus to be
			      ;; restored by the time the menu-chosen
			      ;; command is invoked..
			      (accept-x-input)
			      (menu-dispatch result))
			  (end-of-stream
			   (setq menu-pending output))))))
      (setq menu-process (make-process menu-filter menu-sentinel)))
    (set-process-error-stream menu-process nil)
    (or (start-process menu-process menu-program)
	(error "Can't start menu backend: %s" menu-program))))

(defun menu-stop-process (&optional force)
  (when menu-process
    (cond ((and (not force) (numberp menu-program-stays-running))
	   ;; number of seconds to let it hang around for
	   (require 'timers)
	   (setq menu-timer (make-timer (lambda ()
					  (kill-process menu-process)
					  (setq menu-process nil)
					  (setq menu-timer nil))
					menu-program-stays-running)))
	  ((or force (not menu-program-stays-running))
	   (kill-process menu-process)
	   (setq menu-process nil)))))

(defun menu-preprocessor (cell)
  (when cell
    (let
	((label (car cell)))
      (cond ((functionp (cdr cell))
	     (setq cell (funcall (cdr cell))))
	    ((and (symbolp (cdr cell)) (not (null (cdr cell))))
	     (setq cell (symbol-value (cdr cell)))
	     (when (functionp cell)
	       (setq cell (funcall cell))))
	    (t
	     (setq cell (cdr cell))))
      (when (and (consp (car cell)) (stringp (car (car cell))))
	(setq cell (mapcar menu-preprocessor cell)))
      (cons label cell))))

(defun menu-dispatch (result)
  (let
      ((orig-win menu-active))
    (menu-stop-process)
    (setq menu-active nil)
    (frame-draw-mutex nil)
    (when result
      (when (windowp orig-win)
	(current-event-window orig-win))
      (cond ((commandp result)
	     (call-command result))
	    ((functionp result)
	     (result))
	    ((consp result)
	     (eval result))
	    (t
	     result)))))

;;;###autoload
(defun popup-menu (spec)
  (or spec (error "No menu given to popup-menu"))
  (if (and menu-active menu-process (process-in-use-p menu-process))
      (error "Menu already active")
    (let*
	((part (clicked-frame-part))
	 (offset (and part (frame-part-position part)))
	 (dims (and part (frame-part-dimensions part))))
      (setq menu-active (or (current-event-window) (input-focus)))
      (menu-start-process)
      ;; prevent any depressed button being redrawn until the menu
      ;; is popped down
      (frame-draw-mutex t)
      ;; This function is probably called from a ButtonPress event,
      ;; so cancel the implicit pointer grab (to allow the menu's grab
      ;; to succeed)
      (ungrab-pointer)
      (ungrab-keyboard)
      (sync-server)
      (when (functionp spec)
	(setq spec (spec)))
      ;; XXX this is a hack, but I want menus to appear under buttons
      (if (and part (setq part (frame-part-get part 'class))
	       (windowp menu-active)
	       (string-match "-button$" (symbol-name part)))
	  (progn
	    (rplaca offset (max 0 (+ (car offset)
				     (car (window-position menu-active)))))
	    (rplacd offset (max 0 (+ (cdr offset) (cdr dims)
				     (cdr (window-position menu-active))))))
	(setq offset nil))
      (format menu-process "(popup-menu %S %S %S)\n"
	      (mapcar menu-preprocessor spec) (x-server-timestamp) offset))))

;;;###autoload
(defun popup-window-menu ()
  "Display the menu listing all window operations."
  (interactive)
  (popup-menu window-ops-menu))

;;;###autoload
(defun popup-root-menu ()
  "Display the main menu."
  (interactive)
  (popup-menu root-menu))

;;;###autoload
(defun popup-apps-menu ()
  "Display the applications menu."
  (interactive)
  (popup-menu apps-menu))
