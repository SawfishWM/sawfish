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

;;;###autoload (setq custom-required (cons 'menus custom-required))

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
  '(("Move" move-window-interactively)
    ("Resize" resize-window-interactively)
    ("Iconify" iconify-window)
    ("Delete" delete-window)
    ("Destroy" destroy-window)
    ("Send left" send-to-previous-workspace)
    ("Send right" send-to-next-workspace)
    ("Toggle sticky" toggle-window-sticky)
    ("Toggle shaded" toggle-window-shaded)
    ("Depth"
     ("Raise" raise-window)
     ("Lower" lower-window)
     ("Upper layer" raise-window-depth)
     ("Lower layer" lower-window-depth))
    ("Maximize"
     ("Vertically" maximize-window-vertically)
     ("Horizontally" maximize-window-horizontally)
     ("Both" maximize-window)
     ()
     ("Fill vertically" maximize-fill-window-vertically)
     ("Fill horizontally" maximize-fill-window-horizontally)
     ("Fill both" maximize-fill-window))
    ("Un-maximize" unmaximize-window)
    ("Frame type"
     ("Normal" set-frame:default)
     ("Title-only" set-frame:shaped)
     ("Border-only" set-frame:transient)
     ("Top-border" set-frame:shaped-transient)
     ("None" set-frame:unframed))
    ("Frame style" . frame-style-menu)))

(defvar root-menu
  '(("Workspaces" . workspace-menu)
    ("Windows" . window-menu)
    (apps-menu)
    ("Customize" . custom-menu)
    ("About..." (lambda () (customize 'about)))
    ()
    ("Restart" restart)
    ("Quit" quit)))

(defvar apps-menu
  '("Applications"
    ("xterm" (lambda () (system "xterm &")))
    ("Emacs" (lambda () (system "emacs &")))
    ("Netscape" (lambda () (system "netscape &")))
    ("The GIMP" (lambda () (system "gimp &")))
    ("XFIG" (lambda () (system "xfig &")))
    ("GV" (lambda () (system "gv &")))
    ("xcalc" (lambda () (system "xcalc &")))))

(defun menu-start-process ()
  (when menu-timer
    (delete-timer menu-timer)
    (setq menu-timer nil))
  (unless (and menu-process (process-in-use-p menu-process))
    (when menu-process
      (kill-process menu-process))
    (setq menu-process (make-process 'menu-filter 'menu-sentinel))
    (set-process-error-stream menu-process nil)
    (or (start-process menu-process menu-program)
	(error "Can't start menu backend: %s" menu-program))))

(defun menu-stop-process (&optional force)
  (when menu-process
    (cond ((and (not force) (numberp menu-program-stays-running))
	   ;; number of seconds to let it hang around for
	   (require 'timers)
	   (setq menu-timer (make-timer #'(lambda ()
					    (kill-process menu-process)
					    (setq menu-process nil)
					    (setq menu-timer nil))
					menu-program-stays-running)))
	  ((or force (not menu-program-stays-running))
	   (kill-process menu-process)
	   (setq menu-process nil)))))

(defun menu-filter (output)
  (setq output (concat menu-pending output))
  (setq menu-pending nil)
  (condition-case nil
      (let
	  ((result (read-from-string output)))
	;; GTK takes the focus for its menu, but later returns it to
	;; the original window. We want the focus to be restored by the
	;; time the menu-chosen command is invoked..
	(accept-x-input)
	(menu-dispatch result))
    (end-of-stream
     (setq menu-pending output))))

(defun menu-sentinel (process)
  (when (and menu-process (not (process-in-use-p menu-process)))
    (setq menu-process nil))
  (when menu-timer
    (delete-timer menu-timer)
    (setq menu-timer nil)))

(defun menu-preprocessor (cell)
  (when (and cell (symbolp (car cell)) (not (functionp cell)))
    (setq cell (symbol-value (car cell))))
  (when cell
    (let
	((label (car cell)))
      (if (functionp (cdr cell))
	  (setq cell (funcall (cdr cell)))
	(setq cell (cdr cell)))
      (when (and (consp (car cell)) (not (functionp (car cell))))
	(setq cell (mapcar 'menu-preprocessor cell)))
      (cons label cell))))

(defun menu-dispatch (result)
  (let
      ((orig-win menu-active))
    (menu-stop-process)
    (setq menu-active nil)
    (when result
      (when (windowp orig-win)
	(current-event-window orig-win))
      (cond ((commandp result)
	     (call-command result))
	    ((functionp result)
	     (funcall result))
	    (t
	     result)))))

;;;###autoload
(defun popup-menu (spec)
  (if (and menu-active menu-process (process-in-use-p menu-process))
      (error "Menu already active")
    (setq menu-active (or (current-event-window) (input-focus)))
    (menu-start-process)
    ;; This function is probably called from a ButtonPress event,
    ;; so cancel the implicit pointer grab (to allow the menu's grab
    ;; to succeed)
    (ungrab-pointer)
    (sync-server)
    (when (functionp spec)
      (setq spec (funcall spec)))
    (format menu-process "(popup-menu %S %S)\n"
	    (mapcar 'menu-preprocessor spec) (x-server-timestamp))))

;;;###autoload
(defun popup-window-menu ()
  "Display the menu listing all window operations."
  (interactive)
  (if window-ops-menu
      (popup-menu window-ops-menu)
    (beep)))

;;;###autoload
(defun popup-root-menu ()
  "Display the main menu."
  (interactive)
  (if root-menu
      (popup-menu root-menu)
    (beep)))

;;;###autoload
(defun popup-apps-menu ()
  "Display the applications menu."
  (interactive)
  (if root-menu
      (popup-menu (cdr apps-menu))
    (beep)))
