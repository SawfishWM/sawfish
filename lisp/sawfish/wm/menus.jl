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

(defvar menu-program "sawmill-ui"
  "Name of the program implementing sawmill's high-level user-interface.")

(defvar menu-program-stays-running nil
  "When non-nil, the user-interface program is never stopped.")

;; the active user interface process
(defvar menu-process nil)

;; output from the user-interface process that's received but not
;; yet processed
(defvar menu-pending nil)

;; non-nil when we're waiting for a response from the ui process
(defvar menu-active nil)

(defvar window-ops-menu
  '(("Move" move-window-interactively)
    ("Resize" resize-window-interactively)
    ("Raise" raise-window)
    ("Lower" lower-window)
    ("Iconify" iconify-window)
    ("Delete" delete-window)
    ("Destroy" destroy-window)
    ("Send left" send-to-previous-workspace)
    ("Send right" send-to-next-workspace)))

(defvar root-menu
  '(("Workspaces" . workspace-menu)
    ("Windows" . window-menu)
    (apps-menu)
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
  (unless (and menu-process (process-in-use-p menu-process))
    (when menu-process
      (kill-process menu-process))
    (setq menu-process (make-process 'menu-filter 'menu-sentinel))
    (or (start-process menu-process menu-program)
	(error "Can't start menu backend: %s" menu-program))))

(defun menu-stop-process ()
  (when (and menu-process (not menu-program-stays-running))
    (kill-process menu-process)
    (setq menu-process nil)))

(defun menu-filter (output)
  (setq output (concat menu-pending output))
  (setq menu-pending nil)
  (condition-case nil
      (let
	  ((result (read-from-string output)))
	(throw 'menu-done result))
    (end-of-stream
     (setq menu-pending output))))

(defun menu-sentinel (process)
  (when (and menu-process (not (process-in-use-p menu-process)))
    (setq menu-process nil)))

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

;;;###autoload
(defun popup-menu (spec)
  (if menu-active
      (error "Menu already active")
    (setq menu-active)
    (menu-start-process)
    ;; This function is probably called from a ButtonPress event,
    ;; so cancel the implicit pointer grab (to allow the menu's grab
    ;; to succeed)
    (ungrab-pointer)
    (sync-server)
    (unwind-protect
	(let
	    ((result (catch 'menu-done
		       (format menu-process "(popup-menu %S)\n"
			       (mapcar 'menu-preprocessor spec))
		       (while t
			 (accept-process-output 10)))))
	  (setq menu-active nil)
	  (when result
	    (cond ((commandp result)
		   (call-command result))
		  ((functionp result)
		   (funcall result))
		  (t
		   result))))
      (menu-stop-process))))

;;;###autoload
(defun popup-window-menu ()
  (interactive)
  (if window-ops-menu
      (popup-menu window-ops-menu)
    (beep)))

;;;###autoload
(defun popup-root-menu ()
  (interactive)
  (if root-menu
      (popup-menu root-menu)
    (beep)))
