;; cycle.jl -- simple window cycling
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

(define-structure sawfish.wm.commands.cycle

    (export next-workspace-window
	    previous-workspace-window
	    next-window
	    previous-window)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.workspace
	  sawfish.wm.util.display-window)

  (define-structure-alias cycle sawfish.wm.commands.cycle)

  (define (next-workspace-window)
    "Select the next window of the current workspace."
    (let ((windows (delete-if-not window-in-cycle-p
				  (workspace-windows current-workspace))))
      (display-window (or (nth 1 (memq (input-focus) windows))
			  (car windows)))))

  (define (previous-workspace-window)
    "Focus on the previous window of the current workspace."
    (let ((windows (nreverse
		    (delete-if-not window-in-cycle-p
				   (workspace-windows current-workspace)))))
      (display-window (or (nth 1 (memq (input-focus) windows))
			  (car windows)))))

  (define (next-window)
    "Select the next window, cycling through all possible workspaces."
    (catch 'out
      (let* ((space current-workspace)
	     (limits (workspace-limits))
	     (windows (lambda ()
			(delete-if-not window-in-cycle-p
				       (workspace-windows space))))
	     (win (nth 1 (memq (input-focus) (windows)))))
	(while (not win)
	  (setq space (1+ space))
	  (when (> space (cdr limits))
	    (setq space (car limits)))
	  (when (= space current-workspace)
	    (throw 'out nil))
	  (setq win (car (windows))))
	(when win
	  (display-window win)))))

  (define (previous-window)
    "Select the previous window, cycling through all possible workspaces."
    (catch 'out
      (let* ((space current-workspace)
	     (limits (workspace-limits))
	     (windows (lambda ()
			(nreverse (delete-if-not window-in-cycle-p
						 (workspace-windows space)))))
	     (win (nth 1 (memq (input-focus) (windows)))))
	(while (not win)
	  (setq space (1- space))
	  (when (< space (car limits))
	    (setq space (cdr limits)))
	  (when (= space current-workspace)
	    (throw 'out nil))
	  (setq win (car (windows))))
	(when win
	  (display-window win)))))

  ;;###autoload
  (define-command 'next-workspace-window next-workspace-window)
  (define-command 'previous-workspace-window previous-workspace-window)
  (define-command 'next-window next-window)
  (define-command 'previous-window previous-window))
