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

;;;###autoload
(defun next-workspace-window ()
  "Select the next window of the current workspace."
  (interactive)
  (let
      ((windows (workspace-windows current-workspace)))
    (display-window (or (nth 1 (memq (input-focus) windows)) (car windows)))))

;;;###autoload
(defun previous-workspace-window ()
  "Focus on the previous window of the current workspace."
  (interactive)
  (let
      ((windows (nreverse (workspace-windows current-workspace))))
    (display-window (or (nth 1 (memq (input-focus) windows)) (car windows)))))

;;;###autoload
(defun next-window ()
  "Select the next window, cycling through all possible workspaces."
  (interactive)
  (catch 'out
    (let*
	((space current-workspace)
	 (limits (workspace-limits))
	 (windows (workspace-windows space))
	 (win (nth 1 (memq (input-focus) windows))))
      (while (not win)
	(setq space (1+ space))
	(when (> space (cdr limits))
	  (setq space (car limits)))
	(when (= space current-workspace)
	  (throw 'out nil))
	(setq windows (workspace-windows space))
	(setq win (car windows)))
      (when win
	(display-window win)))))

;;;###autoload
(defun previous-window ()
  "Select the previous window, cycling through all possible workspaces."
  (interactive)
  (catch 'out
    (let*
	((space current-workspace)
	 (limits (workspace-limits))
	 (windows (nreverse (workspace-windows space)))
	 (win (nth 1 (memq (input-focus) windows))))
      (while (not win)
	(setq space (1- space))
	(when (< space (car limits))
	  (setq space (cdr limits)))
	(when (= space current-workspace)
	  (throw 'out nil))
	(setq windows (nreverse (workspace-windows space)))
	(setq win (car windows)))
      (when win
	(display-window win)))))
