;; groups.jl -- commands for manipulating window groups
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


;; iconification

;;;###autoload
(defun iconify-group (w)
  (interactive "%W")
  (map-window-group iconify-window w))

;;;###autoload
(defun uniconify-group (w)
  (interactive "%W")
  (map-window-group uniconify-window w))


;; sticky

;;;###autoload
(defun make-group-sticky (w)
  (interactive "%W")
  (map-window-group make-window-sticky w))

;;;###autoload
(defun make-group-unsticky (w)
  (interactive "%W")
  (map-window-group make-window-unsticky w))

;;;###autoload
(defun toggle-group-sticky (w)
  (interactive "%W")
  (if (window-get w 'sticky)
      (make-group-unsticky w)
    (make-group-sticky w)))


;; workspaces

;;;###autoload
(defun send-group-to-workspace (w send-group-dest-space)
  (map-window-group
   (lambda (x)
     (unless (window-get x 'sticky)
       (ws-move-window x (nearest-workspace-with-window x current-workspace)
		       send-group-dest-space (eq x (input-focus))))) w))

;;;###autoload
(defun send-group-to-current-workspace (w)
  (interactive "%W")
  (send-group-to-workspace w current-workspace))

;;;###autoload
(defun send-group-to-next-workspace (send-group-window count)
  (interactive "%W\np")
  (ws-call-with-workspace (lambda (space)
			    (send-group-to-workspace send-group-window space)
			    (select-workspace space))
			  count workspace-send-boundary-mode))

;;;###autoload
(defun send-group-to-previous-workspace (w count)
  (interactive "%W\np")
  (send-group-to-next-workspace w (- count)))


;; viewports

;;;###autoload
(defun move-group-to-current-viewport (w)
  (interactive "%W")
  (map-window-group move-window-to-current-viewport w))

;;;###autoload
(defun move-group-viewport (w col row)
  (map-window-group (lambda (x)
		      (move-window-viewport x col row)) w)
  (move-viewport-to-window w))

;;;###autoload
(defun move-group-left (w)
  (interactive "%W")
  (move-group-viewport w -1 0))

;;;###autoload
(defun move-group-right (w)
  (interactive "%W")
  (move-group-viewport w 1 0))

;;;###autoload
(defun move-group-up (w)
  (interactive "%W")
  (move-group-viewport w 0 -1))

;;;###autoload
(defun move-group-down (w)
  (interactive "%W")
  (move-group-viewport w 0 1))


;; stacking

;;;###autoload
(defun raise-group (w)
  (interactive "%W")
  (let ((order (windows-in-group w t)))
    (mapc raise-window order)
    (raise-window w)))

;;;###autoload
(defun lower-group (w)
  (interactive "%W")
  (let ((order (windows-in-group w t)))
    (mapc lower-window (nreverse order))
    (lower-window w)))

;;;###autoload
(defun raise-group-depth (w)
  (interactive "%W")
  (map-window-group raise-window-depth w))

;;;###autoload
(defun lower-group-depth (w)
  (interactive "%W")
  (map-window-group lower-window-depth w))

;;;###autoload
(defun raise-lower-group (w)
  (interactive "%W")
  (if (or (eq (window-visibility w) 'unobscured)
	  (let ((order (windows-in-group w t)))
	    (and (window-on-top-p (car order))
		 ;; look for the group as a block.. this is a heuristic
		 (let loop ((rest (memq (car order) (stacking-order))))
		   (cond ((null rest) nil)
			 ((eq (car rest) w) t)
			 ((memq (car rest) order) (loop (cdr rest)))
			 (t nil))))))
      (lower-group w)
    (raise-group w)))


;; framing

;;;###autoload
(defun set-group-frame-style (set-group-w set-group-style
			      &optional set-group-type set-group-from-user)
  (map-window-group (lambda (w)
		      (set-window-frame-style
		       w set-group-style set-group-type set-group-from-user))
		    set-group-w))

