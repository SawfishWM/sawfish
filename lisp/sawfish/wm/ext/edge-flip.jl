;; edge-flip.jl -- move viewports by pushing pointer against screen edges
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

(provide 'edge-flip)

;;;###autoload (defgroup edge-flip "Edge Flipping" :group workspace :require edge-flip)

(defgroup edge-flip "Edge Flipping"
  :group workspace
  :require edge-flip)

;; for the compiler's benefit
(eval-when-compile (require 'move-resize))
(eval-when-compile (require 'flippers))
(eval-when-compile (require 'timers))

(defcustom edge-flip-enabled nil
  "Select the next desktop when the pointer hits screen edge."
  :type boolean
  :user-level novice
  :require edge-flip
  :group (workspace edge-flip)
  :after-set (lambda () (edge-flip-enable)))

(defcustom edge-flip-type 'viewport
  "Hitting the screen edge selects the next: \\w"
  :type (choice viewport workspace)
  :depends edge-flip-enabled
  :group (workspace edge-flip))

(defcustom edge-flip-only-when-moving nil
  "Only flip when interactively moving a window."
  :type boolean
  :depends edge-flip-enabled
  :group (workspace edge-flip)
  :after-set (lambda () (edge-flip-enable)))

(defcustom edge-flip-delay 250
  "Milliseconds to delay before flipping: \\w"
  :type (number 0 1000)
  :depends edge-flip-enabled
  :group (workspace edge-flip))

(defcustom edge-flip-warp-pointer t
  "Warp pointer to opposite screen edge when flipping."
  :type boolean
  :user-level expert
  :depends edge-flip-enabled
  :group (workspace edge-flip)
  :after-set (lambda () (edge-flip-enable)))

(defvar ef-current-edge nil)
(defvar ef-timer nil)

(defun edge-flip-enable ()
  (if (and edge-flip-enabled (not edge-flip-only-when-moving))
      (progn
	(require 'flippers)
	(require 'timers)
	(enable-flippers))
    (when (featurep 'flippers)
      (disable-flippers))))

(defun edge-flip-enter (edge)
  (if (<= edge-flip-delay 0)
      (edge-flip-for-edge edge)
    (setq ef-current-edge edge)
    (if ef-timer
	(set-timer ef-timer)
      (setq ef-timer (make-timer (lambda ()
				   (setq ef-timer nil)
				   (edge-flip-for-edge ef-current-edge))
				 (quotient edge-flip-delay 1000)
				 (mod edge-flip-delay 1000))))))

(defun edge-flip-leave (edge)
  (setq ef-current-edge nil)
  (when ef-timer
    (delete-timer ef-timer)
    (setq ef-timer nil)))

(defun edge-flip-for-edge (edge)
  (let
      ((ptr (query-pointer t)))
    (if (eq edge-flip-type 'viewport)
	(progn
	  (cond ((eq edge 'left)
		 (when (move-viewport -1 0)
		   (rplaca ptr (- (screen-width) 2))))
		((eq edge 'right)
		 (when (move-viewport 1 0)
		   (rplaca ptr 1)))
		((eq edge 'top)
		 (when (move-viewport 0 -1)
		   (rplacd ptr (- (screen-height) 2))))
		((eq edge 'bottom)
		 (when (move-viewport 0 1)
		   (rplacd ptr 1))))
	  (when edge-flip-warp-pointer
	    (warp-cursor (car ptr) (cdr ptr))))
      (let
	  ((orig current-workspace))
	(cond ((eq edge 'left)
	       (previous-workspace 1)
	       (rplaca ptr (- (screen-width) 2)))
	      ((eq edge 'right)
	       (next-workspace 1)
	       (rplaca ptr 1))
	      ((eq edge 'top)
	       (previous-workspace 1)
	       (rplacd ptr (- (screen-height) 2)))
	      ((eq edge 'bottom)
	       (next-workspace 1)
	       (rplacd ptr 1)))
	(unless (or (= current-workspace orig)
		    (not edge-flip-warp-pointer))
	  (warp-cursor (car ptr) (cdr ptr)))))))

;; this is a hack -- while the pointer's grabbed the flipper windows
;; won't get enter/leave notify events (this is normally the right
;; thing to do), so synthesize them ourselves while interactively
;; moving windows
;; XXX this probably doesn't handle the screen corners correctly
(defun edge-flip-synthesize ()
  (when edge-flip-enabled
    (let
	((ptr (query-pointer))
	 edge)
      (cond ((zerop (car ptr))
	     (setq edge 'left))
	    ((= (car ptr) (1- (screen-width)))
	     (setq edge 'right))
	    ((zerop (cdr ptr))
	     (setq edge 'top))
	    ((= (cdr ptr) (1- (screen-height)))
	     (setq edge 'bottom)))
      (unless (eq edge ef-current-edge)
	(if edge
	    (call-hook 'enter-flipper-hook (list edge))
	  (call-hook 'leave-flipper-hook (list ef-current-edge)))))))

(defun edge-flip-while-moving (w)
  (let
      ((original-space current-workspace)
       (edge-flip-delay 0))
    (when edge-flip-enabled
      (edge-flip-synthesize)
      (when (and (eq edge-flip-type 'workspace)
		 (/= original-space current-workspace)
		 (not (window-get w 'sticky)))
	(ws-move-window w original-space current-workspace t)))))

(add-hook 'enter-flipper-hook edge-flip-enter)
(add-hook 'leave-flipper-hook edge-flip-leave)
(add-hook 'while-moving-hook edge-flip-while-moving)

(unless batch-mode
  (edge-flip-enable))
