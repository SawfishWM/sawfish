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

;;;###autoload (setq custom-required (cons 'edge-flip custom-required))

(defgroup edge-flip "Edge Flipping")

;; for the compiler's benefit
(eval-when-compile (progn
		     (require 'move-resize)
		     (require 'flippers)
		     (require 'timers)))

(defcustom edge-flip-enabled nil
  "Flip to next viewport/workspace when pointer hits screen edge."
  :type boolean
  :require edge-flip
  :group edge-flip
  :after-set (lambda () (edge-flip-enable)))

(defcustom edge-flip-type 'viewport
  "What hitting the screen edge actually flips."
  :type symbol
  :options (viewport workspace)
  :group edge-flip)

(defcustom edge-flip-delay 250
  "Number of milliseconds to wait after pointer hits screen edge before
flipping."
  :type number
  :group edge-flip
  :range (0 . 1000))

(defvar ef-current-edge nil)
(defvar ef-timer nil)

(defun edge-flip-enable ()
  (if edge-flip-enabled
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
				 (/ edge-flip-delay 1000)
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
	  (warp-cursor (car ptr) (cdr ptr)))
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
	(unless (= current-workspace orig)
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
  (when edge-flip-enabled
    (edge-flip-synthesize)
  (when (eq edge-flip-type 'workspace)
    (ws-move-window w current-workspace t))))

(add-hook 'enter-flipper-hook edge-flip-enter)
(add-hook 'leave-flipper-hook edge-flip-leave)
(add-hook 'while-moving-hook edge-flip-while-moving)

(unless batch-mode
  (edge-flip-enable))
