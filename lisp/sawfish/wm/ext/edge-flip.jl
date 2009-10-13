;; edge-flip.jl -- move viewports by pushing pointer against screen edges

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.wm.ext.edge-flip

    (export edge-flip-for-edge
	    edge-flip-synthesize
	    edge-flip-while-moving
	    edge-flip-enable)

    (open rep
	  rep.system
	  rep.io.timers
	  sawfish.wm.custom
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.viewport
	  sawfish.wm.workspace
	  sawfish.wm.commands.move-resize
	  sawfish.wm.ext.workspace-grid)

  (define-structure-alias edge-flip sawfish.wm.ext.edge-flip)

  ;; for the compiler's benefit
  (eval-when-compile (require 'sawfish.wm.util.flippers))

  (defgroup edge-flip "Edge Flipping"
    :group workspace
    :require sawfish.wm.ext.edge-flip)

  (defcustom edge-flip-enabled nil
    "Select the next desktop when the pointer hits screen edge."
    :type boolean
    :require sawfish.wm.ext.edge-flip
    :group (workspace edge-flip)
    :after-set (lambda () (edge-flip-enable)))

  (defcustom edge-flip-type 'workspace
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
    "Milliseconds to delay before edge flipping."
    :type number
    :depends edge-flip-enabled
    :group (workspace edge-flip)
    :after-set (lambda () (edge-flip-enable)))

  (define ef-current-edge nil)
  (define ef-timer nil)

  (defvar before-edge-flip-hook '()
    "Hook called immediately before edge-flipping.")
  (defvar after-edge-flip-hook '()
    "Hook called immediately after edge-flipping.")

  (define (edge-flip-enable)
    (if (and edge-flip-enabled (not edge-flip-only-when-moving))
	(progn
	  (require 'sawfish.wm.util.flippers)
	  (enable-flippers))
      (when (featurep 'sawfish.wm.util.flippers)
	(disable-flippers))))

  (define (edge-flip-enter edge)
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

  (define (edge-flip-leave edge)
    (declare (unused edge))
    (setq ef-current-edge nil)
    (when ef-timer
      (delete-timer ef-timer)
      (setq ef-timer nil)))

  (define (edge-flip-for-edge edge)
    (let ((ptr (query-pointer t)))
      (call-hook 'before-edge-flip-hook)
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
	    ;; always warp the pointer to keep it logically static
	    (warp-cursor (car ptr) (cdr ptr)))
	(let ((orig current-workspace))
	  (cond ((eq edge 'left)
		 (workspace-left)
		 (rplaca ptr (- (screen-width) 2)))
		((eq edge 'right)
		 (workspace-right)
		 (rplaca ptr 1))
		((eq edge 'top)
		 (workspace-up)
		 (rplacd ptr (- (screen-height) 2)))
		((eq edge 'bottom)
		 (workspace-down)
		 (rplacd ptr 1)))
	  (unless (= current-workspace orig)
	    (warp-cursor (car ptr) (cdr ptr)))))
      (call-hook 'after-edge-flip-hook)))

  ;; this is a hack -- while the pointer's grabbed the flipper windows
  ;; won't get enter/leave notify events (this is normally the right
  ;; thing to do), so synthesize them ourselves while interactively
  ;; moving windows
  ;; XXX this probably doesn't handle the screen corners correctly
  (define (edge-flip-synthesize)
    (when edge-flip-enabled
      (let ((ptr (query-pointer))
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

;;; ugly hacks to make flipping work while dragging windows

  ;; current-workspace before flipping
  (define original-space)

  (define (before-flip)
    (when move-resize-window
      (setq original-space current-workspace)))

  (define (after-flip)
    (let ((w move-resize-window))
      (when w
	(when (and (eq edge-flip-type 'workspace)
		   (/= original-space current-workspace)
		   (not (window-get w 'sticky)))
	  (move-window-to-workspace w original-space current-workspace t)))))

  (define (edge-flip-while-moving w)
    (declare (unused w))
    (when edge-flip-enabled
      (edge-flip-synthesize)))

  (add-hook 'before-edge-flip-hook before-flip)
  (add-hook 'after-edge-flip-hook after-flip)
  (add-hook 'while-moving-hook edge-flip-while-moving)

;;; init

  (add-hook 'enter-flipper-hook edge-flip-enter)
  (add-hook 'leave-flipper-hook edge-flip-leave)

  (unless batch-mode
    (edge-flip-enable)))
