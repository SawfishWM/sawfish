;; place-window.jl -- decide where to initially place a window
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

(define-structure sawfish.wm.placement

    (export define-placement-mode
	    autoload-placement-mode
	    placement-mode
	    acceptable-placement
	    place-window)

    (open rep
	  rep.system
	  rep.util.autoloader
	  sawfish.wm.state.maximize
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.windows
	  sawfish.wm.util.groups
	  sawfish.wm.custom)

  (defvar placement-modes nil
    "List containing all symbols naming window placement modes.")

  (defcustom place-window-mode 'top-left
    "Method of placing windows: \\w"
    :type symbol
    :group placement)

  (defcustom place-transient-mode 'centered-on-parent
    "Method of placing dialog windows: \\w"
    :type symbol
    :group placement)

  (defcustom ignore-program-positions nil
    "Ignore program-specified window placements."
    :type boolean
    :group placement)


;;; utility functions

  (define (define-placement-mode name fun)
    "Define a new window placement mode called NAME (a symbol). The function
FUN will be called with a single argument when a window should be placed using
this mode. The single argument is the window to be placed."
    (unless (memq name placement-modes)
      (setq placement-modes (nconc placement-modes (list name)))
      (custom-set-property 'place-window-mode ':options placement-modes)
      (custom-set-property 'place-transient-mode ':options placement-modes))
    (put name 'placement-mode fun))

  ;; autoload handling
  (define (getter symbol) (get symbol 'placement-mode))

  (define autoload-placement-mode
    (make-autoloader getter define-placement-mode))

  (define placement-mode (autoloader-ref getter))

  (define (adjust-window-for-gravity w grav #!optional unadjust)
    (let ((coords (adjust-position-for-gravity
		   w grav (window-position w) unadjust)))
      (move-window-to w (car coords) (cdr coords))))

  ;; make sure the window doesn't overlap an avoided window
  (define (acceptable-placement w position)
    (require 'sawfish.wm.util.rects)
    (or (window-avoided-p w)
	(let* ((avoided-windows (filter-windows window-avoided-p))
	       (rects (rectangles-from-windows avoided-windows))
	       (dims (window-frame-dimensions w)))
	  (= 0 (rect-total-overlap dims position rects)))))

  ;; called from the place-window-hook
  (define (place-window w)
    (let ((hints (window-size-hints w)))
      ;; The only time this is ever called with (window-get w 'placed) non-nil
      ;; is when we're initializing the wm, and we want to adjust the window's
      ;; position for its gravity setting
      (if (or (window-get w 'placed)
	      (cdr (assq 'user-position hints))
	      (and (not (window-get w 'ignore-program-position))
		   (not ignore-program-positions)
		   (cdr (assq 'program-position hints))
		   (or (window-get w 'ignored)
		       (acceptable-placement w (window-position w))))
	      (window-get w 'fixed-position))
	  (adjust-window-for-gravity w (window-gravity w))
	(let ((mode (or (window-get w 'place-mode)
			(if (window-transient-p w)
			    place-transient-mode
			  place-window-mode))))
	  ((or (placement-mode mode) place-window-randomly) w)))
      t))

  (define (unplace-window w)
    (adjust-window-for-gravity w (window-gravity w) t))

  (add-hook 'place-window-hook place-window t)
  (add-hook 'remove-window-hook unplace-window)


;;; standard placement modes

  (define (place-window-randomly w)
    (let* ((dims (window-frame-dimensions w))
	   (max-rect (maximize-find-workarea w))
	   (rect-dims (if max-rect
			  (cons (- (nth 2 max-rect) (nth 0 max-rect))
				(- (nth 3 max-rect) (nth 1 max-rect)))
			(current-head-dimensions)))
	   (rect-pos (if max-rect
			 (cons (nth 0 max-rect) (nth 1 max-rect))
		       (current-head-offset)))
	   (x (+ (car rect-pos)
		 (if (< (car dims) (car rect-dims))
		     (random (- (car rect-dims) (car dims)))
		   0)))
	   (y (+ (cdr rect-pos)
		 (if (< (cdr dims) (cdr rect-dims))
		     (random (- (cdr rect-dims) (cdr dims)))
		   0))))
      (move-window-to w x y)))

  (define (place-window-interactively w)
    (require 'sawfish.wm.commands.move-resize)
    (let ((move-outline-mode 'box)
	  (ptr (query-pointer))
	  (dims (window-frame-dimensions w)))
      ;; XXX hacktastic! I don't know why the next thing is needed,
      ;; XXX but it is -- if the window was popped by a button click
      ;; XXX the ButtonRelease can get caught by move-window-int..
      ;; XXX (try double clicking on a gmc icon)
      (accept-x-input)
      (when (window-id w)
	(move-window-to w (- (car ptr) (quotient (car dims) 2))
			(- (cdr ptr) (quotient (cdr dims) 2)))
	(move-window-interactively w))))

  (define (place-window-centered w)
    (let ((dims (window-frame-dimensions w))
	  (h-dims (current-head-dimensions))
	  (h-off (current-head-offset))
	  (screen (maximize-find-workarea w)))
      (move-window-to w
		      (clamp* (+ (car h-off)
				 (quotient (- (car h-dims) (car dims)) 2))
			      (car dims) (nth 0 screen) (nth 2 screen))
		      (clamp* (+ (cdr h-off)
				 (quotient (- (cdr h-dims) (cdr dims)) 2))
			      (cdr dims) (nth 1 screen) (nth 3 screen)))))

  (define (place-window-centered-on-parent w)
    (let ((parent (let ((id (window-transient-p w)))
		    (and id (get-window-by-id id)))))
      ;; if no known parent, look if the focused window is in the
      ;; same group as W, if so use it
      (when (and (not parent) (not (eq (input-focus) w))
		 (memq (input-focus) (windows-in-group w)))
	(setq parent (input-focus)))
      (if (not parent)
	  (place-window-centered w)
	(let ((dims (window-frame-dimensions w))
	      (pdims (window-frame-dimensions parent))
	      (coords (window-position parent))
	      (screen (maximize-find-workarea w)))
	  (rplaca coords (clamp* (+ (car coords)
				    (quotient (- (car pdims) (car dims)) 2))
				 (car dims) (nth 0 screen) (nth 2 screen)))
	  (rplacd coords (clamp* (+ (cdr coords)
				    (quotient (- (cdr pdims) (cdr dims)) 2))
				 (cdr dims) (nth 1 screen) (nth 3 screen)))
	  (move-window-to w (car coords) (cdr coords))))))

  (define (place-window-under-pointer w)
    (let ((dims (window-frame-dimensions w))
	  (coords (query-pointer))
	  (screen (maximize-find-workarea w #:head (pointer-head))))
      (rplaca coords (clamp* (- (car coords) (quotient (car dims) 2))
			     (car dims) (nth 0 screen) (nth 2 screen)))
      (rplacd coords (clamp* (- (cdr coords) (quotient (cdr dims) 2))
			     (cdr dims) (nth 1 screen) (nth 3 screen)))
      (move-window-to w (car coords) (cdr coords))))

  (define-placement-mode 'randomly place-window-randomly)
  (define-placement-mode 'interactively place-window-interactively)
  (define-placement-mode 'centered place-window-centered)
  (define-placement-mode 'centered-on-parent place-window-centered-on-parent)
  (define-placement-mode 'under-pointer place-window-under-pointer)
  (define-placement-mode 'none nop))
