;; place-window.jl -- decide where to initially place a window
;; $Id: placement.jl,v 1.55 2003/01/12 20:48:36 jsh Exp $

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

(define-structure sawfish.wm.placement

    (export define-placement-mode
	    autoload-placement-mode
	    placement-mode
	    acceptable-placement
	    place-window)

    (open rep
	  rep.system
	  rep.util.autoloader
	  sawfish.wm.misc
	  sawfish.wm.events
	  sawfish.wm.windows
	  sawfish.wm.util.groups
	  sawfish.wm.util.workarea
	  sawfish.wm.custom)

  (defcustom place-window-mode 'top-left
    "Method of placing windows: \\w"
    :type symbol
    :group (misc placement))

  (defcustom place-transient-mode 'centered-on-parent
    "Method of placing dialog windows: \\w"
    :type symbol
    :group (misc placement))

  (defcustom ignore-program-positions t
    "Ignore program-specified window placements."
    :type boolean
    :group (misc placement))

  (defvar placement-modes '()
    "List of names of all placement modes.")

;;; utility functions

  ;; autoload handling
  (define (getter symbol) (get symbol 'placement-mode))
  (define (setter symbol value)
    (unless (memq symbol placement-modes)
      (setq placement-modes (nconc placement-modes (list symbol))))
    (put symbol 'placement-mode value))
  (define autoloader (make-autoloader getter setter))
  (define placement-mode (autoloader-ref getter))

  (define (apply-keys name #!key for-normal for-dialogs)
    (define (add-to sym name)
      (let ((current (custom-get-property sym ':options)))
	(custom-set-property sym ':options
			     (nconc (delq name current) (list name)))))
    (when for-normal
      (add-to 'place-window-mode name))
    (when for-dialogs
      (add-to 'place-transient-mode name)))

  (define (define-placement-mode name fun . keys)
    "Define a new window placement mode called NAME (a symbol). The function
FUN will be called with a single argument when a window should be placed using
this mode. The single argument is the window to be placed."
    (setter name fun)
    (apply apply-keys name keys))

  (define (autoload-placement-mode name module . keys)
    (autoloader name module)
    (apply apply-keys name keys))

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
	   (max-rect (calculate-workarea #:window w #:head (current-head)))
	   (rect-dims (cons (- (nth 2 max-rect) (nth 0 max-rect))
			    (- (nth 3 max-rect) (nth 1 max-rect))))
	   (rect-pos (cons (nth 0 max-rect) (nth 1 max-rect)))
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
	  (screen (calculate-workarea #:window w #:head (current-head))))
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
	      (screen (calculate-workarea #:window w #:head (current-head parent))))
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
	  (screen (calculate-workarea #:window w #:head (pointer-head))))
      (rplaca coords (clamp* (- (car coords) (quotient (car dims) 2))
			     (car dims) (nth 0 screen) (nth 2 screen)))
      (rplacd coords (clamp* (- (cdr coords) (quotient (cdr dims) 2))
			     (cdr dims) (nth 1 screen) (nth 3 screen)))
      (move-window-to w (car coords) (cdr coords))))

  (define-placement-mode 'randomly place-window-randomly #:for-normal t #:for-dialogs t)
  (define-placement-mode 'interactively place-window-interactively #:for-normal t #:for-dialogs t)
  (define-placement-mode 'centered place-window-centered #:for-normal t #:for-dialogs t)
  (define-placement-mode 'centered-on-parent place-window-centered-on-parent #:for-normal t #:for-dialogs t)
  (define-placement-mode 'under-pointer place-window-under-pointer #:for-normal t #:for-dialogs t)
  (define-placement-mode 'none nop #:for-normal t #:for-dialogs t))
