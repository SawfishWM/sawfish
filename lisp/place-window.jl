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

(provide 'place-window)

(defvar placement-modes nil
  "List containing all symbols naming window placement modes.")

(defcustom place-window-mode 'best-fit
  "Method of selecting the position of a freshly-mapped window."
  :type symbol
  :group placement)

(defcustom place-transient-mode 'random
  "Method of selecting the position of a freshly-mapped transient window."
  :type symbol
  :group placement)

(defcustom ignore-program-positions nil
  "Ignore program-specified window positions."
  :type boolean
  :group placement)


;; utility functions

(defun define-placement-mode (name fun)
  "Define a new window placement mode called NAME (a symbol). The function FUN
will be called with a single argument when a window should be placed using
this mode. The single argument is the window to be placed."
  (unless (memq name placement-modes)
    (setq placement-modes (nconc placement-modes (list name))))
  (custom-set-property 'place-window-mode ':options placement-modes)
  (custom-set-property 'place-transient-mode ':options placement-modes)
  (put name 'placement-mode fun))

(defun adjust-window-for-gravity (w grav)
  (let
      ((coords (window-position w))
       (dims (window-dimensions w))
       (fdims (window-frame-dimensions w))
       (off (window-frame-offset w)))
    (if (eq grav 'static)
	(progn
	  ;; static gravity is relative to the original
	  ;; client window position
	  (rplaca coords (+ (car coords) (car off)))
	  (rplacd coords (+ (cdr coords) (cdr off))))
      (when (memq grav '(east south-east north-east))
	;; relative to the right of the frame
	(rplaca coords (- (car coords) (- (car fdims) (car dims))
			  (* -2 (window-border-width w)))))
      (when (memq grav '(south south-east south-west))
	;; relative to the bottom of the frame
	(rplacd coords (- (cdr coords) (- (cdr fdims) (cdr dims))
			  (* -2 (window-border-width w))))))
    (move-window-to w (car coords) (cdr coords))))

;; called from the place-window-hook
(defun place-window (w)
  (let
      ((hints (window-size-hints w)))
    (if (or (cdr (assq 'user-position hints))
	    (and (not (window-get w 'ignore-program-position))
		 (not ignore-program-positions)
		 (cdr (assq 'program-position hints))))
	(let
	    ((gravity (or (window-get w 'gravity)
			  (cdr (assq 'window-gravity hints)))))
	  (when gravity
	    (adjust-window-for-gravity w gravity)))
      (let
	  ((mode (or (window-get w 'place-mode)
		     (if (window-transient-p w)
			 place-transient-mode
		       place-window-mode))))
	((or (get mode 'placement-mode) place-window-randomly) w)
	t))))

(add-hook 'place-window-hook place-window t)


;; standard placement modes

(defun place-window-randomly (w)
  (require 'maximize)
  (let*
      ((dims (window-frame-dimensions w))
       (max-rect (maximize-find-workarea w))
       (x (cond ((and max-rect (< (car dims) (nth 2 max-rect)))
		 (+ (nth 0 max-rect) (random (- (nth 2 max-rect) (car dims)))))
		((< (car dims) (screen-width))
		 (random (- (screen-width) (car dims))))
		(t 0)))
       (y (cond ((and max-rect (< (cdr dims) (nth 3 max-rect)))
		 (+ (nth 1 max-rect) (random (- (nth 3 max-rect) (cdr dims)))))
		((< (cdr dims) (screen-height))
		 (random (- (screen-height) (cdr dims))))
		(t 0))))
    (move-window-to w x y)))

(defun place-window-interactively (w)
  (require 'move-resize)
  (let
      ((move-outline-mode nil)
       (ptr (query-pointer))
       (dims (window-frame-dimensions w)))
    ;; XXX hacktastic! I don't know why the next thing is needed,
    ;; XXX but it is -- if the window was popped by a button click
    ;; XXX the ButtonRelease can get caught by move-window-int..
    ;; XXX (try double clicking on a gmc icon)
    (accept-x-input)
    (move-window-to w (- (car ptr) (quotient (car dims) 2))
		    (- (cdr ptr) (quotient (cdr dims) 2)))
    (move-window-interactively w)))

(defun place-window-centered (w)
  (let
      ((dims (window-frame-dimensions w)))
    (move-window-to w (quotient (max 0 (- (screen-width) (car dims))) 2)
		    (quotient (max 0 (- (screen-height) (cdr dims))) 2))))

(defun place-window-centered-on-parent (w)
  (let
      ((parent (window-transient-p w)))
    (if (or (not parent) (not (setq parent (get-window-by-id parent))))
	(place-window-centered w)
      (let
	  ((dims (window-frame-dimensions w))
	   (pdims (window-frame-dimensions parent))
	   (coords (window-position parent)))
	(rplaca coords (+ (car coords)
			  (quotient (- (car pdims) (car dims)) 2)))
	(rplacd coords (+ (cdr coords)
			  (quotient (- (cdr pdims) (cdr dims)) 2)))
	(move-window-to w (car coords) (cdr coords))))))

(defun place-window-under-pointer (w)
  (let
      ((dims (window-frame-dimensions w))
       (coords (query-pointer)))
    (rplaca coords (min (- (screen-width) (car dims))
			(max 0 (- (car coords) (quotient (car dims) 2)))))
    (rplacd coords (min (- (screen-height) (cdr dims))
			(max 0 (- (cdr coords) (quotient (cdr dims) 2)))))
    (move-window-to w (car coords) (cdr coords))))

(define-placement-mode 'randomly place-window-randomly)
(define-placement-mode 'interactively place-window-interactively)
(define-placement-mode 'centered place-window-centered)
(define-placement-mode 'centered-on-parent place-window-centered-on-parent)
(define-placement-mode 'under-pointer place-window-under-pointer)
(define-placement-mode 'none nop)
