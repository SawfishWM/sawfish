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

(defvar place-window-modes '(random interactive first-fit best-fit
			     first-fit-or-interactive centered
			     centered-on-parent under-pointer none))

(defcustom place-window-mode 'best-fit
  "Method of selecting the position of a freshly-mapped window."
  :type symbol
  :group placement)

(custom-set-property 'place-window-mode ':options place-window-modes)

(defcustom place-transient-mode 'random
  "Method of selecting the position of a freshly-mapped transient window."
  :type symbol
  :group placement)

(custom-set-property 'place-transient-mode ':options place-window-modes)

(defcustom ignore-program-positions nil
  "Ignore program-specified window positions."
  :type boolean
  :group placement)

(defun place-window-randomly (w)
  (let
      ((dims (window-frame-dimensions w))
       x y)
    (if (< (car dims) (screen-width))
	(setq x (random (- (screen-width) (car dims))))
      (setq x 0))
    (if (< (cdr dims) (screen-height))
	(setq y (random (- (screen-height) (cdr dims))))
      (setq y 0))
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
    (move-window-to w (- (car ptr) (/ (car dims) 2))
		    (- (cdr ptr) (/ (cdr dims) 2)))
    (move-window-interactively w)))

(defun place-window-centered (w)
  (let
      ((dims (window-frame-dimensions w)))
    (move-window-to w (/ (max 0 (- (screen-width) (car dims))) 2)
		    (/ (max 0 (- (screen-height) (cdr dims))) 2))))

(defun place-window-centered-on-parent (w)
  (let
      ((parent (window-transient-p w)))
    (if (or (not parent) (not (setq parent (get-window-by-id parent))))
	(place-window-centered w)
      (let
	  ((dims (window-frame-dimensions w))
	   (pdims (window-frame-dimensions parent))
	   (coords (window-position parent)))
	(rplaca coords (+ (car coords) (/ (- (car pdims) (car dims)) 2)))
	(rplacd coords (+ (cdr coords) (/ (- (cdr pdims) (cdr dims)) 2)))
	(move-window-to w (car coords) (cdr coords))))))

(defun place-window-under-pointer (w)
  (let
      ((dims (window-frame-dimensions w))
       (coords (query-pointer)))
    (rplaca coords (min (- (screen-width) (car dims))
			(max 0 (- (car coords) (/ (car dims) 2)))))
    (rplacd coords (min (- (screen-height) (cdr dims))
			(max 0 (- (cdr coords) (/ (cdr dims) 2)))))
    (move-window-to w (car coords) (cdr coords))))

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
	    ((gravity (cdr (assq 'window-gravity hints))))
	  (when gravity
	    (adjust-window-for-gravity w gravity)))
      (let
	  ((mode (or (window-get w 'place-mode)
		     (if (window-transient-p w)
			 place-transient-mode
		       place-window-mode))))
	((or (get mode 'placement-mode) place-window-randomly) w)
	t))))

(put 'interactive 'placement-mode place-window-interactively)
(put 'random 'placement-mode place-window-randomly)
(put 'first-fit 'placement-mode place-window-first-fit)
(put 'best-fit 'placement-mode place-window-best-fit)
(put 'first-fit-or-interactive 'placement-mode
     place-window-first-fit-or-interactive)
(put 'centered 'placement-mode place-window-centered)
(put 'centered-on-parent 'placement-mode place-window-centered-on-parent)
(put 'under-pointer 'placement-mode place-window-under-pointer)
(put 'none 'placement-mode nop)

(add-hook 'place-window-hook place-window t)
