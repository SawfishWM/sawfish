#| display-wininfo.jl -- shows info about focused window

   Copyright (C) 2000 Unai Uribarri <unaiur@telecable.es>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.util.display-wininfo

    (export display-wininfo)

    (open rep
	  rep.system
	  rep.data.records
	  sawfish.wm.colors
	  sawfish.wm.events
	  sawfish.wm.fonts
	  sawfish.wm.images
	  sawfish.wm.misc
	  sawfish.wm.util.x
	  sawfish.wm.windows)

;;; variables

  ;; margins
  (defconst x-margin 10)
  (defconst y-margin 10)

  (defconst icon-size (32 . 32))

  ;; window currently displayed, or nil
  (define info-window nil)

;;; utilities

  ;; type used to encapsulate a vertically stacked set of strings, the
  ;; font used to draw them, and the screen space they would take when
  ;; drawn
  (define-record-type :text-item
    (make-text-item-1 strings font width height)
    ;; [no predicate]
    (strings ti-strings)
    (font ti-font)
    (width ti-width)
    (height ti-height))

  (define (make-text-item strings font)
    (make-text-item-1 strings font
		      (apply max (mapcar (lambda (s)
					   (text-width s font)) strings))
		      (* (length strings) (font-height font))))

  ;; Calculates where to put an info window associated with W with size
  ;; DIMS
  (define (get-window-pos w dims)
    (let ((head (current-head w)))
      (if head
	  (cons (+ (quotient (- (car (head-dimensions head)) (car dims)) 2)
		   (car (head-offset head)))
		(+ (quotient (- (cdr (head-dimensions head)) (cdr dims)) 2)
		   (cdr (head-offset head))))
	(cons (quotient (- (screen-width) (car dims)) 2)
	      (quotient (- (screen-height) (cdr dims)) 2)))))

  ;; Returns a list of strings describing window W in some way
  (define (window-info w)
    (list (concat (and (window-get w 'iconified) ?[)
		  (window-name w)
		  (and (window-get w 'iconified) ?]))))

;;; entry point

  ;; What must be shown?
  ;;  * The window icon at left.
  ;;  * At right, the window's title and (maybe) its class.

  (define (display-wininfo w)
    "Shows window information about W. Includes at least window name and
icon (if available). With a null W any displayed information is removed."

    ;; if there's an old window, destroy it
    (when info-window
      (x-destroy-window info-window)
      (setq info-window nil))
	
    (when w
      (let* ((text (make-text-item (window-info w) default-font))
	     (icon (let ((i (window-icon-image w)))
		     (and i (scale-image i (car icon-size) (cdr icon-size)))))
	     (icon-dims (if icon icon-size '(0 . 0)))
	     (win-size (cons (+ (car icon-dims)
				(ti-width text)
				(* x-margin (if icon 3 2)))
			     (+ (* 2 y-margin)
				(max (cdr icon-dims) (ti-height text))))))

	(define (event-handler type xw)
	  ;; XW is the handle of the X drawable to draw in
	  (case type
	    ((expose)
	     (x-clear-window xw)

	     ;; draw the icon
	     (when icon
	       (x-draw-image icon xw
			     (cons x-margin
				   (quotient (- (x-drawable-height xw)
						(cdr icon-dims)) 2))))
	       
	     ;; draw lines of text one at a time
	     (let ((gc (x-create-gc xw
				    `((foreground . ,(get-color "black"))
				      (background . ,(get-color "white")))))
		   (x (+ (if icon (* 2 x-margin) x-margin) (car icon-dims))))

	       (do ((rest (ti-strings text) (cdr rest))
		    (y (+ (font-ascent (ti-font text))
			  (quotient (- (x-drawable-height xw)
				       (ti-height text)) 2))
		       (+ y (font-height (ti-font text)))))
		   ((null rest))
		 (x-draw-string xw gc (cons x y) (car rest) (ti-font text)))

	       (x-destroy-gc gc)))))

	;; create new window
	(setq info-window (x-create-window
			   (get-window-pos w win-size) win-size 1
			   `((background . ,(get-color "white"))
			     (border-color . ,(get-color "black")))
			   event-handler))
	(x-map-window info-window)))))
