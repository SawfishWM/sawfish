;; make-theme.jl -- support for theme builder
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.theming.make-theme

    (export make-theme)

    (open rep
	  rep.regexp
	  sawfish.wm.images
	  sawfish.wm.colors
	  sawfish.wm.fonts
	  sawfish.wm.gaol)

  (define-structure-alias make-theme sawfish.wm.theming.make-theme)

  (define (make-frames patterns-alist frame-alist)
    (let ((image-cache '()))
      
      (define (make-image file)
	(or (cdr (assoc file image-cache))
	    (let
		((img (gaol-eval `(make-image ',file))))
	      (setq image-cache (cons (cons file img) image-cache))
	      img)))

      (define (make-pattern def)
	(mapcar (lambda (elt)
		  (let ((state (car elt))
			(value (cdr elt)))
		    (cond ((stringp value)
			   (setq value (get-color value)))
			  ((and (consp value) (stringp (car value)))
			   (let
			       ((img (make-image (car value))))
			     (when img
			       (mapc (lambda (attr)
				       (cond
					((eq (car attr) 'tiled)
					 (image-put img 'tiled (cdr attr)))
					((eq (car attr) 'border)
					 (apply set-image-border
						img (cdr attr)))))
				     (cdr value)))
			     (setq value img))))
		    (cons state value))) def))

      (let ((loaded-patterns (mapcar (lambda (cell)
				       (cons (car cell)
					     (make-pattern (cdr cell))))
				     patterns-alist)))
	    
	(define (make-frame-part def)
	  (mapcar (lambda (cell)
		    (cons (car cell)
			  (cond ((and (eq (car cell) 'text)
				      (symbolp (cdr cell)))
				 (gaol-eval (cdr cell)))
				((and (memq (car cell)
					    '(foreground background))
				      (stringp (cdr cell)))
				 (if (string-match "^#" (cdr cell))
				     ;; color
				     (get-color (cdr cell))
				   (cdr (assoc (cdr cell) loaded-patterns))))
				((and (eq (car cell) 'font)
				      (stringp (cdr cell)))
				 (get-font (cdr cell)))
				(t
				 (cdr cell))))) def))

	(mapcar (lambda (cell)
		  (cons (car cell) (mapcar make-frame-part (cdr cell))))
		frame-alist))))
			      
  (define (make-theme patterns-alist frame-alist mapping-alist)
    (let ((real-frames (make-frames patterns-alist frame-alist)))
      (lambda (w type)
	(declare (unused w))
	(let ((frame-name (or (cdr (assq type mapping-alist)))))
	  (and frame-name (cdr (assoc frame-name real-frames)))))))

  (gaol-define 'make-theme make-theme))
