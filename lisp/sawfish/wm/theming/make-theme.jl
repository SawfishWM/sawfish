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

(require 'gaol)
(provide 'make-theme)
			      
(defun make-theme (patterns-alist frame-alist mapping-alist)
  (let*
      ((make-pattern
	(lambda (def)
	  (mapcar (lambda (x)
		    (cond ((stringp x)
			   (get-color x))
			  ((and (consp x) (stringp (car x)))
			   (let
			       ((img (gaol-eval `(make-image ',(car x)))))
			     (when img
			       (mapc (lambda (attr)
				       (cond
					((eq (car attr) 'tiled)
					 (image-put img 'tiled (cdr attr)))
					((eq (car attr) 'border)
					 (apply set-image-border
						img (cdr attr)))))
				     (cdr x)))
			     img))))
		  def)))

       (loaded-patterns
	(mapcar (lambda (cell)
		  (cons (car cell) (make-pattern (cdr cell))))
		patterns-alist))

       (make-frame-part
	(lambda (def)
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
				 (cdr cell))))) def)))

       (real-frames
	(mapcar (lambda (cell)
		  (cons (car cell) (mapcar make-frame-part (cdr cell))))
		frame-alist)))

    (lambda (w type)
      (let
	  ((frame-name (or (cdr (assq type mapping-alist))
			   (cdr (assq 'default mapping-alist)))))
	(if frame-name
	    (or (cdr (assoc frame-name real-frames)) nil-frame)
	  nil-frame)))))

(gaol-add-function 'make-theme)
