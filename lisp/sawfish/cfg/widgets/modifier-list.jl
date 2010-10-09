;; modifier-list.jl -- widget for a list of modifier keys
;;
;; Author: John Harper <john@dcs.warwick.ac.uk>
;;
;; Copyright (C) 2000 Eazel, Inc.
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(define-structure sawfish.cfg.widgets.modifier-list

    (export )

    (open rep
          rep.regexp
          sawfish.gtk.widget)

  ;; XXX should read the non-null modifiers from the wm?

  (define modifiers '(shift control meta alt hyper super))

  (define (list->symbol x)
    (if (null x)
	nil
      (intern (mapconcat symbol-name x #\space))))

  (define (symbol->list x)
    (if (not x)
	'()
      (mapcar (lambda (x)
		(intern (string-downcase x)))
	      (string-split "[-,; \t]" (symbol-name x)))))

  (define (validp x)
    (and (listp x)
	 (not (null x))
	 (let loop ((rest x))
	   (cond ((null rest) t)
		 ((not (memq (car rest) modifiers)) nil)
		 (t (loop (cdr rest)))))))

  (define (make-item changed-callback)
    (let (base)

      (define (real-changed)
	(let ((value (widget-ref base)))
	  (when (validp (symbol->list value))
	    (changed-callback))))

      (setq base (make-widget `(symbol ,@modifiers) real-changed))

      (lambda (op)
	(case op
	  ((set) (lambda (x) (widget-set base (list->symbol x))))
	  ((ref) (lambda () (symbol->list (widget-ref base))))
	  ((validp) validp)
	  (t (base op))))))

  (define-widget-type 'modifier-list make-item))
