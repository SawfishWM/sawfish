#| autoloader.jl -- abstractions for autoloading `definitions'

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.util.autoloader

    (export make-autoloader
	    autoloader-ref)

    (open rep rep.structures)

  ;; (GETTER SYMBOL) => VALUE
  ;; (SETTER SYMBOL VALUE [REST...])

  ;; used to tag autoload cells (in the car, cdr is module name)
  (define autoload-tag (make-symbol "autoload"))

  ;; Return a function of two args (SYMBOL MODULE) that can be
  ;; used to create autoload definitions. GETTER should return
  ;; the current definition of SYMBOL; SETTER should set the
  ;; current definition to VALUE

  (define (make-autoloader getter setter)
    (lambda (symbol module . rest)
      (unless (getter symbol)
	(apply setter symbol (cons autoload-tag module) rest))))

  ;; Return a function of one arg (SYMBOL) that returns the definition
  ;; of SYMBOL. If an autoload has been installed for that identifier,
  ;; load it, then dereference SYMBOL for a second time.

  (define (autoloader-ref getter)
    (lambda (symbol)
      (let ((value (getter symbol)))
	(if (eq (car value) autoload-tag)
	    (progn
	      (intern-structure (cdr value))
	      (getter symbol))
	  value)))))
