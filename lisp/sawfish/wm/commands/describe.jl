;; describe.jl --

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.commands.describe

    (export describe-symbol
	    apropos-function
	    apropos-variable)

    (open rep
	  rep.lang.doc
	  sawfish.wm.commands)

  (define-structure-alias describe sawfish.wm.commands.describe)

  (define (describe-symbol fun)
    "Print the documentation of a specified symbol to the standard output ."
    (describe-value (symbol-value fun t) fun)
    (format standard-output "\n%s\n"
	    (or (documentation fun nil (symbol-value fun t)) "Undocumented.")))

  ;;###autoload
  (define-command 'describe-symbol describe-symbol
    #:spec "SSymbol:" #:class 'advanced)
  (define-command-to-screen 'describe-symbol-to-screen describe-symbol
    #:spec "SSymbol:" #:class 'advanced
    #:doc "Enter a name of var/func, and prints its doc.")

  (define (apropos-output symbols)
    (let ((separator (make-string 72 ?-)))
      (mapc (lambda (sym)
	      (write standard-output separator)
	      (write standard-output #\newline)
	      (describe-symbol sym)) symbols)))

  (define (apropos-function regexp #!optional all-functions)
    (format standard-output "Apropos %s `%s':\n\n"
	    (if all-functions "function" "command") regexp)
    (apropos-output (apropos regexp (if all-functions
					(lambda (s)
					  (and (boundp s)
					       (functionp (symbol-value s))))
				      commandp))))

  ;;###autoload
  (define-command 'apropos-function apropos-function
    #:spec "sApropos functions:\nP" #:class 'advanced)
  (define-command-to-screen 'apropos-function-to-screen apropos-function
    #:spec "sApropos functions:\nP" #:class 'advanced)

  (define (apropos-variable regexp)
    (format standard-output "Apropos variable `%s':\n" regexp)
    (apropos-output (apropos regexp boundp)))

  ;;###autoload
  (define-command 'apropos-variable apropos-variable
    #:spec "sApropos variables:" #:class 'advanced)
  (define-command-to-screen 'apropos-variable-to-screen apropos-variable
    #:spec "sApropos variables:" #:class 'advanced))
