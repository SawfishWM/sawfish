;; describe.jl -- 
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

(require 'lisp-doc)

;;;###autoload
(defun describe-symbol (fun)
  (interactive "SSymbol:")
  "Display the documentation of a specified symbol."
  (describe-value (symbol-value fun t) fun)
  (format standard-output "\n%s\n" (or (documentation fun (symbol-value fun t))
				       "Undocumented.")))

(defun apropos-output (symbols)
  (let
      ((separator (make-string 72 ?-)))
    (mapc (lambda (sym)
	    (write standard-output separator)
	    (write standard-output #\newline)
	    (describe-symbol sym)) symbols)))

;;;###autoload
(defun apropos-function (regexp &optional all-functions)
  (interactive "sApropos functions:\nP")
  (format standard-output "Apropos %s `%s':\n\n"
	  (if all-functions "function" "command") regexp)
  (apropos-output (apropos regexp (if all-functions
				      (lambda (s)
					(and (boundp s)
					     (functionp (symbol-value s))))
				    commandp))))

;;;###autoload
(defun apropos-variable (regexp)
  (interactive "sApropos variables:")
  (format standard-output "Apropos variable `%s':\n" regexp)
  (apropos-output (apropos regexp boundp)))


;; `to-screen' variants

;;;###autoload (autoload 'describe-symbol-to-screen "describe" t)
;;;###autoload (autoload 'apropos-function-to-screen "describe" t)
;;;###autoload (autoload 'apropos-variable-to-screen "describe" t)

(define-command-to-screen
 describe-symbol-to-screen describe-symbol "SSymbol:")

(define-command-to-screen
 apropos-function-to-screen apropos-function "sApropos functions:\nP")

(define-command-to-screen
 apropos-variable-to-screen apropos-variable "sApropos variables:")
