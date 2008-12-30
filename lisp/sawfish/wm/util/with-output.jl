;; with-output.jl -- call a command/function redirecting stdout
;; $Id: with-output.jl,v 1.7 2000/09/11 07:44:42 john Exp $

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

(define-structure sawfish.wm.util.with-output

    (export call-with-output-to-screen
	    with-output-to-screen)

    (open rep
	  rep.regexp
	  sawfish.wm.misc)

  (define-structure-alias with-output sawfish.wm.util.with-output)

  (define (call-with-output-to-screen thunk)
    "Call the zero-parameter function THUNK with the `standard-output'
stream bound so that output is collected. After THUNK returns, the
emitted text will be display on the screen."

    (let ((standard-output (make-string-output-stream)))
      (unwind-protect
	  (thunk)
	(let ((out (get-output-stream-string standard-output)))
	  (unless (string= out "")
	    ;; display-message doesn't grok TAB characters,
	    ;; this is grossly inefficient; wtf..
	    (while (string-match "\t" out)
	      (setq out (concat (substring out 0 (match-start))
				#\space (substring out (match-end)))))
	    (display-message out))))))

  (defmacro with-output-to-screen (#!rest forms)
    "Evaluate FORMS. Any data they print to standard-output will be
displayed on the screen after they return."
    `(call-with-output-to-screen (lambda () ,@forms))))
