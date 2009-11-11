#!/bin/sh
exec rep "$0" "$@"
!#

;; sawfish-client.jl -- window manager remote client
;;
;; $Id$
;;
;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawmill.
;;
;; sawmill is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawmill is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawmill; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'sawfish.client)

(define current-repl (make-fluid))

(define (usage)
  (write standard-output "\
usage: sawfish-client OPTIONS...

where OPTIONS are any of:

        --display X     Connect to the window manager on display X
        -q              Be quiet (perform commands asynchronously)
        -f FUNCTION     Call Lisp function FUNCTION on the server
        -c COMMAND      Call the interactive Lisp function COMMAND
        -r FEATURE      Require the module called FEATURE
        -e FORM         Evaluate Lisp form FORM on the server
        -               Read lines of input until EOF, evaluating each
                         one as it is read
        --              Read forms from standard input until EOF, evaluating
                         the whole lot in one go (inside a progn)\n"))

(define (exit n) (throw 'quit n))

(define (main)
  (let ((dpy (or (get-command-line-option "-display" t)
                 (get-command-line-option "--display" t))))
    (when dpy
      (sawfish-client-display dpy)))

  (when (get-command-line-option "--help")
    (usage)
    (exit 0))

  (let ((quiet nil))

    (define (do-eval form)
      (if quiet
          (sawfish-client-eval-async form)
        (write standard-output (sawfish-client-eval form t))
        (write standard-output #\newline)))

    (let loop ((args (or command-line-args '("-"))))
      (cond ((null args))

            ((string= (car args) "-q")
             (setq quiet t)
             (loop (cdr args)))

            ((string= (car args) "-d") (loop (cdr args)))

            ((and (string= (car args) "-f") (cdr args))
             (do-eval `(,(read-from-string (cadr args))))
             (loop (cddr args)))

	    ((and (string= (car args) "-c") (cdr args))
	     (do-eval `(call-command ',(read-from-string (cadr args))))
	     (loop (cddr args)))

            ((and (string= (car args) "-r") (cdr args))
	     (do-eval `(require ',(read-from-string (cadr args))))
	     (loop (cddr args)))

	    ((and (string= (car args) "-e") (cdr args))
	     (do-eval (read-from-string (cadr args)))
	     (loop (cddr args)))

	    ((string= (car args) "-")
	     (require 'rep.io.readline)
	     (require 'rep.util.repl)
	     (format standard-output "\
sawfish %s, Copyright (C) 1999-2000 John Harper
sawfish comes with ABSOLUTELY NO WARRANTY; for details see the file COPYING\n"
		     (sawfish-client-eval 'sawfish-version))
	     (let ((r (sawfish-client-eval
		       `(progn
			  (require 'rep.util.repl)
                          (define-repl-command 'quit
                            (lambda () (throw 'sawfish-client-exit)))
			  (make-repl 'user)))))
	       (let-fluids ((current-repl r))
		 (write standard-output "\nEnter `,help' to list commands.\n")
		 (let loop ()
		   (let ((input (readline
				 (format nil (if (repl-pending r) "" "%s> ")
					 (repl-struct r)))))
		     (when input
		       (let ((out (sawfish-client-eval
				   `(progn
				      (require 'rep.util.repl)
				      (let* ((standard-output
					      (make-string-output-stream))
					     (standard-error standard-output)
					     (r ',r))
					(cons (catch 'sawfish-client-exit
                                                (and (repl-iterate r ',input)
                                                     r))
					      (get-output-stream-string
					       standard-output)))))))
			 (setq r (car out))
			 (fluid-set current-repl r)
			 (write standard-output (cdr out))
			 (when r (loop))))))))
	     (loop (cdr args)))

	    ((string= (car args) "--")
	     (let ((text (make-string-output-stream)))
	       (copy-stream standard-input text)
	       (do-eval (read-from-string
			 (concat "\(progn"
				 (get-output-stream-string text)
				 "\)")))
	       (loop (cdr args))))

	    (t (format standard-error
		       "sawfish-client: unknown option `%s'\n" (car args))
	       (format standard-error
		       "Try `sawfish-client --help' for more information.\n")
	       (exit 1))))
    (exit 0)))

(define (rl-completion-generator w)
  (sawfish-client-eval
   `(repl-completions ',(fluid current-repl) ',w)))

(main)

;; Local variables:
;; major-mode: lisp-mode
;; End:
