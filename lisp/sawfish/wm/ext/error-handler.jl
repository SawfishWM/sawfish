;; error-handler.jl -- replace the standard rep error handler
;; $Id: error-handler.jl,v 1.7 2002/04/21 03:39:34 jsh Exp $

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

(define-structure sawfish.wm.ext.error-handler

    (export retrieve-errors
	    display-errors)

    (open rep
	  rep.system
	  rep.data.ring
	  sawfish.wm.misc
	  sawfish.wm.custom
	  sawfish.wm.commands)

  (define-structure-alias error-handler sawfish.wm.ext.error-handler)

  (defgroup error-handling "Error Handling"
    :group misc)

  (defcustom error-handler-beep t
    "Beep when errors occur."
    :type boolean
    :group (misc error-handling))

  (defcustom error-destination 'standard-error
    "Display error messages to: \\w"
    :type (choice nowhere screen standard-error both)
    :group (misc error-handling))

  ;; ring buffer for containing error messages
  (define error-ring (make-ring))

;;; code

  (define (error->string err data)
    (format nil "%s: %s"
	    (or (get err 'error-message) err)
	    (mapconcat (lambda (x)
			 (format nil "%s" x)) data ", ")))

  (define (handler err data)
    (let ((text (error->string err data)))
      (ring-append error-ring
		   (format nil "[%s] %s"
			   (current-time-string nil "%Y-%m-%d %H:%M:%S") text))
      (when error-handler-beep
	(beep))
      (case error-destination
	((screen)
	 (display-message text))
	((standard-error)
	 (write standard-error text)
	 (write standard-error #\newline))
	((both)
	 (display-message text)
	 (write standard-error text)
	 (write standard-error #\newline)))))

  (define (retrieve-errors) (ring->list error-ring))

  (define (display-errors)
    "Display all errors that have occurred to the screen."
    (let ((errors (retrieve-errors)))
      (display-message (if errors
			   (mapconcat identity errors #\newline)
			 (_ "No errors.")))))

  (define-command 'display-errors display-errors)

  ;; install our error handler as the system-wide handler
  (setq error-handler-function handler))
