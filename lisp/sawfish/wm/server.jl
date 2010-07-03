;; server.jl -- execute forms on demand

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

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

;; Commentary:

;; The protocol is something like:

;;	1. Send a ClientMessage event to the window identified by the
;;	property _SAWFISH_REQUEST_WIN on the root window, event mask 0,
;;	with the event's window field set to the root window, type
;;	_SAWFISH_REQUEST, format 32. data.l[0] = protocol version (1),
;;	data.l[1] = window to use for properties, data.l[2] = property
;;	containing form to evaluate, data.l[3] is non-zero if a result
;;	it required.

;;	2. The wm reads and evaluates the form from the specified
;;	property on the specified window.

;;	3. If a result is required it overwrites the form with its
;;	value, and assumes that the client will delete the property
;;	after it has read it. If no result is required, it will delete
;;	the property after having read it.

;; The result is a string, it's first byte defines whether an error
;; occurred or not, \001 if okay, \002 if an error
(define-structure sawfish.wm.server

    (export server-eval
	    server-net-init
	    server-net-exit)

    (open rep
	  rep.system
	  sawfish.wm.misc
	  sawfish.wm.windows)

  (defconst protocol-version 1)

  (define server-window nil)

  ;; Set `print-error' to non-nil for example for an async call.
  ;; Then the error is printed here.
  (define (server-eval form #!optional print-error)
    (let ((print-escape t))
      (condition-case error-data
	  ;; This enables backtrace printing inside of sawfish-client
	  (let ((%in-condition-case nil))
	    (setq form (read-from-string form))
	    (format nil "\001%S" (user-eval form)))
	(error
	 (when print-error
	   (let ((error-handler-beep nil)
		 (error-destination 'standard-error))
	     (error-handler-function 'error error-data)))
	 (format nil "\002%S" error-data)))))

  (define (server-client-message-handler w type data)
    (when (and server-window (eq w 'root)
	       (eq type '_SAWFISH_REQUEST)
	       (= (aref data 0) protocol-version))
      (let* ((window (aref data 1))
	     (prop (x-atom-name (aref data 2)))
	     (needs-result (/= (aref data 3) 0))
	     (form-data (get-x-property window prop)))
	(if needs-result
	    (set-x-property
	     window prop (server-eval (nth 2 form-data)) 'STRING 8)
	  (delete-x-property window prop)
	  (server-eval (nth 2 form-data)))
	t)))

  (define (server-net-init)
    (unless server-window
      (setq server-window (create-window 'root -100 -100 10 10))
      (set-x-property 'root '_SAWFISH_REQUEST_WIN
		      (vector server-window) 'CARDINAL 32)
      (set-x-property server-window '_SAWFISH_REQUEST_WIN
		      (vector server-window) 'CARDINAL 32)))

  (define (server-net-exit)
    (when server-window
      (delete-x-property 'root '_SAWFISH_REQUEST_WIN)
      (destroy-window server-window)
      (setq server-window nil)))

  (add-hook 'client-message-hook server-client-message-handler)
  (add-hook 'before-exit-hook server-net-exit))
