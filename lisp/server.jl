;; server.jl -- execute forms on demand
;; $Id$

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

(provide 'server)

;; Commentary:

;; The protocol is something like:

;;	1. Send a ClientMessage event to the window identified by the
;;	property _SAWMILL_REQUEST_WIN on the root window, event mask 0,
;;	with the event's window field set to the root window, type
;;	_SAWMILL_REQUEST, format 32. data.l[0] = window to use for
;;	properties, data.l[1] = property containing form to evaluate,
;;	data.l[2] is non-zero if a result it required.

;;	2. The wm reads and evaluates the form from the specified
;;	property on the specified window.

;;	3. If a result is required it overwrites the form with its
;;	value, and assumes that the client will delete the property
;;	after it has read it. If no result is required, it will delete
;;	the property after having read it.

(defvar server-window nil)

(defun server-client-message-handler (w type data)
  (when (and (eq w 'root) (eq type '_SAWMILL_REQUEST))
    (let*
	((window (aref data 0))
	 (prop (x-atom-name (aref data 1)))
	 (needs-result (/= (aref data 2) 0))
	 (form-data (get-x-property window prop))
	 form value)
      (condition-case error-data
	  (progn
	    (setq form (read-from-string (nth 2 form-data) 0))
	    (setq value (eval form))
	    (if needs-result
		(set-x-property window prop
				(let
				    ((print-escape t))
				  (format nil "%S" value))
				'STRING 8)
	      (delete-x-property window prop)))
	(error
	 (if needs-result
	     (set-x-property window prop
			     (let
				 ((print-escape t))
			       (format nil "error--> %S" error-data))
			     'STRING 8)
	   (delete-x-property window prop))))
      t)))

(defun server-init ()
  (unless server-window
    (setq server-window (create-window 'root -100 -100 10 10))
    (set-x-property 'root '_SAWMILL_REQUEST_WIN
		    (vector server-window) 'CARDINAL 32)))
(defun server-exit ()
  (when server-window
    (delete-x-property 'root '_SAWMILL_REQUEST_WIN)
    (destroy-window server-window)))


;; initialisation

(server-init)
(add-hook 'client-message-hook 'server-client-message-handler)
(add-hook 'before-exit-hook 'server-exit)
