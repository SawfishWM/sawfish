;; sm-init.jl -- session manager code loaded on startup
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

(define-structure sawfish.wm.session.init

    (export sm-find-file
	    delete-session
	    sm-add-saved-properties
	    sm-get-window-prop
	    sm-save-yourself
	    sm-init)

    (open rep
	  rep.regexp
	  rep.system
	  rep.io.files
	  rep.io.streams
	  rep.io.processes
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.session.util)

  (defvar sm-client-id nil
    "A string identifying the current session.")

  ;; this is before both the panel and gmc..?
  (defconst sm-gsm-priority 30)

  (defvar sm-save-directory "~/.sawfish/sessions")

  (defvar sm-saved-window-properties nil
    "List of window properties saved with the session.")

  (defvar sm-window-save-functions nil
    "List of functions called when the state of each window is saved. Each
function should return a list of alist elements that will be saved in
the state file.")

  (defvar sm-restore-window-hook nil
    "List of functions called when the state of a window is restored. Each is
called with args (WINDOW ALIST), where ALIST defines the state saved for
the window.")

  (defvar sm-after-restore-hook nil
    "Hook called after loading a saved session.")

;;; utilities

  (define (sm-find-file id)
    (expand-file-name id sm-save-directory))

  (define (delete-session id)
    (let ((file (sm-find-file id)))
      (when (file-exists-p file)
	(delete-file file))))

  (define (sm-add-saved-properties #!rest props)
    (mapc (lambda (p)
	    (or (memq p sm-saved-window-properties)
		(setq sm-saved-window-properties
		      (cons p sm-saved-window-properties))))
	  props))

  ;; find PROP associated with W, or nil
  (define (sm-get-window-prop w prop)
    ;; first look in the window itself,
    (or (nth 2 (get-x-property w prop))
	;; else try the leader
	(let* (tem
	       (leader (cond ((and (setq tem (get-x-property
					      w 'WM_CLIENT_LEADER))
				   (eq (car tem) 'WINDOW)
				   (eq (nth 1 tem) 32))
			      (aref (nth 2 tem) 0))
			     ((window-group-id w))
			     ((window-transient-p w)))))
	  (and leader (nth 2 (get-x-property leader prop))))))

;; callback

  (define (sm-save-yourself)
    (require 'sawfish.wm.session.save)
    (save-session sm-client-id))

;;; initialisation

  (define (sm-init id)
    (when (setq sm-client-id (sm-connect id))
      ;; 1. setup all session manager properties

      ;; remove any --sm-client-id option from saved-command-line-args
      (let ((args saved-command-line-args)
	    tem)
	(while (cdr args)
	  (when (string-match "^(--sm-client-id|-clientId)" (car (cdr args)))
	    (setq tem (car (cdr args)))
	    (rplacd args (cdr (cdr args)))
	    (unless (string-match "^(--sm-client-id|-clientId)=" tem)
	      (rplacd args (cdr (cdr args)))))
	  (setq args (cdr args))))

      ;; XXX should I set this to SmRestartImmediately (2) instead
      ;; XXX of SmRestartIfRunning (0) ?
      (sm-set-property "RestartStyleHint" 0)

      (sm-set-property "CloneCommand" saved-command-line-args)

      ;; fix saved-command-line-args to include the client-id
      (rplacd saved-command-line-args (list* "--sm-client-id" sm-client-id
					     (cdr saved-command-line-args)))

      (sm-set-property "RestartCommand" saved-command-line-args)

      (sm-set-property "CurrentDirectory" default-directory)
      (sm-set-property
       "DiscardCommand" (list "rm" "-f" (local-file-name
					 (sm-find-file sm-client-id))))
      (sm-set-property "ProcessId" (format nil "%d" (process-id)))
      (sm-set-property "Program" (car saved-command-line-args))
      (sm-set-property "UserId" (user-login-name))

      ;; we need to start before gmc, otherwise it won't hint its icons
      (sm-set-property "_GSM_Priority" sm-gsm-priority)

      ;; 2. load the session if it exists
      (when (file-exists-p (sm-find-file sm-client-id))
	(require 'sawfish.wm.session.load)
	(load-session sm-client-id)))))
