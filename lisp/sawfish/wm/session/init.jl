;; sm-init.jl -- session manager code loaded on startup

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

(define-structure sawfish.wm.session.init

    (export sm-find-file
	    sm-add-saved-properties
	    sm-add-restored-properties
	    sm-get-window-prop
	    sm-save-yourself
	    sm-init)

    (open rep
	  rep.regexp
	  rep.system
	  rep.io.files
	  rep.io.processes
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.session.util)

  (define sm-client-id nil
          "A string identifying the current session.")

  (define sm-prefix nil
          "A string used to uniquify the session file.")

  ;; this is before both the panel and gmc..?
  (defconst sm-gsm-priority 30)

  (defvar sm-save-directory "~/.sawfish/sessions")

  (defvar sm-saved-window-properties nil
    "List of window properties saved with the session.")

  (defvar sm-restored-window-properties nil
    "Extra window properties restored from the session.")

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

  (defvar sm-sloppy-id-matching nil
    "When loading sessions, the algorithm that matches saved session data
to running clients requires that if one has a session id, then so must
the other, and they must match. Setting this variable to true turns
that feature off, allowing some broken clients to be session managed.")

;;; utilities

  ;; PREFIX may be null
  (define (sm-find-file id prefix)
    (if prefix
	(expand-file-name (format nil "%s-%s" prefix id) sm-save-directory)
      (expand-file-name id sm-save-directory)))

  (define (sm-add-saved-properties #!rest props)
    (mapc (lambda (p)
	    (or (memq p sm-saved-window-properties)
		(setq sm-saved-window-properties
		      (cons p sm-saved-window-properties))))
	  props))

  (define (sm-add-restored-properties #!rest props)
    (mapc (lambda (p)
	    (or (memq p sm-restored-window-properties)
		(setq sm-restored-window-properties
		      (cons p sm-restored-window-properties))))
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
				   (eq (nth 1 tem) 32)
				   (not (zerop (aref (nth 2 tem) 0))))
			      (aref (nth 2 tem) 0))
			     ((window-group-id w))
			     ((window-transient-p w)))))
	  (and leader (nth 2 (get-x-property leader prop))))))

  (define (make-unique-prefix) (number->string (current-utime) 36))

  ;; callback

  (define (sm-save-yourself)
    (require 'sawfish.wm.session.save)

    ;; We're not allowed to reuse the files used to save sessions.
    ;; So generate a new name each time.
    (setq sm-prefix (make-unique-prefix))

    (save-session (sm-find-file sm-client-id sm-prefix))
    (set-restart-command)
    (set-discard-command))

  ;; But the session manager doesn't delete files that we
  ;; leave around when we exit normally..

  (define (before-exit)
    (unless (or (eq (exit-type) 'session-quit)
		(not sm-client-id))
      (remove-sm-options)
      (let ((file (sm-find-file sm-client-id sm-prefix)))
	(when (file-exists-p file)
	  (delete-file file)))))

  (add-hook 'before-exit-hook before-exit)

;;; initialisation

  (define (remove-sm-options)
    ;; remove any sm options from saved-command-line-args
    (let loop ((args saved-command-line-args))
      (when (cdr args)
	(if (string-match "^(--sm-client-id|-clientId|--sm-prefix)"
			  (cadr args))
	    (progn
	      (if (string-match "=" (cadr args))
		  (rplacd args (cddr args))
		(rplacd args (cdddr args)))
	      (loop args))
	  (loop (cdr args))))))

  (define (set-discard-command)
    (sm-set-property
     "DiscardCommand"
     (list "rm" "-f" (local-file-name (sm-find-file sm-client-id sm-prefix)))))

  (define (set-restart-command)
    (remove-sm-options)
    (if sm-prefix
	(rplacd saved-command-line-args (list* "--sm-client-id" sm-client-id
					       "--sm-prefix" sm-prefix
					       (cdr saved-command-line-args)))
      (rplacd saved-command-line-args (list* "--sm-client-id" sm-client-id
					     (cdr saved-command-line-args))))
    (sm-set-property "RestartCommand" saved-command-line-args))

  (define (sm-init id prefix)
    (when (setq sm-client-id (sm-connect id))

      ;; 1. setup all session manager properties
      (setq sm-prefix prefix)

      ;; XXX should I set this to SmRestartImmediately (2) instead
      ;; XXX of SmRestartIfRunning (0) ?
      (sm-set-property "RestartStyleHint" 2)

      (remove-sm-options)
      (sm-set-property "CloneCommand" saved-command-line-args)

      (set-restart-command)

      (sm-set-property "CurrentDirectory" (local-file-name default-directory))
      (sm-set-property "ProcessId" (format nil "%d" (process-id)))
      (sm-set-property "Program" (car saved-command-line-args))
      (sm-set-property "UserId" (user-login-name))

      ;; we need to start before gmc, otherwise it won't hint its icons
      (sm-set-property "_GSM_Priority" sm-gsm-priority)

      ;; 2. load the session if it exists
      (let ((file (sm-find-file sm-client-id sm-prefix)))
	(when (file-exists-p file)
	  (require 'sawfish.wm.session.load)
	  (load-session file)
	  (set-discard-command))))))
