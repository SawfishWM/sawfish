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

(provide 'sm-init)

(defvar sm-save-directory "~/.sawmill/sessions")

(defvar sm-saved-window-properties '(depth type sticky ignored iconified
				     unmaximized-geometry frame-style)
  "List of window properties saved with the session.")

(defvar sm-window-save-functions nil
  "List of functions called when the state of each window is saved. Each
function should return a list of alist elements that will be saved in
the state file.")

(defvar sm-restore-window-hook nil
  "List of functions called when the state of a window is restored. Each is
called with args (WINDOW ALIST), where ALIST defines the state saved for
the window.")

(defun sm-find-file (id)
  (expand-file-name id sm-save-directory))

(defun delete-session (id)
  (let
      ((file (sm-find-file id)))
    (when (file-exists-p file)
      (delete-file file))))


;; initialisation

(when (and (not batch-mode) sm-client-id)
  ;; 1. setup all session manager properties

  ;; remove any --sm-client-id option from saved-command-line-args
  (let
      ((args saved-command-line-args)
       tem)
    (while (cdr args)
      (when (string-match "^--sm-client-id" (car (cdr args)))
	(setq tem (car (cdr args)))
	(rplacd args (cdr (cdr args)))
	(unless (string-match "^--sm-client-id=" tem)
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
  (sm-set-property "ProcessId" (format nil "%d" (getpid)))
  (sm-set-property "Program" (car saved-command-line-args))
  (sm-set-property "UserId" (user-login-name))
  (sm-set-property
   "Environment" (apply 'nconc
			(mapcar #'(lambda (e)
				    (when (string-match "=" e)
				      (list (substring e 0 (match-start))
					    (substring e (match-end)))))
				process-environment)))

  ;; 2. load the session if it exists
  (when (file-exists-p (sm-find-file sm-client-id))
    (load-session sm-client-id)))
