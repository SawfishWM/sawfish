;; sm-load.jl -- session manager code to reload a saved session
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

;; Commentary:

;; This currently assumes that load-session is called before any
;; windows are adopted. If this isn't the case apply-to-window
;; will need extra code to act on the restored state (which would
;; otherwise be done by functions in the add-window-hook)

(define-structure sawfish.wm.session.load

    (export load-session)

    (open rep
	  rep.system
	  rep.io.files
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.session.init
	  sawfish.wm.session.util)

  (define restored-session nil)

  (define (load-session filename)
    (setq restored-session nil)
    (when (file-exists-p filename)
      (let ((file (open-file filename 'read)))
	(when file
	  (unwind-protect
	      (condition-case nil
		  (while t
		    (setq restored-session (cons (read file)
						 restored-session)))
		(end-of-stream
		 (setq restored-session (nreverse restored-session))))
	    (close-file file))
	  (map-windows match-window)))))

  ;; Scan restored-session for window W
  (define (match-window w)
    (let ((item (catch 'found
		  (mapc (lambda (x)
			  (when (match-window-to-alist w x)
			    (throw 'found x)))
			restored-session))))
      (when item
	(setq restored-session (delq item restored-session))
	(apply-to-window w item))))

  ;; Match window W to ALIST
  (define (match-window-to-alist w alist)
    (let ((client-id (sm-get-window-prop w 'SM_CLIENT_ID))
	  (role (nth 2 (get-x-property w 'WM_WINDOW_ROLE)))
	  (class (sm-get-window-prop w 'WM_CLASS))
	  (command (sm-get-window-prop w 'WM_COMMAND)))
      (catch 'out
	(when (not (eq (not (cdr (assq 'client-id alist))) (not client-id)))
	  ;; one has a client-id, the other doesn't -- no match
	  (throw 'out nil))

	(cond (client-id
	       (unless (string= client-id (cdr (assq 'client-id alist)))
		 ;; id's don't match
		 (throw 'out nil)))
	      ;; no SM_CLIENT_ID, so try matching WM_COMMAND
	      ((and command (cdr (assq 'command alist)))
	       (unless (string= command (cdr (assq 'command alist)))
		 (throw 'out nil)))
	      ;; no WM_COMMAND so no match
	      (t
	       (throw 'out nil)))

	(if (and role (cdr (assq 'role alist)))
	    (unless (string= role (cdr (assq 'role alist)))
	      (throw 'out nil))
	  ;; no WM_WINDOW_ROLE, so try matching WM_CLASS
	  (when (and class (cdr (assq 'class alist))
		     (not (string= class (cdr (assq 'class alist)))))
	    (throw 'out nil)))

	;; XXX match on WM_NAME and WM_CLIENT_MACHINE..?

	;; if we got here it must be a match
	t)))

  ;; Apply saved state in ALIST to W. This assumes that the add window
  ;; hook hasn't been called yet
  (define (apply-to-window w alist)
    (let (tem)
      (when (setq tem (cdr (assq 'dimensions alist)))
	(resize-window-to w (car tem) (cdr tem)))
      (mapc (lambda (lst)
	      (mapc (lambda (sym)
		      (when (setq tem (cdr (assq sym alist)))
			(window-put w sym tem))) lst))
	    (list sm-saved-window-properties sm-restored-window-properties))
      (call-window-hook 'sm-restore-window-hook w (list alist))))

  (add-hook 'before-add-window-hook match-window))
