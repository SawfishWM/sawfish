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

(require 'sm-init)
(require 'sm-common)
(provide 'sm-load)

;; Commentary:

;; This currently assumes that load-session is called before any
;; windows are adopted. If this isn't the case sm-apply-to-window
;; will need extra code to act on the restored state (which would
;; otherwise be done by functions in the add-window-hook)

(defvar sm-restored-session nil)

;;;###autoload
(defun load-session (id)
  (setq sm-restored-session nil)
  (when (file-exists-p (sm-find-file id))
    (let
	((file (open-file (sm-find-file id) 'read)))
      (when file
	(unwind-protect
	    (condition-case nil
		(while t
		  (setq sm-restored-session (cons (read file)
						  sm-restored-session)))
	      (end-of-stream
	       (setq sm-restored-session (nreverse sm-restored-session))))
	  (close-file file))
	(mapc 'sm-match-window (managed-windows))))))

;; Scan sm-restored-session for window W
(defun sm-match-window (w)
  (let
      ((elt (catch 'found
	      (mapc #'(lambda (x)
			(when (sm-match-window-to-alist w x)
			  (throw 'found x)))
		    sm-restored-session))))
    (when elt
      (setq sm-restored-session (delq elt sm-restored-session))
      (sm-apply-to-window w elt))))

;; Match window W to ALIST
(defun sm-match-window-to-alist (w alist)
  (let
      ((client-id (sm-get-window-prop w 'SM_CLIENT_ID))
       (role (nth 2 (get-x-property w 'WM_WINDOW_ROLE)))
       (class (sm-get-window-prop w 'WM_CLASS))
       (command (sm-get-window-prop w 'WM_COMMAND))
       (machine (sm-get-window-prop w 'WM_CLIENT_MACHINE)))
    (catch 'out
      (when (not (eq (not (cdr (assq 'client-id alist))) (not client-id)))
	;; one has a client-id, the other doesn't -- no match
	(throw 'out nil))

      (if client-id
	  (unless (string= client-id (cdr (assq 'client-id alist)))
	    ;; id's don't match
	    (throw 'out nil))
	;; no id, so try matching WM_COMMAND
	(when (and command (cdr (assq 'command alist))
		   (not (string= command (cdr (assq 'command alist)))))
	  (throw 'out nil)))

      (if (and role (cdr (assq 'role alist)))
	  (unless (string= role (cdr (assq 'role alist)))
	    (throw 'out nil))
	;; no role, so try matching WM_CLASS
	(when (and class (cdr (assq 'class alist))
		   (not (string= class (cdr (assq 'class alist)))))
	  (throw 'out nil)))

      ;; XXX match on WM_NAME and WM_CLIENT_MACHINE..?

      ;; if we got here it must be a match
      t)))

;; Apply saved state in ALIST to W. This assumes that the add window
;; hook hasn't been called yet
(defun sm-apply-to-window (w alist)
  (let
      (tem)
    (when (setq tem (cdr (assq 'dimensions alist)))
      (resize-window-to w (car tem) (cdr tem)))
    (mapc #'(lambda (sym)
	      (when (setq tem (cdr (assq sym alist)))
		(window-put w sym tem))) sm-saved-window-properties)
    (call-window-hook 'sm-restore-window-hook w (list alist))))

(add-hook 'before-add-window-hook 'sm-match-window)
