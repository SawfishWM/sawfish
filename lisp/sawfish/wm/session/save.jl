;; sm-save.jl -- session manager code to save the current session
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

(define-structure sawfish.wm.session.save

    (export save-session)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.session.init
	  sawfish.wm.session.util)

  ;; create an alist defining the current state of window W
  (define (sm-get-window-state w)
    (let ((alist (apply nconc (mapcar (lambda (fun)
					(fun w)) sm-window-save-functions))))
      (mapc (lambda (sym)
	      (when (window-get w sym)
		(setq alist (cons (cons sym (window-get w sym)) alist))))
	    sm-saved-window-properties)

      ;; some standard items
      (setq alist `((name . ,(window-name w))
		    (dimensions . ,(window-dimensions w))
		    (client-id . ,(sm-get-window-prop w 'SM_CLIENT_ID))
		    (role . ,(nth 2 (get-x-property w 'WM_WINDOW_ROLE)))
		    (class . ,(sm-get-window-prop w 'WM_CLASS))
		    (command . ,(sm-get-window-prop w 'WM_COMMAND))
		    (machine . ,(sm-get-window-prop w 'WM_CLIENT_MACHINE))
		    ,@alist))
      alist))

  (define (sm-print-alist stream alist)
    (if (null alist)
	(write stream "()\n")
      (let (not-first)
	(mapc (lambda (x)
		(format stream "%s%S" (if not-first "\n " "\(") x)
		(setq not-first t))
	      alist)
	(write stream "\)\n\n"))))

  (define (save-session id)
    (unless (file-exists-p sm-save-directory)
      (make-directory-recursively sm-save-directory))
    (let ((file (open-file (sm-find-file id) 'write)))
      (when file
	(unwind-protect
	    (progn
	      (format file (concat ";; session saved for %s@%s\n"
				   ";; sawfish version %s; %s\n\n")
		      (user-login-name) (system-name)
		      sawfish-version (current-time-string))
	      (map-windows (lambda (w)
			     (sm-print-alist file (sm-get-window-state w))))
	      t)
	  (close-file file))))))
