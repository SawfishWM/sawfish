;; customize.jl -- configuration user interface
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

(define-structure sawfish.wm.customize

    (export customize
	    customize-read-user-file
	    customize-write-user-file
	    customize-set)

    (open rep
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.misc)

  (defvar customize-program "sawfish-ui"
    "Location of the program implementing sawfish's configuration interface.")

  (defvar customize-group-opt "--group")

  (define customize-user-forms nil)
  (define customize-user-file-read nil)
  (define customize-user-file-dirty nil)


;;; ui

  (define (customize &optional group)
    "Invoke the user-customization system."
    (system (format nil "%s %s '%S' >/dev/null 2>&1 </dev/null &"
		    customize-program
		    (and group customize-group-opt)
		    (or group ""))))

  ;;###autoload
  (define-command 'customize customize)


;;; setting variables

  (define (customize-read-user-file)
    (unless customize-user-file-read
      (let ((filename
	     (if (file-exists-p custom-user-file)
		 custom-user-file
	       (or (locate-file (concat custom-default-file ".jl") load-path)
		   (error "Can't find custom-default-file")))))
	(setq customize-user-forms nil)
	(when (file-exists-p filename)
	  (let ((file (open-file filename 'read)))
	    (unwind-protect
		(condition-case nil
		    (while t
		      (setq customize-user-forms (cons (read file)
						       customize-user-forms)))
		  (end-of-stream))
	      (close-file file))
	    (setq customize-user-forms
		  ;; remove obsolete variables
		  (delete-if (lambda (form)
			       (get (cadadr form) 'custom-obsolete))
			     (nreverse customize-user-forms)))))
	(setq customize-user-file-read t)
	(setq customize-user-file-dirty nil))))

  (define (customize-write-user-file)
    (when customize-user-file-dirty
      (make-directory-recursively (file-name-directory custom-user-file))
      (let ((file (open-file custom-user-file 'write)))
	(when file
	  (unwind-protect
	      (progn
		(format file "\
;; sawfish user customization -- do not edit by hand!
;; sawfish version %s, written %s\n\n"
			      sawfish-version (current-time-string))
		(mapc (lambda (f)
			(format file "%S\n" f)) customize-user-forms))
	    (close-file file))
	  (setq customize-user-file-dirty nil)))))

  (define (customize-set symbol value)
    (customize-read-user-file)
    (let ((form (make-custom-form symbol value)))
      (catch 'done
	(mapc (lambda (f)
		(when (eq (nth 1 (nth 1 f)) symbol)
		  (setq customize-user-forms
			(cons form (delq f customize-user-forms)))
		  (throw 'done t)))
	      customize-user-forms)
	(setq customize-user-forms (cons form customize-user-forms)))
      (setq customize-user-file-dirty t)
      (custom-eval form)))

  (unless batch-mode
    (add-hook 'idle-hook customize-write-user-file)
    (add-hook 'before-exit-hook customize-write-user-file)))
