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

(require 'custom)
(provide 'customize)

(defvar customize-program "sawfish-ui"
  "Location of the program implementing sawfish's configuration interface.")

(defvar customize-group-opt "--group")
(defvar customize-args nil)

(defvar customize-user-forms nil)
(defvar customize-user-file-read nil)
(defvar customize-user-file-dirty nil)


;; ui

;;;###autoload
(defun customize (&optional group)
  "Invoke the user-customization system."
  (interactive)
  (system (format nil "%s %s %s '%S' >/dev/null 2>&1 </dev/null &"
		  customize-program customize-args
		  (and group customize-group-opt) (or group ""))))


;; setting variables

(defun customize-read-user-file ()
  (unless customize-user-file-read
    (let
	((filename
	  (if (file-exists-p custom-user-file)
	      custom-user-file
	    (or (locate-file (concat custom-default-file ".jl") load-path)
		(error "Can't find custom-default-file")))))
      (setq customize-user-forms nil)
      (when (file-exists-p filename)
	(let
	    ((file (open-file filename 'read)))
	  (unwind-protect
	      (condition-case nil
		  (while t
		    (setq customize-user-forms (cons (read file)
						     customize-user-forms)))
		(end-of-stream))
	    (close-file file))
	  (setq customize-user-forms (nreverse customize-user-forms))))
      (setq customize-user-file-read t)
      (setq customize-user-file-dirty nil))))

(defun customize-write-user-file ()
  (when customize-user-file-dirty
    (make-directory-recursively (file-name-directory custom-user-file))
    (let
	((file (open-file custom-user-file 'write)))
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

;;;###autoload
(defun customize-set (symbol value)
  (customize-read-user-file)
  (let
      ((fun (get symbol 'custom-set))
       form)
    (if fun
	(setq fun (or (cdr (assq fun custom-set-alist)) 'custom-set-variable))
      (setq fun 'custom-set-variable))
    (setq form `(,fun ',symbol ',value
		,@(and (get symbol 'custom-require)
		       (list (list 'quote (get symbol 'custom-require))))))
    (catch 'done
      (mapc (lambda (f)
	      (when (eq (nth 1 (nth 1 f)) symbol)
		(setq customize-user-forms
		      (cons form (delq f customize-user-forms)))
		(throw 'done t)))
	    customize-user-forms)
      (setq customize-user-forms (cons form customize-user-forms)))
    (setq customize-user-file-dirty t)
    (eval form)))

(unless batch-mode
  (add-hook 'idle-hook customize-write-user-file)
  (add-hook 'before-exit-hook customize-write-user-file))
