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

(defcustom customize-program "sawmill-ui"
  "Location of the program implementing sawmill's configuration interface."
  :group customize
  :type program-name)

(defcustom customize-show-symbols nil
  "Show variable names of each customization option."
  :group customize
  :type boolean)

;; the active user interface process
(defvar customize-process nil)

;; output from the user-interface process that's received but not
;; yet processed
(defvar customize-pending nil)

(defvar customize-user-forms nil)
(defvar customize-read-user-file nil)
(defvar customize-dirty-user-file nil)


;; defcustom's for some built-in variables

(defcustom default-font nil
  "Font used for text which doesn't have a font explicitly specified."
  :group appearance
  :type font)


;; subprocess handling

(defun customize-start-process ()
  (unless (and customize-process (process-in-use-p customize-process))
    (when customize-process
      (kill-process customize-process))
    (setq customize-process (make-process 'customize-filter
					  'customize-sentinel))
    (set-process-error-stream customize-process nil)
    (or (start-process customize-process customize-program "--backend")
	(error "Can't start customize backend: %s" customize-program))))

(defun customize-stop-process ()
  (when customize-process
    (when (process-in-use-p customize-process)
      (kill-process customize-process))
    (setq customize-process nil)
    (customize-write-user-file)))

(defun customize-filter (output)
  (setq output (concat customize-pending output))
  (setq customize-pending nil)
  (condition-case nil
      (let
	  ((result (read-from-string output)))
	(cond ((eq (car result) 'ui-exit)
	       (customize-stop-process))
	      ((eq (car result) 'ui-refresh)
	       (format customize-process "%S\n" (customize-ui-spec)))
	      (t
	       (eval result))))
    (end-of-stream
     (setq customize-pending output))))

(defun customize-sentinel (process)
  (when (and customize-process (not (process-in-use-p customize-process)))
    (setq customize-process nil)
    (customize-write-user-file)))


;; ui

(defun customize-symbol-spec (symbol)
  (let
      ((type (or (get symbol 'custom-type) 'boolean))
       (doc (or (documentation symbol t) (symbol-name symbol)))
       (value (funcall (or (get symbol 'custom-get) 'symbol-value) symbol)))
    (when customize-show-symbols
      (setq doc (format nil "%s\n[%s]" doc (symbol-name symbol))))
    (cond ((eq type 'boolean)
	   `(toggle ,doc
		    :variable ,symbol
		    :value ,value))

	  ((eq type 'number)
	   `(hbox (number :variable ,symbol
			  :value ,(if (numberp value) value 0)
			  :allow-nil ,(get symbol 'custom-allow-nil))
		  (label ,doc)))

	  ;; XXX all but the first should have their own widget types
	  ((memq type '(string file-name program-name))
	   `(hbox (string :variable ,symbol
			  :value ,(if (stringp value) value "")
			  :allow-nil ,(get symbol 'custom-allow-nil))
		  (label ,doc)))

	  ((memq type '(font color))
	   `(hbox (,type :variable ,symbol
			 :value ,value
			 :allow-nil ,(get symbol 'custom-allow-nil))
		  (label ,doc)))

	  ((and (consp type) (eq (car type) 'set))
	   `(hbox (set ,(cdr type)
		       :variable ,symbol
		       :value ,value)
		  (label ,doc)))

	  (t
	   (let
	       ((fun (get symbol 'custom-widget)))
	     (when fun
	       (funcall fun symbol value doc)))))))

(defun customize-ui-spec ()
  (mapc 'require custom-required)
  (list*
   'pages
   (mapcar #'(lambda (group-list)
	       (let
		   ((group (car group-list))
		    (spec (mapcar 'customize-symbol-spec (cdr group-list))))
		 (list (or (get group 'custom-group-doc)
			   (symbol-name group))
		       (if (get group 'custom-group-widget)
			   (funcall
			    (get group 'custom-group-widget) group spec)
			 (list* 'vbox spec)))))
	   custom-groups)))

;;;###autoload
(defun customize ()
  (interactive)
  (if customize-process
      (error "Customize already active")
    (customize-start-process)
    (format customize-process "%S\n" (customize-ui-spec))))


;; setting variables

(defun customize-read-user-file ()
  (unless customize-read-user-file
    (setq customize-user-forms nil)
    (when (file-exists-p custom-user-file)
      (let
	  ((file (open-file custom-user-file 'read)))
	(unwind-protect
	    (condition-case nil
		(while t
		  (setq customize-user-forms (cons (read file)
						   customize-user-forms)))
	      (end-of-stream))
	  (close-file file))
	(setq customize-user-forms (nreverse customize-user-forms))))
    (setq customize-read-user-file t)
    (setq customize-dirty-user-file nil)))

(defun customize-write-user-file ()
  (when customize-dirty-user-file
    (let
	((file (open-file custom-user-file 'write)))
      (when file
	(unwind-protect
	    (progn
	      (format file "\
;; sawmill user customization -- do not edit by hand!
;; sawmill version %s, written %s\n\n"
		      sawmill-version (current-time-string))
	      (mapc #'(lambda (f)
			(format file "%S\n" f)) customize-user-forms))
	  (close-file file))
	(setq customize-dirty-user-file nil)))))

(defun customize-set (symbol value)
  (customize-read-user-file)
  (let
      ((form `(,(or (get symbol 'custom-set) 'custom-set-variable)
	       ',symbol ',value
	       ,@(and (get symbol 'custom-require)
		      (list (list 'quote (get symbol 'custom-require)))))))
    (catch 'done
      (mapc #'(lambda (f)
		(when (eq (nth 1 (nth 1 f)) symbol)
		  (setq customize-user-forms
			(cons form (delq f customize-user-forms)))
		  (throw 'done t)))
	    customize-user-forms)
      (setq customize-user-forms (cons form customize-user-forms)))
    (setq customize-dirty-user-file t)
    (eval form)))


;; some blurb

(defgroup about "About"
  :widget (lambda ()
	    (list 'label (format nil "\
Sawmill %s

Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

This is free software -- you are welcome to redistribute it and/or \
modify it under the terms of the GNU General Public License as \
published by the Free Software Foundation; either version 2, or \
(at your option) any later version.

Sawmill is distributed in the hope that it will be useful, but \
WITHOUT ANY WARRANTY; without even the implied warranty of \
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \
GNU General Public License for more details.

Visit the Sawmill homepage at http://www.dcs.warwick.ac.uk/~john/sw/sawmill/
"
				 sawmill-version))))
