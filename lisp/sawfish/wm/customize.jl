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

(defvar customize-program "sawmill-ui"
  "Location of the program implementing sawmill's configuration interface.")

(defvar customize-group-opt "--group")
(defvar customize-args nil)

(defcustom customize-show-symbols nil
  "Show variable names of each customization option."
  :group misc
  :type boolean)

(defvar customize-user-forms nil)
(defvar customize-user-file-read nil)
(defvar customize-user-file-dirty nil)


;; defcustom's for some built-in variables

(defcustom default-font nil
  "Font used by default."
  :group appearance
  :type font
  :after-set (lambda () (after-setting-frame-option)))

(defcustom default-bevel-percent nil
  "Intensity of bevels (percentage)."
  :group (appearance advanced)
  :type number
  :range (0 . 100)
  :after-set (lambda () (after-setting-frame-option)))


;; ui

(defun customize-symbol-spec (symbol)
  (require 'lisp-doc)
  (let
      ((type (or (get symbol 'custom-type) 'boolean))
       (doc (or (documentation symbol) (symbol-name symbol)))
       (value (funcall (or (get symbol 'custom-get) symbol-value) symbol)))
    (when (stringp doc)
      (setq doc (_ doc)))
    (when customize-show-symbols
      (setq doc (format nil "%s\n[%s]" doc (symbol-name symbol))))
    (cond ((eq type 'boolean)
	   `(toggle ,doc
		    :variable ,symbol
		    :value ,value))

	  ((eq type 'number)
	   (let
	       ((range (get symbol 'custom-range)))
	     (when range
	       (setq range (list ':range range)))
	     `(hbox (number :variable ,symbol
			    :value ,(if (numberp value) value 0)
			    :allow-nil ,(get symbol 'custom-allow-nil)
			    ,@range)
		    (label ,doc))))

	  ;; XXX all but the first should have their own widget types
	  ((memq type '(string program-name))
	   `(hbox (string :variable ,symbol
			  :value ,(if (stringp value) value "")
			  :allow-nil ,(get symbol 'custom-allow-nil))
		  (label ,doc)))

	  ((memq type '(font color file-name))
	   `(hbox (,type :variable ,symbol
			 :value ,value
			 :allow-nil ,(get symbol 'custom-allow-nil))
		  (label ,doc)))

	  ((eq type 'symbol)
	   `(hbox (symbol ,(get symbol 'custom-options)
		       :variable ,symbol
		       :value ,value
		       :widget ,(get symbol 'custom-widget))
		  (label ,doc)))

	  (t
	   (let
	       ((fun (get symbol 'custom-widget)))
	     (when fun
	       (funcall fun symbol value doc)))))))

(defun customize-spec (item)
  (cond ((null item) nil)
	((symbolp item)
	 (customize-symbol-spec item))
	(t
	 (let
	     ((items (mapcar customize-symbol-spec (filter atom (cddr item))))
	      (subtrees (mapcar customize-spec (filter consp (cddr item)))))
	   (list (_ (cadr item))
		 (cond
		  ((get (car item) 'custom-group-widget)
		   ((get (car item) 'custom-group-widget) (car item) items))
		  ((eq (car item) 'root)
		   `(vbox (label ,(format nil (_ "\
Sawmill %s (rep %s)

Copyright (C) 1999 John Harper <jsh@users.sourceforge.net>

This is free software -- you are welcome to redistribute it and/or \
modify it under the terms of the GNU General Public License as \
published by the Free Software Foundation; either version 2, or \
(at your option) any later version.

Sawmill is distributed in the hope that it will be useful, but \
WITHOUT ANY WARRANTY; without even the implied warranty of \
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \
GNU General Public License for more details.

Visit the Sawmill homepage at http://sawmill.sourceforge.net/")
				     sawmill-version rep-version))))
		  (t
		   (cons 'vbox items)))
		 subtrees)))))

(defun customize-ui-spec (&optional group)
  (mapc require custom-required)
  (let
      ((groups (if (or (null group) (eq group t))
		   custom-groups
		 (custom-find-group group))))
    (if (filter consp groups)
	(cons 'tree (customize-spec groups))
      (cadr (customize-spec groups)))))

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
;; sawmill user customization -- do not edit by hand!
;; sawmill version %s, written %s\n\n"
		      sawmill-version (current-time-string))
	      (mapc (lambda (f)
		      (format file "%S\n" f)) customize-user-forms))
	  (close-file file))
	(setq customize-user-file-dirty nil)))))

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
