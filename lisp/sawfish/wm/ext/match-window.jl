;; match-window.jl -- match windows to properties
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

(provide 'match-window)

;; Commentary:

;; This module provides a general mechanism for setting properties on
;; matched windows as they're created. It removes much of the need for
;; manually creating functions to put in before-add-window-hook.

;; For example, doing:

;;	(add-window-matcher 'WM_CLASS "Term" '(place-mode . interactive))

;; makes all terminal windows be placed interactively. Or even better:

;;	(add-window-matcher 'WM_CLASS "Netscape/Navigator"
;;			    '(ignore-program-position . t))

;; makes netscape windows a lot easier to live with.


;; configuration

;; List of (MATCH-ELTS . ACTION-ELTS)

;; Each MATCH-ELT is (PROP . REGEXP or NUMBER or SYMBOL)
;; Each ACTION-ELT is (PROP . VALUE)

(defvar match-window-profile nil)


;; main entry point

;;;###autoload
(defun add-window-matcher (prop value &rest actions)
  (catch 'out
    (let
	((pair (cons prop value))
	 (add-to (lambda (slot)
		   (mapc (lambda (action)
			   (let
			       ((tem (assq (car action) (cdr slot))))
			     (if tem
				 (rplacd tem (cdr action))
			       (rplacd slot (cons action (cdr slot))))))
			 actions))))
      ;; does the list already contain a (PROP . VALUE) pair?
      (mapc (lambda (cell)
	      (when (member pair (car cell))
		(add-to cell)
		(throw 'out t)))
	    match-window-profile)
      ;; no
      (setq match-window-profile (cons (list (cons pair nil))
				       match-window-profile))
      (add-to (car match-window-profile)))))

;;;###autoload
(defun remove-window-matcher (prop value &rest props)
  (let
      ((pair (cons prop value))
       (remove-from (lambda (slot)
		      (mapc (lambda (p)
			      (let
				  ((tem (assq p (cdr slot))))
				(when tem
				  (rplacd slot (delq tem (cdr slot))))))
			    props))))
    (mapc (lambda (cell)
	    (when (member pair (car cell))
	      (remove-from cell)))
	  match-window-profile)
    ;; remove any empty matchers
    (setq match-window-profile
	  (delete-if (lambda (cell)
		       (null (cdr cell))) match-window-profile))))


;; matcher code

(defun match-window (w)
  (let*
      ((prop-cache nil)

       ;; Get the X property P of window W, uses a cache, will
       ;; reformat text properties with a match-window-formatter
       ;; property 
       (get-prop (lambda (p)
		   (let
		       ((tem (assq p prop-cache)))
		     (if tem
			 (cdr tem)
		       (setq tem (get-x-property w p))
		       (when (and tem (eq (car tem) 'STRING))
			 (let
			     ((vec (get-x-text-property w p)))
			   (when (get p 'match-window-formatter)
			     (setq vec ((get p 'match-window-formatter) vec)))
			   (rplaca (cdr (cdr tem)) vec)))
		       (setq prop-cache (cons (cons p tem) prop-cache))
		       tem))))

       ;; Return t if X property PROP of window W matches INPUT (a
       ;; regexp, number, or symbol)
       (match-prop (lambda (prop input)
		     (catch 'out
		       (cond ((and (stringp input)
				   (or (stringp prop)
				       (and (vectorp prop)
					    (stringp (aref prop 0)))))
			      ;; regexp match
			      (if (vectorp prop)
				  (let
				      ((i 0))
				    (while (< i (length prop))
				      (when (string-match input (aref prop i))
					(throw 'out t))
				      (setq i (1+ i)))
				    nil)
				(string-match input prop)))
			     ((and (numberp input) (numberp prop))
			      (= input prop))
			     (t
			      (equal input prop))))))

       ;; Execute the list of actions for window W
       (run-actions (lambda (actions)
		      (mapc (lambda (cell)
			      (window-put w (car cell) (cdr cell))) actions))))

    (mapc (lambda (cell)
	    (when (catch 'out
		    (mapc (lambda (match)
			    (let
				((prop (get-prop (car match))))
			      (when prop
				(unless (match-prop (nth 2 prop) (cdr match))
				  (throw 'out nil)))))
			  (car cell))
		    t)
	      (run-actions (cdr cell))))
	  match-window-profile)))

(add-hook 'before-add-window-hook match-window)


;; custom property formatters

(put 'WM_CLASS 'match-window-formatter
     (lambda (vec)
       (format nil "%s/%s" (aref vec 1) (aref vec 0))))
