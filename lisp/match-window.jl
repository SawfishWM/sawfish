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


;; configuration and customize stuff

;;;###autoload (setq custom-required (cons 'match-window custom-required))

(defvar match-window-x-properties
  '((WM_NAME . "Name")
    (WM_CLASS . "Class")
    (WM_ICON_NAME . "Icon Name")
    (WM_CLIENT_MACHINE . "Host")
    (WM_COMMAND . "Command")
    (WM_LOCALE_NAME . "Locale")))

(defvar match-window-properties
  `((ignored boolean)
    (iconified boolean)
    (shaded boolean)
    (avoid boolean)
    (sticky boolean)
    (sticky-viewport boolean)
    (focus-click-through boolean)
    (ignore-window-input-hint boolean)
    (ignore-program-position boolean)
    (raise-on-focus boolean)
    (never-focus boolean)
    (ungrouped boolean)
    (group symbol ,(lambda ()
		     (delete-if-not symbolp (window-group-ids))))
    (place-mode symbol ,(lambda ()
			  (custom-get-options 'place-window-mode)))
    (frame-type symbol ,(lambda ()
			  (mapcar car match-window-types)))
    (frame-style symbol ,(lambda ()
			   (find-all-frame-styles t)))
    (position pair)
    (size pair)
    (workspace number)
    (viewport pair)
    (depth number)
    (placement-weight number)))

(defvar match-window-types
  '((normal . default)
    (title-only . shaped)
    (border-only . transient)
    (top-border . shaped-transient)
    (none . unframed)))

(defun match-window-widget (symbol value doc)
  (let
      ((props (mapcar (lambda (prop)
			(if (and (eq (nth 1 prop) 'symbol)
				 (functionp (nth 2 prop)))
			    (list (car prop) (nth 1 prop) ((nth 2 prop)))
			  prop))
		      match-window-properties)))
    `(match-window :variable ,symbol
		   :value ,value
		   :properties ,props
		   :x-properties ,match-window-x-properties)))

;; use this so the single widget expands properly
(defun match-window-group-widget (group spec)
  (car spec))

(put 'match-window 'custom-widget match-window-widget)
(put 'match-window 'custom-group-widget match-window-group-widget)

(defgroup match-window "Matched Windows")

;; List of (MATCH-ELTS . ACTION-ELTS)
;; Each MATCH-ELT is (PROP . REGEXP or NUMBER or SYMBOL)
;; Each ACTION-ELT is (PROP . VALUE)
(defcustom match-window-profile nil
  "Match windows to properties."
  :type match-window
  :group match-window
  :require match-window)

;; used by sawmill-ui when grabbing property values
(defun match-window-grab-x-property (prop-name)
  (let
      ((real-prop (or (car (rassoc prop-name match-window-x-properties))
		      (intern prop-name)))
       (window (select-window))
       prop)
    (when window
      (setq prop (get-x-text-property window real-prop))
      (when prop
	(if (get real-prop 'match-window-formatter)
	    (setq prop ((get real-prop 'match-window-formatter) prop))
	  (setq prop (aref prop 0)))))
    prop))


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
					    (> (length prop) 0)
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
			      ((or (get (car cell) 'match-window-setter)
				   window-put)
			       w (car cell) (cdr cell)))
			    actions)
		      ;; hack alert!
		      (when (assq 'position actions)
			(window-put w 'placed t)))))

    (mapc (lambda (cell)
	    (when (catch 'out
		    (mapc (lambda (match)
			    (let
				((prop (get-prop (car match))))
			      (when (or (not prop)
					(not (match-prop (nth 2 prop)
							 (cdr match))))
				(throw 'out nil))))
			  (car cell))
		    t)
	      (run-actions (cdr cell))))
	  match-window-profile)))

(add-hook 'before-add-window-hook match-window)


;; custom property formatters and setters

;; ensure the functions get compiled
(progn
  (put 'WM_CLASS 'match-window-formatter
       (lambda (vec)
	 (format nil "%s/%s" (aref vec 1) (aref vec 0))))

  (put 'WM_COMMAND 'match-window-formatter
       (lambda (vec)
	 (let
	     ((i 0)
	      parts)
	   (while (< i (length vec))
	     (when parts
	       (setq parts (cons ?  parts)))
	     (setq parts (cons (aref vec i) parts))
	     (setq i (1+ i)))
	   (apply concat (nreverse parts)))))

  (put 'workspace 'match-window-setter
       (lambda (w prop value)
	 (unless (or (window-get w 'placed) (window-workspaces w))
	   ;; translate from 1.. to 0..
	   (window-add-to-workspace w (1- value))
	   (select-workspace-from-first (1- value)))))

  (put 'position 'match-window-setter
       (lambda (w prop value)
	 (let
	     ((x (car value))
	      (y (cdr value)))
	   (when (< x 0)
	     ;; XXX should change placement gravity
	     (setq x (+ (screen-width) x)))
	   (when (< y 0)
	     ;; XXX should change placement gravity
	     (setq y (+ (screen-height) y)))
	   (move-window-to w x y))))

  (put 'size 'match-window-setter
       (lambda (w prop value)
	 (resize-window-with-hints w (car value) (cdr value))))

  (put 'viewport 'match-window-setter
       (lambda (w prop value)
	 (unless (window-get w 'placed)
	   (set-screen-viewport (1- (car value)) (1- (cdr value)))
	   (set-window-viewport w (1- (car value)) (1- (cdr value))))))

  (put 'frame-type 'match-window-setter
       (lambda (w prop value)
	 (window-put w 'type (or (cdr (assq value match-window-types))
				 value))))

  (put 'ungrouped 'match-window-setter
       (lambda (w prop value)
	 (when value
	   (add-window-to-new-group w)))))
