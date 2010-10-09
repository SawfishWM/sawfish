;; match-window.jl -- match windows to properties

;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>

;; This file is part of sawfish.

;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; Commentary:

;; This module provides a general mechanism for setting properties on
;; matched windows as they're created.

(define-structure sawfish.wm.ext.match-window

    (export match-window-grab-x-property
	    define-match-window-group
	    define-match-window-property
	    define-match-window-setter
	    define-match-window-formatter
	    add-window-matcher
	    remove-window-matcher
	    match-window)

    (open rep
	  rep.system
	  rep.regexp
	  sawfish.wm
	  sawfish.wm.windows
	  sawfish.wm.util.groups)

  (define-structure-alias match-window sawfish.wm.ext.match-window)

;;; configuration and customize stuff

  (i18n-defvar match-window-x-properties
               '((WM_NAME . "Name")
                 (WM_CLASS . "Class")
                 (WM_ICON_NAME . "Icon Name")
                 (WM_WINDOW_ROLE . "Role")
                 (WM_CLIENT_MACHINE . "Host")
                 (WM_COMMAND . "Command")
                 (WM_LOCALE_NAME . "Locale")))

  ;; Actions (for example, `ignore-program-position') are,
  ;; if declared by `define-match-window-setter' in this file,
  ;; handled by it. Otherwise registered with `window-put'.
  (i18n-defvar
   match-window-properties
   `((geometry ,(_ "Geometry")
	       (ignore-program-position boolean)
	       (place-mode ,(lambda () `(choice ,@placement-modes)))
	       ;; If for example x is -100, then the right border
	       ;; lies 100 pixels away from the right edge of the screen.
	       (position (or
			  (pair (number -65536 65536 0)
				(number -65536 65536 0))
			  (choice center east north north-east
				  north-west south south-east
				  south-west west)))
	       (workspace (number 1))
	       (new-workspace boolean)
	       (new-viewport boolean)
	       (viewport (pair (number 1) (number 1)))
	       (depth (number -16 16 0))
	       (placement-weight (number 0))
	       (fixed-position boolean)
	       (fixed-size boolean)
	       (sticky boolean)
	       (sticky-viewport boolean)
	       )
     (focus ,(_ "Focus")
            (raise-on-focus boolean)
            (focus-when-mapped boolean)
            (never-focus boolean)
            (focus-click-through boolean)
            (focus-mode ,(lambda () `(choice ,@focus-modes))))
     (appearance ,(_ "Appearance")
                 (frame-type ,(lambda ()
                                `(choice ,@(mapcar car match-window-types))))
                 (frame-style ,(lambda ()
                                 `(symbol ,@(find-all-frame-styles t))))
		 (dimensions (pair (number 1) (number 1)))
		 (maximized (choice all vertical horizontal
				    fullscreen full-xinerama))
		 (iconified boolean)
		 (shaded boolean)
		 (shade-hover boolean)
		 (never-iconify boolean)
		 (never-maximize boolean)
		 )
     (other ,(_ "Other")
            (avoid boolean)
            (ignored boolean)
            (group ,(lambda ()
                      `(symbol ,@(delete-if-not symbolp (window-group-ids)))))
            (ungrouped boolean)
            (cycle-skip boolean)
            (window-list-skip boolean)
            (task-list-skip boolean)
            (unique-name boolean)
            (window-name string)
            (transients-above (choice all parents none))
            (ignore-stacking-requests boolean)
	    (auto-gravity boolean)
	    (never-delete boolean)
	    )))

  ;; alist of (PROPERTY . FEATURE) mapping properties to the lisp
  ;; libraries implementing them
  (defvar match-window-features
    '((raise-on-focus . auto-raise)
      (shade-hover . shade-hover)))

  (defvar match-window-types
    '((normal . default)
      (title-only . shaped)
      (border-only . transient)
      (top-border . shaped-transient)
      (none . unframed)))

  (define (match-window-widget)
    (let ((props (mapcar
		  (lambda (sub)
		    (cons (cadr sub)
			  (mapcar (lambda (prop)
				    (if (functionp (cadr prop))
					(list* (car prop)
					       ((cadr prop))
					       (cddr prop))
				      prop))
				  (cddr sub)))) match-window-properties)))
      `(match-window ,props ,match-window-x-properties)))

  (put 'match-window 'custom-widget match-window-widget)

  ;;###autoload (defgroup match-window "Matched Windows" :layout single :require sawfish.wm.ext.match-window)
  (defgroup match-window "Window Rules"
    :layout single
    :require sawfish.wm.ext.match-window)

  ;; List of (MATCH-ELTS . ACTION-ELTS)
  ;; Each MATCH-ELT is (PROP . REGEXP or NUMBER or SYMBOL)
  ;; Each ACTION-ELT is (PROP . VALUE)
  (defcustom match-window-profile nil
    nil
    :type match-window
    :group match-window
    :require sawfish.wm.ext.match-window)

  ;; used by sawfish-config when grabbing property values
  (define (match-window-grab-x-property real-prop)
    (let ((window (select-window))
	  prop)
      (when window
	(setq prop (get-x-property window real-prop))
	(if (and prop (eq (car prop) 'STRING))
	    (setq prop (get-x-text-property window real-prop))
	  (setq prop (nth 2 prop)))
	(when prop
	  (cond ((get real-prop 'match-window-formatter)
		 (setq prop ((get real-prop 'match-window-formatter) prop)))
		((vectorp prop)
		 (setq prop (aref prop 0))))))
      (when (stringp prop)
	(setq prop (concat #\^ (quote-regexp prop) #\$)))
      prop))

  (define (define-match-window-group group name)
    (unless (assq group match-window-properties)
      (setq match-window-properties (nconc match-window-properties
					   (list (list group name))))))

  (define (define-match-window-property name group . def)
    (let* ((group-cell (or (assq group match-window-properties)
			   (error "Unknown match-window group: %s" group)))
	   (item-cell (assq name (cddr group-cell))))
      (if item-cell
	  (rplacd item-cell def)
	(rplacd (cdr group-cell) (nconc (cddr group-cell)
					(list (cons name def)))))))

  (define (define-match-window-setter name setter)
    (put name 'match-window-setter setter))

  (define (define-match-window-formatter name formatter)
    (put name 'match-window-formatter formatter))

  (define (alist-eq-p a b)
    "Whether alist a and b are equal. Used by add/remove-window-matcher"
    (let (ret-val tem)
      (when (eq (length a)
		(length b))
	(setq ret-val t)
	(while b
	  (setq tem (assoc (caar b) a))
	  (if (and tem
		   (equal (cdr tem) (cdar b)))
	      (setq b (cdr b))
	    (setq ret-val nil)
	    (setq b nil))))
      ret-val))

;;; main entry point

  (define (add-window-matcher rules #!rest actions)
    ;; #!rest is used to support the old usage.
    ;; The new grammar is (add-window-matcher rules actions),
    ;; where `actions' is an alist.
    ;;
    ;; The old grammar remains supported, but it is deprecated.
    ;; It was:
    ;;  (add-window-matcher 'WM_CLASS "Term"
    ;;       '(place-mode . interactive)
    ;;       '(maximize . all)
    ;;        ... )
    ;; The new style is introduced in 1.6. The old one can be deleted in
    ;; future.
    ;;
    ;; It used to have a bug in how the match was done.
    ;; Read the commit log.
    ;;
    ;; This wrapper function exists only to support the old use.
    (if (consp rules)
	(add-window-matcher-core rules (car actions))
      ;; old usage
      (add-window-matcher-core (list (cons rules (car actions)))
			       (cdr actions)))
    )

  (define (add-window-matcher-core rules actions)
    (catch 'out
      (let
	  ((add-to (lambda (slot)
		     (mapc (lambda (action)
			     (let
				 ((tem (assq (car action) (cdr slot))))
			       (if tem
				   (rplacd tem (cdr action))
				 (rplacd slot (cons action (cdr slot))))))
			   actions))))
	;; does the list already contain any actions for rules?
	;; then add, using add-to.
	(mapc (lambda (cell)
		(when (alist-eq-p rules (car cell))
		  (add-to cell)
		  (throw 'out t)))
	      match-window-profile)
	;; no. create new entry
	(setq match-window-profile (cons (list* rules actions)
					 match-window-profile))
      )))

  (define (remove-window-matcher rules #!rest props)
    ;; See add-window-matcher for why this wrapper is.
    (if (consp rules)
	(remove-window-matcher-core rules (car props))
      ;; old usage
      (remove-window-matcher-core (list (cons rules (car props)))
				  (cdr props)))
    )
  
  (define (remove-window-matcher-core rules props)
    (let
	((remove-from (lambda (slot)
			(mapc (lambda (p)
				(let
				    ((tem (assq p (cdr slot))))
				  (when tem
				    (rplacd slot (delq tem (cdr slot))))))
			      props))))
      (mapc (lambda (cell)
	      (when (alist-eq-p rules (car cell))
		(remove-from cell)))
	    match-window-profile)
      ;; remove any empty matchers
      (setq match-window-profile
	    (delete-if (lambda (cell)
			 (null (cdr cell))) match-window-profile))))

;;; matcher code

  (define (safe-string-match re . args)
    (condition-case data
	(apply string-match re args)
      (regexp-error
       (format standard-error
	       "regexp error in match-window: %s, %s" re (car data)))))

  (define (match-window w)
    (let ((prop-cache '()))

      ;; Get the X property P of window W, uses a cache, will
      ;; reformat properties with a match-window-formatter property
      (define (get-prop p)
	(let
	    ((tem (assq p prop-cache)))
	  (if tem
	      (cdr tem)
	    (setq tem (copy-sequence (get-x-property w p)))
	    (when (and tem (eq (car tem) 'STRING))
	      (rplaca (cddr tem) (get-x-text-property w p)))
	    (when (and tem (get p 'match-window-formatter))
	      (rplaca (cddr tem) ((get p 'match-window-formatter)
				  (nth 2 tem))))
	    (setq prop-cache (cons (cons p tem) prop-cache))
	    tem)))

      ;; Return t if X property PROP of window W matches INPUT (a
      ;; regexp, number, or symbol)
      (define (match-prop prop input)
	(catch 'out
	  (cond ((and (stringp input)
		      (or (stringp prop)
			  (and (vectorp prop)
			       (> (length prop) 0)
			       (stringp (aref prop 0)))))
		 ;; regexp match
		 (if (vectorp prop)
		     (do ((i 0 (1+ i)))
			 ((= i (length prop)) nil)
		       (when (safe-string-match input (aref prop i))
			 (throw 'out t)))
		   (safe-string-match input prop)))
		((and (numberp input) (numberp prop))
		 (= input prop))
		(t (equal input prop)))))

      ;; Execute the list of actions for window W
      (define (run-actions actions)
	(mapc (lambda (cell)
		(let ((feature (cdr (assq (car cell) match-window-features))))
		  (when feature
		    (require feature)))
		((or (get (car cell) 'match-window-setter) window-put)
		 w (car cell)
		 (if (eq (cdr cell) '#f) nil (cdr cell))))
	      actions)
	;; hack alert!
	(when (assq 'position actions)
	  (window-put w 'placed t)))

      (mapc (lambda (cell)
	      (when (catch 'out
		      (mapc (lambda (match)
			      (let ((prop (and (symbolp (car match))
					       (get-prop (car match)))))
				(when (or (not prop)
					  (not (match-prop (nth 2 prop)
							   (cdr match))))
				  (throw 'out nil))))
			    (car cell))
		      t)
		(run-actions (cdr cell))))
	    match-window-profile)))

  (add-hook 'before-add-window-hook match-window t)

;;; custom property formatters and setters

  (define-match-window-formatter 'WM_CLASS
    (lambda (vec)
      (format nil "%s/%s" (and (> (length vec) 1) (aref vec 1)) (aref vec 0))))

  (define-match-window-formatter 'WM_COMMAND
    (lambda (vec)
      (let ((i 0)
            parts)
        (while (< i (length vec))
          (when parts
            (setq parts (cons ?  parts)))
          (setq parts (cons (aref vec i) parts))
          (setq i (1+ i)))
        (apply concat (nreverse parts)))))

  (define-match-window-setter 'workspace
    (lambda (w prop value)
      (declare (unused prop))
      (unless (or (window-get w 'placed) (window-workspaces w))
        ;; translate from 1.. to 0..
        (set-window-workspaces w (list (1- value))))))

  (define-match-window-setter 'position
    (lambda (w prop value)
      (declare (unused prop))
      (let* (xx yy
	     (size (window-frame-dimensions w))
             (vp-offset (viewport-offset-coord (window-viewport w)))
             (x (if (symbolp value)
                    (cond ((memq value '(east south-east north-east))
                           (- (screen-width) (car size)))
                          ((memq value '(north center south))
                           (- (quotient (screen-width) 2)
                              (quotient (car size) 2)))
                          (t 0))
		  ;; numeric value
		  (setq xx (car value))
		  (if (< xx 0)
		      (setq xx (+ (- (screen-width) (car size)) xx))
		    xx)
		  ))
             (y (if (symbolp value)
                    (cond ((memq value '(south south-east south-west))
                           (- (screen-height) (cdr size)))
                          ((memq value '(east center west))
                           (- (quotient (screen-height) 2)
                              (quotient (cdr size) 2)))
                          (t 0))
		  ;; numeric value
		  (setq yy (cdr value))
		  (if (< yy 0)
		      (setq yy (+ (- (screen-height) (cdr size)) yy))
		    yy)))
             (gravity (cond ((symbolp value)
                             value)
                            ((and (< x 0) (< y 0))
                             'south-east)
                            ((< x 0)
                             'north-east)
                            ((< y 0)
                             'south-west)
                            (t nil))))
        (when gravity
          (window-put w 'gravity gravity))
        (move-window-to w
                        (+ (car vp-offset) x)
                        (+ (cdr vp-offset) y)))))

  (define-match-window-setter 'dimensions
    (lambda (w prop value)
      (declare (unused prop))
      (resize-window-with-hints w (car value) (cdr value))))

  (define-match-window-setter 'viewport
    (lambda (w prop value)
      (declare (unused prop))
      (unless (window-get w 'placed)
        (set-screen-viewport (1- (car value)) (1- (cdr value)))
        (set-window-viewport w (1- (car value)) (1- (cdr value))))))

  (define-match-window-setter 'frame-type
    (lambda (w prop value)
      (declare (unused prop))
      (set-window-type w (or (cdr (assq value match-window-types)) value))))

  (define-match-window-setter 'ungrouped
    (lambda (w prop value)
      (declare (unused prop))
      (when value
        (add-window-to-new-group w))))

  (define-match-window-setter 'unique-name
    (lambda (w prop value)
      (declare (unused prop))
      (when value
	(uniquify-window-name w))))

  (define-match-window-setter 'focus-mode
    (lambda (w prop value)
      (declare (unused prop))
      (set-focus-mode w value)))

  (define-match-window-setter 'new-workspace
    (lambda (w prop value)
      (declare (unused prop))
      (when value
        (unless (window-get w 'placed)
          (let ((space (car (workspace-limits))))
            (while (not (workspace-empty-p space))
              (setq space (1+ space)))
            (set-window-workspaces w (list space)))))))

  (define-match-window-setter 'new-viewport
    (lambda (w prop value)
      (declare (unused prop))
      (when value
        (unless (window-get w 'placed)
          (let ((row 0)
                (col 0)
                (nomatch t))
            (while (and nomatch (< row (cdr viewport-dimensions)))
              (setq col 0)
              (while (and nomatch (< col (car viewport-dimensions)))
                (if (null (viewport-windows col row nil t))
                    (setq nomatch nil)
                  (setq col (1+ col))))
              (if nomatch
                  (setq row (1+ row))))
            (when nomatch
              (let ((cols (car viewport-dimensions))
                    (rows (cdr viewport-dimensions)))
                (if (<= cols rows)
                    (setq viewport-dimensions (cons (1+ cols) rows)
                          col cols
                          row 0)
                  (setq viewport-dimensions (cons cols (1+ rows))
                        col 0
                        row rows))))
            (set-screen-viewport col row)
            (set-window-viewport w col row))))))


  (define-match-window-setter 'window-name
    (lambda (w prop value)
      (declare (unused prop))
      (when value
        (rename-window-func w value))))

  (define-match-window-setter 'maximized
    (lambda (w prop value)
      (declare (unused prop))
      (cond ((eq value 'vertical)
	     (window-put w 'queued-vertical-maximize t))
	    ((eq value 'horizontal)
	     (window-put w 'queued-horizontal-maximize t))
	    ((eq value 'all)
	     (window-put w 'queued-vertical-maximize t)
	     (window-put w 'queued-horizontal-maximize t))
	    ((eq value 'fullscreen)
	     (window-put w 'queued-fullscreen-maximize t))
	    ((eq value 'full-xinerama)
	     (window-put w 'queued-fullxinerama-maximize))
	    )))

  (define-match-window-setter 'keymap-trans
    (lambda (w prop value)
      (declare (unused prop))
      (let ((keymap (or (window-get w 'keymap)
                        (window-put w 'keymap (copy-sequence window-keymap)))))
        (mapcar
         (lambda (pair)         ; pair of from and to keys
           (bind-keys keymap (car pair)
                      (lambda () (interactive)
                        (synthesize-event (lookup-event (cadr pair))
                                          (current-event-window))))) value))))

)
