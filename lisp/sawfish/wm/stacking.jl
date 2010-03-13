;; stacking.jl -- window stacking

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

(define-structure sawfish.wm.stacking

    (export save-stacking-order
	    mapped-stacking-order
            window-obscured
            stacking-visibility
	    raise-window
	    lower-window
	    stack-window-above
	    stack-window-below
	    restack-window
	    stacking-order-by-depth
	    window-depth
	    set-window-depth
	    window-on-top-p
	    raise-lower-window
	    lower-window-depth
	    raise-window-depth
	    raise-windows
	    lower-windows
	    raise-lower-windows)

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.session.init
	  sawfish.wm.workspace
          sawfish.wm.state.transient
          sawfish.wm.util.rects)

  ;; Each window will have a `depth' property--an integer, zero
  ;; represents the level of normal windows, negative for windows below
  ;; this level, and positive for windows above the normal level

  (defcustom transients-above 'parents
    "Keep transient windows stacked above: \\w"
    :group (misc stacking)
    :type (choice all parents none))

  (defmacro save-stacking-order (#!rest forms)
    "Execute FORMS, then reinstall the original stacking order."
    (let ((tem (gensym)))
      `(let ((,tem (stacking-order)))
	 (unwind-protect
	     (progn ,@forms)
	   (restack-windows ,tem)))))

  ;; this will always return an integer
  (define (window-depth w) (window-get w 'depth))

;;; constraint mechanics (predicates actually..)

  (define (stacking-constraint:layer w)
    (let ((depth (window-depth w)))
      (lambda (above below)
	;; constraints are met locally in ABOVE and BELOW, so just
	;; test against windows adjacent to W
	(and (or (null above)
		 (<= depth (window-depth (car above))))
	     (or (null below)
		 (>= depth (window-depth (car below))))))))

  (define (stacking-constraint:transients-above-parent w)
    (let ((parents (transient-parents w))
	  (children (transient-children w)))

      ;; ignore parents with depth > W, ignore children with depth < W
      (let ((w-depth (window-depth w)))
	(setq parents (delete-if (lambda (x)
				   (> (window-depth x) w-depth)) parents))
	(setq children (delete-if (lambda (x)
				    (< (window-depth x) w-depth)) children)))

      (lambda (above below)
	(and (or (null parents)
		 ;; All parents must be below W
		 (let loop ((rest parents))
                      (cond ((null rest) t)
                            ((memq (car rest) above) nil)
                            (t (loop (cdr rest))))))
	     (or (null children)
		 ;; All children must be above W
		 (let loop ((rest children))
                      (cond ((null rest) t)
                            ((memq (car rest) below) nil)
                            (t (loop (cdr rest))))))))))

  (define (stacking-constraint:transients-above-all w)
    (let ((w-depth (window-depth w)))
      (if (window-transient-p w)
	  ;; ensure there are no normal windows above W, unless they
	  ;; have depth > W
	  (lambda (above below)
	    (declare (unused below))
	    (let loop ((rest above))
                 (cond ((null rest) t)
                       ((and (not (window-transient-p (car rest)))
                             (<= (window-depth (car rest)) w-depth)) nil)
                       (t (loop (cdr rest))))))
	;; ensure no transients below W, unless they're depth < W
	(lambda (above below)
	  (declare (unused above))
	  (let loop ((rest below))
               (cond ((null rest) t)
                     ((and (window-transient-p (car rest))
                           (>= (window-depth (car rest)) w-depth)) nil)
                     (t (loop (cdr rest)))))))))

  (define (combine-constraints constraints)
    "Combine the list of secondary constraint functions into a single
function (using logical `and' combinator)."
    (lambda (above below)
      (let loop ((rest constraints))
           (cond ((null rest) t)
                 ((not ((car rest) above below)) nil)
                 (t (loop (cdr rest)))))))

  (defvar basic-stacking-constraints (list stacking-constraint:layer)
    "List of stacking constraint functions to adhere to (excluding transient
stacking modes controlled by `transients-above' option).")

  (define (make-constraint w)
    "Return the constraint function to use when stacking window W, a function
of two arguments, the list of windows above W (from bottom to top) and the
list of windows below W (from top to bottom). If the constraint holds
for a particular stacking configuration, it will return non-`nil'."
    (combine-constraints
     (mapcar (lambda (c) (c w))
	     (case (or (window-get w 'transients-above) transients-above)
	       ((parents)
		(cons stacking-constraint:transients-above-parent
		      basic-stacking-constraints))
	       ((all)
		(cons stacking-constraint:transients-above-all
		      basic-stacking-constraints))
	       (t basic-stacking-constraints)))))

;;; utilities

  ;; returns (BEFORE . AFTER), modifies LST
  (define (break-window-list lst pivot)
    "Given a list of windows LST and a window PIVOT, return a cons cell
containing two lists, the windows that occur in LST before PIVOT does (in
reverse order), and the list of windows occurring in LST after PIVOT.

LST is destructively modified by this procedure."
    (let loop ((rest lst)
	       (before '()))
         (cond ((null rest)
                (cons before rest))
               ((eq (car rest) pivot)
                (cons before (cdr rest)))
               (t
                (let ((next (cdr rest)))
                  (rplacd rest before)
                  (loop next rest))))))

  (define (stack-rotate-upwards cell)
    "Given a cons cell containing two lists of windows `(ABOVE . BELOW)',
rotate the lists so that the first element of BELOW becomes the first
element of ABOVE. The lists are modified. Returns `nil' if BELOW is
the empty list."
    (if (null (car cell))
	nil
      (let ((next (car cell)))
	(rplaca cell (cdar cell))
	(rplacd next (cdr cell))
	(rplacd cell next)
	cell)))

  (define (stack-rotate-downwards cell)
    "Given a cons cell containing two lists of windows `(ABOVE . BELOW)',
rotate the lists so that the first element of ABOVE becomes the first
element of BELOW. The lists are modified. Returns `nil' if ABOVE is
the empty list."
    (if (null (cdr cell))
	nil
      (let ((next (cdr cell)))
	(rplacd cell (cddr cell))
	(rplacd next (car cell))
	(rplaca cell next)
	cell)))

  (define (mapped-stacking-order)
    (delete-if-not window-mapped-p (stacking-order)))

  (define (window-obscured window)
    "Check whether WINDOW is obscured.  Return `t' if WINDOW is fully obscured
by some other window, otherwise a list of windows partially obscuring WINDOW.
In particular return `nil' if WINDOW is unobscured.  Note that if a list of
partially obscuring windows is returned, taken together they may or may not
fully obscure WINDOW."
    (define w window)
    (define ws (nearest-workspace-with-window w current-workspace))
    (let loop ((stack (stacking-order))
               (obs nil))
         (if (null stack)               ; Should not happen
             obs
           (let ((w2 (car stack)))
             (cond ((eq w2 w) obs)
                   ((and (window-visible-p w2)
                         (window-appears-in-workspace-p w2 ws))
                    (case (apply rect-obscured
                                 (rectangles-from-windows (list w w2)))
                      ((unobscured) (loop (cdr stack) obs))
                      ((fully-obscured) t)
                      (t (loop (cdr stack) (cons w2 obs))))) ; Partially
                   (t (loop (cdr stack) obs)))))))

  (define (stacking-visibility window)
    "Compute the visibility of WINDOW from the stacking order.  This should
work even with the Composite extension, which appears to disable
VisibilityNotify events.  Note that deciding between fully and partially
obscured may require quite a bit of computation.  If you do not need that
distinction, window-obscured should be faster."
    (define (rect-list-minus rs s tail)
      (if (null rs)
          tail
        (rect-list-minus (cdr rs) s
                         (rect-minus (car rs) s tail))))
    (let ((obs (window-obscured window)))
      (case obs
        ((t) 'fully-obscured)
        ((()) 'unobscured)
        (t
         (do ((unobs (rectangles-from-windows (list window))
                     (rect-list-minus unobs (car robs) nil))
              (robs (rectangles-from-windows obs) (cdr robs)))
             ((or (null unobs) (null robs))
              (if (null unobs)
                  'fully-obscured
                'partially-obscured)))))))


;;; stacking functions

  (define (raise-window w)
    "Raise the window to its highest allowed position in the stacking order."
    ;; work downwards from top
    (let ((constraint (make-constraint w))
	  (stack (cons '() (delq w (stacking-order)))))
      (let loop ()
           (cond ((constraint (car stack) (cdr stack))
                  (if (car stack)
                      (x-lower-window w (car (car stack)))
                    (x-raise-window w (car (cdr stack)))))
                 ((null (cdr stack))
                  ;; no position
                  nil)
                 (t
                  (stack-rotate-downwards stack)
                  (loop))))))

  (define (lower-window w)
    "Lower the window to its lowest allowed position in the stacking order."
    (let ((constraint (make-constraint w))
	  (stack (cons (nreverse (delq w (stacking-order))) '())))
      ;; work upwards from bottom
      (let loop ()
           (cond ((constraint (car stack) (cdr stack))
                  ;; found the lowest position
                  (if (cdr stack)
                      (x-raise-window w (car (cdr stack)))
                    (x-lower-window w (car (car stack)))))
                 ((null (car stack))
                  ;; no possible position..
                  nil)
                 (t
                  (stack-rotate-upwards stack)
                  (loop))))))

  (define (stack-window-above above below)
    "Change the stacking of window ABOVE so that it is as closely above window
BELOW as possible."
    (let ((constraint (make-constraint above))
	  (stack (break-window-list
		  (delq above (stacking-order)) below)))
      (rplacd stack (cons below (cdr stack)))
      (let loop ()
           (cond ((constraint (car stack) (cdr stack))
                  ;; found a suitable position
                  (if (car stack)
                      (x-lower-window above (car (car stack)))
                    (x-raise-window above (car (cdr stack)))))
                 ((null (car stack))
                  ;; reached the top
                  nil)
                 (t
                  (stack-rotate-upwards stack)
                  (loop))))))

  (define (stack-window-below below above)
    "Change the stacking of window BELOW so that it is as closely below window
ABOVE as possible."
    (let ((constraint (make-constraint below))
	  (stack (break-window-list
		  (delq below (stacking-order)) above)))
      (rplaca stack (cons above (car stack)))
      (let loop ()
           (cond ((constraint (car stack) (cdr stack))
                  (if (cdr stack)
                      (x-raise-window below (car (cdr stack)))
                    (x-lower-window below (car (car stack)))))
                 ((null (cdr stack))
                  ;; reached the bottom
                  nil)
                 (t
                  (stack-rotate-downwards stack)
                  (loop))))))

  (define (restack-window w)
    "Assuming that the current stacking order is in a consistent state
except, possibly, for the position of window W, restore the consistent
state including window W. This is achieved by raising or lowering
window W as appropriate."
    (let ((constraint (make-constraint w))
	  (stack (break-window-list (stacking-order) w)))
      (unless (constraint (car stack) (cdr stack))
	(if (< (length (car stack)) (length (cdr stack)))
	    (raise-window w)
	  (lower-window w)))))

  (define (stacking-order-by-depth depth)
    "Return a list of windows containing only those in depth DEPTH, in the
order they are stacked within the layer (top to bottom)."
    (let ((order (stacking-order)))
      (delete-if (lambda (x)
		   (/= (window-depth x) depth)) order)))

  (define (set-window-depth w depth)
    "Set the stacking depth of window W to DEPTH."
    (let ((old (window-depth w)))
      (window-put w 'depth depth)
      (cond ((> old depth)
	     ;; window's going downwards
	     (raise-window w))
	    ((< old depth)
	     ;; window's going upwards
	     (lower-window w)))
      (call-window-hook 'window-depth-change-hook w (list depth))
      (call-window-hook 'window-state-change-hook w (list '(stacking)))))

  (define (window-on-top-p w)
    "Return t if window W is as high as it can legally go in the
stacking order."
    (let* ((constraint (make-constraint w))
	   (order (delete-if-not
		   ;; XXX doesn't handle viewports..
		   (let ((space (nearest-workspace-with-window
				 w current-workspace)))
		     (lambda (x)
		       (window-appears-in-workspace-p x space)))
		   (stacking-order)))
	   (old-posn (- (length order) (length (memq w order))))
	   (stack (cons '() (delq w order))))
      (let loop ()
           (if (or (constraint (car stack) (cdr stack))
                   (null (cdr stack)))
               ;; found highest position
               (= (length (car stack)) old-posn)
             (stack-rotate-downwards stack)
             (loop)))))

  (define (raise-lower-window w)
    "If the window is at its highest possible position, then lower it to its
lowest possible position. Otherwise raise it as far as allowed."
    (if (or (not (window-obscured w))
	    (window-on-top-p w))
	(lower-window w)
      (raise-window w)))

  (define (lower-window-depth w)
    "Put the window in the stacking level beneath its current level."
    (set-window-depth w (1- (window-depth w))))

  (define (raise-window-depth w)
    "Put the window in the stacking level above its current level."
    (set-window-depth w (1+ (window-depth w))))

  (define-command 'raise-single-window raise-window
    #:spec "%W" #:class 'advanced)
  (define-command 'lower-single-window lower-window
    #:spec "%W" #:class 'advanced)
  (define-command 'raise-lower-single-window raise-lower-window
    #:spec "%W" #:class 'advanced)
  (define-command 'raise-window-depth raise-window-depth #:spec "%W")
  (define-command 'lower-window-depth lower-window-depth #:spec "%W")

;;; stacking groups of windows

  ;; Helper function for raise-windows and lower-windows. Calls (FIRST
  ;; X) on the first element X of ORDER, then (SUCCESSOR Y X) on each
  ;; pair of adjacent elements X and Y

  (define (apply-group-order order first successor)
    (when order
      (first (car order))
      (do ((rest order (cdr rest)))
	  ((null (cdr rest)))
	(successor (cadr rest) (car rest)))))

  (define (raise-windows w order)
    ;; STRATEGY: take every window in ORDER to its highest possible
    ;; position, giving W priority but otherwise obeying ORDER, which
    ;; must be a valid stacking configuration (highest-to-lowest).  Do
    ;; the restacking in such a way that no part of any window in
    ;; ORDER is exposed during the restacking that is not exposed
    ;; afterwards.

    (define (highest-window order)
      ;; find the first window in ORDER that is allowed
      ;; to be above all other windows in ORDER...
      (let loop ((rest order))
           (cond ((null rest) nil)
                 (((make-constraint (car rest)) '() (remq (car rest) order))
                  (car rest))
                 (t (loop (cdr rest))))))

    ;; Cons up the new order (in reverse), by picking the most-raisable
    ;; each time until no windows are left, then commit that ordering
    (let loop ((rest (cons w (remq w order)))
	       (out '()))
         (if (null rest)
             (apply-group-order (nreverse out) raise-window stack-window-below)
           (let ((highest (or (highest-window rest)
                              (error "Stacking constraint failed"))))
             (loop (delq highest rest) (cons highest out))))))

  (define (lower-windows w order)

    (define (lowest-window order)
      ;; find the last window in ORDER that may be below all other
      ;; windows in ORDER
      (let ((reversed (reverse order)))
	(let loop ((rest reversed))
             (cond ((null rest) nil)
                   (((make-constraint (car rest))
                     (remq (car rest) reversed) '()) (car rest))
                   (t (loop (cdr rest)))))))

    (let loop ((rest (nconc (remq w order) (list w)))
	       (out '()))
         (if (null rest)
             (apply-group-order (nreverse out) lower-window stack-window-above)
           (let ((lowest (or (lowest-window rest)
                             (error "Stacking constraint failed"))))
             (loop (delq lowest rest) (cons lowest out))))))

  (define (raise-lower-windows w order)
    (if (or (not (window-obscured w))
	    (and (window-on-top-p (car order))
		 ;; look for the group as a block.. this is a heuristic
		 (let loop ((rest (memq (car order) (stacking-order))))
                      (cond ((null rest) nil)
                            ((eq (car rest) w) t)
                            ((memq (car rest) order) (loop (cdr rest)))
                            (t nil)))))
	(lower-windows w order)
      (raise-windows w order)))

;;; hooks

  ;; Called from the add-window-hook
  (define (stacking-add-window w)
    (unless (window-get w 'depth)
      (let ((parents (transient-parents w)))
	(if parents
	    ;; put dialogs in at least as high a layer as their
	    ;; highest parent, but never below the default depth
	    (window-put w 'depth (apply max 0 (mapcar window-depth parents)))
	  ;; default depth
	  (window-put w 'depth 0)))))

  (add-hook 'add-window-hook stacking-add-window t)
  (add-hook 'after-add-window-hook restack-window t)
  (add-hook 'map-notify-hook restack-window t)

  (sm-add-saved-properties 'depth)
  (add-swapped-properties 'depth))
