#| windows.jl -- miscellaneous window mgmt functions

   $Id: windows.jl,v 1.32 2003/03/27 06:30:30 jsh Exp $

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of sawfish.

   sawfish is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.windows

    (compound-interface
     (structure-interface sawfish.wm.windows.subrs)
     (export get-window-by-name
	     get-window-by-name-re
	     window-really-wants-input-p
	     window-transient-p
	     mark-window-as-transient
	     desktop-window-p
	     mark-window-as-desktop
	     dock-window-p
	     mark-window-as-dock
	     window-in-cycle-p
	     window-class
	     warp-cursor-to-window
	     activate-window
	     constrain-dimension-to-hints
	     constrain-aspect-to-hints
	     resize-window-with-hints
	     resize-window-with-hints*
	     window-gravity
	     adjust-position-for-gravity/x
	     adjust-position-for-gravity/y
	     adjust-position-for-gravity
	     get-window-wm-protocols
	     window-supports-wm-protocol-p
	     delete-window
	     delete-window-safely
	     uniquify-name
	     uniquify-window-name
	     select-window
	     window-avoided-p
	     avoided-windows
	     call-after-property-changed
	     call-after-state-changed
	     rename-window
	     rename-window-interactive))

    (open rep
	  rep.system
	  rep.regexp
	  sawfish.wm.windows.subrs
	  sawfish.wm.gaol
	  sawfish.wm.custom
	  sawfish.wm.events
	  sawfish.wm.misc
	  sawfish.wm.commands)


  (defcustom ignore-window-input-hint nil
    "Give focus to windows even when they haven't asked for it."
    :type boolean
    :group focus)

  (defgroup warp "Warping" :group misc)

  (defcustom warp-to-window-offset (cons -1 -1)
    "Offset (%) from window edges when warping pointer."
    :type (pair (number 1) (number 1))
    :group (misc warp))

  (defcustom warp-to-window-enabled nil
    "Whether to enable warping the cursor to windows."
    :type boolean
    :group (misc warp))

  (defcustom dont-avoid-ignored t
    "Whether to not ignore avoided windows by default."
    :type boolean
    :group misc)

  (defcustom avoid-by-default nil
    "Whether to avoid any unspecified window by default."
    :type boolean
    :group misc)

  (defcustom uniquify-name-format "%s [%d]"
    "Format to create unique window names."
    :type string
    :group misc)

  (defvar dock-window-properties
    '(window-list-skip cycle-skip fixed-position focus-click-through
      avoid no-history never-iconify never-maximize sticky
      sticky-viewport placed)
    "List of properties set (to true) on windows marked as docks.")

  (defvar desktop-window-properties
    '(fixed-position sticky sticky-viewport)
    "List of properties set (to true) on windows marked as desktops.")

  (defvar desktop-window-depth -4
    "The stacking depth of desktop windows.")

  (defvar dock-window-depth 0
    "The stacking depth of dock windows.")

;;; finding windows, reading properties

  (define (get-window-by-name name)
    "Find a window object whose window-name is NAME. Returns nil if no such
window is found."
    (car (filter-windows (lambda (w)
			   (string= (window-name w) name)))))

  (define (get-window-by-name-re name)
    "Find a window object whose window-name matches the regexp NAME.
Returns nil if no such window is found."
    (car (filter-windows (lambda (w)
			   (string-match name (window-name w))))))

  (define (window-really-wants-input-p w)
    "Return nil if window W should never be focused."
    (and (not (window-get w 'never-focus))
	 (or ignore-window-input-hint
	     (window-get w 'ignore-window-input-hint)
	     (window-wants-input-p w))))

  (define (window-transient-p w)
    "Return non-nil if WINDOW is a transient window. The returned value will
then be the numeric id of its parent window."
    (or (window-get w 'transient-for)
	(let ((prop (get-x-property w 'WM_TRANSIENT_FOR)))
	  (when (and prop (eq (car prop) 'WINDOW)
		     (eql (cadr prop) 32) (>= (length (caddr prop)) 1))
	    (aref (caddr prop) 0)))))

  (define (mark-window-as-transient w)
    "Mark that window W is a dialog window of some sort."
    (require 'sawfish.wm.frames)
    (unless (window-transient-p w)
      (window-put w 'transient-for (root-window-id)))
    (set-window-type w 'transient))

  (define (desktop-window-p arg)
    "Return true if ARG represents a desktop window."
    (or (eq arg 'root) (and (windowp arg) (window-get arg 'desktop))))

  (define (mark-window-as-desktop w)
    "Mark that the window associated with object W is a desktop window."
    (require 'sawfish.wm.stacking)
    (require 'sawfish.wm.frames)
    (window-put w 'desktop t)
    (window-put w 'keymap root-window-keymap)
    (mapc (lambda (p)
	    (window-put w p t)) desktop-window-properties)
    (set-window-type w 'unframed)
    (set-window-depth w desktop-window-depth))

  (define (focus-desktop)
    "Transfer input focus to the desktop window (if one exists)."
    (let ((desktop-window (car (filter-windows desktop-window-p))))
      (when desktop-window
	(set-input-focus desktop-window))))

  (define-command 'focus-desktop focus-desktop)

  (define (dock-window-p arg)
    "Return true if ARG represents a dock window (i.e. the GNOME panel)."
    (and (windowp arg) (window-get arg 'dock-type)))

  (define (mark-window-as-dock w)
    "Mark that the window associated with object W is a dock window."
    (require 'sawfish.wm.stacking)
    (require 'sawfish.wm.frames)
    (window-put w 'dock-type t)
    (mapc (lambda (p)
	    (window-put w p t)) dock-window-properties)
    (set-window-type w 'dock)
    (set-window-depth w dock-window-depth))

  (define (window-in-cycle-p w #!key ignore-cycle-skip)
    "Returns true if the window W should be included when cycling between
windows."
    (and (window-really-wants-input-p w)
	 (not (or (and (not ignore-cycle-skip) (window-get w 'cycle-skip))
		  (desktop-window-p w)))))

  (define (window-class w)
    "Return the class that window W belongs to, as a string. Returns `nil' if W
has no associated class."
    (let ((prop (get-x-text-property w 'WM_CLASS)))
      (and prop (> (length prop) 1) (aref prop 1))))

  (define (get-window-wm-protocols w)
    "Return a list of symbols defining the X11 window manager protocols
supported by client window W."
    (let* ((prop (get-x-property w 'WM_PROTOCOLS))
	   (data (and prop (eq (car prop) 'ATOM) (nth 2 prop))))
      (when data
	(do ((i 0 (1+ i))
	     (out '() (cons (aref data i) out)))
	    ((= i (length data))
	     (nreverse out))))))

  (define (window-supports-wm-protocol-p w atom)
    "Return true if window W includes ATOM in its `WM_PROTOCOLS' property."
    (let* ((prop (get-x-property w 'WM_PROTOCOLS))
	   (data (and prop (eq (car prop) 'ATOM) (nth 2 prop))))
      (when data
	(let loop ((i 0))
	  (cond ((= i (length data)) nil)
		((eq (aref data i) atom) t)
		(t (loop (1+ i))))))))

;;; renaming

;; extra require to make rename-window work
;; (open sawfish.wm.util.prompt) does not work!

(unless batch-mode
  (require 'sawfish.wm.util.prompt))

(define (rename-window window new-name)
  (set-x-text-property window 'WM_NAME (vector new-name)
  (set-x-text-property window '_NET_WM_NAME (vector new-name))))

(define (rename-window-interactive w)
  (rename-window w (prompt-for-string "new title:" (window-name w))))

(define-command 'rename-window rename-window-interactive #:spec "%W")

;;; warping

  (define (warp-cursor-to-window w #!optional x y)
    "Move the mouse pointer to position (X, Y) relative to the client window
associated with object WINDOW.

If X and Y are nil, then the pointer is moved to a default position, as
specified by the user."
    (when warp-to-window-enabled
      (let ((coords (window-position w))
	    (foff (window-frame-offset w))
	    (dims (window-dimensions w)))
	(unless x
	  (setq x
		(if (< (car warp-to-window-offset) 0)
		    (car warp-to-window-offset)
		  (quotient (* (car dims) (car warp-to-window-offset)) 100))))
	(unless y
	  (setq y (if (< (cdr warp-to-window-offset) 0)
		      (cdr warp-to-window-offset)
		    (quotient (* (cdr dims) (cdr warp-to-window-offset)) 100))))
	(warp-cursor
	 (max 0 (min (1- (screen-width)) (+ x (car coords) (- (car foff)))))
	 (max 0 (min (1- (screen-height)) (+ y (cdr coords) (- (cdr foff)))))))))

  (define (activate-window w)
    (require 'sawfish.wm.focus)
    (require 'sawfish.wm.util.stacking)
    (require 'sawfish.wm.util.window-order)
    (raise-window* w)
    (when (window-really-wants-input-p w)
      (set-input-focus w))
    (warp-pointer-if-necessary w)
    (window-order-push w)
    (call-window-hook 'activate-window-hook w))

;;; resizing windows in accordance with their size hints

  (define (constrain-dimension-to-hints x dimension hints)
    (let ((base (cdr (assq (if (eq dimension 'x)
			       'base-width 'base-height) hints)))
	  (minimum (cdr (assq (if (eq dimension 'x)
				  'min-width 'min-height) hints)))
	  (maximum (or (cdr (assq (if (eq dimension 'x)
				      'max-width 'max-height) hints)) 65535))
	  (inc (or (cdr (assq (if (eq dimension 'x)
				  'width-inc 'height-inc) hints)) 1)))
      (let ((bottom (or base minimum 1)))
	(unless (= (mod (- x bottom) inc) 0)
	  (setq x (inexact->exact
		   (+ (* (ceiling (/ (- x bottom) inc)) inc) bottom)))))
      (clamp x (or minimum base 1) maximum)))

  (define (constrain-aspect-to-hints a old-b dimension min-aspect max-aspect)
    (let ((min-ratio (/ (cadr min-aspect) (cddr min-aspect)))
          (max-ratio (/ (cadr max-aspect) (cddr min-aspect))))
      (if (eq dimension 'y)
          (clamp old-b (floor (* a min-ratio)) (floor (* a max-ratio)))
        (clamp old-b (floor (/ a max-ratio)) (floor (/ a min-ratio))))))

  (define (resize-window-with-hints w cols rows #!optional hints)
    "Resize window W to COLS x ROWS, using the window's size hints to define
the row and column size, and the minimum possible size.

If HINTS is non-nil, then it is the size hints structure to use. Otherwise
(window-size-hints W) is used."
    (unless hints
      (setq hints (window-size-hints w)))
    (let ((x-base (or (cdr (or (assq 'base-width hints)
			       (assq 'min-width hints))) 1))
	  (x-inc (or (cdr (assq 'width-inc hints)) 1))
	  (y-base (or (cdr (or (assq 'base-height hints)
			       (assq 'min-height hints))) 1))
	  (y-inc (or (cdr (assq 'height-inc hints)) 1)))
      (resize-window-to
       w (constrain-dimension-to-hints (+ x-base (* x-inc cols)) 'x hints)
       (constrain-dimension-to-hints (+ y-base (* y-inc rows)) 'y hints))))

  (define (resize-window-with-hints* w width height #!optional hints)
    "Resize window W to WIDTH x HEIGHT, with WIDTH and HEIGHT defined in
terms of pixels. The window's size hints structure defines the minimum
and maximum dimensions of the window, within which WIDTH and HEIGHT are
constrained.

If HINTS is non-nil, then it is the size hints structure to use. Otherwise
(window-size-hints W) is used."
    (unless hints
      (setq hints (window-size-hints w)))
    (resize-window-to w (constrain-dimension-to-hints width 'x hints)
		      (constrain-dimension-to-hints height 'y hints)))

  (define (window-gravity w #!optional hints)
    (or (window-get w 'gravity)
	(cdr (assq 'window-gravity (or hints (window-size-hints w))))
	;; default gravity is NorthWest (from ICCCM)
	'north-west))

  (define (adjust-position-for-gravity/x w grav x #!key inverse)
    (let* ((tl-off (car (window-frame-offset w)))
	   (br-off (- (car (window-frame-dimensions w))
		      (car (window-dimensions w))))
	   (sign (if inverse -1 +1)))
      (cond ((eq grav 'static)
	     ;; static gravity is relative to the original
	     ;; client window position
	     (+ x (* sign tl-off)))
	    ((memq grav '(east south-east north-east))
	     ;; relative to the right of the frame
	     (- x (* sign (+ br-off (* -2 (window-border-width w))))))
	    ((memq grav '(north center south))
	     ;; relative to the horizontal center of the frame
	     (- x (* sign (quotient
			   (+ br-off (* -2 (window-border-width w))) 2))))
	    (t x))))

  (define (adjust-position-for-gravity/y w grav y #!key inverse)
    (let* ((tl-off (cdr (window-frame-offset w)))
	   (br-off (- (cdr (window-frame-dimensions w))
		      (cdr (window-dimensions w))))
	   (sign (if inverse -1 +1)))
      (cond ((eq grav 'static)
	     ;; static gravity is relative to the original
	     ;; client window position
	     (+ y (* sign tl-off)))
	    ((memq grav '(south south-east south-west))
	     ;; relative to the bottom of the frame
	     (- y (* sign (+ br-off (* -2 (window-border-width w))))))
	    ((memq grav '(east center west))
	     ;; relative to the vertical center of the frame
	     (- y (* sign (quotient
			   (+ br-off (* -2 (window-border-width w))) 2))))
	    (t y))))

  ;; UNADJUST means to reverse the gravity compensation, suitable for
  ;; when unmanaging windows at shutdown
  (define (adjust-position-for-gravity w grav coords #!optional unadjust)
    (cons (adjust-position-for-gravity/x
	   w grav (car coords) #:inverse unadjust)
	  (adjust-position-for-gravity/y
	   w grav (cdr coords) #:inverse unadjust)))

;;; deleting windows

  (define (delete-window w #!optional safely)
    "Delete the window."
    (cond
     ((window-supports-wm-protocol-p w 'WM_DELETE_WINDOW)
      (send-client-message w 'WM_PROTOCOLS (vector (x-atom 'WM_DELETE_WINDOW)
						   (x-server-timestamp)) 32))
     (safely (beep))
     (t (x-kill-client w))))

  (define-command 'delete-window delete-window #:spec "%W")

  (define (delete-window-safely w)
    "Delete the window, or beep if the window can't be closed safely."
    (delete-window w t))

  (define-command 'delete-window-safely delete-window-safely #:spec "%W")

;;; making window names unique

  (define (uniquify-name in existing)
    "Uniquify the string IN from the list of strings EXISTING. Uses the format
string `uniquify-name-format' to generate unique names."
    (letrec
	((again (lambda (i)
		  (if (member (format nil uniquify-name-format in i) existing)
		      (again (1+ i))
		    (format nil uniquify-name-format in i)))))
      (if (member in existing)
	  (again 2)
	in)))

  (define (uniquify-window-name w)
    "Force the current window to have a unique title."
    (set-x-text-property
     w 'WM_NAME
     (vector (uniquify-name (window-name w)
			    (mapcar window-name (delq w (managed-windows)))))))

  (define-command 'uniquify-window-name uniquify-window-name #:spec "%W")

;; selecting a single window

  (define select-window-map
    (bind-keys (make-keymap)
      "Any-Click1" (lambda ()
		     (throw 'select-window (query-pointer-window)))))

  (defvar select-window-cursor-shape 'crosshair)

  (define (select-window)
    (allow-events 'async-pointer)
    (when (grab-pointer nil select-window-cursor-shape)
      (unwind-protect
	  (let ((override-keymap select-window-map))
	    (catch 'select-window
	      (recursive-edit)))
	(ungrab-pointer))))

;;; avoided (i.e. non-overlapped) windows

  (define (window-avoided-p w)
    "Return t if window W should be kept unobscured by other windows wherever
possible."
    (cond ((or (not (window-mapped-p w))
	       (not (window-visible-p w))) nil)
	  ((window-get w 'avoid) t)
	  ((and dont-avoid-ignored (window-get w 'ignored)) nil)
	  (t avoid-by-default)))

  (define (avoided-windows #!optional window)
    "Returns a list of all windows that should be left unobscured where
possible. If WINDOW is defined, then it defines a window that will be never
returned in the list."
    (filter-windows (lambda (w)
		      (and (not (eq w window))
			   (window-avoided-p w)))))

;;; property and window-state changed interface

  (define prop-changes '())

  (define (call-after-property-changed prop fun)
    "Arrange for function FUN to be called with arguments (WINDOW PROPERTY
STATE) when the X11 property named PROP (a symbol) changes. PROP may also
be a list of property names to monitor.

Kluge: if PROP is `WM_NAME', it is replaced with `(WM_NAME _NET_WM_NAME)'.
This is done to cope with themes that want to update title bars on name
changes but only watch `WM_NAME'.  A warning is printed on stderr when
this substitution occurs.  Those themes should really be fixed."
    (setq prop-changes
          (cons
           (cons (cond ((listp prop) prop)
                       ((eq prop 'WM_NAME)
                        (format standard-error
"(call-after-property-changed 'WM_NAME ...) should probably be
(call-after-property-changed '(WM_NAME _NET_WM_NAME) ...);
use '(WM_NAME) if you really want only WM_NAME\n")
                        '(WM_NAME _NET_WM_NAME))
                       (t (list prop)))
                 fun)
           prop-changes)))

  (add-hook 'property-notify-hook
	    (lambda (w prop state)
	      (mapc (lambda (cell)
                      (when (memq prop (car cell))
			((cdr cell) w prop state)))
		    prop-changes)))

  (define state-changes '())

  (define (call-after-state-changed states fun)
    "Arrange for function FUN to be called with arguments (WINDOW
CHANGED-STATES) when one of the states defined by the list of symbols
STATES has been changed. STATES may also be a single symbol."
    (setq state-changes (cons (cons (if (listp states)
					states
				      (list states)) fun) state-changes)))

  (add-hook
   'window-state-change-hook
   (lambda (w states)
     (mapc (lambda (cell)
	     (let
		  ((relevant (filter (lambda (state)
					(memq state (car cell))) states)))
	       (when relevant
		 ((cdr cell) w relevant))))
	    state-changes)))

;; gaollable functions

  (gaol-add window-really-wants-input-p window-class window-avoided-p
	    call-after-property-changed call-after-state-changed
	    window-get window-name window-full-name window-icon-name
	    window-mapped-p window-frame set-window-frame rebuild-frame
	    window-position window-dimensions window-frame-dimensions windowp
	    managed-windows stacking-order window-visibility
	    window-transient-p window-urgent-p window-shaped-p window-visible-p
	    window-framed-p window-id window-group-id window-size-hints
	    call-window-hook input-focus window-icon-image map-windows
	    filter-windows))
