;; functions.jl -- miscellaneous stuff
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

(provide 'functions)

(defcustom ignore-window-input-hint t
  "Give focus to windows even when they haven't asked for it."
  :type boolean
  :user-level expert
  :group focus)

(defcustom warp-to-window-x-offset -1
  "Offset in percent from left window edge, when warping.
A negative number means warp to outside the left window edge."
  :type (number -65536 65535)
  :user-level expert
  :group focus)
 
(defcustom warp-to-window-y-offset -1
  "Offset in percent from top window edge, when warping.
A negative number means warp to outside the top window edge."
  :type (number -65536 65535)
  :user-level expert
  :group focus)
 
(defvar dont-avoid-ignored t
  "When non-nil, ignored windows aren't avoided by default.")

(defvar avoid-by-default nil
  "When non-nil, any unspecified windows are avoided by default.")

(defcustom xterm-program "xterm"
  "The name of the program launched by the `xterm' command."
  :type string
  :user-level expert
  :group misc)
(defvar xterm-args nil
  "Either a string defining the list of arguments given to the `xterm' command,
or the symbol `nil'."
  :type (optional string)
  :user-level expert
  :group misc)

(defcustom uniquify-name-format "%s [%d]"
  "Format string used to give windows unique names. Has two arguments (NAME
INDEX) applied to it."
  :type string
  :user-level expert
  :group misc)

(defun get-window-by-name (name &optional lst)
  "Find a window object whose window-name is NAME. If LST is non-nil, then it
is the list of windows to search. Returns nil if no such window is found."
  (catch 'foo
    (mapc (lambda (w)
	    (when (string= (window-name w) name)
	      (throw 'foo w))) (or lst (managed-windows)))
    nil))

(defun get-window-by-name-re (name &optional lst)
  "Find a window object whose window-name matches the regexp NAME.
If LST is non-nil, then it is the list of windows to search. Returns nil
if no such window is found."
  (catch 'foo
    (mapc (lambda (w)
	    (when (string-match name (window-name w))
	      (throw 'foo w))) (or lst (managed-windows)))
    nil))

(defmacro map-windows (fun)
  "Map the single-parameter function FUN over all existing windows."
  `(mapc ,fun (managed-windows)))

(defmacro with-server-grabbed (&rest forms)
  "Execute FORMS with the server grabbed."
  `(progn
     (grab-server)
     (unwind-protect
	 (progn ,@forms)
       (ungrab-server))))

(let
    ((counter 0))
  (defun call-with-keyboard-grabbed (thunk)
"Call the zero-parameter function THUNK with the keyboard grabbed. If unable
to grab the keyboard then THUNK won't be called."
    (when (grab-keyboard)
      (unwind-protect
	 (progn
	   (setq counter (1+ counter))
	   (thunk))
	(when (zerop (setq counter (1- counter)))
	  (ungrab-keyboard))))))

(defmacro save-stacking-order (&rest forms)
  "Execute FORMS, then reinstall the original stacking order."
  (let
      ((tem (gensym)))
    `(let
	 ((,tem (stacking-order)))
       (unwind-protect
	   (progn ,@forms)
	 (restack-windows ,tem)))))

(defun make-directory-recursively (dir)
  "Try to create dir and all nonexistent parent dirs (like mkdir -p)."
  (while (not (file-exists-p dir))
    (let
	((tem dir))
      (while (not (file-exists-p (expand-file-name ".." tem)))
	(setq tem (expand-file-name ".." tem)))
      (make-directory tem))))

(defun locate-file (filename dirs)
  "Search for a file called FILENAME in any of the directories named by the
list of strings DIRS."
  (letrec
      ((loop (lambda (rest)
	       (cond ((null rest) nil)
		     ((file-exists-p (expand-file-name filename (car rest)))
		      (expand-file-name filename (car rest)))
		     (t (loop (cdr rest)))))))
    (loop dirs)))

(defun xterm ()
  "Start a new xterm."
  (interactive)
  (system (format nil "%s %s >/dev/null 2>&1 </dev/null &"
		  xterm-program (or xterm-args ""))))

(defun window-really-wants-input-p (w)
  "Return nil if window W should never be focused."
  (and (not (window-get w 'never-focus))
       (or ignore-window-input-hint
	   (window-get w 'ignore-window-input-hint)
	   (window-wants-input-p w))))

(defun window-class (w)
  "Return the class that window W belongs to, as a string. Returns `nil' if W
has no associated class."
  (let
      ((prop (get-x-text-property w 'WM_CLASS)))
    (and prop (aref prop 1))))

(defun member-if (fun lst)
  (cond ((null lst) '())
	((fun (car lst)) lst)
	(t (member-if fun (cdr lst)))))

(defun uniquify-list (lst)
  "Remove all duplicates from list, tests using eq, order is lost."
  (let
      (out)
    (mapc (lambda (x)
	    (unless (memq x out)
	      (setq out (cons x out)))) lst)
    out))

(defun uniquify-name (in existing)
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

(defun uniquify-window-name (w)
  "Force the current window to have a unique title."
  (interactive "%W")
  (set-x-text-property
   w 'WM_NAME
   (vector (uniquify-name (window-name w)
			  (mapcar window-name (delq w (managed-windows)))))))

(defun warp-cursor-to-window (w &optional x y)
  "Move the mouse pointer to position (X, Y) relative to the client window
associated with object WINDOW.

If X and Y are nil, then the pointer is moved to a default position, as
specified by the user."
  (let
      ((coords (window-position w))
       (foff (window-frame-offset w))
       (dims (window-dimensions w)))
    (unless x
      (setq x
            (if (< warp-to-window-x-offset 0)
		warp-to-window-x-offset
	      (quotient (* (car dims) warp-to-window-x-offset) 100))))
    (unless y
      (setq y (if (< warp-to-window-y-offset 0)
		  warp-to-window-y-offset
		(quotient (* (cdr dims) warp-to-window-y-offset) 100))))
    (warp-cursor
     (max 0 (min (1- (screen-width)) (+ x (car coords) (- (car foff)))))
     (max 0 (min (1- (screen-height)) (+ y (cdr coords) (- (cdr foff))))))))

(defun resize-window-with-hints (w cols rows &optional hints)
  "Resize window W to COLS x ROWS, using the window's size hints to define
the row and column size, and the minimum possible size.

If HINTS is non-nil, then it is the size hints structure to use. Otherwise
(window-size-hints W) is used."
  (unless hints
    (setq hints (window-size-hints w)))
  (let
      ((x-base (or (cdr (or (assq 'base-width hints)
			    (assq 'min-width hints))) 1))
       (x-inc (or (cdr (assq 'width-inc hints)) 1))
       (y-base (or (cdr (or (assq 'base-height hints)
			    (assq 'min-height hints))) 1))
       (y-inc (or (cdr (assq 'height-inc hints)) 1))
       (x-max (cdr (assq 'max-width hints)))
       (y-max (cdr (assq 'max-height hints)))
       (scale (lambda (x base inc maximum)
		(min (+ base (* inc (max 0 x))) (or maximum 65535)))))
    (resize-window-to w (scale cols x-base x-inc x-max)
		      (scale rows y-base y-inc y-max))))

(defun get-window-wm-protocols (w)
  "Return a list of symbols defining the X11 window manager protocols supported
by client window W."
  (let*
      ((prop (get-x-property w 'WM_PROTOCOLS))
       (data (and prop (eq (car prop) 'ATOM) (nth 2 prop))))
    (when data
      (let ((i 0)
	    out)
	(while (< i (length data))
	  (setq out (cons (aref data i) out))
	  (setq i (1+ i)))
	(nreverse out)))))

(defun delete-window (w &optional safely)
  "Delete the window."
  (interactive "%W")
  (cond
   ((memq 'WM_DELETE_WINDOW (get-window-wm-protocols w))
    (send-client-message w 'WM_PROTOCOLS (vector (x-atom 'WM_DELETE_WINDOW)
						 (x-server-timestamp)) 32))
   (safely
    (beep))
   (t
    (x-kill-client w))))

(defun delete-window-safely (w)
  "Delete the window, or beep if the window can't be closed safely."
  (interactive "%W")
  (delete-window w t))

(defun screen-dimensions ()
  "Return the screen dimensions in pixels as a cons cell `(WIDTH . HEIGHT)'."
  (cons (screen-width) (screen-height)))

(defun current-head (&optional w)
  "Return the ID of the `current' head."
  (unless w
    (setq w (input-focus)))
  (if w
      (let ((point (window-position w))
	    (dims (window-dimensions w)))
	(find-head (+ (car point) (quotient (car dims) 2))
		   (+ (cdr point) (quotient (cdr dims) 2))))
    (find-head (query-pointer))))

(defun current-head-dimensions (&optional w)
  (head-dimensions (current-head w)))

(defun current-head-offset (&optional w)
  (head-offset (current-head w)))


;; property and window-state changed interface

(let
    (prop-changes)

  (defun call-after-property-changed (prop fun)
    "Arrange for function FUN to be called with arguments (WINDOW PROPERTY
STATE) when the X11 property named PROP (a symbol) changes. PROP may also
be a list of property names to monitor."
    (setq prop-changes (cons (cons (if (listp prop)
				       prop
				     (list prop)) fun) prop-changes)))
  
  (add-hook 'property-notify-hook
	    (lambda (w prop state)
	      (mapc (lambda (cell)
                      (when (memq prop (car cell))
			((cdr cell) w prop state)))
		    prop-changes))))

(let
    (state-changes)
  
  (defun call-after-state-changed (states fun)
    "Arrange for function FUN to be called with arguments (WINDOW CHANGED-STATES)
when one of the states defined by the list of symbols STATES has been changed.
STATES may also be a single symbol."
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
	    state-changes))))


;; avoided (i.e. non-overlapped) windows

(defun window-avoided-p (w)
  "Return t if window W should be kept unobscured by other windows wherever
possible."
  (cond ((or (not (window-mapped-p w))
	     (not (window-visible-p w)))
	 nil)
	((window-get w 'avoid)
	 t)
	((and dont-avoid-ignored (window-get w 'ignored))
	 nil)
	(t
	 avoid-by-default)))

(defun avoided-windows (&optional window)
  "Returns a list of all windows that should be left unobscured where possible.
If WINDOW is defined, then it defines a window that will be never returned
in the list."
  (delete-if (lambda (w)
	       (or (eq w window) (not (window-avoided-p w))))
	     (managed-windows)))
