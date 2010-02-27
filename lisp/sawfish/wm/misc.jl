;; sawfish.wm.misc bootstrap
;;
;; Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
;;
;; This file is part of sawfish.
;;
;; sawfish is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; sawfish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sawfish; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(declare (in-module sawfish.wm.misc))

(open-structures '(rep
		   rep.regexp
		   rep.module-system
		   rep.io.files
		   #|
		     Don't add more sawfish.wm.* here unless you know
		     what you're doing. Instead, embed 'require' in
		     definition. It's because this file is read
		     from windows.jl. See windows.jl for more.
		   |#
		   sawfish.wm.windows.subrs
		   sawfish.wm.events))

(defvar *user-module* nil)

(defmacro with-server-grabbed (#!rest forms)
  "Execute FORMS with the server grabbed."
  `(progn
     (grab-server)
     (unwind-protect
	 (progn ,@forms)
       (ungrab-server))))

(define (call-with-server-ungrabbed thunk)
  "Return the result of calling the zero-parameter function THUNK. If the
server is currently grabbed, ungrab it first, restoring the original grab
status after the call to THUNK returns."
  (let loop ((counter 0))
       (if (server-grabbed-p)
           (progn
             (ungrab-server)
             (loop (1+ counter)))
         (unwind-protect
             (thunk)
           (do ((i 0 (1+ i)))
               ((= i counter))
             (grab-server))))))

(define grab-counter 0)

(define (call-with-keyboard-grabbed thunk)
  "Call the zero-parameter function THUNK with the keyboard grabbed. If unable
to grab the keyboard then THUNK won't be called."
  (when (grab-keyboard)
    (unwind-protect
	(progn
	  (setq grab-counter (1+ grab-counter))
	  (thunk))
      (when (zerop (setq grab-counter (1- grab-counter)))
	(ungrab-keyboard)))))

(define (call-with-error-handler thunk)
  (condition-case data
      (thunk)
    (error
     (error-handler-function (car data) (cdr data)))))

(define (make-directory-recursively dir)
  "Try to create dir and all nonexistent parent dirs (like mkdir -p)."
  (while (not (file-exists-p dir))
    (let ((tem dir))
      (while (not (file-exists-p (expand-file-name ".." tem)))
	(setq tem (expand-file-name ".." tem)))
      (make-directory tem))))

(define (locate-file filename dirs)
  "Search for a file called FILENAME in any of the directories named by the
list of strings DIRS."
  (let loop ((rest dirs))
       (cond ((null rest) nil)
             ((file-exists-p (expand-file-name filename (car rest)))
              (expand-file-name filename (car rest)))
             (t (loop (cdr rest))))))

(define (clamp x lower upper)
  (cond ((< x lower) lower)
	((> x upper) upper)
	(t x)))

(define (clamp* x w lower upper)
  (cond ((< x lower) lower)
	((> (+ x w) upper) (- upper w))
	(t x)))

(define (uniquify-list lst)
  "Remove all duplicates from list, tests using eq, order is lost."
  (let (out)
    (mapc (lambda (x)
	    (unless (memq x out)
	      (setq out (cons x out)))) lst)
    out))

(define (screen-dimensions)
  "Return the screen dimensions in pixels as a cons cell `(WIDTH . HEIGHT)'."
  (cons (screen-width) (screen-height)))

(define (pointer-head)
  "Return the id of a head currently containing the mouse pointer."
  (find-head (query-pointer)))

(define (current-head #!optional (w (input-focus)))
  "Return the id of the `current' head."
  (require 'sawfish.wm.windows)
  (if (not (desktop-window-p w))
      (or (and w (let ((point (window-position w))
                       (dims (window-dimensions w)))
                   (find-head (+ (car point) (quotient (car dims) 2))
                              (+ (cdr point) (quotient (cdr dims) 2)))))
          (pointer-head))
    (pointer-head)))

(define (current-head-dimensions #!optional (w (input-focus)))
  "Return a cons-cell defining the size in pixels of the current head (that
containing the window W, or containing the window with input focus if W is
false). Returns the screen dimensions if no such head can be identified."
  (or (and w (let ((head (current-head w)))
               (if head
                   (head-dimensions head)
                 (screen-dimensions))))
      (head-dimensions (pointer-head))))

(define (current-head-offset #!optional (w (input-focus)))
  "Return a cons-cell defining the origin of the current head (that
containing the window W, or containing the window with input focus if W is
false). Returns '(0 . 0) if no such head can be identified."
  (or (and w (let ((head (current-head w)))
               (if head
                   (head-offset head)
                 (cons 0 0))))
      (head-offset (pointer-head))))

(define (load-module name)
  "Ensure that the module called NAME has been loaded. Note that this does
_not_ import its bindings (or even make them accessible)."
  (intern-structure name))

(define (eval-in form struct-name)
  "Evaluate FORM in the module called STRUCT-NAME."
  (eval form (or (get-structure struct-name)
		 (error "Unknown module: %s" struct-name))))

(define (user-eval form)
  "Evaluate FORM in the `user' module."
  (eval form *user-module*))

(define (user-require feature)
  "Require FEATURE in the `user' module."
  (user-eval `(require ',feature)))

(define (quote-menu-item string)
  "Escape any `_' characters in STRING such that the result can be used as
the label of a menu item."
  (string-replace "_" "__" string))

(define (get-x-text-property w prop)
  "Fetches the X property named PROP from window W and returns a
vector of strings representing the contents of the property."
  (let ((value (get-x-property w prop)))
    (when (and value (stringp (caddr value)))
      (let ((string (caddr value)))
	;; regexp code chokes on NUL bytes, so do this the long way..
	(let loop ((start 0)
		   (point 0)
		   (out '()))
             (cond ((= point (length string))
                    (apply vector (nreverse (cons (substring
                                                   string start point) out))))
                   ((= (aref string point) 0)
                    (loop (1+ point) (1+ point)
                          (cons (substring string start point) out)))
                   (t (loop start (1+ point) out))))))))

(define (set-x-text-property w prop seq #!optional (encoding 'STRING))
  "Set the X property named PROP on window W to the text property obtained
by concatenating the sequence of strings SEQ."
  (set-x-property w prop (mapconcat identity seq 0) encoding 8))

;; exports

(export-bindings
 '(with-server-grabbed call-with-server-ungrabbed
                       call-with-keyboard-grabbed call-with-error-handler
                       make-directory-recursively locate-file
                       clamp clamp* uniquify-list screen-dimensions
                       pointer-head current-head current-head-dimensions
                       current-head-offset load-module eval-in
                       user-eval user-require quote-menu-item
                       get-x-text-property set-x-text-property))
