;; decode-events.jl -- symbolic event manipulation
;; $Id$

;; Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

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

(define-structure sawfish.wm.util.decode-events

    (export decode-event
	    encode-event
	    string->keysym
	    modifier->keysyms
	    should-grab-button-event-p)

    (open rep
	  sawfish.wm.events)

  (define-structure-alias decode-events sawfish.wm.util.decode-events)

  (define (decode-event event)
    "Return a symbol description of the low-level event structure EVENT (a cons
cell). The symbolic description has the form `(TYPE MODIFIER-LIST ACTION)'."
    (let* ((code (car event))
	   (mods (cdr event))

	   (decode-mods
	    (lambda ()
	      (let ((out '()))
		(do ((i 0 (1+ i)))
		    ((= i 13))
		  (when (not (zerop (logand mods (lsh 1 i))))
		    (setq out (cons (aref [shift lock control mod-1 mod-2
						 mod-3 mod-4 mod-5 button-1
						 button-2 button-3 button-4
						 button-5] i) out))))
		(when (not (zerop (logand mods (lsh 1 20))))
		  (setq out (cons 'meta out)))
		(when (not (zerop (logand mods (lsh 1 21))))
		  (setq out (cons 'alt out)))
		(when (not (zerop (logand mods (lsh 1 24))))
		  (setq out (cons 'hyper out)))
		(when (not (zerop (logand mods (lsh 1 25))))
		  (setq out (cons 'super out)))
		(when (not (zerop (logand mods (lsh 1 22))))
		  (setq out (cons 'any out)))
		(when (not (zerop (logand mods (lsh 1 23))))
		  (setq out (cons 'release out)))
		out))))

      (cond ((not (zerop (logand mods (lsh 1 16))))
	     ;; keyboard event
	     (list 'key (decode-mods) (x-keysym-name code)))
	    ((not (zerop (logand mods (lsh 1 17))))
	     ;; mouse event
	     (list 'mouse (decode-mods)
		   (aref [click-1 click-2 move off click-3] (1- code))))
	    (t (error "Unknown event type")))))

  (define (encode-event event)
    "Return the low-level event structure (cons cell) representing the symbolic
event description EVENT, a list `(TYPE MODIFIER-LIST ACTION)'."
    (let* ((code 0)

	   (encode-mod-map '((shift . 1) (lock . 2)
			     (control . 4) (mod-1 . 8)
			     (mod-2 . 16) (mod-3 . 32) (mod-4 . 64)
			     (mod-5 . 128) (button-1 . 256)
			     (button-2 . 512) (button-3 . 1024)
			     (button-4 . 2048) (button-5 . 4096)
			     (meta . #x100000) (alt . #x200000)
			     (hyper . #x1000000) (super . #x2000000)
			     (any . #x400000) (release . #x800000)))

	   (mods (apply + (mapcar (lambda (m)
				    (cdr (assq m encode-mod-map)))
				  (cadr event)))))

      (cond ((eq (car event) 'key)
	     (setq mods (logior mods (lsh 1 16)))
	     (setq code (x-lookup-keysym (caddr event))))
	    ((eq (car event) 'mouse)
	     (setq mods (logior mods (lsh 1 17)))
	     (setq code (cdr (assq (caddr event) '((click-1 . 1)
						   (click-2 . 2)
						   (move . 3)
						   (off . 4)
						   (click-3 . 5))))))
	    (t (error "Unknown event type: %s" (car event))))
      (cons code mods)))

  (define (string->keysym string)
    "Convert a string naming a key into a symbol naming an X11 keysym."
    (x-keysym-name (car (lookup-event string))))

  (define (modifier->keysyms modifier)
    "Convert a symbol naming an event modifier into a list of symbols
representing the X11 keysyms that may generate the modifier."
    (cond ((eq modifier 'alt)
	   (mapcar string->keysym alt-keysyms))
	  ((eq modifier 'meta)
	   (mapcar string->keysym meta-keysyms))
	  ((eq modifier 'hyper)
	   (mapcar string->keysym hyper-keysyms))
	  ((eq modifier 'super)
	   (mapcar string->keysym super-keysyms))
	  ((eq modifier 'shift)
	   '(Shift_L Shift_R))
	  ((eq modifier 'control)
	   '(Control_L Control_R))
	  (t (error "Unknown modifier: %s" modifier))))

  (define (should-grab-button-event-p event keymap)
    (let* ((decoded (decode-event event))
	   (variants (mapcar (lambda (action)
			       (encode-event
				(list (car decoded) (cadr decoded) action)))
			     '(click-2 click-3 move off))))
      (let loop ((rest (cdr keymap)))
	(cond ((null rest) nil)
	      ((member (cdar rest) variants) t)
	      (t (loop (cdr rest))))))))
