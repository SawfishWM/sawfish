;; keymap.jl -- some keymap utilities, mostly copied from jade
;; $Id: keymap.jl,v 1.18 2000/12/19 23:49:54 jsh Exp $

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

(define-structure sawfish.wm.util.keymap

    (export map-keymap
	    substitute-keymap-command
	    substitute-keymap-event
	    lazy-bind-keys
	    where-is
	    make-memoizing-where-is
	    describe-key
	    read-event
	    quote-event)

    (open rep
	  rep.system
	  rep.data.symbol-table
	  sawfish.wm.events
	  sawfish.wm.commands
	  sawfish.wm.misc
	  sawfish.wm.windows)

  (define-structure-alias keymap sawfish.wm.util.keymap)

;;; Map a function over all key bindings in a keymap

  (defvar map-keymap-recursively t
    "When nil, the map-keymap function will only map over the first level of
keymaps, i.e. all prefix keys are ignored.")

  (define (map-keymap function keymap)
    "Map FUNCTION over all key bindings under the keymap or list of
keymaps KEYMAP. FUNCTION is called as (FUNCTION KEY), where KEY is a
cons cell (COMMAND . EVENT)."
    (when (keymapp keymap)
      (setq keymap (list keymap)))
    (let ((keymap-list keymap)
	 (done-list nil))
      (while keymap-list
	(let
	    ((keymap (car keymap-list)))
	  (setq keymap-list (cdr keymap-list))
	  (when (and (not (keymapp keymap)) (consp keymap))
	    (setq keymap (car keymap)))
	  (unless (memq keymap done-list)
	    (setq done-list (cons keymap done-list))
	    (when (symbolp keymap)
	      (setq keymap (symbol-value keymap)))
	    (when (keymapp keymap)
	      (mapc (lambda (k)
		      (funcall function k)) (cdr keymap))))))))

;;; Substitute one command for another in a keymap

  (define (substitute-keymap-command olddef newdef #!optional keymap)
    "Substitute all occurrences of the command OLDDEF for the command NEWDEF
in the keybindings under the keymap or list of keymaps KEYMAP."
    (map-keymap (lambda (k)
		  (when (eq (car k) olddef)
		    (rplaca k newdef))) keymap))

  (define (substitute-keymap-event old-ev new-ev #!optional keymap)
    "Substitute all occurrences of the event OLD-EV for the event NEW-EV
in the keybindings under the keymap or list of keymaps KEYMAP."
    (when (stringp old-ev)
      (setq old-ev (lookup-event old-ev)))
    (when (stringp new-ev)
      (setq new-ev (lookup-event new-ev)))
    (map-keymap (lambda (k)
		  (when (equal (cdr k) old-ev)
		    (rplacd k new-ev))) keymap))


;;; Adding bindings to a feature that may not yet be loaded

  (defmacro lazy-bind-keys (feature keymap #!rest bindings)
    "Install the list of BINDINGS in KEYMAP, assuming that KEYMAP is available
once FEATURE has been provided. If FEATURE has not yet been loaded, arrange
for the bindings to be installed if and when it is."
    `(if (featurep ',feature)
	 (bind-keys ,keymap ,@bindings)
       (eval-after-load ,(symbol-name feature)
			'(bind-keys ,keymap ,@bindings))))

;;; Search for a named command in the current keymap configuration

  (define (where-is command #!optional keymap)
    (require 'sawfish.wm.util.decode-events)
    (let ((where-is-results '()))
      (map-keymap (lambda (k)
		    (when (eq (car k) command)
		      (setq where-is-results (cons (event-name (substitute-wm-modifier (cdr k))) where-is-results))))
		  (or keymap global-keymap))
      where-is-results))

  ;; XXX make this handle prefix keymaps
  (define (make-memoizing-where-is keymaps)
    "Return a function which when called with a command name as its single
argument, will return either false or a string naming the event to which
the command is found in the list of keymaps KEYMAPS."
    (require 'sawfish.wm.util.decode-events)
    (let ((cache (make-symbol-table)))
      (mapc (lambda (keymap)
	      (map-keymap (lambda (k)
			    (when (symbolp (car k))
			      (symbol-table-set cache (car k) (cdr k))))
			  keymap))
	    keymaps)
      (lambda (command)
	(let ((key (symbol-table-ref cache command)))
	  (and key (event-name (substitute-wm-modifier key)))))))

  (define (describe-key #!optional map)
    "Prompt for a key sequence, then print its binding."
    (require 'rep.lang.doc)
    (require 'sawfish.wm.commands.describe)
    (let (components)
      (letrec
	  ((loop
	    (lambda (keymap)
	      (let* ((key (read-event (concat "Describe key: " components)))
		     (binding (and key (search-keymap key keymap))))
		(when binding
		  (setq binding (car binding))
		  (setq components (concat components
					   (and components ? )
					   (event-name key)))
		  (cond ((keymapp binding)
			 (loop binding))
			((and (symbolp binding)
			      (keymapp (symbol-value binding t)))
			 (loop (symbol-value binding)))
			(t
			 (format standard-output
				 "`%s' is bound to `%s'\n\n"
				 components binding)
			 (describe-symbol binding))))))))
	(loop (or map global-keymap)))))

  ;;###autoload
  (define-command 'describe-key describe-key)
  (define-command-to-screen 'describe-key-to-screen describe-key)

;;; grab the next key event

  (define (read-event #!optional prompt)
    (call-with-keyboard-grabbed
     (lambda ()
       (unwind-protect
	   (let
	       ((override-keymap '(keymap))
		(unbound-key-hook
		 (list (lambda ()
			 (throw 'read-event (current-event))))))
	     (display-message (or prompt (_ "Press key...")))
	     (catch 'read-event
	       (recursive-edit)))
	 (display-message nil)))))

  (define (quote-event)
    "Sends the next key event directly to the focused client window, ignoring
any window manager bindings that it may have."
    (require 'sawfish.wm.util.decode-events)
    (let ((override-keymap '(keymap))
	  (unbound-key-hook (list (lambda ()
				    (let ((ev (decode-event (current-event))))
				      (if (eq 'key (car ev))
					  (progn
					    (allow-events 'replay-keyboard)
					    (throw 'quote-event))
					(allow-events 'sync-keyboard)))))))
      ;; grab on the client window, so that the events get replayed
      ;; to that window, not to our frame (thus avoiding any passive
      ;; grabs on the window)
      (when (grab-keyboard (window-id (input-focus)) nil t)
	(unwind-protect
	    (catch 'quote-event
	      (allow-events 'sync-keyboard)
	      (display-message (_ "[Quote]"))
	      (recursive-edit))
	  (display-message nil)
	  (ungrab-keyboard)))))

  ;;###autoload
  (define-command 'quote-event quote-event))
