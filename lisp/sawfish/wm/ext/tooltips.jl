;; tooltips.jl -- display frame-part keymap descriptions
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

(require 'timers)
(provide 'tooltips)

(defvar tooltips-timer nil)

(defgroup tooltips "Tooltips"
  :group misc)

(defcustom tooltips-enabled nil
  "Display tooltips for window frames."
  :type boolean
  :group (misc tooltips)
  :require tooltips)

(defcustom tooltips-timeout-enabled nil
  "Remove tooltips after a period of time."
  :type boolean
  :group (misc tooltips))

(defcustom tooltips-show-doc-strings t
  "Show full documentation in tooltips."
  :type boolean
  :group (misc tooltips))

(defcustom tooltips-delay 500
  "Number of milliseconds before displaying tooltips."
  :type number
  :range (0 . nil)
  :group (misc tooltips))

(defcustom tooltips-timeout-delay 5000
  "Number of milliseconds before removing tooltips."
  :type number
  :range (0 . nil)
  :group (misc tooltips))

(defcustom tooltips-font "-*-lucidatypewriter-medium-*-*-*-10-*-*-*-*-*-*-*"
 "Font used to display tooltips."
 :type font
 :group (misc tooltips))

(defcustom tooltips-background-color "grey85"
  "Color used for the tooltips background"
  :group (misc tooltips)
  :type color)

(defcustom tooltips-foreground-color "black"
  "Color used for the tooltips foreground"
  :group (misc tooltips)
  :type color)

;; the window it's displayed for
(defvar tooltips-displayed nil)

(defun tooltips-cleanup ()
  (when (in-hook-p 'pre-command-hook tooltips-cleanup)
    (remove-hook 'pre-command-hook tooltips-cleanup))
  (when tooltips-displayed
    (display-message nil)
    (setq tooltips-displayed nil))
  (when tooltips-timer
    (delete-timer tooltips-timer)
    (setq tooltips-timer nil)))

;; each item is (EVENT-DESC . DOC)
(defun tooltips-format (items)
  (let
      ((max-event-width 0)
       (split (lambda (s)
		(let
		    (point parts)
		  (while (string-match "\n" s point)
		    (setq parts (cons (substring
				       s (or point 0) (match-start)) parts))
		    (setq point (match-end)))
		  (setq parts (cons (substring s (or point 0)) parts))
		  (nreverse parts))))
       out)
    (mapc (lambda (cell)
	    (setq max-event-width (max max-event-width (length (car cell)))))
	  items)
    (setq max-event-width (1+ max-event-width))
    (mapc (lambda (cell)
	    (setq out (cons (car cell) out))
	    (setq out (cons (make-string (- max-event-width
					    (length (car cell))) ? ) out))
	    (let
		((parts (split (cdr cell))))
	      (when parts
		(setq out (cons (car parts) out))
		(setq out (cons ?\n out)))
	      (mapc (lambda (string)
		      (setq out (cons (make-string max-event-width ? ) out))
		      (setq out (cons string out))
		      (setq out (cons ?\n out))) (cdr parts)))) items)
    (apply concat (nreverse out))))

(defun tooltips-display (win fp)
  (let
      ((keymap (frame-part-get fp 'keymap))
       (pos (query-pointer))
       (pos-fn (lambda (in size inc)
		 (if (< in (/ size 2))
		     (+ in inc)
		   (- (+ (- size in) inc)))))
       (doc-fn (lambda (command)
		 (let
		     (doc)
		   (if (and tooltips-show-doc-strings command
			    (symbolp command)
			    (progn
			      (require 'lisp-doc)
			      (setq doc (documentation command))))
		       (_ doc)
		     (format nil "%S" command)))))
       items)
    (when (symbolp keymap)
      (setq keymap (symbol-value keymap)))
    (map-keymap (lambda (cell)
		  (setq items (cons (cons (event-name (cdr cell))
					  (doc-fn (car cell))) items)))
		keymap)
    (when items
      (rplaca pos (pos-fn (car pos) (screen-width) 0))
      (rplacd pos (pos-fn (cdr pos) (screen-height) 16))
      (display-message (tooltips-format (nreverse items))
		       `((position . ,pos)
			 (background . ,tooltips-background-color)
			 (foreground . ,tooltips-foreground-color)
			 (x-justify . left)
			 (spacing . 2)
			 (font . ,tooltips-font)))
      (setq tooltips-displayed win)
      (when tooltips-timeout-enabled 
	(setq tooltips-timer (make-timer tooltips-cleanup
					 (quotient tooltips-timeout-delay 1000)
					 (mod tooltips-timeout-delay 1000))))
      (unless (in-hook-p 'pre-command-hook tooltips-cleanup)
	(add-hook 'pre-command-hook tooltips-cleanup)))))

(defun tooltips-fp-enter (win fp)
  (when tooltips-enabled
    (let
	((callback (lambda ()
		     (setq tooltips-timer nil)
		     (unless (clicked-frame-part)
		       (tooltips-display win fp)))))
      (when tooltips-timer
	(delete-timer tooltips-timer))
      (setq tooltips-timer (make-timer callback
				       (quotient tooltips-delay 1000)
				       (mod tooltips-delay 1000)))
      (unless (in-hook-p 'pre-command-hook tooltips-cleanup)
	(add-hook 'pre-command-hook tooltips-cleanup)))))

(defun tooltips-unmapped (win)
  (when (eq win tooltips-displayed)
    (tooltips-cleanup)))

(add-hook 'enter-frame-part-hook tooltips-fp-enter)
(add-hook 'leave-frame-part-hook tooltips-cleanup)
(add-hook 'unmap-notify-hook tooltips-unmapped)
