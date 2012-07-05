;; tooltips.jl -- display frame-part keymap descriptions

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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

(define-structure sawfish.wm.ext.tooltips

    (export display-tooltip
	    display-tooltip-after-delay
	    remove-tooltip)

    (open rep
	  rep.system
	  rep.regexp
	  rep.io.timers
	  sawfish.wm.commands
	  sawfish.wm.custom
	  sawfish.wm.colors
	  sawfish.wm.misc
	  sawfish.wm.windows
	  sawfish.wm.frames
	  sawfish.wm.events
	  sawfish.wm.util.keymap)

  (define-structure-alias tooltips sawfish.wm.ext.tooltips)

  (define tooltips-timer nil)

  ;;###autoload (defgroup tooltips "Tooltips" :group appearance :require sawfish.wm.ext.tooltips)

  (defgroup tooltips "Tooltips"
    :group appearance
    :require sawfish.wm.ext.tooltips)

  (defcustom tooltips-enabled nil
    "Display tooltips for window frames."
    :type boolean
    :group (appearance tooltips)
    :require sawfish.wm.ext.tooltips)

  (defcustom tooltips-show-doc-strings nil
    "Show full documentation in tooltips."
    :type boolean
    :depends tooltips-enabled
    :group (appearance tooltips))

  (defcustom tooltips-delay 500
    "Number of milliseconds before displaying tooltips."
    :type number
    :depends tooltips-enabled
    :group (appearance tooltips))

  (defcustom tooltips-timeout-delay 5000
    "Number of milliseconds before removing tooltips."
    :type number
    :depends tooltips-enabled
    :group (appearance tooltips))

  (defcustom tooltips-font default-font
    "Font used to display tooltips, or nil for default."
    :type font
    :depends tooltips-enabled
    :group (appearance tooltips))

  (defcustom tooltips-background-color "grey85"
    "Color used for the tooltips background"
    :type color
    :depends tooltips-enabled
    :group (appearance tooltips))

  (defcustom tooltips-foreground-color "black"
    "Color used for the tooltips foreground"
    :type color
    :depends tooltips-enabled
    :group (appearance tooltips))

;;; displaying tooltips

  ;; the window it's displayed for (or t)
  (define tooltips-displayed nil)

  (define (display-tooltip text #!optional win)
    (let ((pos (query-pointer))
	  (pos-fn (lambda (in size inc)
		    (if (< in (/ size 2))
			(+ in inc)
		      (- (+ (- size in) inc))))))
      (rplaca pos (pos-fn (car pos) (screen-width) 0))
      (rplacd pos (pos-fn (cdr pos) (screen-height) 16))
      (display-message (if (functionp text) (text) text)
		       `((position . ,pos)
			 (background . ,tooltips-background-color)
			 (foreground . ,tooltips-foreground-color)
			 (x-justify . left)
			 (spacing . 2)
			 ,@(and tooltips-font
				(list (cons 'font tooltips-font)))))
      (setq tooltips-displayed (or win t))
      (setq tooltips-timer
	    (make-timer remove-tooltip
			(quotient tooltips-timeout-delay 1000)
			(mod tooltips-timeout-delay 1000)))
      (unless (in-hook-p 'pre-command-hook remove-tooltip)
	(add-hook 'pre-command-hook remove-tooltip))))

  (define (remove-tooltip)
    (remove-hook 'pre-command-hook remove-tooltip)
    (when tooltips-displayed
      (display-message nil)
      (setq tooltips-displayed nil))
    (when tooltips-timer
      (delete-timer tooltips-timer)
      (setq tooltips-timer nil)))

  (define (tooltips-unmapped win)
    (when (eq win tooltips-displayed)
      (remove-tooltip)))

  (add-hook 'unmap-notify-hook tooltips-unmapped)

  (define (call-after-delay thunk)
    (remove-tooltip)
    (when tooltips-enabled
      (setq tooltips-timer (make-timer (lambda ()
					 (setq tooltips-timer nil)
					 (thunk))
				       (quotient tooltips-delay 1000)
				       (mod tooltips-delay 1000)))
      (unless (in-hook-p 'pre-command-hook remove-tooltip)
	(add-hook 'pre-command-hook remove-tooltip))))

  (define (display-tooltip-after-delay . args)
    (call-after-delay (lambda ()
			(apply display-tooltip args))))

;;; frame-part tooltips

  ;; each item is (EVENT-DESC . DOC)
  (define (tooltips-format items)
    (let ((max-event-width 0)
	  (split (lambda (s)
		   (let (point parts)
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
	      (let ((parts (split (cdr cell))))
		(when parts
		  (setq out (cons (car parts) out))
		  (setq out (cons ?\n out)))
		(mapc (lambda (string)
			(setq out (cons (make-string max-event-width ? ) out))
			(setq out (cons string out))
			(setq out (cons ?\n out))) (cdr parts)))) items)
      (apply concat (nreverse out))))

  (define (command-info command)
    (let (doc)
      (if (and tooltips-show-doc-strings command
	       (symbolp command)
	       (setq doc (command-documentation command)))
	  (_ doc)
	(format nil "%S" command))))

  (define (display-fp-tooltip fp)
    (let ((keymap (frame-part-get fp 'keymap))
	  items)
      (when (symbolp keymap)
	(setq keymap (symbol-value keymap)))
      (when (cdr keymap)
	(map-keymap (lambda (cell)
		      (setq items (cons (cons (event-name (cdr cell))
					      (command-info (car cell))) items)))
		    keymap)
	(display-tooltip (tooltips-format (nreverse items))))))

  (define (tooltips-fp-enter win fp)
    (declare (unused win))
    (when tooltips-enabled
      (call-after-delay (lambda ()
			  (unless (clicked-frame-part)
			    (display-fp-tooltip fp))))))

  (add-hook 'enter-frame-part-hook tooltips-fp-enter)
  (add-hook 'leave-frame-part-hook remove-tooltip))
