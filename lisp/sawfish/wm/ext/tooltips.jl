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

(defgroup tooltips "Tooltips")

(defcustom tooltips-enabled t
  "Display tooltips for window frames."
  :type boolean
  :group tooltips
  :require tooltips)

(defcustom tooltips-timeout-enabled nil
  "Remove tooltips after a period of time."
  :type boolean
  :group tooltips)

(defcustom tooltips-delay 500
  "Number of milliseconds before displaying tooltips."
  :type number
  :range (0 . nil)
  :group tooltips)

(defcustom tooltips-timeout-delay 5000
  "Number of milliseconds before removing tooltips."
  :type number
  :range (0 . nil)
  :group tooltips)

(defvar tooltips-background-color "lightyellow2")

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

(defun tooltips-display (win fp)
  (let
      ((text (make-string-output-stream))
       (keymap (frame-part-get fp 'keymap))
       (pos (query-pointer))
       (pos-fn (lambda (in size inc)
		 (if (< in (/ size 2))
		     (+ in inc)
		   (- (+ (- size in) inc))))))
    (when (symbolp keymap)
      (setq keymap (symbol-value keymap)))
    (map-keymap (lambda (cell)
		  (format text "<%s>   %S\n"
			  (event-name (cdr cell)) (car cell)))
		keymap)
    (setq text (get-output-stream-string text))
    (unless (string= text "")
      (rplaca pos (pos-fn (car pos) (screen-width) 0))
      (rplacd pos (pos-fn (cdr pos) (screen-height) 16))
      (display-message text `((position . ,pos)
			      (background . ,tooltips-background-color)
			      (x-justify . left)
			      (spacing . 2)))
      (setq tooltips-displayed t)
      (when tooltips-timeout-enabled 
	(setq tooltips-timer (make-timer tooltips-cleanup
					 (/ tooltips-timeout-delay 1000)
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
				       (/ tooltips-delay 1000)
				       (mod tooltips-delay 1000)))
      (unless (in-hook-p 'pre-command-hook tooltips-cleanup)
	(add-hook 'pre-command-hook tooltips-cleanup)))))

(add-hook 'enter-frame-part-hook tooltips-fp-enter)
(add-hook 'leave-frame-part-hook tooltips-cleanup)
