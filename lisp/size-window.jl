;; size-window -- resizing window iteratively (like `C-x ^' in emacs)
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

(provide 'size-window)

(defcustom size-window-def-increment 16
  "Default increment for size-window commands (when the window doesn't specify
it's own increments)."
  :group move
  :user-level expert
  :type (number 1))

(defun size-window (w cols rows)
  "Increase the size of window W by COLS columns and ROWS rows (each of which
may be negative. If the window doesn't define it's increment size, then the
size of each row or column is taken from `size-window-def-increment.'"
  (let*
      ((hints (window-size-hints w))
       (dims (window-dimensions w))
       (x-base (or (cdr (or (assq 'base-width hints)
			    (assq 'min-width hints))) 1))
       (x-inc (or (cdr (assq 'width-inc hints)) size-window-def-increment))
       (y-base (or (cdr (or (assq 'base-height hints)
			    (assq 'min-height hints))) 1))
       (y-inc (or (cdr (assq 'height-inc hints)) size-window-def-increment))
       (x-max (cdr (assq 'max-width hints)))
       (y-max (cdr (assq 'max-height hints)))
       (scale (lambda (x base inc maximum)
		(min (+ base (* inc (max 0 x))) (or maximum 65535))))
       (descale (lambda (x base inc)
		  (quotient (- x base) inc))))
    ;; use the configure-request handler (to handle gravity)
    (call-window-hook
     'configure-request-hook w
     (list (list (cons 'dimensions
		       (cons (scale (+ (descale (car dims) x-base x-inc) cols)
				    x-base x-inc x-max)
			     (scale (+ (descale (cdr dims) y-base y-inc) rows)
				    y-base y-inc y-max))))))))

;;;###autoload
(defun size-window-add-row (w)
  "Increase the size of the current window by one row."
  (interactive "%W")
  (size-window w 0 +1))

;;;###autoload
(defun size-window-subtract-row (w)
  "Decrease the size of the current window by one row."
  (interactive "%W")
  (size-window w 0 -1))

;;;###autoload
(defun size-window-add-column (w)
  "Increase the size of the current window by one column."
  (interactive "%W")
  (size-window w +1 0))

;;;###autoload
(defun size-window-subtract-column (w)
  "Decrease the size of the current window by one column."
  (interactive "%W")
  (size-window w -1 0))
