;; transient.jl -- support transient windows
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

;; XXX rename this file frames.jl

(provide 'transient)

(defvar fallback-frameset '((default . default-frame)
			    (shaped . default-frame)
			    (transient . nil-frame)
			    (shaped-transient . nil-frame)))

(defvar default-frameset fallback-frameset)

(defvar ignored-window-names nil
  "A list of regular expressions matching windows that don't get a frame.")

(defvar sticky-window-names nil
  "A list of regular expressions matching window names that exist across
workspaces.")

(defun window-type (w)
  (if (window-transient-p w)
      (if (window-shaped-p w)
	  'shaped-transient
	'transient)
    (if (window-shaped-p w)
	'shaped
      'default)))

;; called from the add-window-hook
(defun transient-add-window (w)
  (when (catch 'foo
	  (mapc #'(lambda (r)
		    (when (string-match r (window-name w))
		      (throw 'foo t))) ignored-window-names)
	  nil)
    (window-put w 'ignored t)
    (set-window-frame w 'nil-frame))
  (when (catch 'foo
	  (mapc #'(lambda (r)
		    (when (string-match r (window-name w))
		      (throw 'foo t))) sticky-window-names)
	  nil)
    (window-put w 'sticky t))
  (unless (window-frame w)
    (let
	((type (window-type w))
	 (set (or (window-get w 'frameset) default-frameset)))
      (set-window-frame w (or (cdr (assq type set))
			      (cdr (assq type fallback-frameset))
			      default-frame)))))

;; add the above function into the `add-window-hook' hook
(add-hook 'add-window-hook 'transient-add-window)
