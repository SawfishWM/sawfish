;; old-window-menu.jl

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
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; This is the "old" window menu. By default, beos-window-menu is used
;; instead. But **DON'T** remove. There sure exist those who use it,
;; according to a questionnaire.

;; Usage: add (require 'sawfish.wm.ext.old-window-menu) in your rc file.

(define-structure sawfish.wm.ext.old-window-menu

    (export old-window-menu)

    (open rep
	  rep.util.utf8
	  sawfish.wm.windows
	  sawfish.wm.workspace
	  sawfish.wm.misc
	  sawfish.wm.menus
	  sawfish.wm.util.display-window)

  (define-structure-alias old-window-menu sawfish.wm.ext.old-window-menu)

  (define (make-label w)
    (let ((name (window-name w)))
      (quote-menu-item (concat (and (window-get w 'iconified) ?\[)
			       (if (> (utf8-string-length name) 20)
				   (concat (utf8-substring name 0 20) "...")
				 name)
			       (and (window-get w 'iconified)  ?\])))))

  (define (old-window-menu)
    (let* ((limits (workspace-limits))
	   (windows (managed-windows))
	   menu)
      (do ((i (car limits) (1+ i)))
	  ((> i (cdr limits)))
	(mapc (lambda (w)
		(when (and (window-in-workspace-p w i)
			   (window-mapped-p w)
			   (not (window-get w 'window-list-skip))
			   (or (not (window-get w 'ignored))
			       (window-get w 'iconified)))
		  (setq menu (cons (list (make-label w)
					 (lambda ()
					   (when (windowp w)
					     (display-window w)))
					 (cons 'check (eq (input-focus) w))
					 '(group . old-window-menu))
				   menu))))
	      windows)
	(unless (or (= i (cdr limits)) (null (car menu)))
	  (setq menu (cons nil menu))))
      ;; search for any iconified windows that aren't anywhere else in the menu
      (let (extra)
	(mapc (lambda (w)
		(when (and (not (window-get w 'window-list-skip))
			   (window-get w 'iconified)
			   (window-get w 'sticky))
		  (setq extra (cons (list (make-label w)
					  `(display-window
					    (get-window-by-id ,(window-id w))))
				    extra))))
	      windows)
	(when extra
	  (setq menu (if menu (nconc extra (list nil) menu) extra))))
      (nreverse menu)))

  (setq window-menu old-window-menu))
