;; make-theme-preview.jl -- previewing for theme builder
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

(define-structure sawfish.wm.theming.make-theme-preview

    (export make-theme-preview)

    (open rep
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.theming.make-theme
	  sawfish.wm.frames)

  (define preview-window nil)
  (define preview-type 'default)
  (define preview-theme nil)

  (add-hook 'before-add-window-hook
	    (lambda (w)
	      (let
		  ((class (get-x-text-property w 'WM_CLASS)))
		(when (and (>= (length class) 2)
			   (string= (aref class 1) "SawfishThemer")
			   (string= (aref class 0) "preview"))
		  (setq preview-window w)
		  (when preview-theme
		    (set-window-frame w (preview-theme w preview-type)))))))

  (add-hook 'destroy-notify-hook
	    (lambda (w)
	      (when (eq w preview-window)
		(setq preview-window nil))))

  (defun make-theme-preview (patterns frames mappings #!optional type)
    (let ((theme (make-theme patterns frames mappings)))
      (setq preview-theme theme)
      (when type
	(setq preview-type type))
      (when preview-window
	(set-window-frame preview-window
			  (theme preview-window preview-type))))))
