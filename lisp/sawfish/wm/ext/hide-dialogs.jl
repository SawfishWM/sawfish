;; hide-dialogs.jl -- only show dialog windows for the focused group
;;
;; Copyright (C) 2001 Eazel, Inc.
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
;;
;; Authors: John Harper <jsh@eazel.com>

;; Commentary:

;; Small module to implement the NeXTSTEP feature Robin mentioned.

;; The idea is to minimize dialog windows for all groups but the group
;; containing the currently focused windows.

(define-structure sawfish.wm.ext.hide-dialogs

    (define )

    (open rep
          rep.system
          rep.io.timers
          sawfish.wm.util.groups
          sawfish.wm.state.iconify
          sawfish.wm.state.transient
          sawfish.wm.windows)

  (define (dialogs-in-group w)
    (filter window-transient-p (windows-in-group w)))

  (define (hide-dialogs w)
    (let ((iconify-group-mode nil))
      (mapc iconify-window (dialogs-in-group w))))

  (define (show-dialogs w)
    (let ((focus-windows-on-uniconify nil)
	  (raise-windows-on-uniconify nil)
	  (uniconify-to-current-workspace nil)
	  (uniconify-group-mode nil))
      (mapc uniconify-window (dialogs-in-group w))))

  (define (focus-in-group w)
    (and (input-focus)
	 (memq (input-focus) (windows-in-group w))))

  (define (focus-in w mode)
    (when (eq mode 'normal)
      (show-dialogs w)))

  (define (focus-out w mode)
    (when (eq mode 'normal)
      (make-timer (lambda ()
		    (unless (focus-in-group w)
		      (hide-dialogs w)))
		  0 100)))

  (add-hook 'focus-in-hook focus-in)
  (add-hook 'focus-out-hook focus-out))
