;; mwm.jl -- handle some of the Motif hints
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

;; Commentary:

;; There's a conflict between the way that sawmill allows frame-styles
;; to be defined, and the way that the window hints (gnome, mwm, ol,
;; ...) ask for particular decorations.

;; One solution might be to add a `class' attibute to each frame part.
;; These would be things like title, border, close, etc... Then if the
;; window has asked for that decor include it, otherwise don't.

;; But this doesn't work because we may be left with holes in the
;; window frame. For example, leaving the close button out of a
;; brushed-metal frame will just take a chunk out of the window corner.

;; For the moment I'll just have to mash the window hints into the four
;; available window types: default, shaped, transient, shaped-transient

(define-structure sawfish.wm.state.mwm ()

    (open rep
	  rep.system
	  sawfish.wm.windows
	  sawfish.wm.misc
	  sawfish.wm.frames)

;;; Hint definitions

  ;; Motif window hints are [FLAGS FUNCTIONS DECORATIONS INPUT-MODE STATUS],
  ;; first three are CARD32, last is INT32, last is CARD32
  (defconst mwm-window-hint-flags 0)
  (defconst mwm-window-hint-functions 1)
  (defconst mwm-window-hint-decorations 2)
  (defconst mwm-window-hint-input-mode 3)
  (defconst mwm-window-hint-status 4)

  ;; Flags define which hints are set
  (defconst mwm-flag-functions 1)
  (defconst mwm-flag-decorations 2)

  ;; Definitions for functions hint
  (defconst mwm-func-all 1)
  (defconst mwm-func-resize 2)
  (defconst mwm-func-move 4)
  (defconst mwm-func-minimize 8)
  (defconst mwm-func-maximize 16)
  (defconst mwm-func-close 32)

  ;; Definitions for decorations hint
  (defconst mwm-decor-all 1)
  (defconst mwm-decor-border 2)
  (defconst mwm-decor-resizeh 4)
  (defconst mwm-decor-title 8)
  (defconst mwm-decor-menu 16)
  (defconst mwm-decor-minimize 32)
  (defconst mwm-decor-maximize 64)

  ;; Definitions for input-mode hint
  (defconst mwm-input-modeless 0)
  (defconst mwm-input-primary-app-modal 1)
  (defconst mwm-input-system-modal 2)
  (defconst mwm-input-full-app-modal 3)

;;; Acting on the hints

  ;; this must be called _before_ the frames.jl add-window function
  (define (mwm-add-window w)
    (let* ((hints (get-x-property w '_MOTIF_WM_HINTS))
	   (type (window-type w))
	   data)
      ;; XXX act on input-mode hints...
      (when hints
	(setq data (nth 2 hints))
	(unless (zerop (logand (aref data mwm-window-hint-flags)
			       mwm-flag-decorations))
	  ;; decor hints supplied
	  (let ((decor (aref data mwm-window-hint-decorations)))
	    (if (zerop (logand decor mwm-decor-all))
		(progn
		  (when (zerop (logand decor mwm-decor-border))
		    (setq type (window-type-remove-border type)))
		  (when (zerop (logand decor mwm-decor-title))
		    (setq type (window-type-remove-title type)))
		  (when (zerop (logand decor mwm-decor-menu))
		    (remove-frame-class w 'menu-button))
		  (when (zerop (logand decor mwm-decor-minimize))
		    (remove-frame-class w 'iconify-button))
		  (when (zerop (logand decor mwm-decor-maximize))
		    (remove-frame-class w 'maximize-button)))
	      (setq type 'default))))
	(unless (zerop (logand (aref data mwm-window-hint-flags)
			       mwm-flag-functions))
	  ;; function hints supplied
	  (let ((func (aref data mwm-window-hint-functions)))
	    (when (zerop (logand func mwm-func-all))
	      (when (zerop (logand func mwm-func-minimize))
		(remove-frame-class w 'iconify-button))
	      (when (zerop (logand func mwm-func-maximize))
		(remove-frame-class w 'maximize-button))
	      (when (zerop (logand func mwm-func-close))
		(remove-frame-class w 'close-button))))))
      (set-window-type w type)))

  (add-hook 'before-add-window-hook mwm-add-window))
