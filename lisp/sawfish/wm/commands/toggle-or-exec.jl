;;; toggle-or-exec.jl -- turn window into drop-down-app (v0.1)

;; Copyright (C) 2010 Christopher Roy Bratusek <zanghar@freenet.de>

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

;;; Description

;; This function either focusses a given window, or starts
;; the application instead, if called on the same window again,
;; it will be hidden, optionally leaving the window may hide it, too.
;;
;; In other words: toggle-or-exec turns the window into an drop-down
;; window like for example Guake or Yakuake drop-down-terminals.
;;
;; Examples:
;;
;; NOTE: the `t' tells jump-or-exec to match on WM_CLASS
;; ( bind-keys global-keymap "W-F2"
;;   `( toggle-or-exec "Gnome-run-dialog" "gnome-run-dialog" t ) )
;;
;; NOTE: a missing `t' or a `nil' tells toggle-or-exec to
;;       match on WM_NAME
;; ( bind-keys global-keymap "W-F10"
;;   `( toggle-or-exec "Geany" "geany" nil ) )
;;
;; NOTE: first `nil' and then `t' tells toggle-or-exec to
;;       to also hide the window, when it' left
;; ( bind-keys global-keymap "W-F11"
;;   `( toggle-or-exec "Opera" nil t ) )

(define-structure sawfish.wm.commands.toggle-or-exec

    (export toggle-or-exec
            toggle-or-exec-leave)

    (open rep
          rep.system
	  rep.regexp
          sawfish.wm.misc
          sawfish.wm.windows
          sawfish.wm.events
	  sawfish.wm.util.display-window
	  sawfish.wm.state.iconify
	  sawfish.wm.commands)

  (define (toggle-or-exec re prog #!optional class exit-when-leave)
    "jump to a window matched by re, or start program otherwise."
      (let ((wind (if class
                    (get-window-by-class-re re)
                    (get-window-by-name-re re))))
      (let ((curwin (input-focus)))
            (if (string-match re (window-name curwin))
                 (toggle-or-exec-leave re class)
	         (display-window wind)))
      (if (not (windowp wind))
           (if (functionp prog)
                 (funcall prog)
                 (system (concat prog "&"))))
      (if exit-when-leave
	(progn
	  ;; is there a better way?
	  (remove-hook 'leave-notify-hook (lambda () (toggle-or-exec-leave re class)))
          (add-hook 'leave-notify-hook (lambda () (toggle-or-exec-leave re class)))))))

  (define (toggle-or-exec-leave re #!optional class)
    (let ((wind (if class
                    (get-window-by-class-re re)
                    (get-window-by-name-re re))))
      (when (eq wind (current-event-window))
         (progn
           ;; defvar here, so that in-session-changes to the value get respected
	   (defvar backup-window-animator default-window-animator)
           (setq default-window-animator 'none)
           (iconify-window wind)
           (setq default-window-animator backup-window-animator)))))

  (define-command 'toggle-or-exec toggle-or-exec #:class 'default))
