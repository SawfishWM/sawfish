;;; jump-or-exec.jl 1.0.0 -- flexible application shortcut keys

;; Copyright (C) 2002 Damien Elmes <resolve@repose.cx>
;; Copyright (C) 2010 Christopher Roy Bratusek <zanghar@freenet.de>
;; Copyright (C) 2010 Teika Kazura <teika@lavabit.com>

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
;; the application instead.
;;
;; Examples:
;;
;; => application gnome-run-dialog matched on it's WM_CLASS
;; ( bind-keys global-keymap "W-F2"
;;   `( jump-or-exec "Gnome-run-dialog" "gnome-run-dialog" #:match-class t ) )
;;
;; => application geany matched on it's WM_NAME
;; ( bind-keys global-keymap "W-F10"
;;   `( jump-or-exec "Geany" "geany" #:match-class nil ) )
;;
;; NOTE: `#:match-class nil' can be skipped, as it's nil by default
;;
;; In version 1.0.0 toggle-or-exec was merged into jump-or-exec. It's basically
;; the same, but it turns windows into drop-down-terminal like ones, that means
;; then you press the key while on the corresponding window, it will be hidden.
;; Optionally you may add a window-matcher, wich will also hide the window when
;; you leave it (not done by default).
;;
;; Examples:
;;
;; => application dolphin matched on it's WM_NAME 
;;  => will be iconified when key pressed while it's focused
;;  ( bind-keys global-keymap "Home" 
;;    `( toggle-or-exec "Dolphin" "dolphin ~" ) 
;;
;; => application konsole matched on it's WM_CLASS
;;  => will be iconified when key pressed while it's focused
;;  => will also be iconified when the cursor leaves it
;;  ( bind-keys global-keymap "F12"
;;    `( toggle-or-exec "Konsole" "konsole" #:match-class t ) 
;;
;;  ( add-window-matcher '( ( WM_CLASS . "^Konsole/konsole$" ) )
;;    '( ( iconify-on-leave .t ) ) )

(define-structure sawfish.wm.commands.jump-or-exec

    (export jump-or-exec
	    jump-or-exec-leave
	    toggle-or-exec)

    (open rep
          rep.system
	  rep.regexp
          sawfish.wm.misc
          sawfish.wm.windows
	  sawfish.wm.events
	  sawfish.wm.state.iconify
	  sawfish.wm.util.display-window
	  sawfish.wm.commands)

  (define (jump-or-exec re prog #!key match-class onfocused)
    "jump to a window matched by re, or start program otherwise."
    (catch 'return
      (let ((wind (if match-class
                    (get-window-by-class-re re)
                    (get-window-by-name-re re))))
        (if (functionp onfocused) ; check if already focused
            (let ((curwin (input-focus)))
              (if curwin
                  (if (or (string-match re (window-class curwin))
			  (string-match re (window-name curwin)))
		      (progn
			;; shouldn't it be curwin
                        (funcall onfocused wind)
                        (throw 'return))))))
        (if (windowp wind)
            (display-window wind)
          (if (functionp prog)
                (funcall prog)
              (system (concat prog "&")))))))

  (define-command 'jump-or-exec jump-or-exec #:class 'default)

  (define (jump-or-exec-leave)
    (let ((default-window-animator 'none))
      (iconify-window (input-focus))))

  (define (toggle-or-exec re prog #!key match-class)
    (if match-class
        (jump-or-exec re prog #:match-class t #:onfocused jump-or-exec-leave)
      (jump-or-exec re prog #:onfocused jump-or-exec-leave)))

  (define (jump-or-exec-hook)
    (if (and (not (eq (current-event-window) 'root)) ;; may error on startup else
	     (window-get (current-event-window) 'iconify-on-leave))
      (let ((default-window-animator 'none)) ;; no animator
	(iconify-window (current-event-window)))))

  (add-hook 'leave-notify-hook jump-or-exec-hook))
