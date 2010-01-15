;;; jump-or-exec.jl --- flexible application shortcut keys (v0.2)

;; Copyright (C) 2002 Damien Elmes <resolve@repose.cx>

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
;; NOTE: the `t' tells jump-or-exec to match on WM_CLASS
;; ( bind-keys global-keymap "W-F2"
;;   `( jump-or-exec "Gnome-run-dialog" "gnome-run-dialog" t ) )
;;
;; NOTE: a missing `t' or a `nil' (the later is required if making
;;       use of the `onfocused' arguement) tells jump-or-exec to
;;       match on WM_NAME
;; ( bind-keys global-keymap "W-F10"
;;   `( jump-or-exec "Geany" "geany" nil ) )

(define-structure sawfish.wm.commands.jump-or-exec

    (export jump-or-exec)

    (open rep
          rep.system
	  rep.regexp
          sawfish.wm.misc
          sawfish.wm.windows
	  sawfish.wm.util.display-window
	  sawfish.wm.commands)

  (define (jump-or-exec re prog #!optional class onfocused)
    "jump to a window matched by re, or start program otherwise."
    (catch 'return
      (let ((wind (if class
                    (get-window-by-class-re re)
                    (get-window-by-name-re re))))
        (if (functionp onfocused) ; check if already focused
            (let ((curwin (input-focus)))
              (if curwin
                  (if (string-match re (window-name curwin))
                      (progn
                        (funcall onfocused wind)
                        (throw 'return))))))
        (if (windowp wind)
            (display-window wind)
          (if (functionp prog)
                (funcall prog)
              (system (concat prog "&")))))))

  (define-command 'jump-or-exec jump-or-exec #:class 'default))
