;;; jump-or-exec.jl -- flexible application shortcut keys

;; Copyright (C) 2002 Damien Elmes <resolve@repose.cx>
;; Copyright (C) 2010 Christopher Roy Bratusek <nano@jpberlin.de>
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
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA.

;;; Description

;; Provides key bindings to focus a given window, or starts the
;; application if absent.
;;

(define-structure sawfish.wm.commands.jump-or-exec

    (export jump-or-exec
	    toggle-or-exec)

    (open rep
          rep.system
	  rep.regexp
          sawfish.wm.misc
          sawfish.wm.windows
	  sawfish.wm.state.iconify
	  sawfish.wm.util.display-window
	  sawfish.wm.commands)

  (define (jump-or-exec regex prog #!key match-class onfocused)
    "Jump to a window, or when absent, start program."
    (let ((wind (if match-class
		    (get-window-by-class regex #:regex t)
		  (get-window-by-name regex #:regex t)))
	  (curwin (input-focus)))
      (cond ((and onfocused
		  curwin
		  (if match-class
		      (string-match regex (window-class curwin))
		    (string-match regex (window-name curwin))))
	     ;; Exec "onfocused"
	     (cond ((commandp onfocused)
		    (call-command onfocused))
		   ((functionp onfocused)
		    (funcall onfocused wind))
		   (t (user-eval onfocused ))))
	    ;; Jump to the window.
	    ((windowp wind) (display-window wind))
	    ;; Exec "prog"
	    ((stringp prog) (system (concat prog "&")))
	    ((functionp prog) (funcall prog))
	    ((commandp prog) (call-command prog))
	    (t (user-eval prog)))))

  (define-command 'jump-or-exec jump-or-exec)

  (define (toggle-or-exec regex prog #!key match-class)
    (jump-or-exec regex prog
		  #:match-class match-class
		  #:onfocused iconify-window))
  
  (define-command 'toggle-or-exec toggle-or-exec))
