#| tabgroup.jl - emulate fluxbox tab system

   $Id: tabgroup.jl 4424 2009-02-18 21:38:42+0100 chrisb $

   Copyright (C) Yann Hodique <Yann.Hodique@lifl.fr>

   This file is an official accepted contribution into sawfish.

   This script is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 22, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.tabs.tabgroup

	(export tab-release-window
	        tab-raise-left-window
		tab-raise-right-window
		tab-find-window
		tab-rank
		tab-group-window-list
		tab-group-window)

	(open rep
	      rep.system
	      rep.data.records
	      sawfish.wm.misc
	      sawfish.wm.custom
	      sawfish.wm.commands
	      sawfish.wm.windows
	      sawfish.wm.state.iconify
	      sawfish.wm.state.shading
	      sawfish.wm.stacking)

	(define-structure-alias tabgroup sawfish.wm.tabs.tabgroup)

  (defvar tab-groups nil)

  (defvar tab-refresh-lock t)
  (defvar tab-move-lock t)

  (define-record-type :tab-group
    (tab-build-group p d wl)
    tab-group?
    (p tab-group-position)
    (d tab-group-dimensions)
    (wl tab-group-window-list))

  (define (tab-move-resize-frame-window-to win x y w h)
    "move and resize according to *frame* dimensions"
    (let* ((dim1 (window-dimensions win))
         (dim2 (window-frame-dimensions win))
         (dw (- (car dim2) (car dim1)))
         (dh (- (cdr dim2) (cdr dim1))))
      (move-resize-window-to win x y (- w dw) (- h dh))))

  (define (tab-make-new-group win)
    "Return a new group containing only win"
    (let* ((pos (window-position win))
         (dim (window-frame-dimensions win))
         (group (tab-build-group pos dim (list win))))
      (setq tab-groups (append tab-groups (cons group nil)))
      group))

  (define (tab-find-window win)
    "Return a group containing win"
    (let loop ((gr tab-groups))
       (cond
        ((null gr)
         (tab-make-new-group win)
         )
        ((member win (tab-group-window-list (car gr)))
         (car gr))
        (t
         (loop (cdr gr))))))

  (define (tab-window-group-index win)
    "Return the index of the group containing win"
    (let loop ((index 0))
       (cond
        ((eq index (length tab-groups))
         (tab-make-new-group win)
         index)
        ((member win (tab-group-window-list (nth index tab-groups)))
         index)
        (t
         (loop (+ index 1))))))

  (define (tab-rank elem list)
    (if (eq elem (car list))
      0
      (+ 1 (tab-rank elem (cdr list)))))

  (define (tab-delete-window-from-group win index)
    "Remove a window from a group at given index"
    (let* ((old (nth index tab-groups))
         (l (remove win (tab-group-window-list old))))
      (if (null l)
         (setq tab-groups (delete old tab-groups))
        (rplaca (nthcdr index tab-groups)
              (tab-build-group (tab-group-position old) (tab-group-dimensions old) l))
        (tab-refresh-group (car l) 'frame))))

  (define (tab-delete-window-from-tab-groups w)
    "Find window's group and remove it"
    (tab-delete-window-from-group w (tab-window-group-index w)))

  (define (tab-put-window-in-group win index)
    "Put window in group at given index"
    (let* ((group (nth index tab-groups))
         (dim (tab-group-dimensions group))
         (pos (tab-group-position group)))
      (rplaca (nthcdr index tab-groups)
            (tab-build-group (tab-group-position group)
                         (tab-group-dimensions group)
                         (append (tab-group-window-list group) (list win))))
      (tab-move-resize-frame-window-to win (car pos) (cdr pos) (car dim) (cdr dim))
      (rebuild-frame win)))

  (define (tab-refresh-group win prop)
    "Refresh the entire group containing win according to prop
  prop can be one of the symbols : frame, move, resize, shade, unshade"
    (when tab-refresh-lock
      (setq tab-refresh-lock nil)
      (unwind-protect
         (let* ((index (tab-window-group-index win))
                (wins (tab-group-window-list (nth index tab-groups))))
           (cond
             ((eq prop 'frame)
              (mapcar (lambda (w)
                        (rebuild-frame w)) wins))
             ((or (eq prop 'move) (eq prop 'resize))
              (let ((dim (window-frame-dimensions win))
                    (pos (window-position win)))
                (mapcar (lambda (w)
                          (tab-move-resize-frame-window-to w (car pos) (cdr pos) (car dim) (cdr dim))
                          (rebuild-frame w)) wins)
                (rplaca (nthcdr index tab-groups)
                        (tab-build-group pos dim wins))))
             ((eq prop 'stick)
              (mapcar (lambda (w)
                        (toggle-window-sticky w)) wins))
             ((eq prop 'shade)
              (mapcar (lambda (w)
                        (shade-window w)
                        (rebuild-frame w)) wins))
             ((eq prop 'unshade)
              (mapcar (lambda (w)
                        (unshade-window w)
                        (rebuild-frame w)) wins))))
      (setq tab-refresh-lock t))))

  ;; Entry points
  (define (tab-group-window w win)
    "Put active window in pointer-selected group"
    (interactive)
    (let* ((index (tab-window-group-index win))
           (index2 (tab-window-group-index w)))
      (tab-refresh-group win 'move) ;ugly hack, don't know why it's needed, but new groups are listed with pos (0,0)
      (tab-put-window-in-group w index)
      (tab-delete-window-from-group w index2)
      (tab-refresh-group w 'move)))

  (define (tab-release-window w)
    "Release active window from its group"
    (tab-delete-window-from-tab-groups w)
   (tab-make-new-group w))

  (define-command 'tab-release-window tab-release-window #:spec "%f")

  (define (tab-group-offset win n)
    "Return the window at position (pos+n) in window's group"
    (let* ((gr (tab-group-window-list (tab-find-window win)))
         (size (length gr))
         (r (tab-rank win gr)))
      (nth (modulo (+ r n) size) gr)))

  (define (tab-same-group-p w1 w2)
    "Predicate : true <=> w1 and w2 are grouped together"
    (member w1 (tab-group-window-list (tab-find-window w2))))

  (define (tab-raise-left-window)
    "Raise left window in current group"
    (let ((win (tab-group-offset (input-focus) -1)))
      (raise-window win)
      (set-input-focus win)))

  (define-command 'tab-raise-left-window tab-raise-left-window)

  (define (tab-raise-right-window)
    "Raise right window in current group"
    (let ((win (tab-group-offset (input-focus) 1)))
      (raise-window win)
      (set-input-focus win)))

  (define-command 'tab-raise-right-window tab-raise-right-window)

  (define (map-other-grouped-windows win func)
    ""
    (mapcar func
      (delete-if
        (lambda (w) (eq w win))
        (tab-group-window-list (tab-find-window win)))) )

  (unless batch-mode
    (add-hook 'window-state-change-hook
  	  (lambda (win args)
	    (if (= 'sticky args)
	      (tab-refresh-group win 'stick))))
    (add-hook 'after-move-hook (lambda (win) (tab-refresh-group win 'move)))
    (add-hook 'while-moving-hook (lambda (win) (tab-refresh-group win 'move)))
    (add-hook 'after-resize-hook (lambda (win) (tab-refresh-group win 'resize)))
    (add-hook 'while-resizing-hook (lambda (win) (tab-refresh-group win 'resize)))
    (add-hook 'window-maximized-hook (lambda (win) (tab-refresh-group win 'resize)))
    (add-hook 'window-unmaximized-hook (lambda (win) (tab-refresh-group win 'resize)))
    (add-hook 'shade-window-hook (lambda (win) (tab-refresh-group win 'shade)))
    (add-hook 'unshade-window-hook (lambda (win) (tab-refresh-group win 'unshade)))
    (add-hook 'destroy-notify-hook tab-delete-window-from-tab-groups)))
