#| marks.jl -- provide a way to operate on multiple windows

   $Id$

   Copyright (C) Yann Hodique <Yann.Hodique@lifl.fr>

   This file is an official accepted contribution into sawfish.

   This script is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   sawfish is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with sawfish; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
|#

(define-structure sawfish.wm.util.marks

        (export mark-window
                unmark-window
                unmark-all-windows
                apply-on-marked-windows
                marked-windows)

        (open rep
              rep.system
              sawfish.wm.misc
              sawfish.wm.custom
              sawfish.wm.commands)

  (define-structure-alias marks sawfish.wm.util.marks)

  (defvar marked-windows-list nil)

  (define (mark-window win)
    (setq marked-windows-list (append marked-windows-list (list win))))

  (define-command 'mark-window mark-window #:spec "%W")

  (define (unmark-window win)
    (setq marked-windows-list (remove win marked-windows-list)))

  (define-command 'unmark-window unmark-window #:spec "%W")

  (define (unmark-all-windows)
    (setq marked-windows-list nil))

  (define-command 'unmark-all-windows unmark-all-windows)

  (define (apply-on-marked-windows func)
    (mapcar func marked-windows-list))

  (define (marked-windows)
    (not (eq marked-windows-list nil))))
